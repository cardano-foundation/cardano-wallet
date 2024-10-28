module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Mock
    ( getMockHistory
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ResolveAddress
    , ResolveSlot
    , TxHistory
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory.Mock
    ( mockTxHistory
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResourceM
    , addressToCustomer
    , listCustomers
    )
import Cardano.Wallet.Deposit.Time
    ( unsafeUTCTimeOfSlot
    )
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime (..)
    , diffUTCTime
    , getCurrentTime
    , secondsToNominalDiffTime
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )

nMockDeposits :: Int
nMockDeposits = 50000

type TxHistoryCache = TVar (Maybe (UTCTime, [Address], TxHistory))

{-# NOINLINE globalTxHistoryMock #-}
globalTxHistoryMock :: TxHistoryCache
globalTxHistoryMock = unsafePerformIO $ newTVarIO Nothing

getMockHistory :: WalletResourceM TxHistory
getMockHistory = getMockDepositsByTimeWithCount nMockDeposits

getMockDepositsByTimeWithCount
    :: Int
    -- ^ Number of deposits to generate
    -> WalletResourceM TxHistory
getMockDepositsByTimeWithCount nDeposits = do
    addresses <- fmap snd <$> listCustomers
    solveAddress <- addressToCustomer
    let solveSlot = fmap Down <$> unsafeUTCTimeOfSlot
    liftIO
        $ getCachedMockDeposits
            solveAddress
            solveSlot
            nDeposits
            addresses

getCachedMockDeposits
    :: ResolveAddress
    -> ResolveSlot
    -> Int
    -> [Address]
    -> IO TxHistory
getCachedMockDeposits solveAddress solveSlot nDeposits addresses = do
    now <- getCurrentTime
    cache <- readTVarIO globalTxHistoryMock
    let generate = do
            let newDeposits =
                    mockTxHistory
                        now
                        solveAddress
                        solveSlot
                        addresses
                        nDeposits
            atomically
                $ writeTVar globalTxHistoryMock
                $ Just (now, addresses, newDeposits)
            pure newDeposits
    case cache of
        Just (now', addresses', deposits)
            | diffUTCTime now now'
                < secondsToNominalDiffTime 600
                && addresses' == addresses -> do
                putStrLn "Using cached fake deposits"
                pure deposits
        Just _ -> do
            putStrLn "Regenerating fake deposits"
            generate
        Nothing -> do
            putStrLn "Generating fake deposits"
            generate
