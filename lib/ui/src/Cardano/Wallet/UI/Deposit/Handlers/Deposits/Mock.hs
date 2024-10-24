module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Mock
    ( getMockDepositsByTime
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeUTCTimeOfSlot
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , ResolveAddress
    , ResolveSlot
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory.Mock
    ( mockTxHistoryByTime
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResourceM
    , addressToCustomer
    , listCustomers
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
import Data.Map.Strict
    ()
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime (..)
    , diffUTCTime
    , getCurrentTime
    , secondsToNominalDiffTime
    )

nMockDeposits :: Int
nMockDeposits = 1000

getMockDepositsByTime :: WalletResourceM ByTime
getMockDepositsByTime = getMockDepositsByTimeWithCount nMockDeposits

getMockDepositsByTimeWithCount
    :: Int
    -- ^ Number of deposits to generate
    -> WalletResourceM ByTime
getMockDepositsByTimeWithCount nDeposits = do
    history <- liftIO $ newTVarIO Nothing
    addresses <- fmap snd <$> listCustomers
    solveAddress <- addressToCustomer
    let solveSlot = fmap Down <$> unsafeUTCTimeOfSlot
    liftIO
        $ getCachedMockDeposits
            history
            solveAddress
            solveSlot
            nDeposits
            addresses

getCachedMockDeposits
    :: TVar (Maybe (UTCTime, [Address], ByTime))
    -> ResolveAddress
    -> ResolveSlot
    -> Int
    -> [Address]
    -> IO ByTime
getCachedMockDeposits fakeDepositsCache solveAddress solveSlot nDeposits addresses = do
    now <- getCurrentTime
    cache <- readTVarIO fakeDepositsCache
    let generate = do
            let newDeposits =
                    mockTxHistoryByTime
                        now
                        solveAddress
                        solveSlot
                        addresses
                        nDeposits
            atomically
                $ writeTVar fakeDepositsCache
                $ Just (now, addresses, newDeposits)
            pure newDeposits
    case cache of
        Just (now', addresses', deposits)
            | diffUTCTime now now'
                < secondsToNominalDiffTime 60 -- better not do it on the full set TODO
                && addresses' == addresses -> do
                putStrLn "Using cached fake deposits"
                pure deposits
        Just _ -> do
            putStrLn "Regenerating fake deposits"
            generate
        Nothing -> do
            putStrLn "Generating fake deposits"
            generate
