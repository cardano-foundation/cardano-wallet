{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Fake
    ( getFakeDepositsHistory
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeSlotOfUTCTime
    , unsafeUTCTimeOfSlot
    )
import Cardano.Wallet.Deposit.Map
    ( Map (..)
    , singletonMap
    , singletonPatched
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , ResolveAddress
    , ResolveSlot
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResourceM
    , listCustomers
    )
import Cardano.Wallet.Read
    ( SlotNo (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( txIdR
    , valueTransferG
    )
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Monad
    ( replicateM
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Map.Strict
    ()
import Data.Monoid
    ( First (..)
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
import GHC.IO
    ( unsafePerformIO
    )
import System.Random.MWC.Distributions
    ( standard
    )
import System.Random.Stateful
    ( UniformRange (uniformRM)
    , mkStdGen
    , runStateGen_
    )

import qualified Data.Map.Strict as Map

type FakeDepositsCache = TVar (Maybe (UTCTime, [Address], ByTime))

getFakeDepositsHistory
    :: WalletResourceM ByTime
getFakeDepositsHistory = do
    addresses <- listCustomers
    let solveAddress x =
            Map.lookup x
                $ Map.fromList
                $ fmap (\(c, a) -> (a, c)) addresses
    liftIO
        $ fakeDeposits
            solveAddress
            (fmap Down <$> unsafeUTCTimeOfSlot)
        $ snd <$> addresses

{-# NOINLINE fakeDepositsCache #-}
fakeDepositsCache :: FakeDepositsCache
fakeDepositsCache = unsafePerformIO $ newTVarIO Nothing

fakeDeposits
    :: ResolveAddress
    -> ResolveSlot
    -> [Address]
    -> IO ByTime
fakeDeposits solveAddress solveSlot addresses = do
    now <- getCurrentTime
    cache <- readTVarIO fakeDepositsCache
    let generate = do
            let newDeposits = fakeDepositsCreate now solveAddress solveSlot addresses
            atomically $ writeTVar fakeDepositsCache $ Just (now, addresses, newDeposits)
            pure newDeposits
    case cache of
        Just (now', addresses', deposits)
            | diffUTCTime now now'
                < secondsToNominalDiffTime 6000000 -- better not do it on the full set TODO
                && addresses' == addresses -> do
                putStrLn "Using cached fake deposits"
                pure deposits
        Just _ -> do
            putStrLn "Regenerating fake deposits"
            generate
        Nothing -> do
            putStrLn "Generating fake deposits"
            generate

fakeDepositsCreate
    :: UTCTime
    -> ResolveAddress
    -> ResolveSlot
    -> [Address]
    -> ByTime
fakeDepositsCreate now solveAddress solveSlot addresses =
    runStateGen_ (mkStdGen 0) $ \g -> do
        let ns = 10000
        fmap mconcat
            $ replicateM ns
            $ do
                slot <- case unsafeSlotOfUTCTime now of
                    Origin -> pure Origin
                    At (SlotNo n) -> do
                        slotInt <-
                            floor
                                . (fromIntegral n -)
                                . (* fromIntegral n)
                                . (abs)
                                <$> standard g
                        -- _ <- error $ show slotInt
                        pure
                            $ if slotInt < 0
                                then Origin
                                else At (SlotNo $ fromIntegral @Integer slotInt)
                addressNumber <- uniformRM (0, length addresses - 1) g
                let address = addresses !! addressNumber
                value <- valueTransferG g
                txId <- txIdR g
                let customer = case solveAddress address of
                        Just c -> c
                        Nothing -> error "fakeDepositsCreate: address not found"
                let time = case solveSlot slot of
                        Just t -> t
                        Nothing -> error "fakeDepositsCreate: slot not found"
                pure
                    $ singletonMap time
                    $ singletonPatched (First $ Just slot) customer
                    $ singletonPatched (First $ Just address) txId
                    $ Value value
