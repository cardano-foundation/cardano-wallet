{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeSlotOfUTCTime
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
    , slotsToUTCTimes
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , TxSummary (..)
    , ValueTransfer (ValueTransfer)
    )
import Cardano.Wallet.Deposit.Read
    ( Slot
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , customerAddress
    )
import Cardano.Wallet.Read
    ( Coin
    , SlotNo (..)
    , WithOrigin (..)
    , slotFromChainPoint
    , txIdFromHash
    )
import Cardano.Wallet.Read.Hash
    ( hashFromStringAsHex
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    )
import Cardano.Wallet.UI.Deposit.API
    ( TransactionHistoryParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Cardano.Wallet.UI.Lib.TimeWindow
    ( Match (..)
    , filterByDirection
    , sortByDirection
    , utcTimeByDirection
    )
import Control.Monad
    ( replicateM
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Foldable
    ( toList
    )
import Data.Function
    ( on
    )
import Data.List
    ( sortBy
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromJust
    )
import Data.Time
    ( UTCTime (..)
    , getCurrentTime
    )
import Servant
    ( Handler
    )
import System.Random.Stateful
    ( StatefulGen
    , UniformRange (..)
    , mkStdGen
    , runStateGen_
    )

import qualified Cardano.Wallet.Deposit.REST as REST
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

getCustomerHistory
    :: NetworkEnv IO a
    -> SessionLayer WalletResource
    -> ( Bool
         -> TransactionHistoryParams
         -> Map Slot (WithOrigin UTCTime)
         -> [TxSummary]
         -> html
       )
    -> (BL.ByteString -> html)
    -> TransactionHistoryParams
    -> Handler html
getCustomerHistory
    network
    layer
    render
    alert
    params@TransactionHistoryParams{..} = do
        catchRunWalletResourceHtml layer alert id $ do
            r <- customerAddress txHistoryCustomer
            case r of
                Nothing -> pure $ alert "Address not discovered"
                Just _ -> do
                    h <- REST.getCustomerHistory txHistoryCustomer
                    (b, summaries) <-
                        liftIO
                            $ fakeData txHistoryCustomer . toList
                            $ h
                    let slots =
                            Set.fromList
                                $ slotFromChainPoint
                                    . txChainPoint
                                    <$> summaries
                    times <- liftIO $ slotsToUTCTimes network slots
                    pure
                        $ render b params times
                        $ filterByParams params times summaries

filterByParams
    :: TransactionHistoryParams
    -> Map Slot (WithOrigin UTCTime)
    -> [TxSummary]
    -> [TxSummary]
filterByParams TransactionHistoryParams{..} times =
    sortByDirection txHistorySorting txChainPoint
        . filterByDirection
            txHistorySorting
            startTime
            matchUTCTime
        . filterByTransfer
  where
    startTime =
        utcTimeByDirection
            txHistorySorting
            txHistoryStartYear
            txHistoryStartMonth
    matchUTCTime :: TxSummary -> Match UTCTime
    matchUTCTime TxSummaryC{txChainPoint = cp} =
        do
            let slot :: Slot = slotFromChainPoint cp
            case Map.lookup slot times of
                Just (At t) -> Match t
                Just Origin -> DirectionMatch
                _ -> NoMatch
    filterByTransfer = case (txHistoryReceived, txHistorySpent) of
        (True, False) ->
            filter
                ( \TxSummaryC{txTransfer = ValueTransfer _ received} ->
                    received /= mempty
                )
        (False, True) ->
            filter
                ( \TxSummaryC{txTransfer = ValueTransfer spent _} ->
                    spent /= mempty
                )
        _ -> id

-- fake data generation until DB is implemented

fakeData :: Customer -> [TxSummary] -> IO (Bool, [TxSummary])
fakeData c [] = do
    now <- getCurrentTime
    pure
        $ (True,)
        $ sortBy
            (on compare $ \(TxSummaryC _ cp _) -> cp)
        $ txSummaryG now (fromIntegral c)
fakeData _c xs = pure (False, xs)

unsafeMkTxId :: String -> Read.TxId
unsafeMkTxId = txIdFromHash . fromJust . hashFromStringAsHex

hexOfInt :: Int -> Char
hexOfInt n = "0123456789abcdef" !! (n `mod` 16)

headerHash :: StatefulGen g m => g -> m [Char]
headerHash g = replicateM 64 $ hexOfInt <$> uniformRM (0, 15) g

randomValue :: StatefulGen g f => g -> Read.Coin -> f Read.Value
randomValue g l = Read.ValueC <$> uniformRM (0, l) g <*> pure mempty

maxLovelaces :: Coin
maxLovelaces = 1_000_000_000

createSpent :: StatefulGen g f => g -> Int -> f Read.Value
createSpent g r = randomValue g l
  where
    l = if r >= 0 && r < 5 || r == 11 then maxLovelaces else 0

createReceived :: StatefulGen g f => g -> Int -> f Read.Value
createReceived g r = randomValue g l
  where
    l = if r >= 5 && r <= 11 then maxLovelaces else 0

valueTransferG :: StatefulGen g m => g -> m ValueTransfer
valueTransferG g = do
    spentOrReceived <- uniformRM (0, 11) g
    spent <- createSpent g spentOrReceived
    received <- createReceived g spentOrReceived
    pure $ ValueTransfer spent received

txSummaryG :: UTCTime -> Int -> [TxSummary]
txSummaryG now c = runStateGen_ pureGen $ \g -> do
    ns <- uniformRM (1, 200) g
    replicateM ns $ do
        txId <- txIdR g
        cp <- chainPointR g
        value <- valueTransferG g
        pure $ TxSummaryC txId cp value
  where
    pureGen = mkStdGen c

    chainPointR g = do
        case unsafeSlotOfUTCTime now of
            Origin -> pure Read.GenesisPoint
            At (SlotNo slotNo) -> do
                slotInt :: Int <- uniformRM (-1, fromIntegral slotNo) g
                r <- hashFromStringAsHex <$> headerHash g
                case r of
                    Just h -> do
                        pure
                            $ if slotInt == -1
                                then Read.GenesisPoint
                                else
                                    Read.BlockPoint
                                        (SlotNo $ fromIntegral slotInt)
                                        h
                    Nothing -> error "chainPointR: invalid hash"
txIdR :: StatefulGen g m => g -> m Read.TxId
txIdR g = do
    ls <-
        fmap (concatMap $ replicate 8)
            $ replicateM 8
            $ hexOfInt <$> uniformRM (0, 15) g
    pure $ unsafeMkTxId ls
