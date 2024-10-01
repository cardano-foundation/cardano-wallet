{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
where

import Prelude

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
    ( SlotNo (..)
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
    ( UTCTime
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
import qualified Data.Set as Set

getCustomerHistory
    :: NetworkEnv IO a
    -> SessionLayer WalletResource
    -> (Bool -> TransactionHistoryParams -> [TxSummary] -> Map Slot UTCTime -> html)
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
            h <- REST.getCustomerHistory txHistoryCustomer
            r <- customerAddress txHistoryCustomer
            case r of
                Nothing -> pure $ alert "Address not discovered"
                Just _ -> do
                    let (b, ss) = fakeData txHistoryCustomer . toList $ h
                        slots =
                            Set.fromList
                                $ slotFromChainPoint . txChainPoint <$> ss
                        filtered = case (txHistoryReceived, txHistorySpent) of
                            (True, False) -> filter
                                (\TxSummaryC{txTransfer = ValueTransfer _ received}
                                    -> received /= mempty)
                                ss
                            (False, True) -> filter
                                (\TxSummaryC{txTransfer = ValueTransfer spent _}
                                    -> spent /= mempty) ss
                            _ -> ss

                    times <- liftIO $ slotsToUTCTimes network slots
                    pure $ render b params filtered times

-- fake data generation until DB is implemented

fakeData :: Customer -> [TxSummary] -> (Bool, [TxSummary])
fakeData c [] =
    (True,)
        $ sortBy
            (on compare $ \(TxSummaryC _ cp _) -> cp)
        $ txSummaryG (fromIntegral c)
fakeData _c xs = (False, xs)

unsafeMkTxId :: String -> Read.TxId
unsafeMkTxId = txIdFromHash . fromJust . hashFromStringAsHex

hexOfInt :: Int -> Char
hexOfInt n = "0123456789abcdef" !! (n `mod` 16)

randomValue :: StatefulGen g f => g -> Read.Coin -> f Read.Value
randomValue g l = Read.ValueC <$> uniformRM (0, l) g <*> pure mempty

createSpent :: StatefulGen g f => g -> Int -> f Read.Value
createSpent g r = randomValue g l
  where
    l = if r >= 0 && r < 5 || r == 11 then 1000 else 0

createReceived :: StatefulGen g f => g -> Int -> f Read.Value
createReceived g r = randomValue g l
  where
    l = if r >= 5 && r <= 11 then 1000 else 0

txSummaryG :: Int -> [TxSummary]
txSummaryG c = runStateGen_ pureGen $ \g -> do
    ns <- uniformRM (1, 10) g
    replicateM ns $ do
        txId <- txIdR g
        cp <- chainPointR g
        spentOrReceived <- uniformRM (0, 11) g
        spent <- createSpent g spentOrReceived
        received <- createReceived g spentOrReceived

        pure $ TxSummaryC txId cp $ ValueTransfer spent received
  where
    pureGen = mkStdGen c
    txIdR g = do
        ls <- replicateM 64 $ hexOfInt <$> uniformRM (0, 15) g
        pure $ unsafeMkTxId ls
    headerHash g = replicateM 64 $ hexOfInt <$> uniformRM (0, 15) g
    chainPointR g = do
        slot <- uniformRM (0, 100) g
        case slot of
            0 -> pure Read.GenesisPoint
            _ -> do
                r <- hashFromStringAsHex <$> headerHash g
                case r of
                    Just h -> pure $ Read.BlockPoint (SlotNo slot) h
                    Nothing -> error "chainPointR: invalid hash"
