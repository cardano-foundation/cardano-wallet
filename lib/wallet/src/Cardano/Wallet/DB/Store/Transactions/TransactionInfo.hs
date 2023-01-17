{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.DB.Store.Transactions.TransactionInfo
    ( mkTransactionInfoFromRelation
    , mkTransactionInfoFromReadTx
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..), TxIn (..), TxMeta (..), TxWithdrawal (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( SubmissionMeta )
import Cardano.Wallet.DB.Store.Transactions.Decoration
    ( DecoratedTxIns, lookupTxOut, mkTxOutKey, mkTxOutKeyCollateral )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation (..), fromTxCollateralOut, fromTxOut, txCBORPrism )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, interpretQuery, slotToUTCTime )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxCBOR, TxMeta (..) )
import Cardano.Wallet.Read.Eras
    ( EraFun, EraValue, K, applyEraFun, extractEraValue )
import Cardano.Wallet.Read.Tx.Hash
    ( getEraTxHash )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )

import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as WC
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as WT
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.Generics.Internal.VL as L
import qualified Data.Map.Strict as Map

-- | Compute a high level view of a transaction known as 'TransactionInfo'
-- from a 'TxMeta' and a 'TxRelation'.
-- Assumes that these data refer to the same 'TxId', does /not/ check this.
mkTransactionInfoFromRelation :: Monad m
    => TimeInterpreter m
    -> W.BlockHeader
    -> TxRelation
    -> DecoratedTxIns
    -> DB.TxMeta
    -> m WT.TransactionInfo
mkTransactionInfoFromRelation ti tip TxRelation{..} decor DB.TxMeta{..} = do
    txTime <- interpretQuery ti . slotToUTCTime $ txMetaSlot
    return
        $ WT.TransactionInfo
        { WT.txInfoId = getTxId txMetaTxId
        , WT.txInfoCBOR = cbor >>= mkTxCBOR
        , WT.txInfoFee = WC.Coin . fromIntegral <$> txMetaFee
        , WT.txInfoInputs = mkTxIn <$> ins
        , WT.txInfoCollateralInputs = mkTxCollateral <$> collateralIns
        , WT.txInfoOutputs = fromTxOut <$> outs
        , WT.txInfoCollateralOutput = fromTxCollateralOut <$> collateralOuts
        , WT.txInfoWithdrawals = Map.fromList $ map mkTxWithdrawal withdrawals
        , WT.txInfoMeta = WT.TxMeta
              { WT.status = txMetaStatus
              , WT.direction = txMetaDirection
              , WT.slotNo = txMetaSlot
              , WT.blockHeight = Quantity (txMetaBlockHeight)
              , amount = txMetaAmount
              , WT.expiry = txMetaSlotExpires
              }
        , WT.txInfoMetadata = txMetadata
        , WT.txInfoDepth = Quantity
              $ fromIntegral
              $ if tipH > txMetaBlockHeight
                  then tipH - txMetaBlockHeight
                  else 0
        , WT.txInfoTime = txTime
        , WT.txInfoScriptValidity = txMetaScriptValidity <&> \case
              False -> WT.TxScriptInvalid
              True -> WT.TxScriptValid
        }
  where
    tipH = getQuantity $ tip ^. #blockHeight
    mkTxIn tx =
        ( WT.TxIn
          { inputId = getTxId (txInputSourceTxId tx)
          , inputIx = txInputSourceIndex tx
          }
        , lookupTxOut (mkTxOutKey tx) decor
        )
    mkTxCollateral tx =
        ( WT.TxIn
          { inputId = getTxId (txCollateralSourceTxId tx)
          , inputIx = txCollateralSourceIndex tx
          }
        , lookupTxOut (mkTxOutKeyCollateral tx) decor
        )
    mkTxWithdrawal w = (txWithdrawalAccount w, txWithdrawalAmount w)

    mkTxCBOR :: DB.CBOR -> Maybe TxCBOR
    mkTxCBOR = either (const Nothing) (Just . snd) . L.match txCBORPrism

-- | Compute a high level view of a transaction known as 'TransactionInfo'
-- from a CBOR and a slimmed down version of TxMeta
mkTransactionInfoFromReadTx :: Monad m
    => TimeInterpreter m
    -> W.BlockHeader
    -> DecoratedTxIns
    -> EraValue Read.Tx
    -> SubmissionMeta
    -> m WT.TransactionInfo
mkTransactionInfoFromReadTx _ti tip _decor tx _meta = do
    return
        $ WT.TransactionInfo
        { WT.txInfoId = W.Hash $ value getEraTxHash
        , WT.txInfoCBOR = undefined
        , WT.txInfoFee = undefined
        , WT.txInfoInputs = undefined
        , WT.txInfoCollateralInputs = undefined
        , WT.txInfoOutputs = undefined
        , WT.txInfoCollateralOutput = undefined
        , WT.txInfoWithdrawals = undefined
        , WT.txInfoMeta = WT.TxMeta
              { WT.status = undefined
              , WT.direction = undefined
              , WT.slotNo = undefined
              , WT.blockHeight = undefined
              , amount = undefined
              , WT.expiry = undefined
              }
        , WT.txInfoMetadata = undefined
        , WT.txInfoDepth = Quantity
              $ fromIntegral
              $ if tipH > undefined
                  then tipH - undefined
                  else 0
        , WT.txInfoTime = undefined
        , WT.txInfoScriptValidity = undefined
        }
  where
    tipH = getQuantity $ tip ^. #blockHeight
    
    value :: EraFun Read.Tx (K a) -> a
    value f = extractEraValue $ applyEraFun f tx
