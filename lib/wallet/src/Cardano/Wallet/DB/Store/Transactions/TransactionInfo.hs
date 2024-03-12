{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Transactions.TransactionInfo
    ( mkTransactionInfoFromRelation
    , mkTransactionInfoFromReadTx
    , mkTxCBOR
    ) where

import Prelude hiding
    ( (.)
    )

import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxIn (..)
    , TxMeta (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..)
    )
import Cardano.Wallet.DB.Store.Meta.Model
    ( mkTxMetaFromEntity
    )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( SubmissionMeta (..)
    )
import Cardano.Wallet.DB.Store.Transactions.Decoration
    ( DecoratedTxIns
    , TxOutKey
    , lookupTxOut
    , mkTxOutKey
    , mkTxOutKeyCollateral
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation (..)
    , fromTxCollateralOut
    , fromTxOut
    , txCBORPrism
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralInputs
    ( getCollateralInputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralOutputs
    ( getCollateralOutputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Fee
    ( getFee
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( getInputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( getMetadata
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( getOutputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ScriptValidity
    ( getScriptValidity
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( getValidity
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( getWithdrawals
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , interpretQuery
    , slotToUTCTime
    )
import Cardano.Wallet.Read
    ( Tx
    )
import Cardano.Wallet.Read.Eras
    ( EraValue
    , IsEra
    , applyEraFun
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    , renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( getEraCollateralInputs
    )
import Cardano.Wallet.Read.Tx.CollateralOutputs
    ( getEraCollateralOutputs
    )
import Cardano.Wallet.Read.Tx.Fee
    ( getEraFee
    )
import Cardano.Wallet.Read.Tx.Hash
    ( getEraTxHash
    )
import Cardano.Wallet.Read.Tx.Inputs
    ( getEraInputs
    )
import Cardano.Wallet.Read.Tx.Metadata
    ( getEraMetadata
    )
import Cardano.Wallet.Read.Tx.Outputs
    ( getEraOutputs
    )
import Cardano.Wallet.Read.Tx.ScriptValidity
    ( getEraScriptValidity
    )
import Cardano.Wallet.Read.Tx.Validity
    ( getEraValidity
    )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( getEraWithdrawals
    )
import Cardano.Wallet.Transaction
    ( ValidityIntervalExplicit (invalidHereafter)
    )
import Control.Category
    ( (.)
    )
import Data.Foldable
    ( fold
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL
    ( (^.)
    )
import Data.Quantity
    ( Quantity (..)
    )

import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as WC
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import qualified Cardano.Wallet.Primitive.Types.Tx.TransactionInfo as WT
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as WT
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as WT
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as WT
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
mkTransactionInfoFromRelation ti tip TxRelation{..}
        decor meta@DB.TxMeta{..} = do
    txTime <- interpretQuery ti . slotToUTCTime $ txMetaSlot
    return
        $ WT.TransactionInfo
        { WT.txInfoId = getTxId txMetaTxId
        , WT.txInfoCBOR = cbor >>= mkTxCBOR
        , WT.txInfoFee = WC.Coin . fromIntegral <$> txMetaFee
        , WT.txInfoInputs = mkTxIn <$> ins
        , WT.txInfoCollateralInputs = mkTxCollateral <$> collateralIns
        , WT.txInfoOutputs = fromTxOut <$> outs
        , WT.txInfoCollateralOutput
            = fromTxCollateralOut <$> collateralOuts
        , WT.txInfoWithdrawals
            = Map.fromList $ map mkTxWithdrawal withdrawals
        , WT.txInfoMeta = mkTxMetaFromEntity meta
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
    -> EraValue Read.Tx
    -> DecoratedTxIns
    -> SubmissionMeta
    -> W.TxStatus
    -> m WT.TransactionInfo
mkTransactionInfoFromReadTx ti tip tx decor SubmissionMeta{..} status = do
    txTime <- interpretQuery ti . slotToUTCTime $ submissionMetaSlot
    return
        $ WT.TransactionInfo
        { WT.txInfoId = W.Hash $ value getEraTxHash
        , WT.txInfoCBOR = Just $ renderTxToCBOR tx
        , WT.txInfoFee = value $ getFee . getEraFee
        , WT.txInfoInputs = mkTxIn <$> value (getInputs . getEraInputs)
        , WT.txInfoCollateralInputs = mkTxIn
            <$> value (getCollateralInputs . getEraCollateralInputs)
        , WT.txInfoOutputs = value (getOutputs . getEraOutputs)
        , WT.txInfoCollateralOutput
            = value (getCollateralOutputs . getEraCollateralOutputs)
        , WT.txInfoWithdrawals = fold
            $ value (getWithdrawals . getEraWithdrawals)
        , WT.txInfoMeta = WT.TxMeta
              { WT.status = status
              , WT.direction = submissionMetaDirection
              , WT.slotNo = submissionMetaSlot
              , WT.blockHeight = submissionMetaHeight
              , W.amount = submissionMetaAmount
              , WT.expiry = fmap (SlotNo . getQuantity . invalidHereafter)
                    $ value $ getValidity . getEraValidity
              }
        , WT.txInfoMetadata = value $ getMetadata . getEraMetadata
        , WT.txInfoDepth = Quantity $ fromIntegral $
            if tipH > height
                  then tipH - height
                  else 0
        , WT.txInfoTime = txTime
        , WT.txInfoScriptValidity
            = value $ getScriptValidity . getEraScriptValidity
        }
  where
    tipH = getQuantity $ tip ^. #blockHeight

    value :: (forall era . IsEra era => Tx era -> a) -> a
    value f = applyEraFun f tx

    height = getQuantity submissionMetaHeight

    mkTxIn :: WT.TxIn -> (WT.TxIn, Maybe WT.TxOut)
    mkTxIn txIn =
        ( WT.TxIn
          { inputId = WT.inputId txIn
          , inputIx = WT.inputIx txIn
          }
        , lookupTxOut (mkTxOutPrimitive txIn) decor
        )

mkTxOutPrimitive :: WT.TxIn -> TxOutKey
mkTxOutPrimitive (WT.TxIn transaction count) = (TxId transaction, count)
