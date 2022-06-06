{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

{- |
 Copyright: © 2018-2020 IOHK
 License: Apache-2.0
-}
module Cardano.Wallet.DB.Transactions.Model where

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF), TxRelationF (..) )
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (AssetId) )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Foldable
    ( fold )
import Data.Functor
    ( (<&>) )
import Data.Functor.Identity
    ( Identity (Identity) )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Data.Maybe
    ( maybeToList )
import Data.Monoid
    ()
import Data.Quantity
    ( getQuantity )
import Data.Word
    ( Word32 )
import Prelude

import qualified Data.Map.Strict as Map

mkTxMetaEntity ::
    W.WalletId ->
    TxId ->
    Maybe W.Coin ->
    Maybe W.TxMetadata ->
    W.TxMeta ->
    Maybe W.TxScriptValidity ->
    TxMeta
mkTxMetaEntity wid txid mfee meta derived scriptValidity =
    TxMeta
        { txMetaTxId = txid,
          txMetaWalletId = wid,
          txMetaStatus = derived ^. #status,
          txMetaDirection = derived ^. #direction,
          txMetaSlot = derived ^. #slotNo,
          txMetaBlockHeight = getQuantity (derived ^. #blockHeight),
          txMetaAmount = derived ^. #amount,
          txMetaFee = fromIntegral . W.unCoin <$> mfee,
          txMetaSlotExpires = derived ^. #expiry,
          txMetadata = meta,
          txMetaScriptValidity =
            scriptValidity <&> \case
                W.TxScriptValid -> True
                W.TxScriptInvalid -> False
        }

mkTxIn :: TxId -> (Int, (W.TxIn, W.Coin)) -> TxIn
mkTxIn tid (ix, (txIn, amt)) =
    TxIn
        { txInputTxId = tid,
          txInputOrder = ix,
          txInputSourceTxId = TxId (W.inputId txIn),
          txInputSourceIndex = W.inputIx txIn,
          txInputSourceAmount = amt
        }

mkTxCollateral :: TxId -> (Int, (W.TxIn, W.Coin)) -> TxCollateral
mkTxCollateral tid (ix, (txCollateral, amt)) =
    TxCollateral
        { txCollateralTxId = tid,
          txCollateralOrder = ix,
          txCollateralSourceTxId = TxId (W.inputId txCollateral),
          txCollateralSourceIndex = W.inputIx txCollateral,
          txCollateralSourceAmount = amt
        }

mkTxOut :: TxId -> (Word32, W.TxOut) -> (TxOut, [TxOutToken])
mkTxOut tid (ix, txOut) = (out, tokens)
  where
    out =
        TxOut
            { txOutputTxId = tid,
              txOutputIndex = ix,
              txOutputAddress = view #address txOut,
              txOutputAmount = W.txOutCoin txOut
            }
    tokens =
        mkTxOutToken tid ix
            <$> snd (TokenBundle.toFlatList $ view #tokens txOut)

mkTxOutToken ::
    TxId ->
    Word32 ->
    ( AssetId,
      TokenQuantity
    ) ->
    TxOutToken
mkTxOutToken tid ix (AssetId policy token, quantity) =
    TxOutToken
        { txOutTokenTxId = tid,
          txOutTokenTxIndex = ix,
          txOutTokenPolicyId = policy,
          txOutTokenName = token,
          txOutTokenQuantity = quantity
        }

mkTxCollateralOut ::
    TxId ->
    W.TxOut ->
    (TxCollateralOut, [TxCollateralOutToken])
mkTxCollateralOut tid txCollateralOut = (out, tokens)
  where
    out =
        TxCollateralOut
            { txCollateralOutTxId = tid,
              txCollateralOutAddress = view #address txCollateralOut,
              txCollateralOutAmount = W.txOutCoin txCollateralOut
            }
    tokens =
        mkTxCollateralOutToken tid
            <$> snd (TokenBundle.toFlatList $ view #tokens txCollateralOut)

mkTxCollateralOutToken ::
    TxId ->
    (AssetId, TokenQuantity) ->
    TxCollateralOutToken
mkTxCollateralOutToken tid (AssetId policy token, quantity) =
    TxCollateralOutToken
        { txCollateralOutTokenTxId = tid,
          txCollateralOutTokenPolicyId = policy,
          txCollateralOutTokenName = token,
          txCollateralOutTokenQuantity = quantity
        }
mkTxWithdrawal ::
    TxId ->
    ( RewardAccount,
      W.Coin
    ) ->
    TxWithdrawal
mkTxWithdrawal tid (txWithdrawalAccount, txWithdrawalAmount) =
    TxWithdrawal
        { txWithdrawalTxId,
          txWithdrawalAccount,
          txWithdrawalAmount
        }
  where
    txWithdrawalTxId = tid

mkTxCore ::
    W.WalletId -> W.Tx -> W.TxMeta -> TxHistory
mkTxCore wid tx txmeta = TxHistoryF $ Map.singleton tid $ (meta,) $ TxRelationF
    do fmap (Identity . mkTxIn tid) . ordered . W.resolvedInputs $ tx
    do
        fmap (Identity . mkTxCollateral tid)
            . ordered . W.resolvedCollateralInputs
            $ tx
    do fmap (mkTxOut tid) . ordered . W.outputs $ tx
    do fmap (mkTxCollateralOut tid) . maybeToList . W.collateralOutput $ tx
    do fmap (mkTxWithdrawal tid) . Map.toList . W.withdrawals $ tx
  where
    meta =
        mkTxMetaEntity
            wid
            tid
            (W.fee tx)
            (W.metadata tx)
            txmeta
            (W.scriptValidity tx)
    tid = TxId $ tx ^. #txId
    ordered :: (Enum a, Num a) => [b] -> [(a, b)]
    ordered = zip [0 ..]

mkTxHistory ::
    W.WalletId ->
    [(W.Tx, W.TxMeta)] ->
    TxHistory
mkTxHistory wid txs = fold $ do
    (tx, meta) <- txs
    pure $ mkTxCore wid tx meta

filterMeta :: (TxMeta -> Bool) -> TxHistoryF f -> TxHistoryF f
filterMeta f (TxHistoryF rs) = TxHistoryF $ Map.filter (f . fst) rs
