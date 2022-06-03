{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.DB.Transactions.Update (updateTxHistory) where

import Cardano.DB.Sqlite
    ( dbChunked' )
import Cardano.Wallet.DB.Sqlite.Schema
    ( Key (..)
    , TxCollateral (..)
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
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (AssetId) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Data.Maybe
    ( maybeToList )
import Data.Quantity
    ( getQuantity )
import Database.Persist.Sql
    ( SqlPersistT, repsertMany )
import Prelude

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    SQLite database operations
-------------------------------------------------------------------------------}
updateTxHistory :: W.WalletId -> [(W.Tx, W.TxMeta)] -> SqlPersistT IO ()
updateTxHistory wid txs = do
  let ( txMetas
          , txIns
          , txCollateralIns
          , txOuts
          , txOutTokens
          , txCollateralOuts
          , txCollateralOutTokens
          , txWithdrawals
          ) = mkTxHistory wid txs
  putTxs
      txMetas
      txIns
      txCollateralIns
      txOuts
      txOutTokens
      txCollateralOuts
      txCollateralOutTokens
      txWithdrawals

-- | Insert multiple transactions, removing old instances first.
putTxs
    :: [TxMeta]
    -> [TxIn]
    -> [TxCollateral]
    -> [TxOut]
    -> [TxOutToken]
    -> [TxCollateralOut]
    -> [TxCollateralOutToken]
    -> [TxWithdrawal]
    -> SqlPersistT IO ()
putTxs
    txMetas
    txIns
    txCollateralIns
    txOuts
    txOutTokens
    txCollateralOuts
    txCollateralOutTokens
    txWithdrawals = do
        dbChunked' repsertMany
            [ (TxMetaKey txMetaTxId txMetaWalletId, m)
            | m@TxMeta{..} <- txMetas]
        dbChunked' repsertMany
            [ (TxInKey txInputTxId txInputSourceTxId txInputSourceIndex, i)
            | i@TxIn{..} <- txIns ]
        dbChunked' repsertMany
            [ ( TxCollateralKey
                txCollateralTxId
                txCollateralSourceTxId
                txCollateralSourceIndex
              , i
              )
            | i@TxCollateral{..} <- txCollateralIns ]
        dbChunked' repsertMany
            [ (TxOutKey txOutputTxId txOutputIndex, o)
            | o@TxOut{..} <- txOuts ]
        dbChunked' repsertMany
            [ ( TxOutTokenKey
                txOutTokenTxId
                txOutTokenTxIndex
                txOutTokenPolicyId
                txOutTokenName
              , o
              )
            | o@TxOutToken{..} <- txOutTokens ]
        dbChunked' repsertMany
            [ (TxCollateralOutKey txCollateralOutTxId, o)
            | o@TxCollateralOut{..} <- txCollateralOuts ]
        dbChunked' repsertMany
            [ ( TxCollateralOutTokenKey
                txCollateralOutTokenTxId
                txCollateralOutTokenPolicyId
                txCollateralOutTokenName
              , o
              )
            | o@TxCollateralOutToken{..} <- txCollateralOutTokens ]
        dbChunked' repsertMany
            [ (TxWithdrawalKey txWithdrawalTxId txWithdrawalAccount, w)
            | w@TxWithdrawal{..} <- txWithdrawals ]

mkTxHistory
    :: W.WalletId
    -> [(W.Tx, W.TxMeta)]
    ->  ( [TxMeta]
        , [TxIn]
        , [TxCollateral]
        , [TxOut]
        , [TxOutToken]
        , [TxCollateralOut]
        , [TxCollateralOutToken]
        , [TxWithdrawal]
        )
mkTxHistory wid txs = flatTxHistory
    [ ( mkTxMetaEntity
          wid txid (W.fee tx) (W.metadata tx) derived (W.scriptValidity tx)
      , mkTxInputsOutputs (txid, tx)
      , mkTxWithdrawals (txid, tx)
      )
    | (tx, derived) <- txs
    , let txid = view #txId tx
    ]
  where
    -- | Make flat lists of entities from the result of 'mkTxHistory'.
    flatTxHistory ::
        [ ( TxMeta
          , ( [TxIn]
            , [TxCollateral]
            , [(TxOut, [TxOutToken])]
            , [(TxCollateralOut, [TxCollateralOutToken])]
            )
          , [TxWithdrawal]
          )
        ] ->
        ( [TxMeta]
        , [TxIn]
        , [TxCollateral]
        , [TxOut]
        , [TxOutToken]
        , [TxCollateralOut]
        , [TxCollateralOutToken]
        , [TxWithdrawal]
        )
    flatTxHistory es =
        (               map (                       (\(a, _, _) -> a)) es
        ,         concatMap ((\(a, _, _, _) -> a) . (\(_, b, _) -> b)) es
        ,         concatMap ((\(_, b, _, _) -> b) . (\(_, b, _) -> b)) es
        , fst <$> concatMap ((\(_, _, c, _) -> c) . (\(_, b, _) -> b)) es
        , snd =<< concatMap ((\(_, _, c, _) -> c) . (\(_, b, _) -> b)) es
        , fst <$> concatMap ((\(_, _, _, d) -> d) . (\(_, b, _) -> b)) es
        , snd =<< concatMap ((\(_, _, _, d) -> d) . (\(_, b, _) -> b)) es
        ,         concatMap (                       (\(_, _, c) -> c)) es
        )

mkTxInputsOutputs ::
    (W.Hash "Tx", W.Tx) ->
    ( [TxIn]
    , [TxCollateral]
    , [(TxOut, [TxOutToken])]
    , [(TxCollateralOut, [TxCollateralOutToken])]
    )
mkTxInputsOutputs tx =
    ( (dist mkTxIn . ordered W.resolvedInputs) tx
    , (dist mkTxCollateral . ordered W.resolvedCollateralInputs) tx
    , (dist mkTxOut . ordered W.outputs) tx
    , (dist mkTxCollateralOut . fmap (maybeToList . W.collateralOutput)) tx
    )
  where
    mkTxIn tid (ix, (txIn, amt)) = TxIn
        { txInputTxId = TxId tid
        , txInputOrder = ix
        , txInputSourceTxId = TxId (W.inputId txIn)
        , txInputSourceIndex = W.inputIx txIn
        , txInputSourceAmount = amt
        }
    mkTxCollateral tid (ix, (txCollateral, amt)) = TxCollateral
        { txCollateralTxId = TxId tid
        , txCollateralOrder = ix
        , txCollateralSourceTxId = TxId (W.inputId txCollateral)
        , txCollateralSourceIndex = W.inputIx txCollateral
        , txCollateralSourceAmount = amt
        }
    mkTxOut tid (ix, txOut) = (out, tokens)
      where
        out = TxOut
            { txOutputTxId = TxId tid
            , txOutputIndex = ix
            , txOutputAddress = view #address txOut
            , txOutputAmount = W.txOutCoin txOut
            }
        tokens = mkTxOutToken tid ix <$>
            snd (TokenBundle.toFlatList $ view #tokens txOut)
    mkTxOutToken tid ix (AssetId policy token, quantity) = TxOutToken
        { txOutTokenTxId = TxId tid
        , txOutTokenTxIndex = ix
        , txOutTokenPolicyId = policy
        , txOutTokenName = token
        , txOutTokenQuantity = quantity
        }
    mkTxCollateralOut tid txCollateralOut = (out, tokens)
      where
        out = TxCollateralOut
            { txCollateralOutTxId = TxId tid
            , txCollateralOutAddress = view #address txCollateralOut
            , txCollateralOutAmount = W.txOutCoin txCollateralOut
            }
        tokens = mkTxCollateralOutToken tid <$>
            snd (TokenBundle.toFlatList $ view #tokens txCollateralOut)
    mkTxCollateralOutToken tid (AssetId policy token, quantity) =
        TxCollateralOutToken
        { txCollateralOutTokenTxId = TxId tid
        , txCollateralOutTokenPolicyId = policy
        , txCollateralOutTokenName = token
        , txCollateralOutTokenQuantity = quantity
        }
    ordered f = fmap (zip [0..] . f)
    -- | Distribute `a` across many `b`s using the given function.
    -- >>> dist TxOut (addr, [Coin 1, Coin 42, Coin 14])
    -- [TxOut addr (Coin 1), TxOut addr (Coin 42), TxOut addr (Coin 14)]
    dist :: (a -> b -> c) -> (a, [b]) -> [c]
    dist f (a, bs) = [f a b | b <- bs]

mkTxWithdrawals
    :: (W.Hash "Tx", W.Tx)
    -> [TxWithdrawal]
mkTxWithdrawals (txid, tx) =
    mkTxWithdrawal <$> Map.toList (tx ^. #withdrawals)
  where
    txWithdrawalTxId = TxId txid
    mkTxWithdrawal (txWithdrawalAccount, txWithdrawalAmount) =
        TxWithdrawal
            { txWithdrawalTxId
            , txWithdrawalAccount
            , txWithdrawalAmount
            }

mkTxMetaEntity
    :: W.WalletId
    -> W.Hash "Tx"
    -> Maybe W.Coin
    -> Maybe W.TxMetadata
    -> W.TxMeta
    -> Maybe W.TxScriptValidity
    -> TxMeta
mkTxMetaEntity wid txid mfee meta derived scriptValidity = TxMeta
    { txMetaTxId = TxId txid
    , txMetaWalletId = wid
    , txMetaStatus = derived ^. #status
    , txMetaDirection = derived ^. #direction
    , txMetaSlot = derived ^. #slotNo
    , txMetaBlockHeight = getQuantity (derived ^. #blockHeight)
    , txMetaAmount = derived ^. #amount
    , txMetaFee = fromIntegral . W.unCoin <$> mfee
    , txMetaSlotExpires = derived ^. #expiry
    , txMetadata = meta
    , txMetaScriptValidity = scriptValidity <&> \case
          W.TxScriptValid -> True
          W.TxScriptInvalid -> False
    }
