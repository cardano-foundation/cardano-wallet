{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0
-}
module Cardano.Wallet.DB.Transactions.Transaction.Model where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Control.Arrow
    ( (&&&) )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold )
import Data.Functor.Identity
    ( Identity (Identity) )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Data.List
    ( sortOn )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (build) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Map.Strict as Map

data TxRelationF f
    = TxRelationF
        { txRelation_ins :: [f TxIn]
        , txRelation_colls :: [f TxCollateral]
        , txRelation_outs :: [(TxOut, [TxOutToken])]
        , txRelation_collouts :: Maybe (TxCollateralOut, [TxCollateralOutToken])
        , txRelation_withdraws :: [TxWithdrawal]
        }
    deriving (Generic)

deriving instance (Eq (f TxIn), Eq (f TxCollateral))
     => Eq (TxRelationF f)

deriving instance (Show (f TxIn), Show (f TxCollateral))
     => Show (TxRelationF f)

newtype TxHistoryF f = TxHistoryF
    { txHistory_relations :: Map TxId (TxRelationF f)
    }
    deriving (Generic)

deriving instance (Eq (f TxIn), Eq (f TxCollateral))
     => Eq (TxHistoryF f)

deriving instance (Show (f TxIn), Show (f TxCollateral))
     => Show (TxHistoryF f)

instance Monoid (TxHistoryF f) where
    mempty = TxHistoryF mempty

instance Semigroup (TxHistoryF f) where
    TxHistoryF  h1 <> TxHistoryF h2  = TxHistoryF 
        $ Map.unionWith (error "clash") h1 h2

type TxHistory = TxHistoryF Identity

data WithTxOut a = WithTxOut
    { withTxOut_value :: a
    , withTxOut_context :: Maybe (TxOut, [TxOutToken])
    }

type TxHistoryA = TxHistoryF WithTxOut

instance  (Show (f TxIn), Show (f TxCollateral)) 
    => Buildable (TxHistoryF f) where
    build txs = "TxHistory "
        <> build (show $ txHistory_relations txs)

data DeltaTxHistory
    = ExpandTxHistory TxHistory
    | DeleteTxHistory TxId
    deriving (Show, Eq, Generic)

instance Buildable DeltaTxHistory where
    build action = build $ show action

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (ExpandTxHistory txs) h 
        =  h <> txs
    apply (DeleteTxHistory tid) (TxHistoryF txs) = TxHistoryF
        $ Map.delete tid txs

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

tokenCollateralOrd :: TxCollateralOutToken -> (TokenPolicyId, TokenName)
tokenCollateralOrd = (txCollateralOutTokenPolicyId &&& txCollateralOutTokenName)

tokenOutOrd :: TxOutToken -> (TokenPolicyId, TokenName)
tokenOutOrd = (txOutTokenPolicyId &&& txOutTokenName)

mkTxOut :: TxId -> (Word32, W.TxOut) -> (TxOut, [TxOutToken])
mkTxOut tid (ix, txOut) = (out, sortOn tokenOutOrd tokens)
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
mkTxCollateralOut tid txCollateralOut = (out, sortOn tokenCollateralOrd tokens)
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

mkTxRelation :: W.Tx -> TxRelationF Identity
mkTxRelation tx = TxRelationF
    { txRelation_ins 
        = fmap (Identity . mkTxIn tid) 
        $ ordered . W.resolvedInputs 
        $ tx
    , txRelation_colls 
        = fmap (Identity . mkTxCollateral tid)
        $ ordered
        $ W.resolvedCollateralInputs tx
    , txRelation_outs 
        = fmap (mkTxOut tid) 
        $ ordered 
        $ W.outputs tx
    , txRelation_collouts 
        = mkTxCollateralOut tid
        <$> W.collateralOutput tx
    , txRelation_withdraws 
        = fmap (mkTxWithdrawal tid) 
        $ Map.toList 
        $ W.withdrawals tx
    }
    where
    tid = TxId $ tx ^. #txId
    ordered :: (Enum a, Num a) => [b] -> [(a, b)]
    ordered = zip [0 ..]


mkTxHistory :: [W.Tx] -> TxHistory
mkTxHistory txs = TxHistoryF $ fold $ do
    tx <- txs
    let relation = mkTxRelation tx
    pure $ Map.singleton (TxId $ tx ^. #txId) relation
