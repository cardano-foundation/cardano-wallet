{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'TxHistory' for storing a set of transactions.
Transactions are encoded "as" expressed in DB tables.

-}
module Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxHistory (..)
    , TxHistory
    , TxHistoryF (TxHistoryF)
    , TxRelationF (..)
    , tokenCollateralOrd
    , tokenOutOrd
    , mkTxHistory
    , Decoration (..)
    , WithTxOut (..)
    , decorateWithTxOuts
    , mkTxIn
    , mkTxCollateral
    , mkTxOut
    , undecorateFromTxOuts
    ) where

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
import Control.Arrow
    ( (&&&) )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold, toList )
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
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

-- | A context that carries a TxOut together with its tokens
-- (this will be needed in the future for the DB Layer 
-- to reconstruct 'TransactionInfo').
data WithTxOut txin = WithTxOut
    { txIn :: txin, context :: Maybe (TxOut, [TxOutToken]) }
    deriving ( Show, Eq, Functor )

-- | A kind to index the 2 flavours of a 'TxRelationF', with or without 'TxOuts'
data Decoration
    = Without
    | With

-- | Define the TxOut context type
type family DecorateWithTxOut f a where
    DecorateWithTxOut 'Without a = a
    DecorateWithTxOut 'With a = WithTxOut a

{- | A low level definition of a transaction covering all transaction content
 by collecting all related-to-index database rows.
 Normalization is performed anyway after the first relation level.
 All values used here are records in the database.
 Foreign keys are used to group data correctly,
 but they are not removed from the data.
-}
data TxRelationF (f :: Decoration) =
    TxRelationF
    { ins :: [DecorateWithTxOut f TxIn]
    , collateralIns :: [DecorateWithTxOut f TxCollateral]
    , outs :: [(TxOut, [TxOutToken])]
    , collateralOuts :: Maybe (TxCollateralOut, [TxCollateralOutToken])
    , withdrawals :: [TxWithdrawal]
    }
    deriving ( Generic )

deriving instance ( Eq (DecorateWithTxOut f TxIn)
                  , Eq (DecorateWithTxOut f TxCollateral))
    => Eq (TxRelationF f)

deriving instance ( Show (DecorateWithTxOut f TxIn)
                  , Show (DecorateWithTxOut f TxCollateral))
    => Show (TxRelationF f)

-- | Transactions history is 'TxRelationF's indexed by 'TxId'
newtype TxHistoryF f =
    TxHistoryF { relations :: Map TxId (TxRelationF f) }
    deriving ( Generic )

deriving instance ( Eq (DecorateWithTxOut f TxIn)
                  , Eq (DecorateWithTxOut f TxCollateral))
    => Eq (TxHistoryF f)

deriving instance ( Show (DecorateWithTxOut f TxIn)
                  , Show (DecorateWithTxOut f TxCollateral))
    => Show (TxHistoryF f)

instance Monoid (TxHistoryF f) where
    mempty = TxHistoryF mempty

instance Semigroup (TxHistoryF f) where
    TxHistoryF h1 <> TxHistoryF h2 =
        TxHistoryF $ h1 <> h2

instance ( Show (DecorateWithTxOut f TxIn)
         , Show (DecorateWithTxOut f TxCollateral))
    => Buildable (TxHistoryF f) where
    build txs = "TxHistory " <> build (show $ relations txs)

-- | Shortcut type for transaction history where inputs are not
-- decorated with their corresponding `TxOut`.
type TxHistory = TxHistoryF 'Without

-- | Verbs to change a 'TxHistory'.
data DeltaTxHistory
    = Append TxHistory
    -- ^ Add new set of transactions, rewriting by transaction id.
    | DeleteTx TxId
    -- ^ Try to remove the transaction at the given transaction id.
    deriving ( Show, Eq, Generic )

instance Buildable DeltaTxHistory where
    build action = build $ show action

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    -- transactions are immutable so here there should happen no rewriting
    -- but we mimic the repsert in the store
    apply (Append txs) h = txs <> h 
    apply (DeleteTx tid) (TxHistoryF txs) =
        TxHistoryF $ Map.delete tid txs

mkTxIn :: TxId -> (Int, (W.TxIn, W.Coin)) -> TxIn
mkTxIn tid (ix,(txIn,amt)) =
    TxIn
    { txInputTxId = tid
    , txInputOrder = ix
    , txInputSourceTxId = TxId (W.inputId txIn)
    , txInputSourceIndex = W.inputIx txIn
    , txInputSourceAmount = amt
    }

mkTxCollateral :: TxId
    -> (Int, (W.TxIn, W.Coin)) 
    -> TxCollateral
mkTxCollateral tid (ix,(txCollateral,amt)) =
    TxCollateral
    { txCollateralTxId = tid
    , txCollateralOrder = ix
    , txCollateralSourceTxId = TxId $ W.inputId txCollateral
    , txCollateralSourceIndex = W.inputIx txCollateral
    , txCollateralSourceAmount = amt
    }

-- The key to sort TxCollateralOutToken
tokenCollateralOrd :: TxCollateralOutToken -> (TokenPolicyId, TokenName)
tokenCollateralOrd = txCollateralOutTokenPolicyId &&& txCollateralOutTokenName

-- The key to sort TxOutToken
tokenOutOrd :: TxOutToken -> (TokenPolicyId, TokenName)
tokenOutOrd = txOutTokenPolicyId &&& txOutTokenName

mkTxOut
    :: TxId
    -> (Word32, W.TxOut) -- ^ (index, txout)
    -> (TxOut, [TxOutToken])
mkTxOut tid (ix,txOut) = (out, sortOn tokenOutOrd tokens)
  where
    out =
        TxOut
        { txOutputTxId = tid
        , txOutputIndex = ix
        , txOutputAddress = view #address txOut
        , txOutputAmount = W.txOutCoin txOut
        }
    tokens =
        mkTxOutToken tid ix
        <$> snd (TokenBundle.toFlatList $ view #tokens txOut)

mkTxOutToken
    :: TxId
    -> Word32 -- ^ index
    -> (AssetId, TokenQuantity)
    -> TxOutToken
mkTxOutToken tid ix (AssetId policy token,quantity) =
    TxOutToken
    { txOutTokenTxId = tid
    , txOutTokenTxIndex = ix
    , txOutTokenPolicyId = policy
    , txOutTokenName = token
    , txOutTokenQuantity = quantity
    }

mkTxCollateralOut
    :: TxId
    -> W.TxOut
    -> (TxCollateralOut, [TxCollateralOutToken])
mkTxCollateralOut tid txCollateralOut = (out, sortOn tokenCollateralOrd tokens)
  where
    out =
        TxCollateralOut
        { txCollateralOutTxId = tid
        , txCollateralOutAddress = view #address txCollateralOut
        , txCollateralOutAmount = W.txOutCoin txCollateralOut
        }
    tokens =
        mkTxCollateralOutToken tid
        <$> snd (TokenBundle.toFlatList $ view #tokens txCollateralOut)

mkTxCollateralOutToken
    :: TxId -> (AssetId, TokenQuantity) -> TxCollateralOutToken
mkTxCollateralOutToken tid (AssetId policy token,quantity) =
    TxCollateralOutToken
    { txCollateralOutTokenTxId = tid
    , txCollateralOutTokenPolicyId = policy
    , txCollateralOutTokenName = token
    , txCollateralOutTokenQuantity = quantity
    }

mkTxWithdrawal :: TxId -> (RewardAccount, W.Coin) -> TxWithdrawal
mkTxWithdrawal tid (txWithdrawalAccount,txWithdrawalAmount) =
    TxWithdrawal { txWithdrawalTxId, txWithdrawalAccount, txWithdrawalAmount }
  where
    txWithdrawalTxId = tid

mkTxRelation :: W.Tx -> TxRelationF 'Without
mkTxRelation tx =
    TxRelationF
    { ins = fmap (mkTxIn tid) $ indexed . W.resolvedInputs $ tx
    , collateralIns =
          fmap (mkTxCollateral tid) $ indexed $ W.resolvedCollateralInputs tx
    , outs = fmap (mkTxOut tid) $ indexed $ W.outputs tx
    , collateralOuts = mkTxCollateralOut tid <$> W.collateralOutput tx
    , withdrawals =
          fmap (mkTxWithdrawal tid) $ Map.toList $ W.withdrawals tx
    }
  where
    tid = TxId $ tx ^. #txId
    indexed :: (Enum a, Num a) => [b] -> [(a, b)]
    indexed = zip [0 .. ]

-- | Convert high level transactions definition in low level DB history
mkTxHistory :: [W.Tx] -> TxHistory
mkTxHistory txs = TxHistoryF $ fold $ do
    tx <- txs
    let relation = mkTxRelation tx
    pure $ Map.singleton (TxId $ tx ^. #txId) relation

type TxOutKey = (TxId, Word32)

decorateWithTxOuts :: TxHistoryF 'Without -> TxHistoryF 'With
decorateWithTxOuts (TxHistoryF w) =
    let
        txouts :: Map TxOutKey (TxOut, [TxOutToken])
        txouts = Map.fromList $ do
            TxRelationF {..} <- toList w
            [(txOutputTxId &&& txOutputIndex $ txout, x) | x@(txout,_ ) <- outs]
    in  TxHistoryF $ fmap (solveTxOut txouts) w

decorateInputs
    :: (t -> TxOutKey)
    -> Map TxOutKey (TxOut, [TxOutToken])
    -> [t]
    -> [WithTxOut t]
decorateInputs keyOf txOutMap ins = do
        i <- ins
        pure $ WithTxOut i $ Map.lookup (keyOf i) txOutMap

solveTxOut
    :: Map TxOutKey (TxOut, [TxOutToken])
    -> TxRelationF 'Without
    -> TxRelationF 'With
solveTxOut txOutMap TxRelationF {..} = TxRelationF
    { ins =
        decorateInputs
            (txInputSourceTxId &&& txInputSourceIndex)
            txOutMap
            ins
    , collateralIns =
        decorateInputs
            (txCollateralSourceTxId &&& txCollateralSourceIndex)
            txOutMap
            collateralIns
    , outs = outs
    , collateralOuts = collateralOuts
    , withdrawals = withdrawals
    }

undecorateFromTxOuts :: TxHistoryF 'With -> TxHistoryF 'Without
undecorateFromTxOuts (TxHistoryF w) = TxHistoryF $ fmap unsolveTxOut w

unsolveTxOut :: TxRelationF 'With -> TxRelationF 'Without
unsolveTxOut TxRelationF {..} = TxRelationF
    { ins = fmap txIn ins
    , collateralIns = fmap txIn collateralIns
    , outs = outs
    , collateralOuts = collateralOuts
    , withdrawals = withdrawals
    }
