{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.DB.Transactions.Types where

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Data.Delta
    ( Delta (..) )
import Data.Functor.Identity
import Fmt
    ( Buildable (build) )
import Generics.Deriving.Monoid
import GHC.Generics
    ( Generic )
import Prelude

data TxRelationF f
    = TxRelationF {
        txRelation_metas :: [TxMeta]
        , txRelation_ins :: [f TxIn]
        , txRelation_colls :: [f TxCollateral]
        , txRelation_outs :: [(TxOut, [TxOutToken])]
        , txRelation_collouts :: [(TxCollateralOut, [TxCollateralOutToken])]
        , txRelation_withdraws :: [TxWithdrawal]
        }
    deriving (Generic)

deriving instance (Eq (f TxIn), Eq (f TxCollateral))
     => Eq (TxRelationF f)
deriving instance (Show (f TxIn), Show (f TxCollateral))
     => Show (TxRelationF f)

instance Monoid (TxRelationF f) where
    mempty = memptydefault

instance Semigroup (TxRelationF f) where
    (<>) = mappenddefault

type TxRelation = TxRelationF Identity

data WithTxOut a = WithTxOut
    { withTxOut_value :: a,
      withTxOut_context :: Maybe (TxOut, [TxOutToken])
    }

type TxRelationA = TxRelationF WithTxOut

newtype TxHistory = TxHistory TxRelation  deriving (Show, Eq)

instance Buildable TxHistory where
    build (TxHistory txs) = "TxHistory "
        <> build (length $ txRelation_metas txs)

instance Semigroup TxHistory where
    TxHistory xs <> TxHistory ys = TxHistory zs
      where
        zs = xs <> ys -- FIXME with invariants

instance Monoid TxHistory where
    mempty = TxHistory mempty

newtype DeltaTxHistory = DeltaTxHistory TxHistory

instance Buildable DeltaTxHistory where
    build (DeltaTxHistory (TxHistory txs)) =
        "DeltaTxHistory " <> build (length $ txRelation_metas txs)

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (DeltaTxHistory txs) h = h <> txs

