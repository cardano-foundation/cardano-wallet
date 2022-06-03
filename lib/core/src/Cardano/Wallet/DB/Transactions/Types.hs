{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Types where

import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable (build) )
import Generics.Deriving.Monoid
import Prelude

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
import Data.Functor.Identity
import GHC.Generics
    ( Generic )

newtype TxHistory = TxHistory [(W.Tx, W.TxMeta)] deriving (Eq)

instance Buildable TxHistory where
    build (TxHistory txs) = "TxHistory " <> build (length txs)

instance Semigroup TxHistory where
    TxHistory xs <> TxHistory ys = TxHistory zs
      where
        zs = xs <> ys -- FIXME with invariants

instance Monoid TxHistory where
    mempty = TxHistory []

newtype DeltaTxHistory = DeltaTxHistory TxHistory

instance Buildable DeltaTxHistory where
    build (DeltaTxHistory (TxHistory txs)) =
        "DeltaTxHistory " <> build (length txs)

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (DeltaTxHistory txs) h = h <> txs

data TxRelationF f
    = TxRelationF
        [TxMeta]
        [f TxIn]
        [f TxCollateral]
        [(TxOut, [TxOutToken])]
        [(TxCollateralOut, [TxCollateralOutToken])]
        [TxWithdrawal]
    deriving (Generic)

type TxRelation = TxRelationF Identity

data WithTxOut a = WithTxOut
    { withTxOut_value :: a,
      withTxOut_context :: Maybe (TxOut, [TxOutToken])
    }
type TxRelationA = TxRelationF WithTxOut
instance Monoid (TxRelationF f) where
    mempty = memptydefault
instance Semigroup (TxRelationF f) where
    (<>) = mappenddefault
