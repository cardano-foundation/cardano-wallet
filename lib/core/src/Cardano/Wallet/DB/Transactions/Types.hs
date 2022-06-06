{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.DB.Transactions.Types where

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
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Data.Functor.Identity
import Data.Map.Strict
    ( Map )
import Fmt
    ( Buildable (build) )
import Generics.Deriving.Monoid
import GHC.Generics
    ( Generic )

data TxRelationF f
    = TxRelationF 
        { txRelation_ins :: [f TxIn]
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

newtype TxHistoryF f = TxHistoryF
    { txHistory_relations :: Map TxId (TxMeta,TxRelationF f) }
    deriving (Generic)

overTxHistoryF
    :: TxHistoryF f1
    -> (Map TxId (TxMeta, TxRelationF f1) -> Map TxId (TxMeta, TxRelationF f2))
    -> TxHistoryF f2
overTxHistoryF (TxHistoryF x) f = TxHistoryF $ f x

deriving instance (Eq (f TxIn), Eq (f TxCollateral))
     => Eq (TxHistoryF f)

deriving instance (Show (f TxIn), Show (f TxCollateral))
     => Show (TxHistoryF f)

instance Monoid (TxRelationF f) where
    mempty = memptydefault

instance Semigroup (TxRelationF f) where
    (<>) = mappenddefault

instance Monoid (TxHistoryF f) where
    mempty = memptydefault

instance Semigroup (TxHistoryF f) where
    (<>) = mappenddefault


type TxHistory = TxHistoryF Identity

data WithTxOut a = WithTxOut
    { withTxOut_value :: a
    , withTxOut_context :: Maybe (TxOut, [TxOutToken])
    }

type TxHistoryA = TxHistoryF WithTxOut


instance Buildable (TxHistoryF f) where
    build txs = "TxHistory "
        <> build (length $ txHistory_relations txs)


