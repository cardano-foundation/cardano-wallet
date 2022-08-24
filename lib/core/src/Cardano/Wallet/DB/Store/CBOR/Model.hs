{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.CBOR.Model where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.Primitive.Types.Tx.CBOR
    ( TxCBOR )
import Data.Delta
    ( Delta (..) )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

newtype TxCBORHistory =
    TxCBORHistory {relations :: Map TxId TxCBOR}
    deriving ( Eq, Show, Generic, Monoid, Semigroup )

data DeltaTxCBOR
    = Expand TxCBORHistory
    -- ^ Add or overwrite (by id) local-submission-transactions.
    | Prune [TxId]
    -- ^ Remove submissions by id.
    deriving ( Eq, Show, Generic )

instance Buildable DeltaTxCBOR where
    build = build . show

instance Delta DeltaTxCBOR where
    type Base DeltaTxCBOR = TxCBORHistory
    apply (Expand addendum) x = addendum <> x
    apply (Prune tids) (TxCBORHistory m) = TxCBORHistory
        $ Map.withoutKeys m (Set.fromList tids)
