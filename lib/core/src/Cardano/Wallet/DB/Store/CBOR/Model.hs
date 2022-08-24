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
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

newtype TxCBORHistory =
    TxCBORHistory {relations :: Map TxId TxCBOR}
    deriving ( Eq, Show, Generic, Monoid, Semigroup )

data DeltaTxCBOR
    = Append TxCBORHistory
    -- ^ Add or overwrite (by id) local-submission-transactions.
    | DeleteTx TxId
    -- ^ Remove submissions by id.
    deriving ( Eq, Show, Generic )

instance Buildable DeltaTxCBOR where
    build = build . show

instance Delta DeltaTxCBOR where
    type Base DeltaTxCBOR = TxCBORHistory
    apply (Append addendum) x = addendum <> x
    apply (DeleteTx tid) (TxCBORHistory m) = TxCBORHistory
        $ Map.delete tid m
