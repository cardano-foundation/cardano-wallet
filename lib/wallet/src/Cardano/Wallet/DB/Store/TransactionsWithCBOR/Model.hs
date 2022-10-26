{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( TxPileWithCBOR (..)
    , DeltaTx (..)
    )
    where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( TxCBORPile )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxPile )
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.DB.Store.CBOR.Model as CBOR
import qualified Cardano.Wallet.DB.Store.Transactions.Model as Txs

data TxPileWithCBOR =
    TxPileWithCBOR
      { txHistory :: !TxPile
      , txCBORs :: !TxCBORPile
      }
    deriving ( Eq, Show, Generic )

instance Monoid TxPileWithCBOR where
  mempty = TxPileWithCBOR mempty mempty

instance Semigroup TxPileWithCBOR where
  TxPileWithCBOR tx cb <> TxPileWithCBOR tx' cb' =
    TxPileWithCBOR (tx <> tx') (cb <> cb')

data DeltaTx
    = Append TxPileWithCBOR
    -- ^ Add or overwrite (by id) transactions history.
    | DeleteTx TxId
    -- ^ Remove transaction by id.
    deriving ( Eq, Show, Generic )

instance Buildable DeltaTx where
    build = build . show

instance Delta DeltaTx where
    type Base DeltaTx = TxPileWithCBOR
    apply (Append (TxPileWithCBOR oldtxs oldcbors))
        (TxPileWithCBOR newtxs newcbors)
          = TxPileWithCBOR
              (apply (Txs.Append newtxs) oldtxs)
              (apply (CBOR.Append newcbors) oldcbors)
    apply (DeleteTx tid) (TxPileWithCBOR txs bors) = TxPileWithCBOR
      (apply (Txs.DeleteTx tid) txs)
      (apply (CBOR.DeleteTx tid) bors)
