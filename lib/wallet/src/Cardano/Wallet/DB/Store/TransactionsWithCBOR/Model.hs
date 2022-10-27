{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( TxSetWithCBOR (..)
    , DeltaTx (..)
    )
    where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( TxCBORSet )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxSet )
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.DB.Store.CBOR.Model as CBOR
import qualified Cardano.Wallet.DB.Store.Transactions.Model as Txs

data TxSetWithCBOR =
    TxSetWithCBOR
      { txHistory :: !TxSet
      , txCBORs :: !TxCBORSet
      }
    deriving ( Eq, Show, Generic )

instance Monoid TxSetWithCBOR where
  mempty = TxSetWithCBOR mempty mempty

instance Semigroup TxSetWithCBOR where
  TxSetWithCBOR tx cb <> TxSetWithCBOR tx' cb' =
    TxSetWithCBOR (tx <> tx') (cb <> cb')

data DeltaTx
    = Append TxSetWithCBOR
    -- ^ Add or overwrite (by id) transactions history.
    | DeleteTx TxId
    -- ^ Remove transaction by id.
    deriving ( Eq, Show, Generic )

instance Buildable DeltaTx where
    build = build . show

instance Delta DeltaTx where
    type Base DeltaTx = TxSetWithCBOR
    apply (Append (TxSetWithCBOR oldtxs oldcbors))
        (TxSetWithCBOR newtxs newcbors)
          = TxSetWithCBOR
              (apply (Txs.Append newtxs) oldtxs)
              (apply (CBOR.Append newcbors) oldcbors)
    apply (DeleteTx tid) (TxSetWithCBOR txs bors) = TxSetWithCBOR
      (apply (Txs.DeleteTx tid) txs)
      (apply (CBOR.DeleteTx tid) bors)
