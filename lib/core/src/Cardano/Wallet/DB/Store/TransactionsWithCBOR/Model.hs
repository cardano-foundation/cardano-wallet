{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( TxHistoryWithCBOR (..)
    , DeltaTx (..)
    )
    where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.CBOR.Model
    ( TxCBORHistory )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxHistory )
import Cardano.Wallet.Primitive.Types.Tx.CBOR
    ()
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.DB.Store.CBOR.Model as CBOR
import qualified Cardano.Wallet.DB.Store.Transactions.Model as Txs

data TxHistoryWithCBOR =
    TxHistoryWithCBOR
      { txHistory :: !TxHistory
      , txCBORs :: !TxCBORHistory
      }
    deriving ( Eq, Show, Generic )

instance Monoid TxHistoryWithCBOR where
  mempty = TxHistoryWithCBOR mempty mempty

instance Semigroup TxHistoryWithCBOR where
  TxHistoryWithCBOR tx cb <> TxHistoryWithCBOR tx' cb' =
    TxHistoryWithCBOR (tx <> tx') (cb <> cb')

data DeltaTx
    = Append TxHistoryWithCBOR
    -- ^ Add or overwrite (by id) transactions history.
    | DeleteTx TxId
    -- ^ Remove transaction by id.
    deriving ( Eq, Show, Generic )

instance Buildable DeltaTx where
    build = build . show

instance Delta DeltaTx where
    type Base DeltaTx = TxHistoryWithCBOR
    apply (Append (TxHistoryWithCBOR oldtxs oldcbors))
        (TxHistoryWithCBOR newtxs newcbors)
          = TxHistoryWithCBOR
              (apply (Txs.Append newtxs) oldtxs)
              (apply (CBOR.Append newcbors) oldcbors)
    apply (DeleteTx tid) (TxHistoryWithCBOR txs bors) = TxHistoryWithCBOR
      (apply (Txs.DeleteTx tid) txs)
      (apply (CBOR.DeleteTx tid) bors)
