{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Declaration of primitive types that are specific to a particular backend.
-- Likely, the shape of all types is similar and will eventually converge
-- to one unified design. Though, in the meantime, we have to support different
-- primitive representations for some of them.
--
-- In the case of 'Jormungandr' at the moment, inputs also holds their
-- corresponding resolved value as 'Coin'.

module Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx(..)
    ) where

import Prelude hiding
    ( id )

import Cardano.Wallet.Primitive.Types
    ( Coin, Hash, TxIn, TxOut )
import Control.DeepSeq
    ( NFData (..) )
import Fmt
    ( Buildable (..), blockListF' )
import GHC.Generics
    ( Generic )

data Tx = Tx
    { txid
        :: Hash "Tx"
        -- ^ Jörmungandr computes transaction id by hashing the full content of
        -- the transaction, which includes witnesses. Therefore, we need either
        -- to keep track of the witnesses to be able to re-compute the tx id
        -- every time, or, simply keep track of the id itself.
    , inputs
        :: ![(TxIn, Coin)]
        -- ^ NOTE: Order of inputs matters in the transaction representation. The
        -- transaction id is computed from the binary representation of a tx,
        -- for which inputs are serialized in a specific order.
    , outputs
        :: ![TxOut]
        -- ^ NOTE: Order of outputs matter in the transaction representations. Outputs
        -- are used as inputs for next transactions which refer to them using
        -- their indexes. It matters also for serialization.
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

instance Buildable Tx where
    build (Tx tid ins outs) = mempty
        <> build tid
        <> blockListF' "~>" build (fst <$> ins)
        <> blockListF' "<~" build outs
