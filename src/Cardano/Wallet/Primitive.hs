{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the core primitive of a Wallet. This is roughly a
-- Haskell translation of the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
--
-- It doesn't contain any particular business-logic code, but define a few
-- primitive operations on Wallet core types as well.

module Cardano.Wallet.Primitive
    (
    -- * Block
      Block(..)
    , BlockHeader(..)

    -- * Tx
    , Tx(..)
    , TxIn(..)
    , TxOut(..)

    -- * Address
    , Address (..)

    -- * Coin
    , Coin (..)

    -- * UTxO
    , UTxO (..)
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo
    , Dom(..)

    -- * Generic
    , Hash (..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Data.ByteString
    ( ByteString )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Data.Word
    ( Word16, Word32, Word64 )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- * Block

data Block = Block
    { header
        :: !BlockHeader
    , transactions
        :: !(Set Tx)
    } deriving (Show, Generic)

instance NFData Block


data BlockHeader = BlockHeader
    { epochIndex
        :: !Word64
    , slotNumber
        :: !Word16
    , prevBlockHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Generic)

instance NFData BlockHeader


-- * Tx

data Tx = Tx
    { inputs
        :: ![TxIn]
        -- ^ Order of inputs matters in the transaction representation. The
        -- transaction id is computed from the binary representation of a tx,
        -- for which inputs are serialized in a specific order.
    , outputs
        :: ![TxOut]
        -- ^ Order of outputs matter in the transaction representations. Outputs
        -- are used as inputs for next transactions which refer to them using
        -- their indexes. It matters also for serialization.
    } deriving (Show, Generic)

instance NFData Tx


data TxIn = TxIn
    { txId
        :: !(Hash "Tx")
    , txIx
        :: !Word32
    } deriving (Show, Generic, Eq, Ord)

instance NFData TxIn


data TxOut = TxOut
    { address
        :: !Address
    , coin
        :: !Coin
    } deriving (Show, Generic, Eq, Ord)

instance NFData TxOut


-- * Address

newtype Address = Address
    { getAddress :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData Address


-- * Coin

-- | Coins are stored as Lovelace (reminder: 1 Lovelace = 1e6 ADA)
newtype Coin = Coin
    { getCoin :: Word64
    } deriving stock (Show, Ord, Eq, Generic)
      deriving newtype (Enum, Num, Real, Integral)

instance NFData Coin

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin 45000000000000000

instance Semigroup Coin where
    (Coin a) <> (Coin b) =
        invariant
            ( mconcat
                [ "Cardano.Wallet.Primitive.Coin (<>), sum out of bounds: "
                , show a
                , " + "
                , show b
                ]
            )
            (Coin (a + b))
            (<= maxBound)

instance Monoid Coin where
    mempty  = minBound
    mconcat = foldr (<>) mempty


-- * UTxO

newtype UTxO = UTxO { getUTxO :: Map TxIn TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

instance Dom UTxO where
    type DomElem UTxO = TxIn
    dom (UTxO utxo) = Map.keysSet utxo

-- ins⋪ u
excluding :: UTxO -> Set TxIn ->  UTxO
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- a ⊆ b
isSubsetOf :: UTxO -> UTxO -> Bool
isSubsetOf (UTxO a) (UTxO b) =
    a `Map.isSubmapOf` b

-- ins⊲ u
restrictedBy :: UTxO -> Set TxIn -> UTxO
restrictedBy (UTxO utxo) =
    UTxO . Map.restrictKeys utxo

-- u ⊳ outs
restrictedTo :: UTxO -> Set TxOut ->  UTxO
restrictedTo (UTxO utxo) outs =
    UTxO $ Map.filter (`Set.member` outs) utxo


class Dom a where
    type DomElem a :: *
    dom :: a -> Set (DomElem a)


-- * Helpers

newtype Hash (tag :: Symbol) = Hash
    { getHash :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData (Hash tag)


invariant
    :: String
    -> a
    -> (a -> Bool)
    -> a
invariant msg a predicate =
    if predicate a then a else error msg
