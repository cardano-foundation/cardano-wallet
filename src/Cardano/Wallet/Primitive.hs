{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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
    } deriving (Show, Ord, Eq, Generic)

instance NFData Coin

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty  = Coin 0
  mconcat = foldr (<>) mempty


-- * UTxO

newtype UTxO = UTxO { getUTxO :: Map TxIn TxOut }
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (Semigroup, Monoid)

instance NFData UTxO

instance Dom UTxO where
  type DomElem UTxO = TxIn
  dom (UTxO utxo) = Map.keysSet utxo


class Dom a where
  type DomElem a :: *
  dom :: a -> Set (DomElem a)


-- * Generic Wrappers

newtype Hash (tag :: Symbol) = Hash
    { getHash :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData (Hash tag)
