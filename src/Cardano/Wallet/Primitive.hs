{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
    , txId
    , txIns
    , txOutsOurs
    , updatePending

    -- * Address
    , Address (..)
    , IsOurs(..)

    -- * Coin
    , Coin (..)
    , isValidCoin

    -- * UTxO
    , UTxO (..)
    , balance
    , changeUTxO
    , utxoFromTx
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
import Control.Monad.Trans.State.Strict
    ( State, runState, state )
import Data.ByteString
    ( ByteString )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Set
    ( Set )
import Data.Traversable
    ( for )
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
    } deriving (Show, Eq, Generic)

instance NFData Block


data BlockHeader = BlockHeader
    { epochIndex
        :: !Word64
    , slotNumber
        :: !Word16
    , prevBlockHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Generic)

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
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

-- | Calculating a transaction id. Assumed to be effectively injective
txId :: Tx -> Hash "Tx"
txId = error
    "txId: not yet implemented. We need the ability to encode a Tx to CBOR for:\
    \ BA.convert . hash @_ @Blake2b_256 . CBOR.toStrictByteString . encodeTx"

txIns :: Set Tx -> Set TxIn
txIns =
    foldMap (Set.fromList . inputs)

txOutsOurs
    :: forall s. (IsOurs s)
    => Set Tx
    -> s
    -> (Set TxOut, s)
txOutsOurs txs =
    runState $ Set.fromList <$> forMaybe (foldMap outputs txs) pick
  where
    pick :: TxOut -> State s (Maybe TxOut)
    pick out = do
        predicate <- state $ isOurs (address out)
        return $ if predicate then Just out else Nothing

    forMaybe :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
    forMaybe xs = fmap catMaybes . for xs

updatePending :: Block -> Set Tx -> Set Tx
updatePending b =
    let
        isStillPending ins = Set.null . Set.intersection ins . Set.fromList . inputs
    in
        Set.filter (isStillPending (txIns $ transactions b))


data TxIn = TxIn
    { inputId
        :: !(Hash "Tx")
    , inputIx
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

-- | This abstraction exists to give us the ability to keep the wallet business
-- logic agnostic to the address derivation and discovery mechanisms.
--
-- This is needed because two different address schemes lives on Cardano:
--   - A hierarchical random scheme:
--      rather 'custom' made, with several flaws; this is the original and now
--      legacy address scheme.
--
--   - A hierarchical sequential scheme:
--      a new scheme based on the BIP-0044 specification, which is better suited
--      for our present needs.
--
-- In practice, we will need a wallet that can support both, even if not at the
-- same time, and this little abstraction can buy us this without introducing
-- too much overhead.
class IsOurs s where
    isOurs :: Address -> s -> (Bool, s)


-- * Coin

-- | Coins are stored as Lovelace (reminder: 1 Lovelace = 1e6 ADA)
newtype Coin = Coin
    { getCoin :: Word64
    } deriving stock (Show, Ord, Eq, Generic)

instance NFData Coin

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin 45000000000000000

isValidCoin :: Coin -> Bool
isValidCoin c = c >= minBound && c <= maxBound


-- * UTxO

newtype UTxO = UTxO { getUTxO :: Map TxIn TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

instance Dom UTxO where
    type DomElem UTxO = TxIn
    dom (UTxO utxo) = Map.keysSet utxo

balance :: UTxO -> Integer
balance =
    Map.foldl' (\total out -> total + fromIntegral (getCoin (coin out))) 0 . getUTxO

utxoFromTx :: Tx -> UTxO
utxoFromTx tx@(Tx _ outs) =
    UTxO $ Map.fromList $ zip (TxIn (txId tx) <$> [0..]) outs

changeUTxO
    :: IsOurs s
    => Set Tx
    -> s
    -> (UTxO, s)
changeUTxO pending = runState $ do
    ours <- state $ txOutsOurs pending
    let utxo = foldMap utxoFromTx pending
    let ins = txIns pending
    return $ (utxo `restrictedTo` ours) `restrictedBy` ins

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


-- * Generic

class Dom a where
    type DomElem a :: *
    dom :: a -> Set (DomElem a)


newtype Hash (tag :: Symbol) = Hash
    { getHash :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData (Hash tag)
