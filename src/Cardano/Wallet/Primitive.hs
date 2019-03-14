{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    , txIns
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
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo
    , Dom(..)

    -- * Slotting
    , SlotId (..)
    , isValidSlotId
    , slotsPerEpoch
    , slotDiff
    , slotIncr

    -- * Polymorphic
    , Hash (..)
    , ShowFmt (..)
    , invariant
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Data.Word
    ( Word16, Word32, Word64 )
import Fmt
    ( Buildable (..)
    , blockListF
    , blockListF'
    , fmt
    , indentF
    , nameF
    , ordinalF
    , prefixF
    , suffixF
    )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T

-- * Block

data Block = Block
    { header
        :: !BlockHeader
    , transactions
        :: !(Set Tx)
    } deriving (Show, Eq, Ord, Generic)

instance NFData Block

instance Buildable Block where
    build (Block h txs) =
        "Block (" <> build h <> "): \n" <>
        indentF 2 (blockListF (Set.toList txs))


data BlockHeader = BlockHeader
    { slotId
        :: SlotId
    , prevBlockHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Ord, Generic)

instance NFData BlockHeader

instance Buildable BlockHeader where
    build (BlockHeader s prev) = mempty
        <> build s
        <> " ~"
        <> prefixF 8 prevF
        <> "..."
        <> suffixF 8 prevF
      where
        prevF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash prev

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

instance Buildable Tx where
    build (Tx ins outs) = mempty
        <> nameF "inputs" (blockListF ins)
        <> nameF "outputs" (blockListF outs)

txIns :: Set Tx -> Set TxIn
txIns =
    foldMap (Set.fromList . inputs)

updatePending :: Block -> Set Tx -> Set Tx
updatePending b =
    let
        isStillPending ins =
            Set.null . Set.intersection ins . Set.fromList . inputs
    in
        Set.filter (isStillPending (txIns $ transactions b))


data TxIn = TxIn
    { inputId
        :: !(Hash "Tx")
    , inputIx
        :: !Word32
    } deriving (Show, Generic, Eq, Ord)

instance NFData TxIn

instance Buildable TxIn where
    build txin = mempty
        <> ordinalF (inputIx txin + 1)
        <> " "
        <> prefixF 8 txF
        <> "..."
        <> suffixF 8 txF
      where
        txF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash $ inputId txin


data TxOut = TxOut
    { address
        :: !Address
    , coin
        :: !Coin
    } deriving (Show, Generic, Eq, Ord)

instance NFData TxOut

instance Buildable TxOut where
    build txout = mempty
        <> build (coin txout)
        <> " @ "
        <> prefixF 8 addrF
        <> "..."
        <> suffixF 8 addrF
      where
        addrF = build $ address txout


instance Buildable (TxIn, TxOut) where
    build (txin, txout) = build txin <> " ==> " <> build txout


-- * Address

newtype Address = Address
    { getAddress :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData Address

instance Buildable Address where
    build = build . T.decodeUtf8 . encodeBase58 bitcoinAlphabet . getAddress


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

instance Buildable Coin where
    build = build . getCoin

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

instance Buildable UTxO where
    build (UTxO utxo) =
        nameF "UTxO" $ blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = build inp <> " => " <> build out


balance :: UTxO -> Integer
balance =
    Map.foldl' fn 0 . getUTxO
  where
    fn :: Integer -> TxOut -> Integer
    fn total out = total + fromIntegral (getCoin (coin out))

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


-- * Slotting

-- | Hard-coded for the time being
slotsPerEpoch :: Word64
slotsPerEpoch = 21600

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochIndex :: !Word64
  , slotNumber :: !Word16
  } deriving stock (Show, Eq, Ord, Generic)

instance NFData SlotId

instance Buildable SlotId where
    build (SlotId e s) = build e <> "." <> build s

instance Enum SlotId where
    toEnum i
        | i < 0 = error "SlotId.toEnum: bad argument"
        | otherwise = slotIncr (fromIntegral i) (SlotId 0 0)
    fromEnum (SlotId e s)
        | n > fromIntegral (maxBound @Int) =
            error "SlotId.fromEnum: arithmetic overflow"
        | otherwise = fromIntegral n
      where
        n :: Word64
        n = fromIntegral e * fromIntegral slotsPerEpoch + fromIntegral s

-- | Add a number of slots to an (Epoch, LocalSlotIndex) pair, where the number
-- of slots can be greater than one epoch.
slotIncr :: Word64 -> SlotId -> SlotId
slotIncr n slot = SlotId e s
  where
    e = fromIntegral (fromIntegral n' `div` slotsPerEpoch)
    s = fromIntegral (fromIntegral n' `mod` slotsPerEpoch)
    n' = n + fromIntegral (fromEnum slot)

-- | @slotDiff a b@ is the number of slots by which @a@ is greater than @b@.
slotDiff :: SlotId -> SlotId -> Integer
slotDiff s1 s2 = fromIntegral (fromEnum s1 - fromEnum s2)

-- | Whether the epoch index and slot number are in range.
isValidSlotId :: SlotId -> Bool
isValidSlotId (SlotId e s) =
    e >= 0 && s >= 0 && s < fromIntegral slotsPerEpoch


-- * Polymorphic

class Dom a where
    type DomElem a :: *
    dom :: a -> Set (DomElem a)


newtype Hash (tag :: Symbol) = Hash
    { getHash :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData (Hash tag)


-- | A polymorphic wrapper type with a custom show instance to display data
-- through 'Buildable' instances.
newtype ShowFmt a = ShowFmt a
    deriving (Generic, Eq, Ord)

instance Buildable a => Show (ShowFmt a) where
    show (ShowFmt a) = fmt (build a)


-- | Check whether an invariants holds or not.
--
-- >>> invariant "not empty" [1,2,3] (not . null)
-- [1, 2, 3]
--
-- >>> invariant "not empty" [] (not . null)
-- *** Exception: not empty
invariant
    :: String -- ^ A title / message to throw in case of violation
    -> a
    -> (a -> Bool)
    -> a
invariant msg a predicate =
    if predicate a then a else error msg
