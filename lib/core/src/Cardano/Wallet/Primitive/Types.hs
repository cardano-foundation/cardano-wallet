{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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

module Cardano.Wallet.Primitive.Types
    (
    -- * Block
      Block(..)
    , BlockHeader(..)

    -- * Tx
    , Tx(..)
    , TxId(..)
    , TxIn(..)
    , TxOut(..)
    , TxMeta(..)
    , Direction(..)
    , TxStatus(..)
    , TxWitness (..)
    , txIns

    -- * Address
    , Address (..)
    , AddressState (..)

    -- * Coin
    , Coin (..)
    , isValidCoin

    -- * UTxO
    , UTxO (..)
    , balance
    , balance'
    , pickRandom
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo
    , Dom(..)

    -- * Slotting
    , SlotId (..)
    , slotRatio
    , flatSlot
    , fromFlatSlot

    -- * Wallet Metadata
    , WalletMetadata(..)
    , WalletId(..)
    , WalletName(..)
    , walletNameMinLength
    , walletNameMaxLength
    , WalletState(..)
    , WalletDelegation (..)
    , WalletPassphraseInfo(..)
    , WalletBalance(..)

    -- * Stake Pools
    , PoolId(..)

    -- * Polymorphic
    , Hash (..)
    , ShowFmt (..)
    , invariant
    , distance
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Set
    ( Set )
import Data.String
    ( fromString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time
    ( UTCTime )
import Data.Word
    ( Word16, Word32, Word64 )
import Fmt
    ( Buildable (..)
    , blockListF
    , blockListF'
    , fixedF
    , fmt
    , indentF
    , ordinalF
    , prefixF
    , suffixF
    )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import Numeric.Natural
    ( Natural )

import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                             Wallet Metadata
-------------------------------------------------------------------------------}

-- | Additional information about a wallet that can't simply be derived from
-- the blockchain like @Wallet s t@ is.
--
-- Whereas @Wallet s t@ in 'Cardano.Wallet.Primitive' can be updated using
-- @applyBlock@, @WalletMetadata@ is not*.
--
-- *) Except for possibly 'status' and 'delegation'...
data WalletMetadata = WalletMetadata
    { name
        :: !WalletName
    , passphraseInfo
        :: !(Maybe WalletPassphraseInfo)
    , status
        :: !WalletState
    , delegation
        :: !(WalletDelegation PoolId)
    } deriving (Eq, Show, Generic)

instance NFData WalletMetadata

-- | Length-restricted name of a wallet
newtype WalletName = WalletName { getWalletName ::  Text }
    deriving (Generic, Eq, Show)

instance NFData WalletName

instance FromText WalletName where
    fromText t
        | T.length t < walletNameMinLength =
            Left $ TextDecodingError $
                "name is too short: expected at least "
                    <> show walletNameMinLength <> " character"
        | T.length t > walletNameMaxLength =
            Left $ TextDecodingError $
                "name is too long: expected at most "
                    <> show walletNameMaxLength <> " characters"
        | otherwise =
            return $ WalletName t

instance ToText WalletName where
    toText = getWalletName

-- | Calling 'fromText @WalletName' on shorter longer string will fail.
walletNameMinLength :: Int
walletNameMinLength = 1

-- | Calling 'fromText @WalletName' on a longer string will fail.
walletNameMaxLength :: Int
walletNameMaxLength = 255

newtype WalletId = WalletId { getWalletId :: Digest Blake2b_160 }
    deriving (Generic, Eq, Ord, Show)

instance NFData WalletId

instance FromText WalletId where
    fromText txt = maybe
        (Left $ TextDecodingError msg)
        (Right . WalletId)
        (decodeHex txt >>= digestFromByteString @_ @ByteString)
      where
        msg = "wallet id should be an hex-encoded string of 40 characters"
        decodeHex =
            either (const Nothing) Just . convertFromBase Base16 . T.encodeUtf8

instance ToText WalletId where
    toText = T.decodeUtf8 . convertToBase Base16 . getWalletId

instance Buildable WalletId where
    build wid = prefixF 8 widF <> "..." <> suffixF 8 widF
      where
        widF = toText wid

data WalletState
    = Ready
    | Restoring !(Quantity "percent" Percentage)
    deriving (Generic, Eq, Show)

instance NFData WalletState

data WalletDelegation poolId
    = NotDelegating
    | Delegating !poolId
    deriving (Generic, Eq, Show)
deriving instance Functor WalletDelegation
instance NFData poolId => NFData (WalletDelegation poolId)

newtype WalletPassphraseInfo = WalletPassphraseInfo
    { lastUpdatedAt :: UTCTime }
    deriving (Generic, Eq, Ord, Show)

instance NFData WalletPassphraseInfo

data WalletBalance = WalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

{-------------------------------------------------------------------------------
                                  Stake Pools
-------------------------------------------------------------------------------}

-- | Represent stake pool identifier. Note that the internal representation is
-- left open currently, until we figure out a better type for those.
newtype PoolId = PoolId
    { getPoolId :: Text }
    deriving (Generic, Eq, Show)

instance NFData PoolId

{-------------------------------------------------------------------------------
                                    Block
-------------------------------------------------------------------------------}

data Block = Block
    { header
        :: !BlockHeader
    , transactions
        :: ![Tx]
    } deriving (Show, Eq, Ord, Generic)

instance NFData Block

instance Buildable Block where
    build (Block h txs) = mempty
        <> build h
        <> "\n"
        <> indentF 4 (blockListF txs)


data BlockHeader = BlockHeader
    { slotId
        :: SlotId
    , prevBlockHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Ord, Generic)

instance NFData BlockHeader

instance Buildable BlockHeader where
    build (BlockHeader s prev) = mempty
        <> prefixF 8 prevF
        <> "..."
        <> suffixF 8 prevF
        <> " (" <> build s <> ")"
      where
        prevF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash prev

{-------------------------------------------------------------------------------
                                      Tx
-------------------------------------------------------------------------------}

data Tx = Tx
    { inputs
        :: ![TxIn]
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
    build (Tx ins outs) = mempty
        <> blockListF' "~>" build ins
        <> blockListF' "<~" build outs

-- | An abstraction for computing transaction id. The 'target' is an open-type
-- that can be used to discriminate on. For instance:
--
-- @
-- instance TxId HttpBridge where
--   txId _ = {- ... -}
-- @
--
-- Note that `txId` is ambiguous and requires therefore a type application.
-- Likely, a corresponding target would be found in scope (requires however
-- ScopedTypeVariables).
--
-- For example, assuming there's a type 'target' in scope, one can simply do:
--
-- @
-- txId @target tx
-- @
class TxId target where
    txId :: Tx -> Hash "Tx"

txIns :: Set Tx -> Set TxIn
txIns =
    foldMap (Set.fromList . inputs)

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
        <> build (inputId txin)

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

data TxMeta = TxMeta
    { status :: !TxStatus
    , direction :: !Direction
    , slotId :: !SlotId
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Show, Eq, Ord, Generic)

instance NFData TxMeta

instance Buildable TxMeta where
    build (TxMeta s d sl (Quantity a)) = mempty
        <> (case d of; Incoming -> "+"; Outgoing -> "-")
        <> fixedF @Double 6 (fromIntegral a / 1e6)
        <> " " <> build s
        <> " since " <> build sl

data TxStatus
    = Pending
    | InLedger
    | Invalidated
    deriving (Show, Eq, Ord, Generic)

instance NFData TxStatus

instance Buildable TxStatus where
    build = \case
        Pending -> "pending"
        InLedger -> "in ledger"
        Invalidated -> "invalidated"

instance FromText TxStatus where
    fromText txt = case txt of
        "pending" -> Right Pending
        "in_ledger" -> Right InLedger
        "invalidated" -> Right Invalidated
        _ ->
            Left . TextDecodingError $ show txt
                <> " is neither \"pending\", \"in_ledger\", nor \"invalidated\""

instance ToText TxStatus where
    toText Pending = "pending"
    toText InLedger = "in_ledger"
    toText Invalidated = "invalidated"

data Direction
    = Outgoing -- ^ The wallet balance decreases.
    | Incoming -- ^ The wallet balance increases or stays the same.
    deriving (Show, Eq, Ord, Generic)

instance NFData Direction

instance Buildable Direction where
    build = \case
        Outgoing -> "outgoing"
        Incoming -> "incoming"

instance FromText Direction where
    fromText txt = case txt of
        "outgoing" -> Right Outgoing
        "incoming" -> Right Incoming
        _ ->
            Left . TextDecodingError $ show txt
                <> " is neither \"outgoing\", nor \"incoming\""

instance ToText Direction where
    toText Outgoing = "outgoing"
    toText Incoming = "incoming"

data TxWitness
    = PublicKeyWitness ByteString (Hash "signature")
      -- ^ A signature of a transaction by the owner of the address of an input.
      --
      -- TODO: Use the @XPub@ type instead of @ByteString@
    | ScriptWitness ByteString
      -- ^ Related to Plutus
    | RedeemWitness ByteString
      -- ^ Used to redeem ADA from the pre-sale
    deriving (Eq, Show)

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | Representation of Cardano addresses. Addresses are basically a
-- human-friendly representation of public keys. Historically in Cardano, there
-- exists different sort of addresses, and new one are to come. So far, we can
-- distinguish between three types of addresses:
--
-- - Byron Random addresses, which holds a payload with derivation path details
-- - Byron Sequential addresses, also known as Icarus'style addresses
-- - Shelley base addresses, see also [implementation-decisions/address](https://github.com/input-output-hk/implementation-decisions/blob/master/text/0001-address.md)
--
-- For more details, see [About Address Derivation](https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Derivation)
newtype Address = Address
    { getAddress :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData Address

instance Buildable Address where
    build = build . toText

instance FromText Address where
    fromText x = maybe
        (Left $ TextDecodingError err)
        (pure . Address)
        (decodeBase58 bitcoinAlphabet $ T.encodeUtf8 x)
      where
        err = "Unable to decode Address: expected Base58 encoding"

instance ToText Address where
    toText = T.decodeUtf8 . encodeBase58 bitcoinAlphabet . getAddress


-- | Denotes if an address has been previously used or not... whether that be
-- in the output of a transaction on the blockchain or one in our pending set.
data AddressState = Used | Unused
    deriving (Eq, Generic, Show)

instance FromText AddressState where
    fromText = \case
        "used" ->
            Right Used
        "unused" ->
            Right Unused
        _ ->
            Left $ TextDecodingError "Unable to decode address state: \
            \it's neither \"used\" nor \"unused\""

instance ToText AddressState where
    toText = T.pack . (\(h:q) -> Char.toLower h : q) . show

{-------------------------------------------------------------------------------
                                     Coin
-------------------------------------------------------------------------------}

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


{-------------------------------------------------------------------------------
                                    UTxO
-------------------------------------------------------------------------------}

newtype UTxO = UTxO { getUTxO :: Map TxIn TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

instance Dom UTxO where
    type DomElem UTxO = TxIn
    dom (UTxO utxo) = Map.keysSet utxo

instance Buildable UTxO where
    build (UTxO utxo) =
        blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = build inp <> " => " <> build out

-- | Pick a random element from a UTxO, returns 'Nothing' if the UTxO is empty.
-- Otherwise, returns the selected entry and, the UTxO minus the selected one.
pickRandom
    :: MonadRandom m
    => UTxO
    -> m (Maybe (TxIn, TxOut), UTxO)
pickRandom (UTxO utxo)
    | Map.null utxo =
        return (Nothing, UTxO utxo)
    | otherwise = do
        ix <- fromEnum <$> generateBetween 0 (toEnum (Map.size utxo - 1))
        return (Just $ Map.elemAt ix utxo, UTxO $ Map.deleteAt ix utxo)

-- | Compute the balance of a UTxO
balance :: UTxO -> Natural
balance =
    Map.foldl' fn 0 . getUTxO
  where
    fn :: Natural -> TxOut -> Natural
    fn tot out = tot + fromIntegral (getCoin (coin out))

-- | Compute the balance of a unwrapped UTxO
balance' :: [(TxIn, TxOut)] -> Word64
balance' =
    fromIntegral . balance . UTxO . Map.fromList

-- | ins⋪ u
excluding :: UTxO -> Set TxIn ->  UTxO
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- | a ⊆ b
isSubsetOf :: UTxO -> UTxO -> Bool
isSubsetOf (UTxO a) (UTxO b) =
    a `Map.isSubmapOf` b

-- | ins⊲ u
restrictedBy :: UTxO -> Set TxIn -> UTxO
restrictedBy (UTxO utxo) =
    UTxO . Map.restrictKeys utxo

-- | u ⊳ outs
restrictedTo :: UTxO -> Set TxOut -> UTxO
restrictedTo (UTxO utxo) outs =
    UTxO $ Map.filter (`Set.member` outs) utxo


{-------------------------------------------------------------------------------
                                   Slotting

  Note that we do not define any operation to perform slotting arithmetic of any
  kind. Instead of manipulating slots, we do simply look them up from the chain,
  in their corresponding block. This should be probably enough to cover for
  pretty much all our needs.

  If slotting arithmetic has to be introduced, it will require proper thoughts.
-------------------------------------------------------------------------------}

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochNumber :: !Word64
  , slotNumber :: !Word16
  } deriving stock (Show, Read, Eq, Ord, Generic)

instance NFData SlotId

instance Buildable SlotId where
    build (SlotId e s) = fromString (show e) <> "." <> fromString (show s)

-- | Compute the approximate ratio / progress between two slots. This is an
-- approximation for a few reasons, one of them being that we hard code the
-- epoch length as a static number whereas it may vary in practice.
slotRatio
    :: SlotId
        -- ^ Numerator
    -> SlotId
        -- ^ Denominator
    -> Quantity "percent" Percentage
slotRatio a b =
    let
        n0 = flatSlot a
        n1 = flatSlot b
        tolerance = 5
    in if distance n0 n1 < tolerance || n0 >= n1 then
        maxBound
    else
        Quantity $ toEnum $ fromIntegral $ (100 * n0) `div` n1

-- | Convert a 'SlotId' to the number of slots since genesis.
flatSlot :: SlotId -> Word64
flatSlot (SlotId e s) = epochLength * e + fromIntegral s

-- | Convert a 'flatSlot' index to 'SlotId'.
fromFlatSlot :: Word64 -> SlotId
fromFlatSlot n = SlotId e (fromIntegral s)
  where
    e = n `div` epochLength
    s = n `mod` epochLength

epochLength :: Integral a => a
epochLength = 21600

{-------------------------------------------------------------------------------
                               Polymorphic Types
-------------------------------------------------------------------------------}

-- | Allows us to define the "domain" of any type — @UTxO@ in particular — and
-- use 'dom' to refer to the /inputs/ of an /utxo/.
--
-- This is the terminology used in the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
-- uses.
class Dom a where
    type DomElem a :: *
    dom :: a -> Set (DomElem a)

newtype Hash (tag :: Symbol) = Hash
    { getHash :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData (Hash tag)

instance Buildable (Hash "BlockHeader") where
    build h = mempty
        <> prefixF 8 builder
        <> "..."
        <> suffixF 8 builder
      where
        builder = T.decodeUtf8 . convertToBase Base16 . getHash $ h

instance Buildable (Hash "Tx") where
    build h = mempty
        <> prefixF 8 builder
        <> "..."
        <> suffixF 8 builder
      where
        builder = build . toText $ h

instance FromText (Hash "Tx") where
    fromText x = either
        (const $ Left $ TextDecodingError err)
        (pure . Hash)
        (convertFromBase Base16 $ T.encodeUtf8 x)
      where
        err = "Unable to decode (Hash \"Tx\"): \
                    \expected Base16 encoding"


instance ToText (Hash "Tx") where
    toText = T.decodeUtf8 . convertToBase Base16 . getHash

-- | A polymorphic wrapper type with a custom show instance to display data
-- through 'Buildable' instances.
newtype ShowFmt a = ShowFmt a
    deriving (Generic, Eq, Ord)

instance NFData a => NFData (ShowFmt a)

instance Buildable a => Show (ShowFmt a) where
    show (ShowFmt a) = fmt (build a)

instance {-# OVERLAPS #-} (Buildable a, Foldable f) => Show (ShowFmt (f a)) where
    show (ShowFmt a) = fmt (blockListF a)

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

-- | Compute distance between two numeric values |a - b|
distance :: (Ord a, Num a) => a -> a -> a
distance a b =
    if a < b then b - a else a - b
