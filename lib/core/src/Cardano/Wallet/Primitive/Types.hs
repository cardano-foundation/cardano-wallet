{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
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
    , DefineTx(..)
    , TxIn(..)
    , TxOut(..)
    , TxMeta(..)
    , Direction(..)
    , TxStatus(..)
    , TxWitness(..)
    , TransactionInfo (..)
    , FeePolicy (..)
    , txIns
    , isPending

    -- * Address
    , Address (..)
    , AddressState (..)
    , EncodeAddress (..)
    , DecodeAddress (..)

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
    , UTxOStatistics (..)
    , HistogramBar (..)
    , BoundType
    , computeUtxoStatistics
    , log10

    -- * Slotting
    , SlotId (..)
    , SlotParameters (..)
    , SlotLength (..)
    , EpochLength (..)
    , StartTime (..)
    , slotRatio
    , flatSlot
    , fromFlatSlot
    , slotStartTime
    , slotCeiling
    , slotFloor
    , slotAt
    , slotDifference
    , slotMinBound
    , slotPred
    , slotSucc
    , slotRangeFromTimeRange

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
    , poolIdBytesLength

    -- * Querying
    , SortOrder (..)

    -- * Ranges
    , Range (..)
    , RangeBound (..)
    , wholeRange
    , isAfterRange
    , isBeforeRange
    , isSubrangeOf
    , isWithinRange
    , mapRangeLowerBound
    , mapRangeUpperBound
    , rangeIsFinite
    , rangeIsSingleton
    , rangeIsValid
    , rangeHasLowerBound
    , rangeHasUpperBound
    , rangeLowerBound
    , rangeUpperBound

    -- * ProtocolMagic
    , ProtocolMagic (..)

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
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Int
    ( Int32 )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Set
    ( Set )
import Data.String
    ( fromString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, addUTCTime, diffUTCTime )
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
    , tupleF
    )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import Numeric.Natural
    ( Natural )

import qualified Control.Foldl as F
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as Builder
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
    , creationTime
        :: !UTCTime
    , passphraseInfo
        :: !(Maybe WalletPassphraseInfo)
    , status
        :: !WalletState
    , delegation
        :: !(WalletDelegation PoolId)
    } deriving (Eq, Show, Generic)

instance NFData WalletMetadata

instance Buildable WalletMetadata where
    build (WalletMetadata wName wTime _ wStatus wDelegation) = mempty
        <> build wName <> " (" <> build wStatus <> "), "
        <> "created at " <> build wTime <> ", "
        <> build wDelegation

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

instance Buildable WalletName where
    build = build . toText

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

instance Ord WalletState where
    Ready <= Ready = True
    Ready <= Restoring _ = False
    Restoring _ <= Ready = True
    Restoring a <= Restoring b = a <= b

instance Buildable WalletState where
    build = \case
        Ready ->
            "restored"
        Restoring (Quantity p) ->
            "still restoring (" <> build (toText p) <> ")"

data WalletDelegation poolId
    = NotDelegating
    | Delegating !poolId
    deriving (Generic, Eq, Show)
deriving instance Functor WalletDelegation
instance NFData poolId => NFData (WalletDelegation poolId)

instance Buildable poolId => Buildable (WalletDelegation poolId) where
    build = \case
        NotDelegating ->
            "not delegating"
        Delegating poolId ->
            "delegating to " <> build poolId

newtype WalletPassphraseInfo = WalletPassphraseInfo
    { lastUpdatedAt :: UTCTime }
    deriving (Generic, Eq, Ord, Show)

instance NFData WalletPassphraseInfo

data WalletBalance = WalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

{-------------------------------------------------------------------------------
                                   Queries
-------------------------------------------------------------------------------}

-- | Represents a sort order, applicable to the results returned by a query.
data SortOrder
    = Ascending
        -- ^ Sort in ascending order.
    | Descending
        -- ^ Sort in descending order.
    deriving (Bounded, Enum, Eq, Generic, Show)

instance ToText SortOrder where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText SortOrder where
    fromText = fromTextToBoundedEnum SnakeLowerCase

-- | Represents a range of values.
--
-- A range is defined by two /optional/ bounds:
--
-- 1. an /inclusive/ lower bound
-- 2. an /inclusive/ upper bound
--
-- There are four cases:
--
-- +---------------------------------+-------------+---------------------------+
-- | Value                           | Range       | Membership                |
-- |                                 | Represented | Function                  |
-- +=================================+=============+===========================+
-- | @'Range' ('Just' x) ('Just' y)@ | @[ x, y ]@  | @\\p -> p >= x && p <= y@ |
-- +---------------------------------+-------------+---------------------------+
-- | @'Range' ('Just' x) 'Nothing' @ | @[ x, ∞ )@  | @\\p -> p >= x          @ |
-- +---------------------------------+-------------+---------------------------+
-- | @'Range' 'Nothing'  ('Just' y)@ | @(−∞, y ]@  | @\\p -> p <= y          @ |
-- +---------------------------------+-------------+---------------------------+
-- | @'Range' 'Nothing'  'Nothing' @ | @(−∞, ∞ )@  | @\\p -> True            @ |
-- +---------------------------------+-------------+---------------------------+
--
data Range a = Range
    { inclusiveLowerBound :: Maybe a
    , inclusiveUpperBound :: Maybe a
    } deriving (Eq, Functor, Show)

-- | Apply a function to the lower bound of a range.
mapRangeLowerBound :: (a -> a) -> Range a -> Range a
mapRangeLowerBound f (Range x y) = Range (f <$> x) y

-- | Apply a function to the upper bound of a range.
mapRangeUpperBound :: (a -> a) -> Range a -> Range a
mapRangeUpperBound f (Range x y) = Range x (f <$> y)

-- | Represents a range boundary.
data RangeBound a
    = NegativeInfinity
    | InclusiveBound a
    | PositiveInfinity
    deriving (Eq, Ord)

-- | The range that includes everything.
wholeRange :: Range a
wholeRange = Range Nothing Nothing

-- | Returns 'True' if (and only if) the given range has an upper bound and the
--   specified value is greater than the upper bound.
isAfterRange :: Ord a => a -> Range a -> Bool
isAfterRange x (Range _ high) =
    maybe False (x >) high

-- | Returns 'True' if (and only if) the given range has a lower bound and the
--   specified value is smaller than the lower bound.
isBeforeRange :: Ord a => a -> Range a -> Bool
isBeforeRange x (Range low _) =
    maybe False (x <) low

-- | Returns 'True' if (and only if) the given value is not smaller than the
--   lower bound (if present) of the given range and is not greater than the
--   upper bound (if present) of the given range.
isWithinRange :: Ord a => a -> Range a -> Bool
isWithinRange x (Range low high) =
    (maybe True (x >=) low) &&
    (maybe True (x <=) high)

-- | Returns 'True' if (and only if) the given range has a lower bound.
rangeHasLowerBound :: Range a -> Bool
rangeHasLowerBound = isJust . inclusiveLowerBound

-- | Returns 'True' if (and only if) the given range has an upper bound.
rangeHasUpperBound :: Range a -> Bool
rangeHasUpperBound = isJust . inclusiveUpperBound

-- | Returns 'True' if (and only if) the given range has both a lower and upper
--   bound.
rangeIsFinite :: Range a -> Bool
rangeIsFinite r = rangeHasLowerBound r && rangeHasUpperBound r

-- | Returns 'True' if (and only if) the range covers exactly one value.
rangeIsSingleton :: Eq a => Range a -> Bool
rangeIsSingleton (Range a b) = ((==) <$> a <*> b) == Just True

-- | Returns 'True' if (and only if) the lower bound of a range is not greater
--   than its upper bound.
rangeIsValid :: Ord a => Range a -> Bool
rangeIsValid (Range a b) = ((<=) <$> a <*> b) /= Just False

-- | Get the lower bound of a 'Range'.
rangeLowerBound :: Range a -> RangeBound a
rangeLowerBound = maybe NegativeInfinity InclusiveBound . inclusiveLowerBound

-- | Get the upper bound of a 'Range'.
rangeUpperBound :: Range a -> RangeBound a
rangeUpperBound = maybe PositiveInfinity InclusiveBound . inclusiveUpperBound

-- | Returns 'True' if (and only if) the first given range is a subrange of the
--   second given range.
isSubrangeOf :: Ord a => Range a -> Range a -> Bool
isSubrangeOf r1 r2 =
    rangeLowerBound r1 >= rangeLowerBound r2 &&
    rangeUpperBound r1 <= rangeUpperBound r2

{-------------------------------------------------------------------------------
                                  Stake Pools
-------------------------------------------------------------------------------}

-- | Represent stake pool identifier.
--   VRF PubKey: 32 bytes (ristretto25519) which translates to 64 hex character string
--   see https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/format.md
newtype PoolId = PoolId { getPoolId :: ByteString }
    deriving (Generic, Eq, Show)

poolIdBytesLength :: Int
poolIdBytesLength = 32

instance NFData PoolId

instance Buildable PoolId where
    build poolId = mempty
        <> prefixF 8 poolIdF
        <> "..."
        <> suffixF 8 poolIdF
      where
        poolIdF = build (toText poolId)

instance ToText PoolId where
    toText = T.decodeUtf8
        . convertToBase Base16
        . getPoolId

instance FromText PoolId where
    fromText t = case convertFromBase Base16 $ T.encodeUtf8 t of
        Left err -> textDecodingError
            ("stake pool id wrongly formatted: expected hex string"
             <> " - the exact error: "
             <> err)
        Right bytes ->
            if BS.length bytes == poolIdBytesLength then
                Right $ PoolId bytes
            else
                Left $ TextDecodingError $ "stake pool id invalid: expected "
                 <> show poolIdBytesLength
                 <> " bytes but got "
                 <> show (BS.length bytes)
        where
            textDecodingError = Left . TextDecodingError . show

{-------------------------------------------------------------------------------
                                    Block
-------------------------------------------------------------------------------}

data Block tx = Block
    { header
        :: !BlockHeader
    , transactions
        :: ![tx]
    } deriving (Show, Eq, Ord, Generic)

instance NFData tx => NFData (Block tx)

instance Buildable tx => Buildable (Block tx) where
    build (Block h txs) = mempty
        <> build h
        <> if null txs then " ∅" else "\n" <> indentF 4 (blockListF txs)

data BlockHeader = BlockHeader
    { slotId
        :: SlotId
    , blockHeight
        :: Quantity "block" Natural
    , prevBlockHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Ord, Generic)

instance NFData BlockHeader

instance Buildable BlockHeader where
    build (BlockHeader s (Quantity bh) prev) = "Block ("
        <> build s
        <> "#" <> build (show bh)
        <> ") -> "
        <> prefixF 8 prevF
        <> "..."
        <> suffixF 8 prevF
      where
        prevF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash prev

{-------------------------------------------------------------------------------
                                      Tx
-------------------------------------------------------------------------------}

-- | An abstraction for computing transaction id. The 'target' is an open-type
-- that can be used to discriminate on. For instance:
--
-- @
-- instance DefineTx (HttpBridge network) where
--   txId _ = {- ... -}
--   ,,,
-- @
--
-- Note that `txId` is ambiguous and requires therefore a type application.
-- Likely, a corresponding target would be found in scope (requires however
-- ScopedTypeVariables).
--
-- For example, assuming there's a type 't' in scope, one can simply do:
--
-- @
-- txId @t tx
-- @
class (NFData (Tx t), Show (Tx t), Ord (Tx t), Buildable (Tx t)) => DefineTx t
  where
    type Tx t :: *
    txId :: Tx t -> Hash "Tx"
    -- | Compute a transaction id; assumed to be effectively injective.
    -- It returns an hex-encoded 64-byte hash.
    --
    -- NOTE: This is a rather expensive operation
    inputs :: Tx t -> [TxIn]
    -- | Get transaction's inputs, ordered
    outputs :: Tx t -> [TxOut]
    -- | Get transaction's outputs, ordered

txIns :: forall t. DefineTx t => Set (Tx t) -> Set TxIn
txIns = foldMap (Set.fromList . inputs @t)

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
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance NFData TxStatus

instance Buildable TxStatus where
    build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText TxStatus where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText TxStatus where
    toText = toTextFromBoundedEnum SnakeLowerCase

-- | The effect of a @Transaction@ on the wallet balance.
data Direction
    = Outgoing -- ^ The wallet balance decreases.
    | Incoming -- ^ The wallet balance increases or stays the same.
    deriving (Show, Bounded, Enum, Eq, Ord, Generic)

instance NFData Direction

instance Buildable Direction where
    build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText Direction where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText Direction where
    toText = toTextFromBoundedEnum SnakeLowerCase

-- | @TxWitness@ is proof that transaction inputs are allowed to be spent
newtype TxWitness = TxWitness { unWitness :: ByteString }
    deriving (Show, Eq, Ord)

-- | True if the given tuple refers to a pending transaction
isPending :: TxMeta -> Bool
isPending = (== Pending) . (status :: TxMeta -> TxStatus)

-- | Full expanded and resolved information about a transaction, suitable for
-- presentation to the user.
data TransactionInfo = TransactionInfo
    { txInfoId :: !(Hash "Tx")
    -- ^ Transaction ID of this transaction
    , txInfoInputs :: ![(TxIn, Maybe TxOut)]
    -- ^ Transaction inputs and (maybe) corresponding outputs of the
    -- source. Source information can only be provided for outgoing payments.
    , txInfoOutputs :: ![TxOut]
    -- ^ Payment destination.
    , txInfoMeta :: !TxMeta
    -- ^ Other information calculated from the transaction.
    , txInfoDepth :: Quantity "slot" Natural
    -- ^ Number of slots since the transaction slot.
    , txInfoTime :: UTCTime
    -- ^ Creation time of the block including this transaction.
    } deriving (Show, Eq, Ord)

-- | A linear equation a free variable `x`. Represents the @\s -> a + b*s@
-- function where @s@ can be the transaction size in bytes or, a number of
-- inputs + outputs.
--
-- @a@ and @b@ are constant coefficients.
data FeePolicy =
    LinearFee (Quantity "lovelace" Double) (Quantity "lovelace/x" Double)
    deriving (Eq, Show, Generic)

instance NFData FeePolicy

instance ToText FeePolicy where
    toText (LinearFee (Quantity a) (Quantity b)) =
        toText a <> "x + " <> toText b

instance FromText FeePolicy where
    fromText txt = case T.splitOn "x + " txt of
        [a, b] -> LinearFee
            <$> fmap Quantity (fromText a)
            <*> fmap Quantity (fromText b)
        _ ->
            Left $ TextDecodingError "not a linear equation"

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | Representation of Cardano addresses. Addresses are basically a
-- human-friendly representation of public keys. Historically in Cardano, there
-- exists different sort of addresses, and new ones are to come. So far, we can
-- distinguish between three types of addresses:
--
-- - Byron Random addresses, which holds a payload with derivation path details
-- - Byron Sequential addresses, also known as Icarus'style addresses
-- - Shelley base addresses, see also [implementation-decisions/address](https://github.com/input-output-hk/implementation-decisions/blob/master/text/0001-address.md)
--
-- For more details, see also [About Address Derivation](https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Derivation)
--
-- Shelley base addresses can be declined into two types:
--
-- - Single Addresses: which only holds a public spending key
-- - Group Addresses: which hold both a spending and delegation keys
--
-- It'll therefore seem legitimate to represent addresses as:
--
-- @
-- data Address
--   = ByronAddress !ByteString
--   | SingleAddress !XPub
--   | GroupAddress !XPub XPub
-- @
--
-- However, there's a major drawback to this approach:  we have to consider all
-- three constructors everywhere, and make sure we test every function using
-- them three despite having no need for such fine-grained representation.
--
-- Indeed, from the wallet core code, addresses are nothing more than an opaque
-- bunch of bytes that can be compared with each others. When signing
-- transactions, we have to lookup addresses anyway and therefore, can re-derive
-- their corresponding public keys. The only moment the distinction between
-- address type matters is when it comes to representing addresses at the edge
-- of the application (the API layer). And here, this is precisely where we need
-- to also what target backend we're connected to. Different backends use
-- different encodings which may not be compatible.
--
-- Therefore, for simplicity, it's easier to consider addresses as "bytes", and
-- only peak into these bytes whenever we need to do something with them. This
-- makes it fairly clear that addresses are just an opaque string for the wallet
-- layer and that the underlying encoding is rather agnostic to the underlying
-- backend.
newtype Address = Address
    { unAddress :: ByteString
    } deriving (Show, Generic, Eq, Ord)

instance NFData Address

instance Buildable Address where
    build addr = mempty
        <> prefixF 8 addrF
        <> "..."
        <> suffixF 8 addrF
      where
        addrF = build (toText addr)

instance ToText Address where
    toText = T.decodeUtf8
        . convertToBase Base16
        . unAddress

instance FromText Address where
    fromText = bimap textDecodingError Address
        . convertFromBase Base16
        . T.encodeUtf8
      where
        textDecodingError = TextDecodingError . show

-- | An abstract class to allow encoding of addresses depending on the target
-- backend used.
class EncodeAddress t where
    encodeAddress :: Proxy t -> Address -> Text

-- | An abstract class to allow decoding of addresses depending on the target
-- backend used.
class DecodeAddress t where
    decodeAddress :: Proxy t -> Text -> Either TextDecodingError Address

-- | Denotes if an address has been previously used or not... whether that be
-- in the output of a transaction on the blockchain or one in our pending set.
data AddressState = Used | Unused
    deriving (Bounded, Enum, Eq, Generic, Show)

instance FromText AddressState where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText AddressState where
    toText = toTextFromBoundedEnum SnakeLowerCase

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

data UTxOStatistics = UTxOStatistics
    { histogram :: ![HistogramBar]
    , allStakes :: !Word64
    , boundType :: BoundType
    } deriving (Show, Generic, Ord)

instance Eq UTxOStatistics where
    (UTxOStatistics h s _) == (UTxOStatistics h' s' _) =
        s == s' && sorted h == sorted h'
      where
        sorted :: [HistogramBar] -> [HistogramBar]
        sorted = L.sortOn (\(HistogramBar key _) -> key)

-- An 'HistogramBar' captures the value of a particular bucket. It specifies
-- the bucket upper bound, and its corresponding distribution (on the y-axis).
data HistogramBar = HistogramBar
    { bucketUpperBound :: !Word64
    , bucketCount      :: !Word64
    } deriving (Show, Eq, Ord, Generic)

instance Buildable HistogramBar where
    build (HistogramBar k v) = tupleF (k, v)

--  Buckets boundaries can be constructed in different ways
data BoundType = Log10 deriving (Eq, Show, Ord, Generic)

-- | Smart-constructor to create bounds using a log-10 scale
log10 :: BoundType
log10 = Log10
{-# INLINE log10 #-}

-- | Compute UtxoStatistics from UTxOs
computeUtxoStatistics :: BoundType -> UTxO -> UTxOStatistics
computeUtxoStatistics btype utxos =
    (F.fold foldStatistics (getCoins utxos)) btype
  where
    getCoins :: UTxO -> [Word64]
    getCoins =
        map (getCoin . coin) . Map.elems . getUTxO

    foldStatistics :: F.Fold Word64 (BoundType -> UTxOStatistics)
    foldStatistics = UTxOStatistics
        <$> foldBuckets (generateBounds btype)
        <*> F.sum

    foldBuckets :: NonEmpty Word64 -> F.Fold Word64 [HistogramBar]
    foldBuckets bounds =
        let
            step :: Map Word64 Word64 -> Word64 -> Map Word64 Word64
            step x a = case Map.lookupGE a x of
                Just (k, v) -> Map.insert k (v+1) x
                Nothing -> Map.adjust (+1) (NE.head bounds) x
            initial :: Map Word64 Word64
            initial =
                Map.fromList $ zip (NE.toList bounds) (repeat 0)
            extract :: Map Word64 Word64 -> [HistogramBar]
            extract =
                map (uncurry HistogramBar) . Map.toList
        in
            F.Fold step initial extract

    generateBounds :: BoundType -> NonEmpty Word64
    generateBounds = \case
        Log10 -> NE.fromList $ map (10 ^!) [1..16] ++ [45 * (10 ^! 15)]

    (^!) :: Word64 -> Word64 -> Word64
    (^!) = (^)

{-------------------------------------------------------------------------------
                                   Slotting
-------------------------------------------------------------------------------}

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochNumber :: !Word64
  , slotNumber :: !Word16
  } deriving stock (Show, Read, Eq, Ord, Generic)

instance NFData SlotId

instance Buildable SlotId where
    build (SlotId e s) = fromString (show e) <> "." <> fromString (show s)

-- | The essential parameters necessary for performing slot arithmetic.
data SlotParameters = SlotParameters
    { getEpochLength
        :: EpochLength
    , getSlotLength
        :: SlotLength
    , getGenesisBlockDate
        :: StartTime
    } deriving (Eq, Generic, Show)

-- | Compute the approximate ratio / progress between two slots. This is an
-- approximation for a few reasons, one of them being that we hard code the
-- epoch length as a static number whereas it may vary in practice.
slotRatio
    :: EpochLength
    -> SlotId
        -- ^ Numerator
    -> SlotId
        -- ^ Denominator
    -> Quantity "percent" Percentage
slotRatio epochLength a b =
    let
        n0 = flatSlot epochLength a
        n1 = flatSlot epochLength b
        tolerance = 5
    in if distance n0 n1 < tolerance || n0 >= n1 then
        maxBound
    else
        Quantity $ toEnum $ fromIntegral $ (100 * n0) `div` n1

-- | Convert a 'SlotId' to the number of slots since genesis.
flatSlot :: EpochLength -> SlotId -> Word64
flatSlot (EpochLength epochLength) (SlotId e s) =
    fromIntegral epochLength * e + fromIntegral s

-- | Convert a 'flatSlot' index to 'SlotId'.
fromFlatSlot :: EpochLength -> Word64 -> SlotId
fromFlatSlot (EpochLength epochLength) n = SlotId e (fromIntegral s)
  where
    e = n `div` fromIntegral epochLength
    s = n `mod` fromIntegral epochLength

-- | @slotDifference a b@ is how many slots @a@ is after @b@. The result is
-- non-negative, and if @b > a@ then this function returns zero.
slotDifference :: SlotParameters -> SlotId -> SlotId -> Quantity "slot" Natural
slotDifference (SlotParameters el _ _) a b
    | a' > b' = Quantity $ fromIntegral $ a' - b'
    | otherwise = Quantity 0
  where
    a' = flatSlot el a
    b' = flatSlot el b

-- | Return the slot immediately before the given slot.
slotPred :: SlotParameters -> SlotId -> Maybe SlotId
slotPred (SlotParameters (EpochLength el) _ _) (SlotId en sn)
    | en == 0 && sn == 0 = Nothing
    | sn > 0 = Just $ SlotId en (sn - 1)
    | otherwise = Just $ SlotId (en - 1) (el - 1)

-- | Return the slot immediately after the given slot.
slotSucc :: SlotParameters -> SlotId -> SlotId
slotSucc (SlotParameters (EpochLength el) _ _) (SlotId en sn)
    | sn < el - 1 = SlotId en (sn + 1)
    | otherwise = SlotId (en + 1) 0

-- | The time that a slot begins.
slotStartTime :: SlotParameters -> SlotId -> UTCTime
slotStartTime (SlotParameters el (SlotLength sl) (StartTime st)) slot =
    addUTCTime offset st
  where
    offset = sl * fromIntegral (flatSlot el slot)

-- | For the given time 't', determine the ID of the earliest slot with start
--   time 's' such that 't ≤ s'.
slotCeiling :: SlotParameters -> UTCTime -> SlotId
slotCeiling sp@(SlotParameters _ (SlotLength sl) _) t =
    fromMaybe slotMinBound $ slotAt sp (addUTCTime (pred sl) t)

-- | For the given time 't', determine the ID of the latest slot with start
--   time 's' such that 's ≤ t'.
slotFloor :: SlotParameters -> UTCTime -> Maybe SlotId
slotFloor = slotAt

-- | Returns the earliest slot.
slotMinBound :: SlotId
slotMinBound = SlotId 0 0

-- | For the given time 't', determine the ID of the unique slot with start
--   time 's' and end time 'e' such that 's ≤ t ≤ e'.
slotAt :: SlotParameters -> UTCTime -> Maybe SlotId
slotAt (SlotParameters (EpochLength el) (SlotLength sl) (StartTime st)) t
    | t < st = Nothing
    | otherwise = Just $ SlotId {epochNumber, slotNumber}
  where
    diff :: NominalDiffTime
    diff = t `diffUTCTime` st

    epochLength :: NominalDiffTime
    epochLength = fromIntegral el * sl

    epochNumber :: Word64
    epochNumber = floor (diff / epochLength)

    slotNumber :: Word16
    slotNumber = floor ((diff - (fromIntegral epochNumber) * epochLength) / sl)

-- | Transforms the given inclusive time range into an inclusive slot range.
--
-- This function returns a slot range if (and only if) the specified time range
-- intersects with the life of the blockchain.
--
-- If, on the other hand, the specified time range terminates before the start
-- of the blockchain, this function returns 'Nothing'.
--
slotRangeFromTimeRange
    :: SlotParameters -> Range UTCTime -> Maybe (Range SlotId)
slotRangeFromTimeRange sps (Range mStart mEnd) =
    Range slotStart <$> slotEnd
  where
    slotStart =
        slotCeiling sps <$> mStart
    slotEnd =
        maybe (Just Nothing) (fmap Just . slotFloor sps) mEnd

-- | Duration of a single slot.
newtype SlotLength = SlotLength NominalDiffTime
    deriving (Show, Eq, Generic)

instance NFData SlotLength

-- | Number of slots in a single epoch
newtype EpochLength = EpochLength Word16
    deriving (Show, Eq, Generic)

instance NFData EpochLength

-- | Blockchain start time
newtype StartTime = StartTime UTCTime
    deriving (Show, Eq, Ord, Generic)

instance NFData StartTime

{-------------------------------------------------------------------------------
                                Protocol Magic
-------------------------------------------------------------------------------}

-- | Magic constant associated to a given network
newtype ProtocolMagic = ProtocolMagic Int32
    deriving (Generic, Show)

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

fromTextToHashBase16
    :: forall t . KnownSymbol t => Text -> Either TextDecodingError (Hash t)
fromTextToHashBase16 text = either
    (const $ Left $ TextDecodingError err)
    (pure . Hash)
    (convertFromBase Base16 $ T.encodeUtf8 text)
  where
    err =
        "Unable to decode (Hash \"" <> symbolVal (Proxy @t) <> "\"): \
        \expected Base16 encoding"

toTextFromHashBase16 :: Hash t -> Text
toTextFromHashBase16 = T.decodeUtf8 . convertToBase Base16 . getHash

instance FromText (Hash "Tx") where
    fromText = fromTextToHashBase16

instance ToText (Hash "Tx") where
    toText = toTextFromHashBase16

instance FromText (Hash "BlockHeader") where
    fromText = fromTextToHashBase16

instance ToText (Hash "BlockHeader") where
    toText = toTextFromHashBase16

instance FromText (Hash "Genesis") where
    fromText = fromTextToHashBase16

instance ToText (Hash "Genesis") where
    toText = toTextFromHashBase16

-- | A polymorphic wrapper type with a custom show instance to display data
-- through 'Buildable' instances.
newtype ShowFmt a = ShowFmt { unShowFmt :: a }
    deriving (Generic, Eq, Ord)

instance NFData a => NFData (ShowFmt a)

instance Buildable a => Show (ShowFmt a) where
    show (ShowFmt a) = fmt (build a)

-- | Checks whether or not an invariant holds, by applying the given predicate
--   to the given value.
--
-- If the invariant does not hold (indicated by the predicate function
-- returning 'False'), throws an error with the specified message.
--
-- >>> invariant "not empty" [1,2,3] (not . null)
-- [1, 2, 3]
--
-- >>> invariant "not empty" [] (not . null)
-- *** Exception: not empty
invariant
    :: String
        -- ^ The message
    -> a
        -- ^ The value to test
    -> (a -> Bool)
        -- ^ The predicate
    -> a
invariant msg a predicate =
    if predicate a then a else error msg

-- | Compute distance between two numeric values |a - b|
distance :: (Ord a, Num a) => a -> a -> a
distance a b =
    if a < b then b - a else a - b
