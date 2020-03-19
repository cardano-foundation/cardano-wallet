{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
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
    , Tx (..)
    , TxIn(..)
    , TxOut(..)
    , TxMeta(..)
    , Direction(..)
    , TxStatus(..)
    , SealedTx (..)
    , TransactionInfo (..)
    , UnsignedTx (..)
    , txIns
    , isPending
    , inputs

    -- * Address
    , Address (..)
    , AddressState (..)

    -- * Delegation and stake pools
    , ChimericAccount (..)
    , DelegationCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId
    , PoolRegistrationCertificate (..)

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

    -- * BlockchainParameters
    , BlockchainParameters (..)
    , ActiveSlotCoefficient (..)
    , EpochLength (..)
    , EpochNo (..)
    , FeePolicy (..)
    , SlotId (..)
    , SlotLength (..)
    , SlotNo (..)
    , StartTime (..)
    , slotParams

    -- * Slotting
    , SyncProgress(..)
    , SyncTolerance(..)
    , mkSyncTolerance
    , unsafeEpochNo
    , epochStartTime
    , epochPred
    , epochSucc
    , SlotParameters (..)
    , syncProgress
    , syncProgressRelativeToTime
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
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletDelegationNext (..)
    , WalletPassphraseInfo(..)
    , PassphraseScheme(..)
    , WalletBalance(..)
    , IsDelegatingTo (..)

    -- * Stake Pools
    , PoolId(..)
    , PoolOwner(..)
    , StakeDistribution (..)
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
    , mainnetMagic
    , testnetMagic

    -- * Polymorphic
    , Hash (..)
    , ShowFmt (..)
    , invariant
    , distance
    , hashFromText
    ) where

import Prelude

import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (>=>) )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
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
    ( Percentage (..), Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
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
import Data.Time.Format
    ( defaultTimeLocale, formatTime )
import Data.Word
    ( Word16, Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( Buildable (..)
    , blockListF
    , blockListF'
    , fixedF
    , fmt
    , indentF
    , ordinalF
    , prefixF
    , pretty
    , suffixF
    , tupleF
    )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, KnownSymbol, Symbol, natVal, symbolVal )
import Numeric.Natural
    ( Natural )
import Safe
    ( readMay )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Control.Foldl as F
import qualified Data.ByteString as BS
import qualified Data.Char as C
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
-- the blockchain like @Wallet s@ is.
--
-- Whereas @Wallet s@ in 'Cardano.Wallet.Primitive' can be updated using
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
    , delegation
        :: !WalletDelegation
    } deriving (Eq, Show, Generic)

instance NFData WalletMetadata

formatUTCTime :: UTCTime -> Text
formatUTCTime =
    T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

instance Buildable WalletMetadata where
    build (WalletMetadata wName wTime _ wDelegation) = mempty
        <> build wName <> ", "
        <> "created at " <> build (formatUTCTime wTime) <> ", "
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
        msg = "wallet id should be a hex-encoded string of 40 characters"
        decodeHex =
            either (const Nothing) Just . convertFromBase Base16 . T.encodeUtf8

instance ToText WalletId where
    toText = T.decodeUtf8 . convertToBase Base16 . getWalletId

instance Buildable WalletId where
    build wid = prefixF 8 widF <> "..." <> suffixF 8 widF
      where
        widF = toText wid

data WalletDelegationStatus
    = NotDelegating
    | Delegating !PoolId
    deriving (Generic, Eq, Show)
instance NFData WalletDelegationStatus

instance Buildable WalletDelegationStatus where
    build = \case
        NotDelegating -> "∅"
        Delegating poolId -> build poolId

data WalletDelegationNext = WalletDelegationNext
    { changesAt :: !EpochNo
    , status :: !WalletDelegationStatus
    } deriving (Eq, Generic, Show)
instance NFData WalletDelegationNext

instance Buildable WalletDelegationNext where
    build (WalletDelegationNext e st) =
        build st <> " (in epoch: " <> build e <>")"

data WalletDelegation = WalletDelegation
    { active :: !WalletDelegationStatus
    , next :: ![WalletDelegationNext]
    } deriving (Eq, Generic, Show)
instance NFData WalletDelegation

instance Buildable WalletDelegation where
    build (WalletDelegation act []) =
        "delegating to " <> build act
    build (WalletDelegation act xs) =
        build (WalletDelegation act []) <> " → "
        <> build (T.intercalate " → " $ pretty <$> xs)

class IsDelegatingTo a where
    isDelegatingTo :: (PoolId -> Bool) -> a -> Bool

instance IsDelegatingTo WalletDelegationStatus where
    isDelegatingTo predicate = \case
        Delegating pid -> predicate pid
        NotDelegating  -> False

instance IsDelegatingTo WalletDelegationNext where
    isDelegatingTo predicate WalletDelegationNext{status} =
        isDelegatingTo predicate status

instance IsDelegatingTo WalletDelegation where
    isDelegatingTo predicate WalletDelegation{active,next} =
        isDelegatingTo predicate active || any (isDelegatingTo predicate) next

data WalletPassphraseInfo = WalletPassphraseInfo
    { lastUpdatedAt :: UTCTime
    , passphraseScheme :: PassphraseScheme
    } deriving (Generic, Eq, Ord, Show)

instance NFData WalletPassphraseInfo

-- | A type to capture which encryption scheme should be used
data PassphraseScheme
    = EncryptWithScrypt
        -- ^ Legacy encryption scheme for passphrases
    | EncryptWithPBKDF2
        -- ^ Encryption scheme used since cardano-wallet
    deriving (Generic, Eq, Ord, Show, Read)

instance NFData PassphraseScheme

data WalletBalance = WalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    , reward :: !(Quantity "lovelace" Natural)
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

-- | Identifies a stake pool.
-- For Jörmungandr a 'PoolId' is the blake2b-256 hash of the stake pool
-- registration certificate.
newtype PoolId = PoolId { getPoolId :: ByteString }
    deriving (Generic, Eq, Show, Ord)

poolIdBytesLength :: Int
poolIdBytesLength = 32

instance NFData PoolId

instance Buildable PoolId where
    build poolId = mempty
        <> prefixF 8 poolIdF
      where
        poolIdF = build (toText poolId)

instance ToText PoolId where
    toText = T.decodeUtf8
        . convertToBase Base16
        . getPoolId

instance FromText PoolId where
    fromText t = case convertFromBase Base16 $ T.encodeUtf8 t of
        Left _ ->
            textDecodingError
        Right bytes | BS.length bytes == poolIdBytesLength ->
            Right $ PoolId bytes
        Right _ ->
            textDecodingError
      where
        textDecodingError = Left $ TextDecodingError $ unwords
            [ "Invalid stake pool id: expecting a hex-encoded value that is"
            , show poolIdBytesLength
            , "bytes in length."
            ]

-- | A stake pool owner, which is a public key encoded in bech32 with prefix
-- ed25519_pk.
newtype PoolOwner = PoolOwner { getPoolOwner :: ByteString }
    deriving (Generic, Eq, Show, Ord)

poolOwnerPrefix :: Bech32.HumanReadablePart
poolOwnerPrefix = [Bech32.humanReadablePart|ed25519_pk|]

instance NFData PoolOwner

instance Buildable PoolOwner where
    build poolId = build (toText poolId)

instance ToText PoolOwner where
    toText = Bech32.encodeLenient poolOwnerPrefix
        . Bech32.dataPartFromBytes
        . getPoolOwner

instance FromText PoolOwner where
    fromText t = case fmap Bech32.dataPartToBytes <$> Bech32.decode t of
        Left err ->
            Left $ TextDecodingError $
            "Stake pool owner is not a valid bech32 string: "
            <> show err
        Right (hrp, Just bytes)
            | hrp == poolOwnerPrefix ->
                Right $ PoolOwner bytes
            | otherwise ->
                Left $ TextDecodingError $
                "Stake pool owner has wrong prefix:"
                <> " expected "
                <> T.unpack (Bech32.humanReadablePartToText poolOwnerPrefix)
                <> " but got "
                <> show hrp
        Right (_, Nothing) ->
                Left $ TextDecodingError "Stake pool owner is invalid"

instance FromJSON PoolOwner where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON PoolOwner where
    toJSON = toJSON . toText

data StakeDistribution = StakeDistribution
    { dangling :: Quantity "lovelace" Word64
    , pools :: [(PoolId, Quantity "lovelace" Word64)]
    , unassigned :: Quantity "lovelace" Word64
    } deriving (Eq, Show, Generic)

{-------------------------------------------------------------------------------
                                    Block
-------------------------------------------------------------------------------}

data Block = Block
    { header
        :: !BlockHeader
    , transactions
        :: ![Tx]
    , delegations
        :: ![DelegationCertificate]
    } deriving (Show, Eq, Ord, Generic)

instance NFData Block

instance Buildable (Block) where
    build (Block h txs _) = mempty
        <> build h
        <> if null txs then " ∅" else "\n" <> indentF 4 (blockListF txs)

data BlockHeader = BlockHeader
    { slotId
        :: SlotId
    , blockHeight
        :: Quantity "block" Word32
    , headerHash
        :: !(Hash "BlockHeader")
    , parentHeaderHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Ord, Generic)

instance NFData BlockHeader

instance Buildable BlockHeader where
    build (BlockHeader s (Quantity bh) hh _) =
        prefixF 8 hhF
        <> "-["
        <> build s
        <> "#" <> build (show bh)
        <> "]"
      where
        hhF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash hh

{-------------------------------------------------------------------------------
                                      Tx
-------------------------------------------------------------------------------}

-- | Primitive @Tx@-type.
--
-- Currently tailored for jormungandr in that inputs are @(TxIn, Coin)@
-- instead of @TxIn@. We might have to revisit this when supporting another
-- node.
data Tx = Tx
    { txId
        :: Hash "Tx"
        -- ^ Jörmungandr computes transaction id by hashing the full content of
        -- the transaction, which includes witnesses. Therefore, we need either
        -- to keep track of the witnesses to be able to re-compute the tx id
        -- every time, or, simply keep track of the id itself.
    , resolvedInputs
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

txIns :: Set Tx -> Set TxIn
txIns = foldMap (Set.fromList . inputs)

inputs :: Tx -> [TxIn]
inputs = map fst . resolvedInputs

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
    , blockHeight :: !(Quantity "block" Word32)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Show, Eq, Ord, Generic)

instance NFData TxMeta

instance Buildable TxMeta where
    build (TxMeta s d sl (Quantity bh) (Quantity a)) = mempty
        <> (case d of; Incoming -> "+"; Outgoing -> "-")
        <> fixedF @Double 6 (fromIntegral a / 1e6)
        <> " " <> build s
        <> " since " <> build sl <> "#" <> build bh

data TxStatus
    = Pending
    | InLedger
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance NFData TxStatus

instance Buildable TxStatus where
    build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText TxStatus where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText TxStatus where
    toText = toTextFromBoundedEnum SnakeLowerCase

-- | An unsigned transaction.
--
-- See 'Tx' for a signed transaction.
--
data UnsignedTx = UnsignedTx
    { unsignedInputs
        :: NonEmpty (TxIn, TxOut)
    , unsignedOutputs
        :: NonEmpty TxOut
    }
    deriving (Eq, Show)

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

-- | @SealedTx@ is a serialised transaction that is ready to be submited
-- to the node.
newtype SealedTx = SealedTx { getSealedTx :: ByteString }
    deriving stock (Show, Eq, Generic)
    deriving newtype (ByteArrayAccess)

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
    , txInfoDepth :: Quantity "block" Natural
    -- ^ Number of slots since the transaction slot.
    , txInfoTime :: UTCTime
    -- ^ Creation time of the block including this transaction.
    } deriving (Show, Eq, Ord)

-- | A linear equation of a free variable `x`. Represents the @\x -> a + b*x@
-- function where @x@ can be the transaction size in bytes or, a number of
-- inputs + outputs.
--
-- @a@, @b@ and @c@ are constant coefficients.
--
-- FIXME 'Double' is an old artifact from the Byron era on cardano-sl. It must
-- go.
data FeePolicy = LinearFee
    (Quantity "lovelace" Double)
    (Quantity "lovelace/byte" Double)
    (Quantity "lovelace/certificate" Double)
    deriving (Eq, Show, Generic)

instance NFData FeePolicy

instance ToText FeePolicy where
    toText (LinearFee (Quantity a) (Quantity b) (Quantity c)) =
        toText a <> " + " <> toText b <> "x + " <> toText c <> "y"

instance FromText FeePolicy where
    fromText txt = case T.splitOn " + " txt of
        [a, b, c] | T.takeEnd 1 b == "x" && T.takeEnd 1 c == "y" ->
            left (const err) $ LinearFee
                <$> fmap Quantity (fromText a)
                <*> fmap Quantity (fromText (T.dropEnd 1 b))
                <*> fmap Quantity (fromText (T.dropEnd 1 c))
        _ ->
            Left err
      where
        err = TextDecodingError
            "Unable to decode FeePolicy: \
            \Linear equation not in expected format: a + bx + cy \
            \where 'a', 'b', and 'c' are numbers"


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

-- | Coins are stored as Lovelace (reminder: 1 Lovelace = 1e-6 ADA)
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
                             Blockchain Parameters
-------------------------------------------------------------------------------}

data BlockchainParameters = BlockchainParameters
    { getGenesisBlockHash :: Hash "Genesis"
        -- ^ Hash of the very first block
    , getGenesisBlockDate :: StartTime
        -- ^ Start time of the chain.
    , getFeePolicy :: FeePolicy
        -- ^ Policy regarding transaction fee.
    , getSlotLength :: SlotLength
        -- ^ Length, in seconds, of a slot.
    , getEpochLength :: EpochLength
        -- ^ Number of slots in a single epoch.
    , getTxMaxSize :: Quantity "byte" Word16
        -- ^ Maximum size of a transaction (soft or hard limit).
    , getEpochStability :: Quantity "block" Word32
        -- ^ Length of the suffix of the chain considered unstable
    , getActiveSlotCoefficient :: ActiveSlotCoefficient
        -- ^ In Genesis/Praos, corresponds to the % of active slots
        -- (i.e. slots for which someone can be elected as leader).
    } deriving (Generic, Show, Eq)

instance NFData BlockchainParameters

instance Buildable BlockchainParameters where
    build bp = blockListF' "" id
        [ "Genesis block hash: " <> genesisF (getGenesisBlockHash bp)
        , "Genesis block date: " <> startTimeF (getGenesisBlockDate
            (bp :: BlockchainParameters))
        , "Fee policy:         " <> feePolicyF (getFeePolicy bp)
        , "Slot length:        " <> slotLengthF (getSlotLength
            (bp :: BlockchainParameters))
        , "Epoch length:       " <> epochLengthF (getEpochLength
            (bp :: BlockchainParameters))
        , "Tx max size:        " <> txMaxSizeF (getTxMaxSize bp)
        , "Epoch stability:    " <> epochStabilityF (getEpochStability bp)
        , "Active slot coeff:  " <> build (bp ^. #getActiveSlotCoefficient)
        ]
      where
        genesisF = build . T.decodeUtf8 . convertToBase Base16 . getHash
        startTimeF (StartTime s) = build s
        feePolicyF = build . toText
        slotLengthF (SlotLength s) = build s
        epochLengthF (EpochLength s) = build s
        txMaxSizeF (Quantity s) = build s
        epochStabilityF (Quantity s) = build s

newtype ActiveSlotCoefficient
    = ActiveSlotCoefficient { unActiveSlotCoefficient :: Double }
    deriving stock (Generic, Eq, Show)
    deriving newtype (Buildable, Num, Fractional)

instance NFData ActiveSlotCoefficient

slotParams :: BlockchainParameters -> SlotParameters
slotParams bp =
    SlotParameters
        (bp ^. #getEpochLength)
        (bp ^. #getSlotLength)
        (bp ^. #getGenesisBlockDate)
        (bp ^. #getActiveSlotCoefficient)

{-------------------------------------------------------------------------------
                                   Slotting
-------------------------------------------------------------------------------}

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochNumber :: !EpochNo
  , slotNumber :: !SlotNo
  } deriving stock (Show, Read, Eq, Ord, Generic)

newtype SlotNo = SlotNo { unSlotNo :: Word32 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Buildable, NFData, Enum)

newtype EpochNo = EpochNo { unEpochNo :: Word31 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Bounded, Enum)

instance ToText EpochNo where
    toText = T.pack . show . unEpochNo

instance Buildable EpochNo where
    build (EpochNo e) = build $ fromIntegral @Word31 @Word32 e

instance NFData EpochNo where
    rnf (EpochNo !_) = ()

-- | Convert the specified value into an 'EpochNo', or fail if the value is
--   too large.
unsafeEpochNo :: HasCallStack => Word32 -> EpochNo
unsafeEpochNo epochNo
    | epochNo > maxEpochNo =
        error $ mconcat
            [ "unsafeEpochNo: epoch number ("
            , show epochNo
            , ") out of bounds ("
            , show (minBound @Word31)
            , ", "
            , show (maxBound @Word31)
            , ")."
            ]
    | otherwise =
        EpochNo $ fromIntegral epochNo
  where
    maxEpochNo :: Word32
    maxEpochNo = fromIntegral @Word31 $ unEpochNo maxBound

-- | Calculate the time at which an epoch begins.
epochStartTime :: SlotParameters -> EpochNo -> UTCTime
epochStartTime sps e = slotStartTime sps $ SlotId e 0

-- | Return the epoch immediately before the given epoch, or 'Nothing' if there
--   is no representable epoch before the given epoch.
epochPred :: EpochNo -> Maybe EpochNo
epochPred (EpochNo e)
    | e == minBound = Nothing
    | otherwise = Just $ EpochNo $ pred e

-- | Return the epoch immediately after the given epoch, or 'Nothing' if there
--   is no representable epoch after the given epoch.
epochSucc :: EpochNo -> Maybe EpochNo
epochSucc (EpochNo e)
    | e == maxBound = Nothing
    | otherwise = Just $ EpochNo $ succ e

instance NFData SlotId

instance Buildable SlotId where
    build (SlotId (EpochNo e) (SlotNo s)) =
        fromString (show e) <> "." <> fromString (show s)

-- | The essential parameters necessary for performing slot arithmetic.
data SlotParameters = SlotParameters
    { getEpochLength
        :: EpochLength
    , getSlotLength
        :: SlotLength
    , getGenesisBlockDate
        :: StartTime
    , getActiveSlotCoefficient
        :: ActiveSlotCoefficient
    } deriving (Eq, Generic, Show)

data SyncProgress
    = Ready
    | Syncing !(Quantity "percent" Percentage)
    deriving (Generic, Eq, Show)

instance NFData SyncProgress

instance Ord SyncProgress where
    Ready <= Ready = True
    Ready <= Syncing _ = False
    Syncing _ <= Ready = True
    Syncing a <= Syncing b = a <= b

instance Buildable SyncProgress where
    build = \case
        Ready ->
            "restored"
        Syncing (Quantity p) ->
            "still restoring (" <> build (toText p) <> ")"

newtype SyncTolerance = SyncTolerance NominalDiffTime
    deriving stock (Generic, Eq, Show)

-- | Construct a 'SyncTolerance' from a number of __seconds__
mkSyncTolerance :: Int -> SyncTolerance
mkSyncTolerance =
    SyncTolerance . toEnum . (* pico)
  where
    pico = 1000*1000*1000*1000

instance ToText SyncTolerance where
    toText (SyncTolerance t) = T.pack (show t)

instance FromText SyncTolerance where
    fromText t = case T.splitOn "s" t of
        [v,""] ->
            maybe
                (Left errSyncTolerance)
                (Right . mkSyncTolerance)
                (readMay $ T.unpack v)
        _ ->
            Left errSyncTolerance
      where
        errSyncTolerance = TextDecodingError $ unwords
            [ "Cannot parse given time duration. Here are a few examples of"
            , "valid text representing a sync tolerance: '3s', '3600s', '42s'."
            ]

-- | Estimate restoration progress based on:
--
-- - The current local tip
-- - The last slot
--
-- For the sake of this calculation, we are somewhat conflating the definitions
-- of slots and block height. Because we can't reliably _trust_ that the current
-- node is actually itself synced with the network. So, we compute the progress
-- as:
--
-- @
-- p = h / (h + X)
-- @
--
-- Where:
--
-- - @h@: the number of blocks we have ingested so far.
-- - @X@: the estimatd remaining slots to reach the network tip.
--
-- Initially, `X` gives a relatively poor estimation of the network height, as
-- it assumes that every next slot will be a block. But, as we ingest blocks,
-- `h` becomes bigger and `X` becomes smaller making the progress estimation
-- better and better. At some point, `X` is null, and we have `p = h / h`
syncProgress
    :: SyncTolerance
        -- ^ A time tolerance inside which we consider ourselves synced
    -> SlotParameters
        -- ^ Parameters relative to slot arithmetics
    -> BlockHeader
        -- ^ Local tip
    -> SlotId
        -- ^ Last slot that could have been produced
    -> SyncProgress
syncProgress (SyncTolerance timeTolerance) sp tip slotNow =
    let
        bhTip = fromIntegral . getQuantity $ tip ^. #blockHeight

        epochLength = sp ^. #getEpochLength
        (SlotLength slotLength)  = (sp ^. #getSlotLength)

        n0 = flatSlot epochLength (tip ^. #slotId)
        n1 = flatSlot epochLength slotNow

        tolerance = floor (timeTolerance / slotLength)

        remainingSlots = fromIntegral $ n1 - n0

        ActiveSlotCoefficient f = sp ^. #getActiveSlotCoefficient
        remainingBlocks = round (remainingSlots * f)

        -- Using (max 1) to avoid division by 0.
        progress = bhTip % (max 1 (bhTip + remainingBlocks))
    in if distance n1 n0 < tolerance || n0 >= n1 || progress >= 1 then
        Ready
    else
        Syncing
        . Quantity
        . either (const . error $ errMsg progress) id
        . mkPercentage
        $ progress
  where
    errMsg x = "syncProgress: " ++ show x ++ " is out of bounds"

-- | Helper to compare the /local tip/ with the slot corresponding to a
-- @UTCTime@, and calculate progress based on that.
syncProgressRelativeToTime
    :: SyncTolerance
        -- ^ A time tolerance inside which we consider ourselves synced
    -> SlotParameters
        -- ^ Parameters relative to slot arithmetics
    -> BlockHeader
    -- ^ Local tip
    -> UTCTime
    -- ^ Where we believe the network tip is (e.g. @getCurrentTime@).
    -> SyncProgress
syncProgressRelativeToTime tolerance sp tip time =
    maybe
        (Syncing minBound)
        (syncProgress tolerance sp tip)
        (slotAt sp time)

-- | Convert a 'SlotId' to the number of slots since genesis.
flatSlot :: EpochLength -> SlotId -> Word64
flatSlot (EpochLength epochLength) (SlotId (EpochNo e) (SlotNo s)) =
    fromIntegral epochLength * fromIntegral e + fromIntegral s

-- | Convert a 'flatSlot' index to 'SlotId'.
--
-- This function will fail if applied to a value that is higher than the maximum
-- value of 'flatSlot' for the specified 'EpochLength'.
--
fromFlatSlot :: EpochLength -> Word64 -> SlotId
fromFlatSlot el@(EpochLength epochLength) n
    | n <= maxFlatSlot =
        SlotId (EpochNo $ fromIntegral e) (fromIntegral s)
    | otherwise =
        error $ mconcat
            [ "fromFlatSlot: The specified flat slot number ("
            , show n
            , ") is higher than the maximum flat slot number ("
            , show maxFlatSlot
            , ") for the specified epoch length ("
            , show epochLength
            , ")."
            ]
  where
    e = n `div` fromIntegral epochLength
    s = n `mod` fromIntegral epochLength
    maxFlatSlot =
        flatSlot el (SlotId (EpochNo maxBound) (SlotNo $ epochLength - 1))

-- | @slotDifference a b@ is how many slots @a@ is after @b@. The result is
-- non-negative, and if @b > a@ then this function returns zero.
slotDifference :: SlotParameters -> SlotId -> SlotId -> Quantity "slot" Natural
slotDifference (SlotParameters el _ _ _) a b
    | a' > b' = Quantity $ fromIntegral $ a' - b'
    | otherwise = Quantity 0
  where
    a' = flatSlot el a
    b' = flatSlot el b

-- | Return the slot immediately before the given slot.
slotPred :: SlotParameters -> SlotId -> Maybe SlotId
slotPred (SlotParameters (EpochLength el) _ _ _) (SlotId en sn)
    | en == 0 && sn == 0 = Nothing
    | sn > 0 = Just $ SlotId en (sn - 1)
    | otherwise = Just $ SlotId (en - 1) (SlotNo $ el - 1)

-- | Return the slot immediately after the given slot.
slotSucc :: SlotParameters -> SlotId -> SlotId
slotSucc (SlotParameters (EpochLength el) _ _ _) (SlotId en (SlotNo sn))
    | sn < el - 1 = SlotId en (SlotNo $ sn + 1)
    | otherwise = SlotId (en + 1) 0

-- | The time that a slot begins.
slotStartTime :: SlotParameters -> SlotId -> UTCTime
slotStartTime (SlotParameters el (SlotLength sl) (StartTime st) _) slot =
    addUTCTime offset st
  where
    offset = sl * fromIntegral (flatSlot el slot)

-- | For the given time 't', determine the ID of the earliest slot with start
--   time 's' such that 't ≤ s'.
slotCeiling :: SlotParameters -> UTCTime -> SlotId
slotCeiling sp@(SlotParameters _ (SlotLength sl) _ _) t =
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
slotAt (SlotParameters (EpochLength el) (SlotLength sl) (StartTime st) _) t
    | t < st = Nothing
    | otherwise = Just $ SlotId {epochNumber, slotNumber}
  where
    diff :: NominalDiffTime
    diff = t `diffUTCTime` st

    epochLength :: NominalDiffTime
    epochLength = fromIntegral el * sl

    epochNumber = EpochNo $
        floor (diff / epochLength)

    slotNumber = SlotNo $
        floor ((diff - fromIntegral (unEpochNo epochNumber) * epochLength) / sl)

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
newtype SlotLength = SlotLength { unSlotLength :: NominalDiffTime }
    deriving (Show, Eq, Generic)

instance NFData SlotLength

-- | Number of slots in a single epoch
newtype EpochLength = EpochLength { unEpochLength :: Word32 }
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
newtype ProtocolMagic = ProtocolMagic { getProtocolMagic :: Int32 }
    deriving (Generic, Show, Eq)

instance ToText ProtocolMagic where
    toText (ProtocolMagic pm) = T.pack (show pm)

instance FromText ProtocolMagic where
    fromText = fmap (ProtocolMagic . fromIntegral @Natural) . fromText

-- | Hard-coded protocol magic for the Byron MainNet
mainnetMagic :: ProtocolMagic
mainnetMagic =  ProtocolMagic 764824073

-- | Derive testnet magic from a type-level Nat
testnetMagic :: forall pm. KnownNat pm => ProtocolMagic
testnetMagic = ProtocolMagic $ fromIntegral $ natVal $ Proxy @pm

{-------------------------------------------------------------------------------
              Stake Pool Delegation and Registration Certificates
-------------------------------------------------------------------------------}

-- | Also known as a staking key, chimeric account is used in group-type address
-- for staking purposes. It is a public key of the account address
newtype ChimericAccount = ChimericAccount { unChimericAccount :: ByteString }
    deriving (Generic, Show, Eq, Ord)

instance NFData ChimericAccount

-- | Represent a delegation certificate.
data DelegationCertificate
    = CertDelegateNone ChimericAccount
    | CertDelegateFull ChimericAccount PoolId
    deriving (Generic, Show, Eq, Ord)

instance NFData DelegationCertificate

dlgCertAccount :: DelegationCertificate -> ChimericAccount
dlgCertAccount = \case
    CertDelegateNone acc -> acc
    CertDelegateFull acc _ -> acc

dlgCertPoolId :: DelegationCertificate -> Maybe PoolId
dlgCertPoolId = \case
    CertDelegateNone{} -> Nothing
    CertDelegateFull _ poolId -> Just poolId

-- | Pool ownership data from the stake pool registration certificate.
data PoolRegistrationCertificate = PoolRegistrationCertificate
    { poolId :: !PoolId
    , poolOwners :: ![PoolOwner]
    , poolMargin :: Percentage
    , poolCost :: Quantity "lovelace" Word64
    } deriving (Generic, Show, Eq, Ord)

instance NFData PoolRegistrationCertificate

instance Buildable PoolRegistrationCertificate where
    build (PoolRegistrationCertificate p o _ _) = mempty
        <> "Registration of "
        <> build p
        <> " owned by "
        <> build o

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

newtype Hash (tag :: Symbol) = Hash { getHash :: ByteString }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (ByteArrayAccess)

instance NFData (Hash tag)

instance Buildable (Hash tag) where
    build h = mempty
        <> prefixF 8 builder
      where
        builder = build . toText $ h

instance ToText (Hash tag) where
    toText = T.decodeUtf8 . convertToBase Base16 . getHash

instance FromText (Hash "Tx")           where fromText = hashFromText 32
instance FromText (Hash "Account")      where fromText = hashFromText 32
instance FromText (Hash "Genesis")      where fromText = hashFromText 32
instance FromText (Hash "Block")        where fromText = hashFromText 32
instance FromText (Hash "BlockHeader")  where fromText = hashFromText 32

hashFromText
    :: forall t. (KnownSymbol t)
    => Int
        -- ^ Expected decoded hash length
    -> Text
    -> Either TextDecodingError (Hash t)
hashFromText len text = case decoded of
    Right bytes | BS.length bytes == len ->
        Right $ Hash bytes
    _ ->
        Left $ TextDecodingError $ unwords
            [ "Invalid"
            , mapFirst C.toLower $ symbolVal $ Proxy @t
            , "hash: expecting a hex-encoded value that is"
            , show len
            , "bytes in length."
            ]
  where
    decoded = convertFromBase Base16 $ T.encodeUtf8 text

    mapFirst :: (a -> a) -> [a] -> [a]
    mapFirst _     [] = []
    mapFirst fn (h:q) = fn h:q

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
