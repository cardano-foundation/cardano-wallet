{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains the core primitive of a Wallet. This is roughly a
-- Haskell translation of the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
--
-- It doesn't contain any particular business-logic code, but defines a few
-- primitive operations on Wallet core types as well.

module Cardano.Wallet.Primitive.Types
    (
    -- * Block
      Block(..)
    , BlockHeader(..)

    -- * Delegation and stake pools
    , CertificatePublicationTime (..)
    , DelegationCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , PoolCertificate (..)
    , getPoolCertificatePoolId
    , setPoolCertificatePoolId
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate

    -- * Network Parameters
    , NetworkParameters (..)
    , GenesisParameters (..)
    , SlottingParameters (..)
    , ProtocolParameters (..)
    , TxParameters (..)
    , ActiveSlotCoefficient (..)
    , DecentralizationLevel (..)
    , EpochLength (..)
    , EpochNo (..)
    , unsafeEpochNo
    , isValidEpochNo
    , FeePolicy (..)
    , SlotId (..)
    , SlotNo (..)
    , SlotLength (..)
    , SlotInEpoch (..)
    , StartTime (..)

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
    , StakePool(..)
    , StakePoolsSummary (..)
    , PoolId(..)
    , PoolOwner(..)
    , poolIdBytesLength
    , decodePoolIdBech32
    , encodePoolIdBech32
    , StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker (..)
    , StakeKeyCertificate (..)
    , PoolMetadataGCStatus (..)

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
    , Signature (..)
    , ShowFmt (..)
    , invariant
    , distance

    -- * Settings
    , Settings(..)
    , SmashServer
    , unSmashServer
    , PoolMetadataSource( .. )
    , defaultSettings
    , unsafeToPMS

    -- * InternalState
    , InternalState (..)
    , defaultInternalState

    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), hashFromText )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..) )
import Control.Arrow
    ( left, right )
import Control.DeepSeq
    ( NFData (..) )
import Control.Error.Util
    ( (??) )
import Control.Monad
    ( guard, (<=<), (>=>) )
import Control.Monad.Except
    ( runExceptT )
import Control.Monad.Trans.Except
    ( throwE )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), withObject, (.:), (.:?) )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( set, view, (^.) )
import Data.Generics.Labels
    ()
import Data.Int
    ( Int32 )
import Data.List
    ( intercalate )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
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
    ( NominalDiffTime, UTCTime )
import Data.Time.Clock.POSIX
    ( POSIXTime )
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
    , fmt
    , indentF
    , listF'
    , mapF
    , prefixF
    , pretty
    , suffixF
    )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, natVal )
import Network.URI
    ( URI (..), parseAbsoluteURI, uriQuery, uriScheme, uriToString )
import Numeric.Natural
    ( Natural )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                             Wallet Metadata
-------------------------------------------------------------------------------}

-- | Additional information about a wallet that can't simply be derived from
-- the blockchain like @Wallet s@ is.
--
-- Whereas @Wallet s@ in 'Cardano.Wallet.Primitive' can be updated using
-- @applyBlock@, @WalletMetadata@ can not*.
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

-- | Calling 'fromText @WalletName' on shorter string will fail.
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
        build st <> " (in epoch: " <> build e <> ")"

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

instance NFData WalletBalance

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

data StakePool = StakePool
    { poolId :: PoolId
    , stake :: Quantity "lovelace" Word64
    , production :: Quantity "block" Word64
    , performance :: Double
    , desirability :: Double
    , cost :: Quantity "lovelace" Word64
    , margin :: Percentage
    , pledge :: Quantity "lovelace" Word64
    , saturation :: Double
    } deriving (Show, Generic)

-- Status encoding of the metadata GC thread, which queries
-- the SMASH server for delisted pools.
data PoolMetadataGCStatus
    = NotApplicable
    | NotStarted
    | Restarting POSIXTime -- shows last GC before restart occured
    | HasRun POSIXTime     -- shows last GC
    deriving (Eq, Show, Generic)

-- | A newtype to wrap metadata hash.
--
-- NOTE: not using the 'Hash' type as this newtype is primarily for database
-- interop which doesn't quite like DataKinds.
newtype StakePoolMetadataHash = StakePoolMetadataHash ByteString
    deriving (Eq, Ord, Show, Generic)

instance NFData StakePoolMetadataHash

instance ToText StakePoolMetadataHash where
    toText (StakePoolMetadataHash bytes) =
        toText (Hash bytes)

instance FromText StakePoolMetadataHash where
    fromText = fmap (StakePoolMetadataHash . getHash @"_") . hashFromText 32

instance Buildable StakePoolMetadataHash where
    build (StakePoolMetadataHash hash) = build (Hash hash)

-- | A newtype to wrap metadata Url, mostly needed for database lookups and
-- signature clarity.
newtype StakePoolMetadataUrl = StakePoolMetadataUrl Text
    deriving (Eq, Ord, Show, Generic)

instance NFData StakePoolMetadataUrl

instance ToText StakePoolMetadataUrl where
    toText (StakePoolMetadataUrl url) = url

instance FromText StakePoolMetadataUrl where
    fromText = pure . StakePoolMetadataUrl

-- | Information about a stake pool.
--
-- The metadata information is not used directly by cardano-wallet, but rather
-- passed straight through to API consumers.
data StakePoolMetadata = StakePoolMetadata
    { ticker :: StakePoolTicker
    -- ^ Very short human-readable ID for the stake pool.
    , name :: Text
    -- ^ Name of the stake pool.
    , description :: Maybe Text
    -- ^ Short description of the stake pool.
    , homepage :: Text
    -- ^ Absolute URL for the stake pool's homepage link.
    } deriving (Eq, Ord, Show, Generic)

instance FromJSON StakePoolMetadata where
    parseJSON = withObject "StakePoolMetadta" $ \obj -> do
        ticker <- obj .: "ticker"

        name <- obj .: "name"
        guard (T.length name <= 50)

        description <- obj .:? "description"
        guard ((T.length <$> description) <= Just 250)

        homepage <- obj .: "homepage"
        guard (T.length homepage <= 100)

        pure $ StakePoolMetadata{ticker,name,description,homepage}

-- | Very short name for a stake pool.
newtype StakePoolTicker = StakePoolTicker { unStakePoolTicker :: Text }
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (ToText)

instance FromText StakePoolTicker where
    fromText t
        | T.length t >= 3 && T.length t <= 5
            = Right $ StakePoolTicker t
        | otherwise
            = Left . TextDecodingError $
                "stake pool ticker length must be 3-5 characters"

-- Here to avoid needless orphan instances in the API types.
instance FromJSON StakePoolTicker where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON StakePoolTicker where
    toJSON = toJSON . toText

-- | Identifies a stake pool.
-- For Jörmungandr a 'PoolId' is the blake2b-256 hash of the stake pool
-- registration certificate.
newtype PoolId = PoolId { getPoolId :: ByteString }
    deriving (Generic, Eq, Show, Ord)

poolIdBytesLength :: [Int]
poolIdBytesLength = [28, 32]

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
        Right bytes | BS.length bytes `elem` poolIdBytesLength ->
            Right $ PoolId bytes
        Right _ ->
            textDecodingError
      where
        textDecodingError = Left $ TextDecodingError $ unwords
            [ "Invalid stake pool id: expecting a hex-encoded value that is"
            , intercalate " or " (show <$> poolIdBytesLength)
            , "bytes in length."
            ]

-- | Encode 'PoolId' as Bech32 with "pool" hrp.
encodePoolIdBech32 :: PoolId -> T.Text
encodePoolIdBech32 =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . getPoolId
  where
    hrp = [Bech32.humanReadablePart|pool|]

-- | Decode a Bech32 encoded 'PoolId'.
decodePoolIdBech32 :: T.Text -> Either TextDecodingError PoolId
decodePoolIdBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (_, Just bytes) ->
            Right $ PoolId bytes
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid stake pool id: expecting a Bech32 encoded value with human readable part of 'pool'."
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

data StakePoolsSummary = StakePoolsSummary
    { nOpt :: Int
    , rewards :: Map PoolId (Quantity "lovelace" Word64)
    , stake :: Map PoolId Percentage
    } deriving (Show, Eq)

instance Buildable StakePoolsSummary where
    build StakePoolsSummary{nOpt,rewards,stake} = listF' id
        [ "Stake: " <> mapF (Map.toList stake)
        , "Non-myopic member rewards: " <> mapF (Map.toList rewards)
        , "Optimum number of pools: " <> pretty nOpt
        ]

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
    { slotNo
        :: SlotNo
    , blockHeight
        :: Quantity "block" Word32
    , headerHash
        :: !(Hash "BlockHeader")
    , parentHeaderHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Ord, Generic)

instance NFData BlockHeader

instance Buildable BlockHeader where
    build (BlockHeader s (Quantity bh) hh ph) =
        prefixF 8 phF
        <> "<-["
        <> prefixF 8 hhF
        <> "-"
        <> build s
        <> "#" <> build (show bh)
        <> "]"
      where
        hhF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash hh
        phF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash ph

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

-- | A thin wrapper around derivation indexes. This can be used to represent
-- derivation path as homogeneous lists of 'DerivationIndex'. This is slightly
-- more convenient than having to carry heterogeneous lists of 'Index depth type'
-- and works fine because:
--
-- 1. The 'depth' matters not because what the depth captures is actually the
--    position of the index in that list. It makes sense to carry at the type
--    level when manipulating standalone indexes to avoid mistakes, but when
--    treating them as a part of a list it is redundant.
--
-- 2. The derivationType is captured by representing indexes as plain Word32.
--    The Soft / Hardened notation is for easing human-readability but in the
--    end, a soft index is simply a value < 2^31, whereas a "hardened" index is
--    simply a value >= 2^31. Therefore, instead of representing indexes as
--    derivationType + relative index within 0 and 2^31, we can represent them
--    as just an index between 0 and 2^32, which is what DerivationIndex does.
newtype DerivationIndex
    = DerivationIndex Word32
    deriving (Show, Eq, Ord, Generic)

instance NFData DerivationIndex

instance FromText DerivationIndex where
    fromText = fmap DerivationIndex . fromText

instance ToText DerivationIndex where
    toText (DerivationIndex index) = toText index

{-------------------------------------------------------------------------------
                              Network Parameters
-------------------------------------------------------------------------------}

-- | Records the complete set of parameters currently in use by the network
--   that are relevant to the wallet.
--
data NetworkParameters = NetworkParameters
    { genesisParameters :: GenesisParameters
       -- ^ See 'GenesisParameters'.
    , slottingParameters :: SlottingParameters
       -- ^ See 'SlottingParameters'.
    , protocolParameters :: ProtocolParameters
       -- ^ See 'ProtocolParameters'.
    } deriving (Generic, Show, Eq)

instance NFData NetworkParameters

instance Buildable NetworkParameters where
    build (NetworkParameters gp sp pp) = build gp <> build sp <> build pp

-- | Parameters defined by the __genesis block__.
--
-- At present, these values cannot be changed through the update system.
--
-- They can only be changed through a soft or hard fork.
--
data GenesisParameters = GenesisParameters
    { getGenesisBlockHash :: Hash "Genesis"
        -- ^ Hash of the very first block
    , getGenesisBlockDate :: StartTime
        -- ^ Start time of the chain.
    , getEpochStability :: Quantity "block" Word32
        -- ^ Length of the suffix of the chain considered unstable
    } deriving (Generic, Show, Eq)

instance NFData GenesisParameters

instance Buildable GenesisParameters where
    build gp = blockListF' "" id
        [ "Genesis block hash: " <> genesisF (getGenesisBlockHash gp)
        , "Genesis block date: " <> startTimeF (getGenesisBlockDate
            (gp :: GenesisParameters))
        , "Epoch stability:    " <> epochStabilityF (getEpochStability gp)
        ]
      where
        genesisF = build . T.decodeUtf8 . convertToBase Base16 . getHash
        startTimeF (StartTime s) = build s
        epochStabilityF (Quantity s) = build s

data SlottingParameters = SlottingParameters
    { getSlotLength :: SlotLength
        -- ^ Length, in seconds, of a slot.
    , getEpochLength :: EpochLength
        -- ^ Number of slots in a single epoch.
    , getActiveSlotCoefficient :: ActiveSlotCoefficient
        -- ^ In Genesis/Praos, corresponds to the % of active slots
        -- (i.e. slots for which someone can be elected as leader).
    } deriving (Generic, Show, Eq)

instance NFData SlottingParameters

instance Buildable SlottingParameters where
    build gp = blockListF' "" id
        [ "Slot length:        " <> slotLengthF (getSlotLength
            (gp :: SlottingParameters))
        , "Epoch length:       " <> epochLengthF (getEpochLength
            (gp :: SlottingParameters))
        , "Active slot coeff:  " <> build (gp ^. #getActiveSlotCoefficient)
        ]
      where
        slotLengthF (SlotLength s) = build s
        epochLengthF (EpochLength s) = build s

newtype ActiveSlotCoefficient
    = ActiveSlotCoefficient { unActiveSlotCoefficient :: Double }
    deriving stock (Generic, Eq, Show)
    deriving newtype (Buildable, Num, Fractional)

instance NFData ActiveSlotCoefficient

-- | Protocol parameters that can be changed through the update system.
--
data ProtocolParameters = ProtocolParameters
    { decentralizationLevel
        :: DecentralizationLevel
        -- ^ The current level of decentralization in the network.
    , txParameters
        :: TxParameters
        -- ^ Parameters that affect transaction construction.
    , desiredNumberOfStakePools
        :: Word16
        -- ^ The current desired number of stakepools in the network.
        -- Also known as k parameter.
    , minimumUTxOvalue
        :: Coin
        -- ^ The minimum UTxO value.
    , hardforkEpochNo
        :: Maybe EpochNo
        -- ^ The hardfork epoch number.
    } deriving (Eq, Generic, Show)

instance NFData ProtocolParameters

instance Buildable ProtocolParameters where
    build pp = blockListF' "" id
        [ "Decentralization level: " <> build (pp ^. #decentralizationLevel)
        , "Transaction parameters: " <> build (pp ^. #txParameters)
        , "Desired number of pools: " <> build (pp ^. #desiredNumberOfStakePools)
        , "Minimum UTxO value: " <> build (pp ^. #minimumUTxOvalue)
        , case pp ^. #hardforkEpochNo of
              Just epochNo ->
                  "Hardfork occuring at epoch: " <> build epochNo
              Nothing ->
                  mempty
        ]

-- | Indicates the current level of decentralization in the network.
--
-- According to the Design Specification for Delegation and Incentives in
-- Cardano, the decentralization parameter __/d/__ is a value in the range
-- '[0, 1]', where:
--
--   * __/d/__ = '1' indicates that the network is /completely federalized/.
--   * __/d/__ = '0' indicates that the network is /completely decentralized/.
--
-- However, in Cardano Wallet, we represent the decentralization level as a
-- percentage, where:
--
--   * '  0 %' indicates that the network is /completely federalized/.
--   * '100 %' indicates that the network is /completely decentralized/.
--
newtype DecentralizationLevel = DecentralizationLevel
    { unDecentralizationLevel :: Percentage }
    deriving (Bounded, Eq, Generic, Show)

instance NFData DecentralizationLevel

instance Buildable DecentralizationLevel where
    build = build . unDecentralizationLevel

-- | Parameters that relate to the construction of __transactions__.
--
data TxParameters = TxParameters
    { getFeePolicy :: FeePolicy
        -- ^ Formula for calculating the transaction fee.
    , getTxMaxSize :: Quantity "byte" Word16
        -- ^ Maximum size of a transaction (soft or hard limit).
    } deriving (Generic, Show, Eq)

instance NFData TxParameters

instance Buildable TxParameters where
    build txp = listF' id
        [ "Fee policy: " <> feePolicyF (txp ^. #getFeePolicy)
        , "Tx max size: " <> txMaxSizeF (txp ^. #getTxMaxSize)
        ]
      where
        feePolicyF = build . toText
        txMaxSizeF (Quantity s) = build s

{-------------------------------------------------------------------------------
                                   Slotting
-------------------------------------------------------------------------------}

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochNumber :: !EpochNo
  , slotNumber :: !SlotInEpoch
  } deriving stock (Show, Read, Eq, Ord, Generic)

newtype SlotInEpoch = SlotInEpoch { unSlotInEpoch :: Word32 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Buildable, NFData, Enum)

newtype EpochNo = EpochNo { unEpochNo :: Word31 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Bounded, Enum)

instance ToText EpochNo where
    toText = T.pack . show . unEpochNo

instance FromText EpochNo where
    fromText = validate <=< (fmap (EpochNo . fromIntegral) . fromText @Natural)
      where
        validate x
            | isValidEpochNo x =
                return x
            | otherwise =
                Left $ TextDecodingError "EpochNo value is out of bounds"

isValidEpochNo :: EpochNo -> Bool
isValidEpochNo c = c >= minBound && c <= maxBound

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


instance NFData SlotId

instance Buildable SlotId where
    build (SlotId (EpochNo e) (SlotInEpoch s)) =
        fromString (show e) <> "." <> fromString (show s)

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

-- | Represent a delegation certificate.
data DelegationCertificate
    = CertDelegateNone RewardAccount
    | CertDelegateFull RewardAccount PoolId
    | CertRegisterKey RewardAccount
    deriving (Generic, Show, Eq, Ord)

instance NFData DelegationCertificate

data StakeKeyCertificate
    = StakeKeyRegistration
    | StakeKeyDeregistration
    deriving (Generic, Show, Read, Eq)

instance NFData StakeKeyCertificate

dlgCertAccount :: DelegationCertificate -> RewardAccount
dlgCertAccount = \case
    CertDelegateNone acc -> acc
    CertDelegateFull acc _ -> acc
    CertRegisterKey acc -> acc

dlgCertPoolId :: DelegationCertificate -> Maybe PoolId
dlgCertPoolId = \case
    CertDelegateNone{} -> Nothing
    CertDelegateFull _ poolId -> Just poolId
    CertRegisterKey _ -> Nothing

-- | Sum-type of pool registration- and retirement- certificates. Mirrors the
--  @PoolCert@ type in cardano-ledger-specs.
data PoolCertificate
    = Registration PoolRegistrationCertificate
    | Retirement PoolRetirementCertificate
    deriving (Generic, Show, Eq, Ord)

instance NFData PoolCertificate

getPoolCertificatePoolId :: PoolCertificate -> PoolId
getPoolCertificatePoolId = \case
    Registration cert ->
        view #poolId cert
    Retirement cert ->
        view #poolId cert

setPoolCertificatePoolId :: PoolId -> PoolCertificate -> PoolCertificate
setPoolCertificatePoolId newPoolId = \case
    Registration cert -> Registration
        $ set #poolId newPoolId cert
    Retirement cert -> Retirement
        $ set #poolId newPoolId cert

-- | Pool ownership data from the stake pool registration certificate.
data PoolRegistrationCertificate = PoolRegistrationCertificate
    { poolId :: !PoolId
    , poolOwners :: ![PoolOwner]
    , poolMargin :: Percentage
    , poolCost :: Quantity "lovelace" Word64
    , poolPledge :: Quantity "lovelace" Word64
    , poolMetadata :: Maybe (StakePoolMetadataUrl, StakePoolMetadataHash)
    } deriving (Generic, Show, Eq, Ord)

instance NFData PoolRegistrationCertificate

instance Buildable PoolRegistrationCertificate where
    build (PoolRegistrationCertificate {poolId, poolOwners}) = mempty
        <> "Registration of "
        <> build poolId
        <> " owned by "
        <> build poolOwners

data PoolRetirementCertificate = PoolRetirementCertificate
    { poolId :: !PoolId

    -- | The first epoch when the pool becomes inactive.
    , retirementEpoch :: !EpochNo
    } deriving (Generic, Show, Eq, Ord)

instance NFData PoolRetirementCertificate

instance Buildable PoolRetirementCertificate where
    build (PoolRetirementCertificate p e) = mempty
        <> "Pool "
        <> build p
        <> " with retirement epoch "
        <> build e

-- | Represents an abstract notion of a certificate publication time.
--
-- Certificates published at later times take precedence over certificates
-- published at earlier times.
--
data CertificatePublicationTime = CertificatePublicationTime
    { slotNo
        :: SlotNo
    , slotInternalIndex
        :: Word64
        -- ^ Indicates the relative position of a publication within a slot.
    }
    deriving (Eq, Generic, Ord, Show)

-- | Indicates the current life cycle status of a pool.
--
data PoolLifeCycleStatus
    = PoolNotRegistered
        -- ^ Indicates that a pool is not registered.
    | PoolRegistered
        PoolRegistrationCertificate
        -- ^ Indicates that a pool is registered BUT NOT marked for retirement.
        -- Records the latest registration certificate.
    | PoolRegisteredAndRetired
        PoolRegistrationCertificate
        PoolRetirementCertificate
        -- ^ Indicates that a pool is registered AND ALSO marked for retirement.
        -- Records the latest registration and retirement certificates.
    deriving (Eq, Ord, Show)

getPoolRegistrationCertificate
    :: PoolLifeCycleStatus -> Maybe PoolRegistrationCertificate
getPoolRegistrationCertificate = \case
    PoolNotRegistered            -> Nothing
    PoolRegistered           c   -> Just c
    PoolRegisteredAndRetired c _ -> Just c

getPoolRetirementCertificate
    :: PoolLifeCycleStatus -> Maybe PoolRetirementCertificate
getPoolRetirementCertificate = \case
    PoolNotRegistered            -> Nothing
    PoolRegistered           _   -> Nothing
    PoolRegisteredAndRetired _ c -> Just c

{-------------------------------------------------------------------------------
                               Polymorphic Types
-------------------------------------------------------------------------------}

-- | A newtype to wrap raw bytestring representing signed data, captured with a
-- phantom type.
newtype Signature (what :: *) = Signature { getSignature :: ByteString }
    deriving stock (Show, Eq, Generic)
    deriving newtype (ByteArrayAccess)

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


-- | A SMASH server is either an absolute http or https url.
--
-- Don't export SmashServer constructor, use @fromText@ instance instead.
newtype SmashServer = SmashServer { unSmashServer :: URI }
    deriving (Show, Generic, Eq)

instance ToText SmashServer where
    toText (SmashServer uri) = T.pack $ uriToString id uri ""

instance FromText SmashServer where
    fromText (T.unpack -> uri) = runIdentity $ runExceptT $ do
        uri' <- parseAbsoluteURI uri ?? TextDecodingError
            ("Not a valid absolute URI.")
        case uri' of
            (URI {uriAuthority, uriScheme, uriPath, uriQuery, uriFragment})
                | uriScheme `notElem` ["http:", "https:"] ->
                    throwE (TextDecodingError
                        "Not a valid URI scheme, only http/https is supported.")
                | isNothing uriAuthority ->
                    throwE
                        (TextDecodingError "URI must contain a domain part.")
                | not ((uriPath == "" || uriPath == "/")
                && uriQuery == "" && uriFragment == "") ->
                    throwE
                        (TextDecodingError "URI must not contain a path/query/fragment.")
            _ -> pure $ SmashServer uri'

-- | Source of Stake Pool Metadata aggregation.
data PoolMetadataSource
    = FetchNone
    | FetchDirect
    | FetchSMASH SmashServer
    deriving (Show, Generic, Eq)

instance ToText PoolMetadataSource where
    toText FetchNone = (T.pack "none")
    toText FetchDirect = (T.pack "direct")
    toText (FetchSMASH (SmashServer uri)) = T.pack $ uriToString id uri ""

instance FromText PoolMetadataSource where
    fromText "none" = Right FetchNone
    fromText "direct" = Right FetchDirect
    fromText uri = right FetchSMASH . fromText @SmashServer $ uri

unsafeToPMS :: URI -> PoolMetadataSource
unsafeToPMS = FetchSMASH . SmashServer

-- newtypes are for:
--
-- - you really want the data type scrict and zero-cost over an inner type
-- - it is morally a newtype (e.g. we want to add instances over an existing type)
--
-- @Settings@ here is neither of that. It's a real product type, that is supposed
-- to be extended in the future.
{-# HLINT ignore Settings "Use newtype instead of data" #-}
-- | Wallet application settings. These are stored at runtime and
-- potentially mutable.
data Settings = Settings {
    poolMetadataSource :: PoolMetadataSource
} deriving (Show, Generic, Eq)

defaultSettings :: Settings
defaultSettings = Settings {
    poolMetadataSource = FetchNone
}

-- | Various internal states of the pool DB
--  that need to survive wallet restarts. These aren't
--  exposed settings.
{-# HLINT ignore InternalState "Use newtype instead of data" #-}
data InternalState = InternalState
    { lastMetadataGC :: Maybe POSIXTime
    } deriving (Generic, Show, Eq)

defaultInternalState :: InternalState
defaultInternalState = InternalState
    { lastMetadataGC = Nothing }

instance FromJSON PoolMetadataSource where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON PoolMetadataSource where
    toJSON = toJSON . toText
