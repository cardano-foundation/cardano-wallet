{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- Technically,  instance Buildable Slot
-- in an orphan instance, but `Slot` is a type synonym
-- and the instance is more specific than a vanilla `WithOrigin` instance.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains the core primitive of a Wallet. This is roughly a
-- Haskell translation of the [Formal Specification for a Cardano Wallet](https://github.com/cardano-foundation/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
--
-- It doesn't contain any particular business-logic code, but defines a few
-- primitive operations on Wallet core types as well.

module Cardano.Wallet.Primitive.Types
    (
    -- * Block
      Block(..)
    , BlockHeader(..)
    , isGenesisBlockHeader

    , ChainPoint (..)
    , compareSlot
    , chainPointFromBlockHeader
    , Slot
    , WithOrigin (..)
    , toSlot

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

    , NonWalletCertificate (..)
    , Certificate (..)

    -- * Network Parameters
    , NetworkParameters (..)
    , GenesisParameters (..)
    , SlottingParameters (..)
    , ProtocolParameters (..)
    , TxParameters (..)
    , TokenBundleMaxSize (..)
    , EraInfo (..)
    , emptyEraInfo
    , ActiveSlotCoefficient (..)
    , DecentralizationLevel
    , getDecentralizationLevel
    , getFederationPercentage
    , fromDecentralizationLevel
    , fromFederationPercentage
    , EpochLength (..)
    , EpochNo (..)
    , unsafeEpochNo
    , isValidEpochNo
    , FeePolicy (..)
    , LinearFunction (..)
    , SlotId (..)
    , SlotNo (..)
    , SlotLength (..)
    , SlotInEpoch (..)
    , StartTime (..)
    , stabilityWindowByron
    , stabilityWindowShelley
    , ExecutionUnits (..)
    , ExecutionUnitPrices (..)

    -- * Wallet Metadata
    , WalletMetadata(..)
    , WalletId(..)
    , WalletName(..)
    , walletNameMinLength
    , walletNameMaxLength
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletDelegationNext (..)
    , IsDelegatingTo (..)

    -- * Stake Pools
    , StakeKeyCertificate (..)

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

    -- * Polymorphic
    , Signature (..)

    -- * Settings
    , Settings(..)
    , SmashServer
    , unSmashServer
    , PoolMetadataSource( .. )
    , defaultSettings
    , unsafeToPMS

    , TokenMetadataServer (..)

    -- * InternalState
    , InternalState (..)
    , defaultInternalState

    ) where

import Prelude

import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Cardano.Pool.Types
    ( PoolId
    , PoolOwner
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    , WithOrigin (..)
    )
import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Passphrase.Types
    ( WalletPassphraseInfo (..)
    )
import Cardano.Wallet.Primitive.Types.Block
    ( Block (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , Slot
    , chainPointFromBlockHeader
    , compareSlot
    , isGenesisBlockHeader
    , toSlot
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.DecentralizationLevel
    ( DecentralizationLevel (getDecentralizationLevel)
    , fromDecentralizationLevel
    , fromFederationPercentage
    , getFederationPercentage
    )
import Cardano.Wallet.Primitive.Types.DelegationCertificate
    ( DelegationCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId
    )
import Cardano.Wallet.Primitive.Types.EpochNo
import Cardano.Wallet.Primitive.Types.EraInfo
    ( EraInfo (..)
    , emptyEraInfo
    )
import Cardano.Wallet.Primitive.Types.ExecutionUnitPrices
    ( ExecutionUnitPrices (..)
    )
import Cardano.Wallet.Primitive.Types.FeePolicy
    ( FeePolicy (..)
    , LinearFunction (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters (..)
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , SlotLength (..)
    , SlottingParameters (..)
    , stabilityWindowByron
    , stabilityWindowShelley
    )
import Cardano.Wallet.Primitive.Types.TokenBundleMaxSize
    ( TokenBundleMaxSize (..)
    )
import Cardano.Wallet.Primitive.Types.TxParameters
    ( ExecutionUnits (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    , parseURI
    , uriToText
    )
import Control.Arrow
    ( right
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Monad
    ( (>=>)
    )
import Crypto.Hash
    ( Blake2b_160
    , Digest
    , digestFromByteString
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.ByteArray
    ( ByteArrayAccess
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Data
    ( Proxy (..)
    )
import Data.Generics.Internal.VL.Lens
    ( set
    , view
    )
import Data.Generics.Labels
    ()
import Data.Kind
    ( Type
    )
import Data.Maybe
    ( isJust
    )
import Data.Quantity
    ( Percentage (..)
    )
import Data.String
    ( fromString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import Data.Word
    ( Word32
    , Word64
    )
import Database.Persist.Class.PersistField
    ( PersistField (fromPersistValue, toPersistValue)
    )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueRead
    )
import Database.Persist.Sql
    ( PersistFieldSql (sqlType)
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    , prefixF
    , pretty
    , suffixF
    )
import GHC.Generics
    ( Generic
    )
import Network.URI
    ( URI (..)
    , uriToString
    )

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
    } deriving (Eq, Show, Generic)

instance NFData WalletMetadata

formatUTCTime :: UTCTime -> Text
formatUTCTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

instance Buildable WalletMetadata where
    build (WalletMetadata wName wTime _ ) = mempty
        <> build wName <> ", "
        <> "created at " <> build (formatUTCTime wTime)

instance Buildable (WalletMetadata, WalletDelegation) where
    build (meta, delegation) = build meta <> ", " <>  build delegation

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
                                    Block
-------------------------------------------------------------------------------}

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
    } deriving (Generic, Show, Eq)

instance NFData GenesisParameters

instance Buildable GenesisParameters where
    build gp = blockListF' "" id
        [ "Genesis block hash: " <> genesisF (getGenesisBlockHash gp)
        , "Genesis block date: " <> startTimeF (getGenesisBlockDate
            (gp :: GenesisParameters))
        ]
      where
        genesisF = build . T.decodeUtf8 . convertToBase Base16 . getHash
        startTimeF (StartTime s) = build s

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

instance NFData SlotId

instance Buildable SlotId where
    build (SlotId (EpochNo e) (SlotInEpoch s)) =
        fromString (show e) <> "." <> fromString (show s)

-- | Blockchain start time
newtype StartTime = StartTime {utcTimeOfStartTime :: UTCTime}
    deriving (Show, Eq, Ord, Generic)

instance NFData StartTime

{-------------------------------------------------------------------------------
              Stake Pool Delegation and Registration Certificates
-------------------------------------------------------------------------------}

data StakeKeyCertificate
    = StakeKeyRegistration
    | StakeKeyDeregistration
    deriving (Generic, Show, Read, Eq)

instance NFData StakeKeyCertificate

instance PersistField StakeKeyCertificate where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValueRead

instance PersistFieldSql StakeKeyCertificate where
    sqlType _ = sqlType (Proxy @Text)

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
    , poolCost :: Coin
    , poolPledge :: Coin
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

data NonWalletCertificate
    = GenesisCertificate
    | MIRCertificate
    deriving (Generic, Show, Read, Eq)

instance ToText NonWalletCertificate where
    toText GenesisCertificate = "genesis"
    toText MIRCertificate = "mir"

instance FromText NonWalletCertificate where
    fromText "genesis" = Right GenesisCertificate
    fromText "mir" = Right MIRCertificate
    fromText _ = Left $ TextDecodingError
        "expecting either 'genesis' or 'mir' for NonWalletCertificate text value"

instance NFData NonWalletCertificate

data Certificate =
      CertificateOfDelegation DelegationCertificate
    | CertificateOfPool PoolCertificate
    | CertificateOther NonWalletCertificate
    deriving (Generic, Show, Eq)

instance NFData Certificate

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
newtype Signature (what :: Type) = Signature { getSignature :: ByteString }
    deriving stock (Show, Eq, Generic)
    deriving newtype (ByteArrayAccess)

{-------------------------------------------------------------------------------
                               Metadata services
-------------------------------------------------------------------------------}

newtype TokenMetadataServer = TokenMetadataServer
    { unTokenMetadataServer :: URI }
    deriving (Show, Generic, Eq)

instance ToText TokenMetadataServer where
    toText = uriToText . unTokenMetadataServer

instance FromText TokenMetadataServer where
    fromText = fmap TokenMetadataServer . parseURI

-- | A SMASH server is either an absolute http or https url.
--
-- Don't export SmashServer constructor, use @fromText@ instance instead.
newtype SmashServer = SmashServer { unSmashServer :: URI }
    deriving (Show, Generic, Eq)

instance ToText SmashServer where
    toText = uriToText . unSmashServer

instance FromText SmashServer where
    fromText = fmap SmashServer . parseURI

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
{- HLINT ignore Settings "Use newtype instead of data" -}
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
{- HLINT ignore InternalState "Use newtype instead of data" -}
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
