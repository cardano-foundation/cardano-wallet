{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- Basic types for stake pools and their metadata.
--
module Cardano.Wallet.Primitive.Types.StakePools
    ( -- * Stake pool properties
      PoolId(..)
    , PoolOwner(..)
    , poolIdBytesLength
    , decodePoolIdBech32
    , encodePoolIdBech32
    , StakePoolsSummary (..)

    -- * Stake pool registration and retirement
    , CertificatePublicationTime (..)
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , PoolCertificate (..)
    , getPoolCertificatePoolId
    , setPoolCertificatePoolId
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate

    -- * Stake pool metadata
    , StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker (..)
    , PoolMetadataGCStatus (..)

    -- * Stake pool metadata server settings
    , SmashServer
    , unSmashServer
    , PoolMetadataSource (..)
    , unsafeToPMS

    -- * Settings
    , Settings (..)
    , defaultSettings

    -- * InternalState
    , InternalState (..)
    , defaultInternalState

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), hashFromText )
import Cardano.Wallet.Primitive.Types.Slotting
    ( EpochNo (..), SlotNo )
import Cardano.Wallet.Util
    ( ShowFmt (..), parseURI, uriToText )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( when, (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), withObject, (.:), (.:?) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( set, view )
import Data.Generics.Labels
    ()
import Data.List
    ( intercalate )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Percentage (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock.POSIX
    ( POSIXTime )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), fmt, listF', mapF, prefixF, pretty )
import GHC.Generics
    ( Generic )
import Network.URI
    ( URI, uriToString )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

{-------------------------------------------------------------------------------
              Stake Pool Registration and Retirement Certificates
-------------------------------------------------------------------------------}

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
                              Stake Pool Rankings
-------------------------------------------------------------------------------}

data StakePoolsSummary = StakePoolsSummary
    { nOpt :: Int
    , rewards :: Map PoolId Coin
    , stake :: Map PoolId Percentage
    } deriving (Show, Eq)

instance Buildable StakePoolsSummary where
    build StakePoolsSummary{nOpt,rewards,stake} = listF' id
        [ "Stake: " <> mapF (Map.toList stake)
        , "Non-myopic member rewards: " <> mapF (Map.toList rewards)
        , "Optimum number of pools: " <> pretty nOpt
        ]

{-------------------------------------------------------------------------------
                            Metadata for Stake Pools
-------------------------------------------------------------------------------}

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
        let tickerLen = T.length . unStakePoolTicker $ ticker
        when (tickerLen > 5 || tickerLen < 3)
            $ fail "ticker length must be between 3 and 5 characters"

        name <- obj .: "name"
        when (T.length name > 50)
            $ fail "name exceeds max length of 50 chars"

        description <- obj .:? "description"
        when ((T.length <$> description) > Just 255)
            $ fail "description exceeds max length of 255 characters"

        homepage <- obj .: "homepage"

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
    parseJSON = parseJSON >=> either (fail . fmt . build) pure . fromText

instance ToJSON StakePoolTicker where
    toJSON = toJSON . toText

{-------------------------------------------------------------------------------
                               Metadata services
-------------------------------------------------------------------------------}

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
    fromText uri = fmap FetchSMASH . fromText @SmashServer $ uri

instance FromJSON PoolMetadataSource where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON PoolMetadataSource where
    toJSON = toJSON . toText

unsafeToPMS :: URI -> PoolMetadataSource
unsafeToPMS = FetchSMASH . SmashServer

{-------------------------------------------------------------------------------
              "Settings" - where the only setting is the SMASH URL
-------------------------------------------------------------------------------}

-- | Wallet application settings. These are stored at runtime and
-- potentially mutable.
newtype Settings = Settings
    { poolMetadataSource :: PoolMetadataSource
    } deriving (Show, Generic, Eq)

defaultSettings :: Settings
defaultSettings = Settings { poolMetadataSource = FetchNone }

-- | Various internal states of the pool DB
--  that need to survive wallet restarts. These aren't
--  exposed settings.
newtype InternalState = InternalState
    { lastMetadataGC :: Maybe POSIXTime
    } deriving (Generic, Show, Eq)

defaultInternalState :: InternalState
defaultInternalState = InternalState { lastMetadataGC = Nothing }
