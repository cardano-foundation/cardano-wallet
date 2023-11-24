{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Cardano.Slotting.Slot
    ( SlotNo (..)
    , WithOrigin (..)
    )
import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Passphrase.Types
    ( WalletPassphraseInfo (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( StartTime (..)
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
import Cardano.Wallet.Primitive.Types.Certificates
    ( Certificate (..)
    , CertificatePublicationTime (..)
    , DelegationCertificate (..)
    , NonWalletCertificate (..)
    , PoolCertificate (..)
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , StakeKeyCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId
    , getPoolCertificatePoolId
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate
    , setPoolCertificatePoolId
    )
import Cardano.Wallet.Primitive.Types.DecentralizationLevel
    ( DecentralizationLevel (getDecentralizationLevel)
    , fromDecentralizationLevel
    , fromFederationPercentage
    , getFederationPercentage
    )
import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo (..)
    , isValidEpochNo
    , unsafeEpochNo
    )
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
import Cardano.Wallet.Primitive.Types.GenesisParameters
    ( GenesisParameters (..)
    )
import Cardano.Wallet.Primitive.Types.NetworkParameters
    ( NetworkParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters (..)
    )
import Cardano.Wallet.Primitive.Types.SlotId
    ( SlotId (..)
    , SlotInEpoch (..)
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
import Data.Generics.Labels
    ()
import Data.Kind
    ( Type
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

instance PersistField StakeKeyCertificate where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValueRead

instance PersistFieldSql StakeKeyCertificate where
    sqlType _ = sqlType (Proxy @Text)

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
