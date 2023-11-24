{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.Types.Certificates
    ( DelegationCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId
    , StakeKeyCertificate (..)
    , PoolCertificate (..)
    , getPoolCertificatePoolId
    , setPoolCertificatePoolId
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , NonWalletCertificate (..)
    , Certificate (..)
    , CertificatePublicationTime (..)
    , PoolLifeCycleStatus (..)
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate
    )
where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId
    , PoolOwner
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Cardano.Wallet.Primitive.Types.StakePoolMetadata
    ( StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Lens
    ( set
    , view
    )
import Data.Generics.Labels
    ()
import Data.Quantity
    ( Percentage
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import Data.Word
    ( Word64
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

data DelegationCertificate
    = CertDelegateNone RewardAccount
    | CertDelegateFull RewardAccount PoolId
    | CertRegisterKey RewardAccount
    deriving (Generic, Show, Eq, Ord)

instance NFData DelegationCertificate

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

data StakeKeyCertificate
    = StakeKeyRegistration
    | StakeKeyDeregistration
    deriving (Generic, Show, Read, Eq)

instance NFData StakeKeyCertificate

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
    Registration cert ->
        Registration
            $ set #poolId newPoolId cert
    Retirement cert ->
        Retirement
            $ set #poolId newPoolId cert

-- | Pool ownership data from the stake pool registration certificate.
data PoolRegistrationCertificate = PoolRegistrationCertificate
    { poolId :: !PoolId
    , poolOwners :: ![PoolOwner]
    , poolMargin :: Percentage
    , poolCost :: Coin
    , poolPledge :: Coin
    , poolMetadata :: Maybe (StakePoolMetadataUrl, StakePoolMetadataHash)
    }
    deriving (Generic, Show, Eq, Ord)

instance NFData PoolRegistrationCertificate

instance Buildable PoolRegistrationCertificate where
    build (PoolRegistrationCertificate{poolId, poolOwners}) =
        mempty
            <> "Registration of "
            <> build poolId
            <> " owned by "
            <> build poolOwners

data PoolRetirementCertificate = PoolRetirementCertificate
    { poolId :: !PoolId
    , retirementEpoch :: !EpochNo
    -- ^ The first epoch when the pool becomes inactive.
    }
    deriving (Generic, Show, Eq, Ord)

instance NFData PoolRetirementCertificate

instance Buildable PoolRetirementCertificate where
    build (PoolRetirementCertificate p e) =
        mempty
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
    fromText _ =
        Left
            $ TextDecodingError
                "expecting either 'genesis' or 'mir' for NonWalletCertificate text value"

instance NFData NonWalletCertificate

data Certificate
    = CertificateOfDelegation DelegationCertificate
    | CertificateOfPool PoolCertificate
    | CertificateOther NonWalletCertificate
    deriving (Generic, Show, Eq)

instance NFData Certificate

-- | Represents an abstract notion of a certificate publication time.
--
-- Certificates published at later times take precedence over certificates
-- published at earlier times.
data CertificatePublicationTime = CertificatePublicationTime
    { slotNo
        :: SlotNo
    , slotInternalIndex
        :: Word64
    -- ^ Indicates the relative position of a publication within a slot.
    }
    deriving (Eq, Generic, Ord, Show)

-- | Indicates the current life cycle status of a pool.
data PoolLifeCycleStatus
    = -- | Indicates that a pool is not registered.
      PoolNotRegistered
    | -- | Indicates that a pool is registered BUT NOT marked for retirement.
      -- Records the latest registration certificate.
      PoolRegistered
        PoolRegistrationCertificate
    | -- | Indicates that a pool is registered AND ALSO marked for retirement.
      -- Records the latest registration and retirement certificates.
      PoolRegisteredAndRetired
        PoolRegistrationCertificate
        PoolRetirementCertificate
    deriving (Eq, Ord, Show)

getPoolRegistrationCertificate
    :: PoolLifeCycleStatus -> Maybe PoolRegistrationCertificate
getPoolRegistrationCertificate = \case
    PoolNotRegistered -> Nothing
    PoolRegistered c -> Just c
    PoolRegisteredAndRetired c _ -> Just c

getPoolRetirementCertificate
    :: PoolLifeCycleStatus -> Maybe PoolRetirementCertificate
getPoolRetirementCertificate = \case
    PoolNotRegistered -> Nothing
    PoolRegistered _ -> Nothing
    PoolRegisteredAndRetired _ c -> Just c
