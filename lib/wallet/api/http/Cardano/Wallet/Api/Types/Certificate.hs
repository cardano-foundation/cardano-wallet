{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Types.Certificate
    ( ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiDeregisterPool (..)
    , ApiExternalCertificate (..)
    , ApiRegisterPool (..)
    , mkApiAnyCertificate)
    where

import Prelude

import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash, StakePoolMetadataUrl )
import Cardano.Pool.Types
    ( PoolId (..), PoolOwner )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..) )
import Cardano.Wallet.Api.Lib.ExtendedObject
    ( extendAesonObject, parseExtendedAesonObject )
import Cardano.Wallet.Api.Types.Primitive
    ()
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), NetworkDiscriminant )
import Cardano.Wallet.Primitive.Types
    ( NonWalletCertificate )
import Cardano.Wallet.Primitive.Types.Coin
    ( unCoin )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( DecodeStakeAddress, EncodeStakeAddress )
import Control.DeepSeq
    ( NFData )
import Data.Aeson.Types
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , Options (..)
    , SumEncoding (TaggedObject, contentsFieldName, tagFieldName)
    , ToJSON (toJSON)
    , Value (Object, String)
    , camelTo2
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    , withObject
    , (.:)
    )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Typeable
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Data.Aeson.Types as Aeson
import qualified Data.List.NonEmpty as NE

data ApiExternalCertificate (n :: NetworkDiscriminant)
    = RegisterRewardAccountExternal
        { rewardAccount :: (ApiT W.RewardAccount, Proxy n)
        }
    | JoinPoolExternal
        { rewardAccount :: (ApiT W.RewardAccount, Proxy n)
        , pool :: ApiT PoolId
        }
    | QuitPoolExternal
        { rewardAccount :: (ApiT W.RewardAccount, Proxy n)
        }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData
instance DecodeStakeAddress n => FromJSON (ApiExternalCertificate n) where
    parseJSON = genericParseJSON apiCertificateOptions
instance EncodeStakeAddress n => ToJSON (ApiExternalCertificate n) where
    toJSON = genericToJSON apiCertificateOptions

data ApiRegisterPool = ApiRegisterPool
    { poolId :: ApiT PoolId
    , poolOwners :: [ApiT PoolOwner]
    , poolMargin :: Quantity "percent" Percentage
    , poolCost :: Quantity "lovelace" Natural
    , poolPledge :: Quantity "lovelace" Natural
    , poolMetadata :: Maybe (ApiT StakePoolMetadataUrl, ApiT StakePoolMetadataHash)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiDeregisterPool = ApiDeregisterPool
    { poolId :: ApiT PoolId
    , retirementEpoch :: ApiT W.EpochNo
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiCertificate
    = RegisterRewardAccount
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        }
    | JoinPool
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        , pool :: ApiT PoolId
        }
    | QuitPool
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiAnyCertificate n =
      WalletDelegationCertificate ApiCertificate
    | DelegationCertificate (ApiExternalCertificate n)
    | StakePoolRegister ApiRegisterPool
    | StakePoolDeregister ApiDeregisterPool
    | OtherCertificate (ApiT NonWalletCertificate)
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance FromJSON ApiRegisterPool where
    parseJSON = parseExtendedAesonObject "ApiRegisterPool" "certificate_type"
instance ToJSON ApiRegisterPool where
    toJSON = extendAesonObject ["certificate_type" .= String "register_pool"]

instance FromJSON ApiDeregisterPool where
    parseJSON = parseExtendedAesonObject "ApiDeregisterPool" "certificate_type"
instance ToJSON ApiDeregisterPool where
    toJSON = extendAesonObject ["certificate_type" .= String "deregister_pool"]

instance DecodeStakeAddress n => FromJSON (ApiAnyCertificate n) where
    parseJSON = withObject "ApiAnyCertificate" $ \o -> do
        (certType :: String) <- o .: "certificate_type"
        case certType of
            "register_pool" -> StakePoolRegister <$> parseJSON (Object o)
            "deregister_pool" -> StakePoolDeregister <$> parseJSON (Object o)
            "join_pool" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "quit_pool" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "register_reward_account" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "join_pool_external" -> DelegationCertificate <$> parseJSON (Object o)
            "quit_pool_external" -> DelegationCertificate <$> parseJSON (Object o)
            "register_reward_account_external" -> DelegationCertificate <$> parseJSON (Object o)
            "mir" -> OtherCertificate <$> parseJSON (Object o)
            "genesis" -> OtherCertificate <$> parseJSON (Object o)
            _ -> fail $ "unknown certificate_type: " <> show certType

instance EncodeStakeAddress n => ToJSON (ApiAnyCertificate n) where
    toJSON (WalletDelegationCertificate cert) = toJSON cert
    toJSON (DelegationCertificate cert) = toJSON cert
    toJSON (StakePoolRegister reg) = toJSON reg
    toJSON (StakePoolDeregister dereg) = toJSON dereg
    toJSON (OtherCertificate cert) = toJSON cert

apiCertificateOptions :: Aeson.Options
apiCertificateOptions = defaultOptions
      { constructorTagModifier = camelTo2 '_'
      , tagSingleConstructors = True
      , fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
      , omitNothingFields = True
      , sumEncoding = TaggedObject
          {
            tagFieldName = "certificate_type"
          , contentsFieldName = "details" -- this isn't actually used
          }
      }
instance FromJSON ApiCertificate where
    parseJSON = genericParseJSON apiCertificateOptions

instance ToJSON ApiCertificate where
    toJSON = genericToJSON apiCertificateOptions

mkApiAnyCertificate
    :: forall n . Maybe W.RewardAccount
    -> NonEmpty DerivationIndex
    -> W.Certificate
    -> ApiAnyCertificate n
mkApiAnyCertificate acct' acctPath' = \case
    W.CertificateOfDelegation delCert -> toApiDelCert acct' acctPath' delCert
    W.CertificateOfPool poolCert -> toApiPoolCert poolCert
    W.CertificateOther otherCert -> toApiOtherCert otherCert
    where
    toApiOtherCert = OtherCertificate . ApiT

    toApiPoolCert
        (W.Registration
            (W.PoolRegistrationCertificate
                poolId' poolOwners' poolMargin'
                poolCost' poolPledge' poolMetadata')) =
        let enrich (a, b) = (ApiT a, ApiT b)
        in StakePoolRegister $ ApiRegisterPool
           (ApiT poolId')
           (map ApiT poolOwners')
           (Quantity poolMargin')
           (Quantity $ unCoin poolCost')
           (Quantity $ unCoin poolPledge')
           (enrich <$> poolMetadata')
    toApiPoolCert
        (W.Retirement (W.PoolRetirementCertificate poolId' retirementEpoch')) =
        StakePoolDeregister $ ApiDeregisterPool
        (ApiT poolId')
        (ApiT retirementEpoch')

    toApiDelCert acctM acctPath (W.CertDelegateNone rewardKey) =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $ QuitPool $ NE.map ApiT acctPath
        else
            DelegationCertificate $ QuitPoolExternal (ApiT rewardKey, Proxy @n)
    toApiDelCert acctM acctPath (W.CertRegisterKey rewardKey) =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $
            RegisterRewardAccount $ NE.map ApiT acctPath
        else
            DelegationCertificate $
            RegisterRewardAccountExternal (ApiT rewardKey, Proxy @n)
    toApiDelCert acctM acctPath (W.CertDelegateFull rewardKey poolId') =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $
            JoinPool (NE.map ApiT acctPath) (ApiT poolId')
        else
            DelegationCertificate $
            JoinPoolExternal (ApiT rewardKey, Proxy @n) (ApiT poolId')
