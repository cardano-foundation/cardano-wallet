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

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Types.Certificate
    ( ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiDeregisterPool (..)
    , ApiExternalCertificate (..)
    , ApiRegisterPool (..)
    , ApiRewardAccount (..)
    , mkApiAnyCertificate
    )
    where

import Prelude

import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Cardano.Pool.Types
    ( PoolId (..)
    , PoolOwner
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    )
import Cardano.Wallet.Address.Encoding
    ( decodeStakeAddress
    , encodeStakeAddress
    )
import Cardano.Wallet.Api.Aeson
    ( eitherToParser
    )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..)
    )
import Cardano.Wallet.Api.Lib.ExtendedObject
    ( extendAesonObject
    , parseExtendedAesonObject
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount
    )
import Cardano.Wallet.Api.Types.Primitive
    ()
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (sNetworkId)
    , NetworkDiscriminant
    )
import Cardano.Wallet.Primitive.Types
    ( NonWalletCertificate
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , decodeDRepKeyHashBech32
    , decodeDRepScriptHashBech32
    , encodeDRepKeyHashBech32
    , encodeDRepScriptHashBech32
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Control.Applicative
    ( (<|>)
    )
import Control.DeepSeq
    ( NFData
    )
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
    , withText
    , (.:)
    )
import Data.Bifunctor
    ( bimap
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Percentage
    ( Percentage
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text.Class
    ( TextDecodingError (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Api.Types.Amount as ApiAmount
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Data.Aeson.Types as Aeson
import qualified Data.List.NonEmpty as NE

newtype ApiRewardAccount (n :: NetworkDiscriminant)
    = ApiRewardAccount W.RewardAccount
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance HasSNetworkId n => FromJSON (ApiRewardAccount n)
  where
    parseJSON x = parseJSON x >>= eitherToParser
        . bimap ShowFmt ApiRewardAccount
        . decodeStakeAddress (sNetworkId @n)

instance HasSNetworkId n => ToJSON (ApiRewardAccount n)
  where
    toJSON (ApiRewardAccount acct) = toJSON
        . encodeStakeAddress (sNetworkId @n) $ acct

data ApiExternalCertificate (n :: NetworkDiscriminant)
    = RegisterRewardAccountExternal
        { rewardAccount :: ApiRewardAccount n
        }
    | JoinPoolExternal
        { rewardAccount :: ApiRewardAccount n
        , pool :: ApiT PoolId
        }
    | QuitPoolExternal
        { rewardAccount :: ApiRewardAccount n
        }
    | CastVoteExternal
        { rewardAccount :: ApiRewardAccount n
        , vote :: ApiT DRep
        }
    | JoinPoolCastVoteExternal
        { rewardAccount :: ApiRewardAccount n
        , pool :: ApiT PoolId
        , vote :: ApiT DRep
        }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance HasSNetworkId n => FromJSON (ApiExternalCertificate n) where
    parseJSON = genericParseJSON apiCertificateOptions

instance HasSNetworkId n => ToJSON (ApiExternalCertificate n) where
    toJSON = genericToJSON apiCertificateOptions

data ApiRegisterPool = ApiRegisterPool
    { poolId :: ApiT PoolId
    , poolOwners :: [ApiT PoolOwner]
    , poolMargin :: Quantity "percent" Percentage
    , poolCost :: ApiAmount
    , poolPledge :: ApiAmount
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
    | CastVote
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        , vote :: ApiT DRep
        }
    | JoinPoolCastVote
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        , pool :: ApiT PoolId
        , vote :: ApiT DRep
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

instance HasSNetworkId n => FromJSON (ApiAnyCertificate n) where
    parseJSON = withObject "ApiAnyCertificate" $ \o -> do
        (certType :: String) <- o .: "certificate_type"
        case certType of
            "register_pool" -> StakePoolRegister <$> parseJSON (Object o)
            "deregister_pool" -> StakePoolDeregister <$> parseJSON (Object o)
            "join_pool" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "quit_pool" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "cast_vote" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "join_pool_cast_vote" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "register_reward_account" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "join_pool_external" -> DelegationCertificate <$> parseJSON (Object o)
            "quit_pool_external" -> DelegationCertificate <$> parseJSON (Object o)
            "cast_vote_external" -> DelegationCertificate <$> parseJSON (Object o)
            "join_pool_cast_vote_external" -> DelegationCertificate <$> parseJSON (Object o)
            "register_reward_account_external" -> DelegationCertificate <$> parseJSON (Object o)
            "mir" -> OtherCertificate <$> parseJSON (Object o)
            "genesis" -> OtherCertificate <$> parseJSON (Object o)
            _ -> fail $ "unknown certificate_type: " <> show certType

instance HasSNetworkId n => ToJSON (ApiAnyCertificate n) where
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
    W.CertificateOfDelegation _ delCert -> toApiDelCert acct' acctPath' delCert
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
           (ApiAmount.fromCoin poolCost')
           (ApiAmount.fromCoin poolPledge')
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
            DelegationCertificate
                $ QuitPoolExternal (ApiRewardAccount rewardKey)
    toApiDelCert acctM acctPath (W.CertVoteAndDelegate rewardKey Nothing Nothing) =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $
                RegisterRewardAccount $ NE.map ApiT acctPath
        else
            DelegationCertificate $
                RegisterRewardAccountExternal (ApiRewardAccount rewardKey)
    toApiDelCert acctM acctPath (W.CertVoteAndDelegate rewardKey (Just poolId') Nothing) =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $
            JoinPool (NE.map ApiT acctPath) (ApiT poolId')
        else
            DelegationCertificate $
            JoinPoolExternal (ApiRewardAccount rewardKey) (ApiT poolId')
    toApiDelCert acctM acctPath (W.CertVoteAndDelegate rewardKey Nothing (Just vote')) =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $
            CastVote (NE.map ApiT acctPath) (ApiT vote')
        else
            DelegationCertificate $
            CastVoteExternal (ApiRewardAccount rewardKey) (ApiT vote')
    toApiDelCert acctM acctPath (W.CertVoteAndDelegate rewardKey (Just poolId') (Just vote')) =
        if Just rewardKey == acctM then
            WalletDelegationCertificate $
            JoinPoolCastVote (NE.map ApiT acctPath) (ApiT poolId') (ApiT vote')
        else
            DelegationCertificate $
            JoinPoolCastVoteExternal (ApiRewardAccount rewardKey) (ApiT poolId') (ApiT vote')

instance ToJSON (ApiT DRep) where
    toJSON (ApiT Abstain) = "abstain"
    toJSON (ApiT NoConfidence) = "no_confidence"
    toJSON (ApiT (FromDRepID drep)) = case drep of
        DRepFromKeyHash keyhash ->
            String $ encodeDRepKeyHashBech32 keyhash
        DRepFromScriptHash scripthash ->
            String $ encodeDRepScriptHashBech32 scripthash
instance FromJSON (ApiT DRep) where
    parseJSON t =
        parseAbstain t <|> parseNoConfidence t <|> parseKeyHash t <|> parseScriptHash t
      where
        parseKeyHash = withText "DRepKeyHash" $ \txt ->
            case decodeDRepKeyHashBech32 txt of
                Left (TextDecodingError err) -> fail err
                Right keyhash ->
                    pure $ ApiT $ FromDRepID $ DRepFromKeyHash keyhash
        parseScriptHash = withText "DRepScriptHash" $ \txt ->
            case decodeDRepScriptHashBech32 txt of
                Left (TextDecodingError err) -> fail err
                Right scripthash ->
                    pure $ ApiT $ FromDRepID $ DRepFromScriptHash scripthash
        parseAbstain = withText "Abstain" $ \txt ->
            if txt == "abstain" then
                pure $ ApiT Abstain
            else
                fail "'abstain' is expected."
        parseNoConfidence = withText "NoConfidence" $ \txt ->
            if txt == "no_confidence" then
                pure $ ApiT NoConfidence
            else
                fail "'no_confidence' is expected."
