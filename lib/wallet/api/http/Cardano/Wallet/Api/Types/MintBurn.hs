{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Api.Types.MintBurn
    ( ApiAssetMintBurn (..)
    , ApiTokenAmountFingerprint (..)
    , ApiTokens (..)
    , noApiAsset
    , toApiTokens
    , includePolicyKeyInfo
    , policyIx
    )
where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index
    , getIndex
    )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..)
    )
import Cardano.Wallet.Api.Lib.Options
    ( DefaultRecord (DefaultRecord)
    )
import Cardano.Wallet.Api.Types.Key
    ( ApiPolicyKey
    )
import Cardano.Wallet.Api.Types.Primitive
    (
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( toNestedList
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName
    , TokenPolicyId
    , mkTokenFingerprint
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (unTokenQuantity)
    )
import Cardano.Wallet.Transaction
    ( AnyScript
    , TokenMapWithScripts (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Aeson.Types
    ( FromJSON
    , ToJSON
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map.Strict
    ( Map
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

data ApiTokenAmountFingerprint = ApiTokenAmountFingerprint
    { assetName :: ApiT W.TokenName
    , quantity :: Natural
    , fingerprint :: ApiT W.TokenFingerprint
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiTokenAmountFingerprint
    deriving anyclass (NFData)

data ApiTokens = ApiTokens
    { policyId :: ApiT W.TokenPolicyId
    , policyScript :: ApiT AnyScript
    , assets :: NonEmpty ApiTokenAmountFingerprint
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiTokens
    deriving anyclass (NFData)

data ApiAssetMintBurn = ApiAssetMintBurn
    { tokens :: [ApiTokens]
    , walletPolicyKeyHash :: Maybe ApiPolicyKey
    , walletPolicyKeyIndex :: Maybe (ApiT DerivationIndex)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiAssetMintBurn
    deriving anyclass (NFData)

noApiAsset :: ApiAssetMintBurn
noApiAsset = ApiAssetMintBurn [] Nothing Nothing

toApiTokens :: TokenMapWithScripts -> [ApiTokens]
toApiTokens (TokenMapWithScripts tokenMap scriptMap) =
    map fromIdScriptAssets
        $ toIdScriptAssets scriptMap tokenMap

includePolicyKeyInfo :: TokenMapWithScripts -> Maybe a -> Maybe a
includePolicyKeyInfo (TokenMapWithScripts tokenMap _) xpubM =
    if tokenMap == TokenMap.empty
        then Nothing
        else xpubM

fromIdScriptAssets
    :: (TokenPolicyId, AnyScript, NE.NonEmpty (TokenName, TokenQuantity))
    -> ApiTokens
fromIdScriptAssets (policy, script, tokens') =
    ApiTokens
        { policyId = ApiT policy
        , policyScript = ApiT script
        , assets = NE.map (toTokenAmountFingerprint policy) tokens'
        }

toTokenAmountFingerprint
    :: TokenPolicyId
    -> (TokenName, TokenQuantity)
    -> ApiTokenAmountFingerprint
toTokenAmountFingerprint policy (name, tokenquantity) =
    ApiTokenAmountFingerprint
        { assetName = ApiT name
        , quantity = unTokenQuantity tokenquantity
        , fingerprint = ApiT $ mkTokenFingerprint policy name
        }

toIdScriptAssets
    :: Map TokenPolicyId b
    -> TokenMap.TokenMap
    -> [(TokenPolicyId, b, NE.NonEmpty (TokenName, TokenQuantity))]
toIdScriptAssets scriptmap tokenmap =
    [ (policy, askForScript policy scriptmap, tokenQuantities)
    | (policy, tokenQuantities) <- toNestedList tokenmap
    ]

askForScript :: TokenPolicyId -> Map TokenPolicyId b -> b
askForScript policyId' scriptMap =
    case Map.lookup policyId' scriptMap of
        Just script -> script
        Nothing ->
            error
                "askForScript: no minting/burning without either\
                \ native, plutus script or reference input"

policyIx :: ApiT DerivationIndex
policyIx =
    ApiT
        $ DerivationIndex
        $ getIndex (minBound :: Index 'Hardened 'PolicyK)
