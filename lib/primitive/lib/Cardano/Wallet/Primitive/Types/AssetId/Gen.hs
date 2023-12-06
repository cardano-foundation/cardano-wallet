{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.AssetId.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , shrinkAssetId
    , AssetIdF (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName.Gen
    ( genAssetName
    , genAssetNameLargeRange
    , shrinkAssetName
    , testAssetNames
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId.Gen
    ( genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenPolicyId
    , testTokenPolicyIds
    )
import Data.List
    ( elemIndex
    )
import Data.Maybe
    ( fromMaybe
    )
import GHC.Generics
    ( Generic
    )
import Test.QuickCheck
    ( CoArbitrary (..)
    , Function (..)
    , Gen
    , functionMap
    , variant
    )
import Test.QuickCheck.Extra
    ( genSized2With
    , shrinkInterleaved
    )

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genAssetId :: Gen AssetId
genAssetId = genSized2With AssetId genTokenPolicyId genAssetName

shrinkAssetId :: AssetId -> [AssetId]
shrinkAssetId (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyId)
    (t, shrinkAssetName)

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange = AssetId
    <$> genTokenPolicyIdLargeRange
    <*> genAssetNameLargeRange

--------------------------------------------------------------------------------
-- Filtering functions
--------------------------------------------------------------------------------

newtype AssetIdF = AssetIdF AssetId
    deriving (Generic, Eq, Show, Read)

instance Function AssetIdF where
    function = functionMap show read

instance CoArbitrary AssetIdF where
    coarbitrary (AssetIdF AssetId {tokenName, tokenPolicyId}) genB = do
        let n = fromMaybe 0 (elemIndex tokenName testAssetNames)
        let m = fromMaybe 0 (elemIndex tokenPolicyId testTokenPolicyIds)
        variant (n + m) genB
