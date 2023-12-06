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
    ( genTokenName
    , genTokenNameLargeRange
    , shrinkTokenName
    , testTokenNames
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
genAssetId = genSized2With AssetId genTokenPolicyId genTokenName

shrinkAssetId :: AssetId -> [AssetId]
shrinkAssetId (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyId)
    (t, shrinkTokenName)

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange = AssetId
    <$> genTokenPolicyIdLargeRange
    <*> genTokenNameLargeRange

--------------------------------------------------------------------------------
-- Filtering functions
--------------------------------------------------------------------------------

newtype AssetIdF = AssetIdF AssetId
    deriving (Generic, Eq, Show, Read)

instance Function AssetIdF where
    function = functionMap show read

instance CoArbitrary AssetIdF where
    coarbitrary (AssetIdF AssetId {tokenName, tokenPolicyId}) genB = do
        let n = fromMaybe 0 (elemIndex tokenName testTokenNames)
        let m = fromMaybe 0 (elemIndex tokenPolicyId testTokenPolicyIds)
        variant (n + m) genB
