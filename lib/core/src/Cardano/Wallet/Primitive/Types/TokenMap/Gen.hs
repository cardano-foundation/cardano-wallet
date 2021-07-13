{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdSized
    , genAssetIdLargeRange
    , genAssetIdSmallRange
    , genTokenMapSized
    , genTokenMapSmallRange
    , shrinkAssetIdSized
    , shrinkAssetIdSmallRange
    , shrinkTokenMapSized
    , shrinkTokenMapSmallRange
    , AssetIdF (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameLargeRange
    , genTokenNameSized
    , genTokenNameSmallRange
    , genTokenPolicyIdLargeRange
    , genTokenPolicyIdSized
    , genTokenPolicyIdSmallRange
    , shrinkTokenNameSized
    , shrinkTokenNameSmallRange
    , shrinkTokenPolicyIdSized
    , shrinkTokenPolicyIdSmallRange
    , tokenNamesMediumRange
    , tokenPolicies
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySized
    , genTokenQuantitySmall
    , shrinkTokenQuantitySized
    , shrinkTokenQuantitySmall
    )
import Control.Monad
    ( replicateM )
import Data.List
    ( elemIndex )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( CoArbitrary (..)
    , Function (..)
    , Gen
    , choose
    , functionMap
    , oneof
    , resize
    , shrinkList
    , sized
    , variant
    )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genAssetIdSized :: Gen AssetId
genAssetIdSized = sized $ \size -> do
    -- Ideally, we want to choose asset identifiers from a range that scales
    -- /linearly/ with the size parameter.
    --
    -- However, since each asset identifier has /two/ components that are
    -- generated /separately/, naively combining the generators for these two
    -- components will give rise to a range of asset identifiers that scales
    -- /quadratically/ with the size parameter, which is /not/ what we want.
    --
    -- Therefore, we pass each individual generator a size parameter that
    -- is the square root of the original.
    --
    let sizeSquareRoot = max 1 $ ceiling $ sqrt $ fromIntegral @Int @Double size
    AssetId
        <$> resize sizeSquareRoot genTokenPolicyIdSized
        <*> resize sizeSquareRoot genTokenNameSized

shrinkAssetIdSized :: AssetId -> [AssetId]
shrinkAssetIdSized (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyIdSized)
    (t, shrinkTokenNameSized)

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genAssetIdSmallRange :: Gen AssetId
genAssetIdSmallRange = AssetId
    <$> genTokenPolicyIdSmallRange
    <*> genTokenNameSmallRange

shrinkAssetIdSmallRange :: AssetId -> [AssetId]
shrinkAssetIdSmallRange (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyIdSmallRange)
    (t, shrinkTokenNameSmallRange)

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange = AssetId
    <$> genTokenPolicyIdLargeRange
    <*> genTokenNameLargeRange

--------------------------------------------------------------------------------
-- Token maps with assets and quantities chosen from ranges that depend on the
-- size parameter
--------------------------------------------------------------------------------

genTokenMapSized :: Gen TokenMap
genTokenMapSized = sized $ \size -> do
    assetCount <- choose (0, size)
    TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdSized
        <*> genTokenQuantitySized

shrinkTokenMapSized :: TokenMap -> [TokenMap]
shrinkTokenMapSized
    = fmap TokenMap.fromFlatList
    . shrinkList shrinkAssetQuantity
    . TokenMap.toFlatList
  where
    shrinkAssetQuantity (a, q) = shrinkInterleaved
        (a, shrinkAssetIdSized)
        (q, shrinkTokenQuantitySized)

--------------------------------------------------------------------------------
-- Token maps with assets and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenMapSmallRange :: Gen TokenMap
genTokenMapSmallRange = do
    assetCount <- oneof
        [ pure 0
        , pure 1
        , choose (2, 16)
        ]
    TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdSmallRange
        <*> genTokenQuantitySmall

shrinkTokenMapSmallRange :: TokenMap -> [TokenMap]
shrinkTokenMapSmallRange
    = fmap TokenMap.fromFlatList
    . shrinkList shrinkAssetQuantity
    . TokenMap.toFlatList
  where
    shrinkAssetQuantity (a, q) = shrinkInterleaved
        (a, shrinkAssetIdSmallRange)
        (q, shrinkTokenQuantitySmall)

--------------------------------------------------------------------------------
-- Filtering functions
--------------------------------------------------------------------------------

newtype AssetIdF = AssetIdF AssetId
    deriving (Generic, Eq, Show, Read)

instance Function AssetIdF where
    function = functionMap show read

instance CoArbitrary AssetIdF where
    coarbitrary (AssetIdF AssetId{tokenName, tokenPolicyId}) genB = do
        let n = fromMaybe 0 (elemIndex tokenName tokenNamesMediumRange)
        let m = fromMaybe 0 (elemIndex tokenPolicyId tokenPolicies)
        variant (n+m) genB
