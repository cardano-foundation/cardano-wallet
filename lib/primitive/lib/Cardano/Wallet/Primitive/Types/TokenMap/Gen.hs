{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , genTokenMap
    , genTokenMapSmallRange
    , shrinkAssetId
    , shrinkTokenMap
    , AssetIdF (..)
    , genTokenMapPartition
    , genTokenMapPartitionNonNull
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenName, genTokenNameLargeRange, genTokenPolicyId,
    genTokenPolicyIdLargeRange, shrinkTokenName, shrinkTokenPolicyId,
    testTokenNames, testTokenPolicyIds )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantity, genTokenQuantityPartition, shrinkTokenQuantity )
import Control.Monad
    ( replicateM )
import Data.List
    ( elemIndex, transpose )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )
import Safe
    ( fromJustNote )
import Test.QuickCheck
    ( CoArbitrary (..), Function (..), Gen, choose, functionMap, oneof,
    shrinkList, sized, variant )
import Test.QuickCheck.Extra
    ( genSized2With, shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

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
-- Token maps with assets and quantities chosen from ranges that depend on the
-- size parameter
--------------------------------------------------------------------------------

genTokenMap :: Gen TokenMap
genTokenMap = sized $ \size -> do
    assetCount <- choose (0, size)
    TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetId
        <*> genTokenQuantity

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
        <$> genAssetId
        <*> genTokenQuantity

shrinkTokenMap :: TokenMap -> [TokenMap]
shrinkTokenMap
    = fmap TokenMap.fromFlatList
    . shrinkList shrinkAssetQuantity
    . TokenMap.toFlatList
  where
    shrinkAssetQuantity (a, q) = shrinkInterleaved
        (a, shrinkAssetId)
        (q, shrinkTokenQuantity)

--------------------------------------------------------------------------------
-- Filtering functions
--------------------------------------------------------------------------------

newtype AssetIdF = AssetIdF AssetId
    deriving (Generic, Eq, Show, Read)

instance Function AssetIdF where
    function = functionMap show read

instance CoArbitrary AssetIdF where
    coarbitrary (AssetIdF AssetId{tokenName, tokenPolicyId}) genB = do
        let n = fromMaybe 0 (elemIndex tokenName testTokenNames)
        let m = fromMaybe 0 (elemIndex tokenPolicyId testTokenPolicyIds)
        variant (n+m) genB

--------------------------------------------------------------------------------
-- Partitioning token maps
--------------------------------------------------------------------------------

-- | Partitions a token map randomly into a given number of parts.
--
-- Satisfies the following properties:
--
-- prop> forAll (genTokenMapPartition m i) $ (== m      ) . fold
-- prop> forAll (genTokenMapPartition m i) $ (== max 1 i) . length
--
genTokenMapPartition :: TokenMap -> Int -> Gen (NonEmpty TokenMap)
genTokenMapPartition m i
    | TokenMap.isEmpty m =
        pure $ NE.fromList $ replicate (max 1 i) mempty
    | otherwise =
        fmap TokenMap.fromFlatList . transposeNE <$>
        traverse partitionAQ (TokenMap.toFlatList m)
  where
    partitionAQ :: (a, TokenQuantity) -> Gen (NonEmpty (a, TokenQuantity))
    partitionAQ = fmap sequenceA . traverse (`genTokenQuantityPartition` i)

    transposeNE :: [NonEmpty a] -> NonEmpty [a]
    transposeNE = fromJustNote note . NE.nonEmpty . transpose . fmap NE.toList
      where
        note = "genTokenMapPartition.transposeNE: unexpected empty list"

-- | Like 'genTokenMapPartition', but with empty values removed from the result.
--
genTokenMapPartitionNonNull :: TokenMap -> Int -> Gen [TokenMap]
genTokenMapPartitionNonNull m i =
    filter (/= mempty) . F.toList <$> genTokenMapPartition m i
