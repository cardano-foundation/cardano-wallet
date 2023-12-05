module Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap
    , genTokenMapSmallRange
    , shrinkTokenMap
    , genTokenMapPartition
    , genTokenMapPartitionNonNull
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetId.Gen
    ( genAssetId
    , shrinkAssetId
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantity
    , genTokenQuantityPartition
    , shrinkTokenQuantity
    )
import Control.Monad
    ( replicateM
    )
import Data.List
    ( transpose
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Safe
    ( fromJustNote
    )
import Test.QuickCheck
    ( Gen
    , choose
    , oneof
    , shrinkList
    , sized
    )
import Test.QuickCheck.Extra
    ( shrinkInterleaved
    )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

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
