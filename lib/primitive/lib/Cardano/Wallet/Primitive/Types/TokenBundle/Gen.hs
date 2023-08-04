module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
  ( genTokenBundleSmallRange
  , genTokenBundleSmallRangePositive
  , genTokenBundle
  , shrinkTokenBundle
  , shrinkTokenBundleSmallRange
  , shrinkTokenBundleSmallRangePositive
  , genTokenBundlePartition
  , genTokenBundlePartitionNonNull
  )
where

import Cardano.Wallet.Primitive.Types.Coin.Gen
  ( genCoin
  , genCoinPartition
  , genCoinPositive
  , shrinkCoin
  , shrinkCoinPositive
  )
import Cardano.Wallet.Primitive.Types.TokenBundle
  ( TokenBundle (..)
  )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
  ( genTokenMap
  , genTokenMapPartition
  , genTokenMapSmallRange
  , shrinkTokenMap
  )
import Data.Foldable qualified as F
import Data.List.NonEmpty
  ( NonEmpty
  )
import Data.List.NonEmpty qualified as NE
import Test.QuickCheck
  ( Gen
  )
import Test.QuickCheck.Extra
  ( shrinkInterleaved
  )
import Prelude

--------------------------------------------------------------------------------
-- Token bundles with variable numbers of assets, the upper bound being
-- QuickCheck's size parameter.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genTokenBundle :: Gen TokenBundle
genTokenBundle =
  TokenBundle
    <$> genCoin
    <*> genTokenMap

shrinkTokenBundle :: TokenBundle -> [TokenBundle]
shrinkTokenBundle (TokenBundle c m) =
  uncurry TokenBundle
    <$> shrinkInterleaved
      (c, shrinkCoin)
      (m, shrinkTokenMap)

--------------------------------------------------------------------------------
-- Token bundles with coins, assets, and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange =
  TokenBundle
    <$> genCoin
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange = shrinkTokenBundle

genTokenBundleSmallRangePositive :: Gen TokenBundle
genTokenBundleSmallRangePositive =
  TokenBundle
    <$> genCoinPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRangePositive :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRangePositive (TokenBundle c m) =
  uncurry TokenBundle
    <$> shrinkInterleaved
      (c, shrinkCoinPositive)
      (m, shrinkTokenMap)

--------------------------------------------------------------------------------
-- Partitioning token bundles
--------------------------------------------------------------------------------

-- | Partitions a token bundle randomly into a given number of parts.
--
-- Satisfies the following properties:
--
-- prop> forAll (genTokenBundlePartition b i) $ (==       b) . fold
-- prop> forAll (genTokenBundlePartition b i) $ (== max 1 i) . length
genTokenBundlePartition :: TokenBundle -> Int -> Gen (NonEmpty TokenBundle)
genTokenBundlePartition (TokenBundle c m) i =
  NE.zipWith TokenBundle
    <$> genCoinPartition c i
    <*> genTokenMapPartition m i

-- | Like 'genTokenBundlePartition', but with empty values removed from the
--   result.
genTokenBundlePartitionNonNull :: TokenBundle -> Int -> Gen [TokenBundle]
genTokenBundlePartitionNonNull m i =
  filter (/= mempty) . F.toList <$> genTokenBundlePartition m i
