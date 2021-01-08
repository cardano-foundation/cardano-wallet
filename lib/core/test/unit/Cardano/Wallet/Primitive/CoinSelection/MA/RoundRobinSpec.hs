{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobinSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( groupByKey, makeChange, makeChangeForSurplusAssets, ungroupByKey )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdSmallRange, shrinkAssetIdSmallRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmallPositive, shrinkTokenQuantitySmallPositive )
import Control.Monad
    ( replicateM )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , genericShrink
    , property
    , suchThat
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobinSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Making change" $ do

        it "prop_makeChange_identity" $
            property prop_makeChange_identity
        it "prop_makeChange_length" $
            property prop_makeChange_length
        it "prop_makeChange_sum" $
            property prop_makeChange_sum

    parallel $ describe "Making change for surplus assets" $ do

        it "prop_makeChangeForSurplusAssets_length" $
            property prop_makeChangeForSurplusAssets_length
        it "prop_makeChangeForSurplusAssets_sort" $
            property prop_makeChangeForSurplusAssets_sort
        it "prop_makeChangeForSurplusAssets_sum" $
            property prop_makeChangeForSurplusAssets_sum

    parallel $ describe "Grouping and ungrouping" $ do

        it "prop_groupByKey_ungroupByKey" $
            property $ prop_groupByKey_ungroupByKey @Int @Int
        it "prop_ungroupByKey_groupByKey" $
            property $ prop_ungroupByKey_groupByKey @Int @Int

--------------------------------------------------------------------------------
-- Making change
--------------------------------------------------------------------------------

data MakeChangeData = MakeChangeData
    { inputBundles
        :: NonEmpty TokenBundle
    , outputBundles
        :: NonEmpty TokenBundle
    } deriving (Eq, Show)

isValidMakeChangeData :: MakeChangeData -> Bool
isValidMakeChangeData p = (&&)
    (totalOutputValue `leq` totalInputValue)
    (totalOutputCoinValue > Coin 0)
  where
    totalInputValue = F.fold $ inputBundles p
    totalOutputValue = F.fold $ outputBundles p
    totalOutputCoinValue = TokenBundle.getCoin totalOutputValue

genMakeChangeData :: Gen MakeChangeData
genMakeChangeData = flip suchThat isValidMakeChangeData $ do
    outputBundleCount <- choose (0, 15)
    let inputBundleCount = outputBundleCount * 4
    MakeChangeData
        <$> genTokenBundles inputBundleCount
        <*> genTokenBundles outputBundleCount
  where
    genTokenBundles :: Int -> Gen (NonEmpty TokenBundle)
    genTokenBundles count = (:|)
        <$> genTokenBundleSmallRange
        <*> replicateM count genTokenBundleSmallRange

prop_makeChange_identity
    :: NonEmpty TokenBundle -> Property
prop_makeChange_identity bundles =
    F.fold (makeChange bundles bundles) === TokenBundle.empty

prop_makeChange_length
    :: MakeChangeData -> Property
prop_makeChange_length p = (===)
    (length $ outputBundles p)
    (length $ makeChange (inputBundles p) (outputBundles p))

prop_makeChange_sum
    :: MakeChangeData -> Property
prop_makeChange_sum p =
    totalInputValue === totalOutputValue <> totalChangeValue
  where
    change = makeChange (inputBundles p) (outputBundles p)
    totalInputValue = F.fold $ inputBundles p
    totalOutputValue = F.fold $ outputBundles p
    totalChangeValue = F.fold change

--------------------------------------------------------------------------------
-- Making change for surplus assets
--------------------------------------------------------------------------------

prop_makeChangeForSurplusAssets_length
    :: Map AssetId (NonEmpty TokenQuantity) -> NonEmpty () -> Property
prop_makeChangeForSurplusAssets_length assetQuantities target =
    NE.length (makeChangeForSurplusAssets assetQuantities target)
        === NE.length target

prop_makeChangeForSurplusAssets_sort
    :: Map AssetId (NonEmpty TokenQuantity) -> NonEmpty () -> Property
prop_makeChangeForSurplusAssets_sort assetQuantities target = property $
    inAscendingPartialOrder (makeChangeForSurplusAssets assetQuantities target)

prop_makeChangeForSurplusAssets_sum
    :: Map AssetId (NonEmpty TokenQuantity) -> NonEmpty () -> Property
prop_makeChangeForSurplusAssets_sum assetQuantities target =
    sumActual === sumExpected
  where
    sumActual =
        F.fold $ makeChangeForSurplusAssets assetQuantities target
    sumExpected =
        TokenMap.fromFlatList $ Map.toList $ F.fold <$> assetQuantities

--------------------------------------------------------------------------------
-- Grouping and ungrouping
--------------------------------------------------------------------------------

prop_groupByKey_ungroupByKey
    :: forall k v. (Ord k, Ord v, Show k, Show v)
    => [(k, v)]
    -> Property
prop_groupByKey_ungroupByKey kvs =
    L.sort kvs === L.sort (ungroupByKey $ groupByKey kvs)

prop_ungroupByKey_groupByKey
    :: forall k v. (Ord k, Ord v, Show k, Show v)
    => Map k (NonEmpty v)
    -> Property
prop_ungroupByKey_groupByKey kvs =
    fmap NE.sort kvs === fmap NE.sort (groupByKey $ ungroupByKey kvs)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

inAscendingPartialOrder :: (Foldable f, PartialOrd a) => f a -> Bool
inAscendingPartialOrder = all (uncurry leq) . consecutivePairs . F.toList

--------------------------------------------------------------------------------
-- Arbitraries
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary AssetId where
    arbitrary = genAssetIdSmallRange
    shrink = shrinkAssetIdSmallRange

instance Arbitrary MakeChangeData where
    arbitrary = genMakeChangeData

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantitySmallPositive
    shrink = shrinkTokenQuantitySmallPositive
