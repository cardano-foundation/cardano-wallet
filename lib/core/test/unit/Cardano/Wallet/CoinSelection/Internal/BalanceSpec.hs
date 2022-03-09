{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.BalanceSpec
    ( spec
    , MockAssessTokenBundleSize
    , MockComputeMinimumAdaQuantity
    , MockComputeMinimumCost
    , MockComputeSelectionLimit
    , genMockAssessTokenBundleSize
    , genMockComputeMinimumAdaQuantity
    , genMockComputeMinimumCost
    , genMockComputeSelectionLimit
    , shrinkMockAssessTokenBundleSize
    , shrinkMockComputeMinimumAdaQuantity
    , shrinkMockComputeMinimumCost
    , shrinkMockComputeSelectionLimit
    , unMockAssessTokenBundleSize
    , unMockComputeMinimumAdaQuantity
    , unMockComputeMinimumCost
    , unMockComputeSelectionLimit
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( inAscendingPartialOrder )
import Cardano.Wallet.CoinSelection.Internal.Balance
    ( AssetCount (..)
    , BalanceInsufficientError (..)
    , InsufficientMinCoinValueError (..)
    , MakeChangeCriteria (..)
    , PerformSelection
    , RunSelectionParams (..)
    , SelectionBalanceError (..)
    , SelectionConstraints (..)
    , SelectionLens (..)
    , SelectionLimit
    , SelectionLimitOf (..)
    , SelectionLimitReachedError (..)
    , SelectionParams
    , SelectionParamsOf (..)
    , SelectionResult
    , SelectionResultOf (..)
    , SelectionSkeleton (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    , addMintValueToChangeMaps
    , addMintValuesToChangeMaps
    , assetSelectionLens
    , assignCoinsToChangeMaps
    , balanceMissing
    , coinSelectionLens
    , collateNonUserSpecifiedAssetQuantities
    , computeDeficitInOut
    , computeUTxOBalanceAvailable
    , computeUTxOBalanceRequired
    , computeUTxOBalanceSufficiencyInfo
    , groupByKey
    , isUTxOBalanceSufficient
    , makeChange
    , makeChangeForCoin
    , makeChangeForNonUserSpecifiedAsset
    , makeChangeForNonUserSpecifiedAssets
    , makeChangeForUserSpecifiedAsset
    , mapMaybe
    , performSelection
    , performSelectionEmpty
    , reduceSelectionLimitBy
    , reduceTokenQuantities
    , removeBurnValueFromChangeMaps
    , removeBurnValuesFromChangeMaps
    , runRoundRobin
    , runSelection
    , runSelectionNonEmptyWith
    , runSelectionStep
    , selectionDeltaAllAssets
    , selectionHasValidSurplus
    , selectionMinimumCost
    , splitBundleIfAssetCountExcessive
    , splitBundlesWithExcessiveAssetCounts
    , splitBundlesWithExcessiveTokenQuantities
    , ungroupByKey
    )
import Cardano.Wallet.CoinSelection.Internal.Balance.Gen
    ( genSelectionLimit
    , genSelectionStrategy
    , shrinkSelectionLimit
    , shrinkSelectionStrategy
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( shrinkAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive
    , shrinkTokenBundleSmallRange
    , shrinkTokenBundleSmallRangePositive
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , genTokenMapSmallRange
    , shrinkAssetId
    , shrinkTokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenName )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityPositive, shrinkTokenQuantityPositive )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxIn (..)
    , TxOut (..)
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOut, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( SelectionFilter (..), UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, genUTxOIndexLarge, genUTxOIndexLargeN, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection, UTxOSelectionNonEmpty )
import Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection, shrinkUTxOSelection )
import Control.Arrow
    ( (&&&) )
import Control.Monad
    ( forM_, replicateM )
import Data.Bifunctor
    ( bimap, second )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, set, view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, isNothing, listToMaybe )
import Data.Set
    ( Set )
import Data.Tuple
    ( swap )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, pretty )
import Generics.SOP
    ( NP (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Safe
    ( tailMay )
import Test.Hspec
    ( Expectation, Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Fun
    , Gen
    , Negative (..)
    , Positive (..)
    , Property
    , applyFun
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , coarbitrary
    , conjoin
    , counterexample
    , cover
    , disjoin
    , elements
    , frequency
    , generate
    , genericShrink
    , ioProperty
    , label
    , oneof
    , property
    , shrinkList
    , shrinkMapBy
    , sized
    , suchThat
    , tabulate
    , withMaxSuccess
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Classes
    ( eqLaws, ordLaws )
import Test.QuickCheck.Extra
    ( genFunction, genericRoundRobinShrink, report, verify, (<:>), (<@>) )
import Test.QuickCheck.Monadic
    ( PropertyM (..), assert, monadicIO, monitor, run )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

-- TODO: ADP-1448:
--
-- Replace this type synonym with a type parameter on types that use it.
--
type InputId = (TxIn, Address)

-- TODO: ADP-1448:
--
-- Remove this instance once 'InputId' has been replaced with a type parameter.
--
instance Buildable (InputId, TokenBundle) where
    build ((i, a), b) = build i <> ":" <> build a <> ":" <> build (Flat b)

-- TODO: ADP-1448:
--
-- Remove this function once 'InputId' has been replaced with a type parameter.
--
coarbitraryInputId :: InputId -> Gen a -> Gen a
coarbitraryInputId = coarbitrary . show

-- TODO: ADP-1448:
--
-- Remove this function once 'InputId' has been replaced with a type parameter.
--
genInputIdFunction :: Gen a -> Gen (InputId -> a)
genInputIdFunction = genFunction coarbitraryInputId

spec :: Spec
spec = describe "Cardano.Wallet.CoinSelection.Internal.BalanceSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Coverage" $ do

        it "prop_Small_UTxOIndex_coverage" $
            property prop_Small_UTxOIndex_coverage
        it "prop_Large_UTxOIndex_coverage" $
            property prop_Large_UTxOIndex_coverage

    parallel $ describe "Class instances respect laws" $ do

        testLawsMany @(AssetCount TokenMap)
            [ eqLaws
            , ordLaws
            ]
        testLawsMany @SelectionLimit
            [ eqLaws
            , ordLaws
            ]

    parallel $ describe "Ordering of token maps" $ do

        it "prop_AssetCount_TokenMap_placesEmptyMapsFirst" $
            property prop_AssetCount_TokenMap_placesEmptyMapsFirst

    parallel $ describe "Performing a selection" $ do

        it "prop_performSelection_small" $
            property prop_performSelection_small
        it "prop_performSelection_large" $
            property prop_performSelection_large
        it "prop_performSelection_huge" $
            property prop_performSelection_huge

    parallel $ describe "Performing a selection with zero outputs" $ do

        it "prop_performSelectionEmpty" $
            property prop_performSelectionEmpty

    parallel $ describe "Running a selection (without making change)" $ do

        it "prop_runSelection_UTxO_empty" $
            property prop_runSelection_UTxO_empty
        it "prop_runSelection_UTxO_notEnough" $
            property prop_runSelection_UTxO_notEnough
        it "prop_runSelection_UTxO_exactlyEnough" $
            property prop_runSelection_UTxO_exactlyEnough
        it "prop_runSelection_UTxO_moreThanEnough" $
            property prop_runSelection_UTxO_moreThanEnough
        it "prop_runSelection_UTxO_muchMoreThanEnough" $
            property prop_runSelection_UTxO_muchMoreThanEnough

    parallel $ describe "Running a selection (non-empty)" $ do

        it "prop_runSelectionNonEmpty" $
            property prop_runSelectionNonEmpty

    parallel $ describe "Running a selection step" $ do

        it "prop_runSelectionStep_supplyExhausted" $
            prop_runSelectionStep_supplyExhausted
                & property
        it "prop_runSelectionStep_notYetEnoughToSatisfyMinimum" $
            prop_runSelectionStep_notYetEnoughToSatisfyMinimum
                & property
        it "prop_runSelectionStep_preciselyEnoughToSatisfyMinimum" $
            prop_runSelectionStep_preciselyEnoughToSatisfyMinimum
                & property
        it "prop_runSelectionStep_exceedsMinimalTarget" $
            prop_runSelectionStep_exceedsMinimalTarget
                & property
        it "prop_runSelectionStep_getsCloserToOptimalTargetButDoesNotExceedIt" $
            prop_runSelectionStep_getsCloserToOptimalTargetButDoesNotExceedIt
                & property
        it "prop_runSelectionStep_getsCloserToOptimalTargetAndExceedsIt" $
            prop_runSelectionStep_getsCloserToOptimalTargetAndExceedsIt
                & property
        it "prop_runSelectionStep_exceedsOptimalTargetAndGetsFurtherAway" $
            prop_runSelectionStep_exceedsOptimalTargetAndGetsFurtherAway
                & property

    parallel $ describe "Behaviour of selection lenses" $ do

        it "prop_assetSelectonLens_givesPriorityToSingletonAssets" $
            property prop_assetSelectionLens_givesPriorityToSingletonAssets
        it "prop_coinSelectonLens_givesPriorityToCoins" $
            property prop_coinSelectionLens_givesPriorityToCoins

    parallel $ describe "Boundary tests" $ do

        unit_testBoundaries "Large token quantities"
            boundaryTestMatrix_largeTokenQuantities
        unit_testBoundaries "Large asset counts"
            boundaryTestMatrix_largeAssetCounts
        unit_testBoundaries "Comparison of selection strategies"
            boundaryTestMatrix_selectionStrategies

    parallel $ describe "Making change" $ do

        it "prop_makeChange_identity" $
            property prop_makeChange_identity
        it "prop_makeChange_length" $
            property prop_makeChange_length
        it "prop_makeChange" $
            property prop_makeChange
        unitTests "makeChange"
            unit_makeChange

    parallel $ describe "Collating non-user specified asset quantities" $ do

        it "prop_collateNonUserSpecifiedAssetQuantities" $
            property prop_collateNonUserSpecifiedAssetQuantities
        describe "unit_collateNonUserSpecifiedAssetQuantities"
            unit_collateNonUserSpecifiedAssetQuantities

    parallel $ describe "assignCoinsToChangeMaps" $ do
        unitTests "assignCoinsToChangeMaps"
            unit_assignCoinsToChangeMaps

    parallel $ describe "Making change for coins" $ do

        it "prop_makeChangeForCoin_sum" $
            property prop_makeChangeForCoin_sum
        it "prop_makeChangeForCoin_length" $
            property prop_makeChangeForCoin_length
        unitTests "makeChangeForCoin"
            unit_makeChangeForCoin

    parallel $ describe "Making change for one non-user-specified asset" $ do

        it "prop_makeChangeForNonUserSpecifiedAsset_sum" $
            property prop_makeChangeForNonUserSpecifiedAsset_sum
        it "prop_makeChangeForNonUserSpecifiedAsset_order" $
            property prop_makeChangeForNonUserSpecifiedAsset_order
        it "prop_makeChangeForNonUserSpecifiedAsset_length" $
            property prop_makeChangeForNonUserSpecifiedAsset_length
        unitTests "makeChangeForNonUserSpecifiedAsset"
            unit_makeChangeForNonUserSpecifiedAsset

    parallel $ describe "Making change for many non-user-specified assets" $ do

        it "prop_makeChangeForNonUserSpecifiedAssets_length" $
            property prop_makeChangeForNonUserSpecifiedAssets_length
        it "prop_makeChangeForNonUserSpecifiedAssets_order" $
            property prop_makeChangeForNonUserSpecifiedAssets_order
        it "prop_makeChangeForNonUserSpecifiedAssets_sum" $
            property prop_makeChangeForNonUserSpecifiedAssets_sum
        describe "unit_makeChangeForNonUserSpecifiedAssets"
            unit_makeChangeForNonUserSpecifiedAssets

    parallel $ describe "Making change for user-specified assets" $ do

        it "prop_makeChangeForUserSpecifiedAsset_sum" $
            property prop_makeChangeForUserSpecifiedAsset_sum
        it "prop_makeChangeForUserSpecifiedAsset_length" $
            property prop_makeChangeForUserSpecifiedAsset_length
        unitTests "makeChangeForUserSpecifiedAsset"
            unit_makeChangeForUserSpecifiedAsset

    parallel $ describe "Splitting bundles with excessive asset counts" $ do

        it "prop_splitBundleIfAssetCountExcessive_length" $
            property prop_splitBundleIfAssetCountExcessive_length
        it "prop_splitBundleIfAssetCountExcessive_maximalSplitting" $
            property prop_splitBundleIfAssetCountExcessive_maximalSplitting
        it "prop_splitBundleIfAssetCountExcessive_postCondition" $
            property prop_splitBundleIfAssetCountExcessive_postCondition
        it "prop_splitBundleIfAssetCountExcessive_sum" $
            property prop_splitBundleIfAssetCountExcessive_sum
        it "prop_splitBundlesWithExcessiveAssetCounts_length" $
            property prop_splitBundlesWithExcessiveAssetCounts_length
        it "prop_splitBundlesWithExcessiveAssetCounts_sum" $
            property prop_splitBundlesWithExcessiveAssetCounts_sum

    parallel $ describe "Splitting bundles with excessive token quantities" $ do

        it "prop_splitBundlesWithExcessiveTokenQuantities_length" $
            property prop_splitBundlesWithExcessiveTokenQuantities_length
        it "prop_splitBundlesWithExcessiveTokenQuantities_sum" $
            property prop_splitBundlesWithExcessiveTokenQuantities_sum

    parallel $ describe "Grouping and ungrouping" $ do

        it "prop_groupByKey_ungroupByKey" $
            property $ prop_groupByKey_ungroupByKey @Int @Int
        it "prop_ungroupByKey_groupByKey" $
            property $ prop_ungroupByKey_groupByKey @Int @Int

    parallel $ describe "Round-robin processing" $ do

        it "prop_runRoundRobin_identity" $
            property $ prop_runRoundRobin_identity @Int
        it "prop_runRoundRobin_iterationCount" $
            property $ prop_runRoundRobin_iterationCount @TokenName @Word8
        it "prop_runRoundRobin_iterationOrder" $
            property $ prop_runRoundRobin_iterationOrder @TokenName @Word8
        it "prop_runRoundRobin_generationCount" $
            property $ prop_runRoundRobin_generationCount @TokenName @Word8
        it "prop_runRoundRobin_generationOrder" $
            property $ prop_runRoundRobin_generationOrder @TokenName @Word8

    parallel $ describe "Selection limits" $ do

        it "prop_reduceSelectionLimitBy_lessThanOrEqual" $
            property prop_reduceSelectionLimitBy_lessThanOrEqual
        it "prop_reduceSelectionLimitBy_reductionNegative" $
            property prop_reduceSelectionLimitBy_reductionNegative
        it "prop_reduceSelectionLimitBy_reductionZero" $
            property prop_reduceSelectionLimitBy_reductionZero
        it "prop_reduceSelectionLimitBy_reductionPositive" $
            property prop_reduceSelectionLimitBy_reductionPositive

    parallel $ describe "Utility functions" $ do

        it "prop_mapMaybe_oracle" $
            property prop_mapMaybe_oracle

    parallel $ describe "Minting and burning values from the change maps" $ do

        it "prop_addMintValueToChangeMaps_value" $
            property prop_addMintValueToChangeMaps_value
        it "prop_addMintValueToChangeMaps_length" $
            property prop_addMintValueToChangeMaps_length
        it "prop_addMintValueToChangeMaps_order" $
            property prop_addMintValueToChangeMaps_order
        it "prop_addMintValuesToChangeMaps" $
            property prop_addMintValuesToChangeMaps

        it "prop_removeBurnValueFromChangeMaps_value" $
            property prop_removeBurnValueFromChangeMaps_value
        it "prop_removeBurnValueFromChangeMaps_length" $
            property prop_removeBurnValueFromChangeMaps_length
        it "prop_removeBurnValueFromChangeMaps_order" $
            property prop_removeBurnValueFromChangeMaps_order
        it "prop_removeBurnValuesFromChangeMaps" $
            property prop_removeBurnValuesFromChangeMaps

        it "prop_reduceTokenQuantities_value" $
            property prop_reduceTokenQuantities_value
        it "prop_reduceTokenQuantities_length" $
            property prop_reduceTokenQuantities_length
        it "prop_reduceTokenQuantities_order" $
            property prop_reduceTokenQuantities_order

--------------------------------------------------------------------------------
-- Coverage
--------------------------------------------------------------------------------

prop_Small_UTxOIndex_coverage :: Small (UTxOIndex InputId) -> Property
prop_Small_UTxOIndex_coverage (Small index) =
    checkCoverage $ property
        -- Asset counts:
        $ cover 1 (assetCount == 0)
            "asset count = 0"
        $ cover 80 (assetCount > 0)
            "asset count > 0"
        $ cover 40 (assetCount > 8)
            "asset count > 8"
        -- Entry counts:
        $ cover 1 (entryCount == 0)
            "UTxO set size = 0 entries"
        $ cover 40 (entryCount > 16)
            "UTxO set size > 16 entries"
        $ cover 10 (entryCount > 32)
            "UTxO set size > 32 entries"
        True
  where
    assetCount = Set.size $ UTxOIndex.assets index
    entryCount = UTxOIndex.size index

prop_Large_UTxOIndex_coverage :: Large (UTxOIndex InputId) -> Property
prop_Large_UTxOIndex_coverage (Large index) =
    -- Generation of large UTxO sets takes longer, so limit the number of runs:
    withMaxSuccess 100 $ checkCoverage $ property
        -- Asset counts:
        $ cover 80 (assetCount > 8)
            "asset count > 8"
        -- Entry counts:
        $ cover 80 (entryCount >= 1024)
            "UTxO set size >= 1024 entries"
        $ cover 20 (entryCount >= 2048)
            "UTxO set size >= 2048 entries"
        $ cover 10 (entryCount >= 3072)
            "UTxO set size >= 3072 entries"
        True
  where
    assetCount = Set.size $ UTxOIndex.assets index
    entryCount = UTxOIndex.size index

--------------------------------------------------------------------------------
-- Ordering of token maps
--------------------------------------------------------------------------------

prop_AssetCount_TokenMap_placesEmptyMapsFirst
    :: NonEmpty TokenMap
    -> Property
prop_AssetCount_TokenMap_placesEmptyMapsFirst maps =
    checkCoverage
        -- Check counts of empty maps and non-empty maps:
        $ cover 80 (emptyMapCount >= 1 && nonEmptyMapCount >= 1)
            "empty map count >= 1 && non-empty map count >= 1"
        $ cover 60 (emptyMapCount >= 2 && nonEmptyMapCount >= 2)
            "empty map count >= 2 && non-empty map count >= 2"
        $ cover 40 (emptyMapCount >= 4 && nonEmptyMapCount >= 4)
            "empty map count >= 4 && non-empty map count >= 4"
        $ cover 20 (emptyMapCount >= 8 && nonEmptyMapCount >= 8)
            "empty map count >= 8 && non-empty map count >= 8"
        -- Check head and last element of list:
        $ cover 20 (isEmptyMap $ NE.head maps)
            "head element is empty map"
        $ cover 40 (not $ isEmptyMap $ NE.head maps)
            "head element is non-empty map"
        $ cover 20 (isEmptyMap $ NE.last maps)
            "last element is empty map"
        $ cover 40 (not $ isEmptyMap $ NE.last maps)
            "last element is non-empty map"
        prop
  where
    prop = (===)
        ( NE.span isEmptyMap $ NE.sortWith AssetCount maps )
        ( L.sortOn AssetCount emptyMaps
        , L.sortOn AssetCount nonEmptyMaps
        )

    isEmptyMap = TokenMap.isEmpty
    (emptyMaps, nonEmptyMaps) = NE.partition isEmptyMap maps
    (emptyMapCount, nonEmptyMapCount) = (length emptyMaps, length nonEmptyMaps)

--------------------------------------------------------------------------------
-- Performing a selection
--------------------------------------------------------------------------------

-- | The result of calling 'performSelection'.
--
-- We define this type alias to shorten type signatures.
--
type PerformSelectionResult = Either
    (SelectionBalanceError Address InputId)
    (SelectionResult Address InputId)

genSelectionParams
    :: Gen (InputId -> Bool)
    -> Gen (UTxOIndex InputId)
    -> Gen (SelectionParams Address InputId)
genSelectionParams genPreselectedInputs genUTxOIndex' = do
    utxoAvailable <- genUTxOIndex'
    isInputPreselected <- oneof
        [ genPreselectedInputs
        , genPreselectedInputsNone
        ]
    outputCount <- elements
        [0, 1, max 2 $ UTxOIndex.size utxoAvailable `div` 8]
    outputsToCover <-
        replicateM outputCount ((view #address &&& view #tokens) <$> genTxOut)
    extraCoinSource <-
        oneof [pure $ Coin 0, genCoinPositive]
    extraCoinSink <-
        oneof [pure $ Coin 0, genCoinPositive]
    (assetsToMint, assetsToBurn) <- genAssetsToMintAndBurn utxoAvailable
    selectionStrategy <- genSelectionStrategy
    pure $ SelectionParams
        { outputsToCover
        , utxoAvailable =
            UTxOSelection.fromIndexFiltered isInputPreselected utxoAvailable
        , extraCoinSource
        , extraCoinSink
        , assetsToMint
        , assetsToBurn
        , selectionStrategy
        }
  where
    genAssetsToMintAndBurn :: UTxOIndex InputId -> Gen (TokenMap, TokenMap)
    genAssetsToMintAndBurn utxoAvailable = do
        assetsToMint <- genTokenMapSmallRange
        let assetsToBurn = adjustAllTokenMapQuantities
                (`div` 2)
                (utxoAvailableAssets <> assetsToMint)
        pure (assetsToMint, assetsToBurn)
      where
        utxoAvailableAssets :: TokenMap
        utxoAvailableAssets = view (#balance . #tokens) utxoAvailable

    genPreselectedInputsNone :: Gen (InputId -> Bool)
    genPreselectedInputsNone = pure $ const False

shrinkSelectionParams
    :: SelectionParams Address InputId
    -> [SelectionParams Address InputId]
shrinkSelectionParams = genericRoundRobinShrink
    <@> shrinkList shrinkOutput
    <:> shrinkUTxOSelection
    <:> shrinkCoin
    <:> shrinkCoin
    <:> shrinkTokenMap
    <:> shrinkTokenMap
    <:> shrinkSelectionStrategy
    <:> Nil
  where
    shrinkOutput = genericRoundRobinShrink
        <@> shrinkAddress
        <:> (filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)
        <:> Nil
      where
        tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
        tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

prop_performSelection_small
    :: MockSelectionConstraints
    -> Blind (Small (SelectionParams Address InputId))
    -> Property
prop_performSelection_small mockConstraints (Blind (Small params)) =
    checkCoverage $

    -- Inspect the balance:
    cover 20 (isUTxOBalanceSufficient params)
        "balance sufficient" $
    cover 25 (not $ isUTxOBalanceSufficient params)
        "balance insufficient" $

    -- Inspect the UTxO and user-specified outputs:
    cover 5 (utxoHasAtLeastOneAsset)
        "UTxO has at least one asset" $
    cover 5 (not outputsHaveAtLeastOneAsset)
        "No assets to cover" $
    cover 2 (outputsHaveAtLeastOneAsset && not utxoHasAtLeastOneAsset)
        "Assets to cover, but no assets in UTxO" $
    cover 10 (null (view #outputsToCover params))
        "Outputs to cover = 0" $
    cover 10 (length (view #outputsToCover params) == 1)
        "Outputs to cover = 1" $
    cover 10 (length (view #outputsToCover params) > 1)
        "Outputs to cover > 1" $

    cover 10 (UTxOSelection.selectedSize (view #utxoAvailable params) == 0)
        "Number of inputs preselected == 0" $
    cover 2 (UTxOSelection.selectedSize (view #utxoAvailable params) == 1)
        "Number of inputs preselected == 1" $
    cover 10 (UTxOSelection.selectedSize (view #utxoAvailable params) >= 2)
        "Number of inputs preselected >= 2" $

    -- Inspect the selection strategy:
    cover 20 (view #selectionStrategy params == SelectionStrategyMinimal)
        "Selection strategy: minimal" $
    cover 20 (view #selectionStrategy params == SelectionStrategyOptimal)
        "Selection strategy: optimal" $

    -- Inspect the extra coin source and sink:
    let nonZeroExtraCoinSource =
            Coin 0 < (params & view #extraCoinSource)
        nonZeroExtraCoinSink =
            Coin 0 < (params & view #extraCoinSink)
    in
    cover 20
        (nonZeroExtraCoinSource && nonZeroExtraCoinSink)
        "nonZeroExtraCoinSource && nonZeroExtraCoinSink" $
    cover 20
        (not nonZeroExtraCoinSource && nonZeroExtraCoinSink)
        "not nonZeroExtraCoinSource && nonZeroExtraCoinSink" $
    cover 20
        (nonZeroExtraCoinSource && not nonZeroExtraCoinSink)
        "nonZeroExtraCoinSource && not nonZeroExtraCoinSink" $
    cover 20
        (not nonZeroExtraCoinSource && not nonZeroExtraCoinSink)
        "not nonZeroExtraCoinSource && not nonZeroExtraCoinSink" $

    -- Inspect the sets of minted and burned assets:
    cover 20 (view #assetsToMint params /= TokenMap.empty)
        "Have some assets to mint" $
    cover 20 (view #assetsToBurn params /= TokenMap.empty)
        "Have some assets to burn" $
    cover 2 (view #assetsToMint params == TokenMap.empty)
        "Have no assets to mint" $
    cover 2 (view #assetsToBurn params == TokenMap.empty)
        "Have no assets to burn" $

    -- Inspect the intersection between minted assets and burned assets:
    cover 2 (someAssetsAreBothMintedAndBurned)
        "Some assets are both minted and burned" $
    cover 2 (noAssetsAreBothMintedAndBurned)
        "No assets are both minted and burned" $

    -- Inspect the intersection between minted assets and spent assets:
    cover 2 (someAssetsAreBothMintedAndSpent)
        "Some assets are both minted and spent" $
    cover 2 (noAssetsAreBothMintedAndSpent)
        "No assets are both minted and spent" $

    -- Inspect the intersection between spent assets and burned assets:
    cover 2 (someAssetsAreBothSpentAndBurned)
        "Some assets are both spent and burned" $
    cover 2 (noAssetsAreBothSpentAndBurned)
        "No assets are both spent and burned" $

    -- Inspect the relationship between minted, burned, and spent assets:
    cover 2 (allMintedAssetsEitherBurnedOrSpent)
        "All minted assets were either spent or burned" $
    cover 2 (not allMintedAssetsEitherBurnedOrSpent)
        "Some minted assets were neither spent nor burned" $

    prop_performSelection mockConstraints params $ \result ->
        cover 10 (selectionUnlimited && selectionSufficient result)
            "selection unlimited and sufficient"
        . cover 2 (selectionLimited && selectionSufficient result)
            "selection limited but sufficient"
        . cover 2 (selectionLimited && selectionInsufficient result)
            "selection limited and insufficient"
  where
    utxoHasAtLeastOneAsset = not
        . Set.null
        . TokenBundle.getAssets
        $ computeUTxOBalanceAvailable params

    outputsHaveAtLeastOneAsset =
        not . Set.null $ TokenBundle.getAssets outputTokens
      where
        outputTokens = mconcat
            . F.toList
            . fmap snd
            $ view #outputsToCover params

    constraints :: SelectionConstraints Address
    constraints = unMockSelectionConstraints mockConstraints

    selectionLimit :: SelectionLimit
    selectionLimit = view #computeSelectionLimit constraints
        $ F.toList
        $ view #outputsToCover params

    selectionLimited :: Bool
    selectionLimited = case selectionLimit of
        MaximumInputLimit _ -> True
        NoLimit -> False

    selectionUnlimited :: Bool
    selectionUnlimited = not selectionLimited

    selectionSufficient :: PerformSelectionResult -> Bool
    selectionSufficient = \case
        Right _ -> True
        _ -> False

    selectionInsufficient :: PerformSelectionResult -> Bool
    selectionInsufficient = \case
        Left (SelectionLimitReached _) -> True
        _ -> False

    assetsSpentByUserSpecifiedOutputs :: TokenMap
    assetsSpentByUserSpecifiedOutputs =
        F.foldMap (view #tokens . snd) (view #outputsToCover params)

    allMintedAssetsEitherBurnedOrSpent :: Bool
    allMintedAssetsEitherBurnedOrSpent =
        view #assetsToMint params `leq` TokenMap.add
            (view #assetsToBurn params)
            (assetsSpentByUserSpecifiedOutputs)

    someAssetsAreBothMintedAndBurned :: Bool
    someAssetsAreBothMintedAndBurned
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (view #assetsToMint params)
            (view #assetsToBurn params)

    someAssetsAreBothMintedAndSpent :: Bool
    someAssetsAreBothMintedAndSpent
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (view #assetsToMint params)
            (assetsSpentByUserSpecifiedOutputs)

    someAssetsAreBothSpentAndBurned :: Bool
    someAssetsAreBothSpentAndBurned
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (assetsSpentByUserSpecifiedOutputs)
            (view #assetsToBurn params)

    noAssetsAreBothMintedAndBurned :: Bool
    noAssetsAreBothMintedAndBurned = not someAssetsAreBothMintedAndBurned

    noAssetsAreBothMintedAndSpent :: Bool
    noAssetsAreBothMintedAndSpent = not someAssetsAreBothMintedAndSpent

    noAssetsAreBothSpentAndBurned :: Bool
    noAssetsAreBothSpentAndBurned = not someAssetsAreBothSpentAndBurned

prop_performSelection_large
    :: MockSelectionConstraints
    -> Blind (Large (SelectionParams Address InputId))
    -> Property
prop_performSelection_large mockConstraints (Blind (Large params)) =
    -- Generation of large UTxO sets takes longer, so limit the number of runs:
    withMaxSuccess 100 $
    checkCoverage $
    cover 50 (isUTxOBalanceSufficient params)
        "UTxO balance sufficient" $
    prop_performSelection mockConstraints params (const id)

prop_performSelection_huge :: Property
prop_performSelection_huge = ioProperty $
    -- The UTxO index is generated outside of the property in order to avoid
    -- the cost of re-generating it on every pass. This will still generate
    -- interesting cases, since selection within that large index is random.
    property . prop_performSelection_huge_inner
        <$> generate (genUTxOIndexLargeN 50000)

prop_performSelection_huge_inner
    :: UTxOIndex InputId
    -> MockSelectionConstraints
    -> Large (SelectionParams Address InputId)
    -> Property
prop_performSelection_huge_inner utxoAvailable mockConstraints (Large params) =
    withMaxSuccess 5 $
    prop_performSelection mockConstraints params' (const id)
  where
    params' = params & set #utxoAvailable
        (UTxOSelection.fromIndex utxoAvailable)

prop_performSelection
    :: MockSelectionConstraints
    -> SelectionParams Address InputId
    -> (PerformSelectionResult -> Property -> Property)
    -> Property
prop_performSelection mockConstraints params coverage =
    report extraCoinSource
        "extraCoinSource" $
    report extraCoinSink
        "extraCoinSink" $
    report selectionLimit
        "selectionLimit" $
    report assetsToMint
        "assetsToMint" $
    report assetsToBurn
        "assetsToBurn" $
    monadicIO $ do
        result <- run $ performSelection constraints params
        monitor (coverage result)
        pure $ either onFailure onSuccess result
  where
    constraints :: SelectionConstraints Address
    constraints = unMockSelectionConstraints mockConstraints

    SelectionParams
        { outputsToCover
        , extraCoinSource
        , extraCoinSink
        , assetsToMint
        , assetsToBurn
        } = params

    onSuccess :: SelectionResultOf [] Address InputId -> Property
    onSuccess result =
        counterexample "onSuccess" $
        report
            (utxoBalanceAvailable)
            "available UTXO balance" $
        report
            (utxoBalanceRequired)
            "required UTXO balance" $
        report
            (F.fold $ view #changeGenerated result)
            "change balance" $
        report
            (selectionDeltaAllAssets result)
            "actual delta" $
        report
            (selectionMinimumCost constraints result)
            "minimum cost" $
        report
            (length $ view #outputsCovered result)
            "number of outputs" $
        report
            (length $ view #changeGenerated result)
            "number of change outputs" $
        verify
            (isUTxOBalanceSufficient params)
            "isUTxOBalanceSufficient params" $
        verify
            (selectionHasValidSurplus constraints result)
            "selectionHasValidSurplus constraints result" $
        verify
            (initialSelectionIsSubsetOfFinalSelection)
            "initialSelectionIsSubsetOfFinalSelection" $
        verify
            (view #outputsCovered result == view #outputsToCover params)
            "view #outputsCovered result == view #outputsToCover params" $
        verify
            (view #assetsToMint result == view #assetsToMint params)
            "view #assetsToMint result == view #assetsToMint params" $
        verify
            (view #assetsToBurn result == view #assetsToBurn params)
            "view #assetsToBurn result == view #assetsToBurn params" $
        verify
            (view #extraCoinSource result == view #extraCoinSource params)
            "view #extraCoinSource result == view #extraCoinSource params" $
        verify
            (view #extraCoinSink result == view #extraCoinSink params)
            "view #extraCoinSink result == view #extraCoinSink params" $
        case selectionLimit of
            MaximumInputLimit limit ->
                verify
                    (NE.length (view #inputsSelected result) <= limit)
                    "NE.length (view #inputsSelected result) <= limit" $
                    property True
            NoLimit ->
                property True
      where
        initialSelectionIsSubsetOfFinalSelection :: Bool
        initialSelectionIsSubsetOfFinalSelection =
            view #utxoAvailable params
            `UTxOSelection.isSubSelectionOf`
            UTxOSelection.selectMany
                (view #inputsSelected result <&> fst)
                (view #utxoAvailable params)

    onFailure :: SelectionBalanceError Address InputId -> Property
    onFailure = \case
        BalanceInsufficient e ->
            onBalanceInsufficient e
        SelectionLimitReached e ->
            onSelectionLimitReached e
        InsufficientMinCoinValues es ->
            onInsufficientMinCoinValues es
        UnableToConstructChange e ->
            onUnableToConstructChange e
        EmptyUTxO ->
            onEmptyUTxO

    onBalanceInsufficient :: BalanceInsufficientError -> Property
    onBalanceInsufficient e =
        counterexample "onBalanceInsufficient" $
        report utxoBalanceAvailable
            "available balance" $
        report utxoBalanceRequired
            "required balance" $
        report (balanceMissing e)
            "missing balance" $
        verify
            (not $ isUTxOBalanceSufficient params)
            "not $ isUTxOBalanceSufficient params" $
        verify
            (utxoBalanceAvailable == errorBalanceAvailable)
            "utxoBalanceAvailable == errorBalanceAvailable" $
        verify
            (utxoBalanceRequired == errorBalanceRequired)
            "utxoBalanceRequired == errorBalanceRequired" $
        verify
            (balanceMissing e == view #difference utxoBalanceSufficiencyInfo)
            "balanceMissing e == view #difference utxoBalanceSufficiencyInfo" $
        property True
      where
        BalanceInsufficientError errorBalanceAvailable errorBalanceRequired = e

    onSelectionLimitReached
        :: SelectionLimitReachedError Address InputId -> Property
    onSelectionLimitReached e =
        counterexample "onSelectionLimitReached" $
        report errorBalanceRequired
            "required balance" $
        report errorBalanceSelected
            "selected balance" $
        verify
            (selectionLimit <= MaximumInputLimit (length errorInputsSelected))
            "selectionLimit <= MaximumInputLimit (length errorInputsSelected)" $
        verify
            (utxoBalanceRequired == errorBalanceRequired)
            "utxoBalanceRequired == errorBalanceRequired" $
        verify
            (view #utxoAvailable params /= UTxOSelection.empty)
            "view #utxoAvailable params /= UTxOSelection.empty" $
        property True
      where
        SelectionLimitReachedError
            errorBalanceRequired errorInputsSelected _ = e
        errorBalanceSelected =
            F.foldMap (view #tokens . snd) errorInputsSelected

    onInsufficientMinCoinValues
        :: NonEmpty (InsufficientMinCoinValueError Address) -> Property
    onInsufficientMinCoinValues es =
        counterexample "onInsufficientMinCoinValues" $
        report es
            "error values" $
        report
            (NE.zip (expectedMinCoinValue <$> es) (actualMinCoinValue <$> es))
            "(expected, actual) pairs" $
        verify
            (all (\e -> expectedMinCoinValue e > actualMinCoinValue e) es)
            "all (λe -> expectedMinCoinValue e > actualMinCoinValue e) es" $
        property True
      where
        actualMinCoinValue
            = view #coin . snd . outputWithInsufficientAda

    onUnableToConstructChange :: UnableToConstructChangeError -> Property
    onUnableToConstructChange e =
        counterexample "onUnableToConstructChange" $
        counterexample (show e) $
        verify
            (shortfall e > Coin 0)
            "shortfall e > Coin 0" $

        -- We expect that this error is caused by one or more of the following
        -- conditions:
        --
        --    1.  There's not enough ada available to pay for the fee;
        --    2.  There's not enough ada available to pay for the minimum ada
        --        quantities of all change outputs;
        --    3.  One or more of the generated change bundles are in excess of
        --        the maximum token bundle size, so it's necessary to break
        --        them up, but there isn't enough ada to pay for either the fee
        --        or the minimum ada quantities of the broken-up outputs.
        --    4.  The input selection limit has been reached.
        --
        -- So to test that our expectation is really true, we run the selection
        -- again with modified constraints that:
        --
        --    1.  Require no fee.
        --    2.  Require no minimum ada quantity.
        --    3.  Impose no maximum token bundle size.
        --    4.  Impose no selection limit.
        --
        -- We expect that the selection should succeed.
        --
        let constraints' :: SelectionConstraints Address = constraints
                { assessTokenBundleSize = unMockAssessTokenBundleSize
                    MockAssessTokenBundleSizeUnlimited
                , computeMinimumAdaQuantity = computeMinimumAdaQuantityZero
                , computeMinimumCost = computeMinimumCostZero
                , computeSelectionLimit = const NoLimit
                }
            performSelection' = performSelection constraints' params
        in
        monadicIO $ run performSelection' >>= \case
            Left e' -> do
                monitor $ counterexample $ unlines
                    [ "Failed to re-run selection with relaxed constraints."
                    , show e'
                    ]
                assert False
            Right{} -> do
                assert True

    onEmptyUTxO :: Property
    onEmptyUTxO =
        counterexample "onEmptyUTxO" $
        verify
            (view #utxoAvailable params == UTxOSelection.empty)
            "view #utxoAvailable params == UTxOSelection.empty" $
        property True

    selectionLimit = view #computeSelectionLimit constraints $
        F.toList outputsToCover
    utxoBalanceAvailable = computeUTxOBalanceAvailable params
    utxoBalanceRequired = computeUTxOBalanceRequired params
    utxoBalanceSufficiencyInfo = computeUTxOBalanceSufficiencyInfo params

--------------------------------------------------------------------------------
-- Performing a selection with an empty output list
--------------------------------------------------------------------------------

-- | Verifies that 'performSelectionEmpty' performs a valid transformation
--   on 'performSelectionNonEmpty'.
--
-- Both the parameters and the result are verified.
--
prop_performSelectionEmpty
    :: MockSelectionConstraints
    -> Small (SelectionParams Address InputId)
    -> Property
prop_performSelectionEmpty mockConstraints (Small params) =
    checkCoverage $
    cover 10 (null (view #outputsToCover params))
        "number of outputs = 0" $
    cover 10 (not $ null (view #outputsToCover params))
        "number of outputs > 0" $
    cover 20 (isUTxOBalanceSufficient params)
        "UTxO balance is sufficient" $
    conjoin
        [ prop_conservation
        , prop_transformation
        ]
  where
    -- Checks that functions on the parameters and the result are conserved.
    prop_conservation = conjoin
        [ prop_computeUTxOBalanceSufficiencyInfo
        , prop_computeDeficitInOut
        , prop_selectionDeltaAllAssets
        , prop_selectionHasValidSurplus
        ]
      where
        prop_computeUTxOBalanceSufficiencyInfo =
            counterexample "computeUTxOBalanceSufficiencyInfo" $
            computeUTxOBalanceSufficiencyInfo params ===
            computeUTxOBalanceSufficiencyInfo paramsTransformed

        prop_computeDeficitInOut =
            counterexample "computeDeficitInOut" $
            computeDeficitInOut params ===
            computeDeficitInOut paramsTransformed

        prop_selectionDeltaAllAssets =
            counterexample "selectionDeltaAllAssets" $
            selectionDeltaAllAssets result ===
            selectionDeltaAllAssets resultTransformed

        prop_selectionHasValidSurplus =
            counterexample "selectionHasValidSurplus" $
            selectionHasValidSurplus constraints result .&&.
            selectionHasValidSurplus constraints resultTransformed

    -- Checks that the transformation is correct.
    prop_transformation =
        counterexample "transformation correct" $
        conjoin $ if null (view #outputsToCover params)
        then
            [ length (view #outputsToCover paramsTransformed) === 1
            , length (view #outputsCovered resultTransformed) === 0
            ]
        else
            -- If the initial list of outputs is non-empty, then no
            -- transformation should take place:
            [ params === (paramsTransformed & over #outputsToCover F.toList)
            , resultTransformed === (result & over #outputsCovered F.toList)
            ]

    constraints :: SelectionConstraints Address
    constraints = unMockSelectionConstraints mockConstraints

    paramsTransformed :: SelectionParamsOf NonEmpty Address InputId
    paramsTransformed = view #paramsTransformed transformationReport

    result :: SelectionResultOf NonEmpty Address InputId
    result = expectRight $ view #result transformationReport

    resultTransformed :: SelectionResultOf [] Address InputId
    resultTransformed =
        expectRight $ view #resultTransformed transformationReport

    -- Provides a report of how 'performSelectionEmpty' has transformed
    -- both the parameters and result of 'mockPerformSelectionNonEmpty'.
    --
    transformationReport = performSelectionEmpty f constraints params
      where
        f constraints' params' = withTransformationReport params'
            $ runIdentity
            $ mockPerformSelectionNonEmpty constraints' params'

-- | Provides a report of how function 'f' transforms function 'g', where
--   function 'f' modifies both the parameters and the result of 'g'.
--
data TransformationReport paramsTransformed result resultTransformed =
    TransformationReport
        { paramsTransformed :: paramsTransformed
            -- ^ The transformed parameters.
        , result :: result
            -- ^ The untransformed result.
        , resultTransformed :: resultTransformed
            -- ^ The transformed result.
        }
    deriving (Functor, Generic)

-- | Constructs a function transformation report.
--
-- Both results are initially untransformed. The 'Functor' instance allows the
-- final result to be transformed by the function transformer.
--
withTransformationReport
    :: params -> result -> TransformationReport params result result
withTransformationReport p r = TransformationReport p r r

-- | A mock implementation of 'performSelectionNonEmpty' that always succeeds.
--
-- This function always returns a balanced selection that covers the minimum
-- cost exactly, by creating:
--
--    - a single input to cover the cost and input deficit.
--    - a single change output to cover the output deficit.
--
mockPerformSelectionNonEmpty
    :: PerformSelection Identity NonEmpty Address InputId
mockPerformSelectionNonEmpty constraints params = Identity $ Right result
  where
    result :: SelectionResultOf NonEmpty Address InputId
    result = resultWithoutDelta & set #inputsSelected
        (makeInputsOfValue $ deficitIn <> TokenBundle.fromCoin minimumCost)
      where
        minimumCost :: Coin
        minimumCost = selectionMinimumCost constraints resultWithoutDelta

    resultWithoutDelta :: SelectionResultOf NonEmpty Address InputId
    resultWithoutDelta = SelectionResult
        { inputsSelected = makeInputsOfValue deficitIn
        , changeGenerated = makeChangeOfValue deficitOut
        , assetsToBurn = view #assetsToBurn params
        , assetsToMint = view #assetsToMint params
        , extraCoinSink = view #extraCoinSink params
        , extraCoinSource = view #extraCoinSource params
        , outputsCovered = view #outputsToCover params
        }

    makeInputsOfValue :: TokenBundle -> NonEmpty (InputId, TokenBundle)
    makeInputsOfValue v = ((TxIn (Hash "") 0, Address ""), v) :| []

    makeChangeOfValue :: TokenBundle -> [TokenBundle]
    makeChangeOfValue v = [v]

    deficitIn, deficitOut :: TokenBundle
    (deficitIn, deficitOut) = computeDeficitInOut params

--------------------------------------------------------------------------------
-- Running a selection (without making change)
--------------------------------------------------------------------------------

prop_runSelection_UTxO_empty :: TokenBundle -> SelectionStrategy -> Property
prop_runSelection_UTxO_empty balanceRequested strategy = monadicIO $ do
    result <- run $ runSelection @_ @InputId
        RunSelectionParams
            { selectionLimit = NoLimit
            , utxoAvailable
            , minimumBalance = balanceRequested
            , selectionStrategy = strategy
            }
    let balanceSelected = UTxOSelection.selectedBalance result
    let balanceLeftover = UTxOSelection.leftoverBalance result
    assertWith
        "utxoAvailable `UTxOSelection.isSubSelectionOf` result"
        (utxoAvailable `UTxOSelection.isSubSelectionOf` result)
    assertWith
        "balanceSelected == TokenBundle.empty"
        (balanceSelected == TokenBundle.empty)
    assertWith
        "balanceLeftover == TokenBundle.empty"
        (balanceLeftover == TokenBundle.empty)
  where
    utxoAvailable = UTxOSelection.fromIndex UTxOIndex.empty

prop_runSelection_UTxO_notEnough
    :: UTxOSelection InputId -> SelectionStrategy -> Property
prop_runSelection_UTxO_notEnough utxoAvailable strategy = monadicIO $ do
    result <- run $ runSelection
        RunSelectionParams
            { selectionLimit = NoLimit
            , utxoAvailable
            , minimumBalance = balanceRequested
            , selectionStrategy = strategy
            }
    let balanceSelected = UTxOSelection.selectedBalance result
    let balanceLeftover = UTxOSelection.leftoverBalance result
    assertWith
        "utxoAvailable `UTxOSelection.isSubSelectionOf` result"
        (utxoAvailable `UTxOSelection.isSubSelectionOf` result)
    assertWith
        "balanceSelected == balanceAvailable"
        (balanceSelected == balanceAvailable)
    assertWith
        "balanceLeftover == TokenBundle.empty"
        (balanceLeftover == TokenBundle.empty)
  where
    balanceAvailable = UTxOSelection.availableBalance utxoAvailable
    balanceRequested = adjustAllTokenBundleQuantities (* 2) balanceAvailable

prop_runSelection_UTxO_exactlyEnough
    :: UTxOSelection InputId -> SelectionStrategy -> Property
prop_runSelection_UTxO_exactlyEnough utxoAvailable strategy = monadicIO $ do
    result <- run $ runSelection
        RunSelectionParams
            { selectionLimit = NoLimit
            , utxoAvailable
            , minimumBalance = balanceRequested
            , selectionStrategy = strategy
            }
    let balanceSelected = UTxOSelection.selectedBalance result
    let balanceLeftover = UTxOSelection.leftoverBalance result
    assertWith
        "utxoAvailable `UTxOSelection.isSubSelectionOf` result"
        (utxoAvailable `UTxOSelection.isSubSelectionOf` result)
    assertWith
        "balanceLeftover == TokenBundle.empty"
        (balanceLeftover == TokenBundle.empty)
    if utxoAvailable == UTxOSelection.empty then
        assertWith
            "balanceSelected == TokenBundle.empty"
            (balanceSelected == TokenBundle.empty)
    else
        assertWith
            "balanceSelected == balanceRequested"
            (balanceSelected == balanceRequested)
  where
    balanceRequested = UTxOSelection.availableBalance utxoAvailable

prop_runSelection_UTxO_moreThanEnough
    :: UTxOSelection InputId -> SelectionStrategy -> Property
prop_runSelection_UTxO_moreThanEnough utxoAvailable strategy = monadicIO $ do
    result <- run $ runSelection
        RunSelectionParams
            { selectionLimit = NoLimit
            , utxoAvailable
            , minimumBalance = balanceRequested
            , selectionStrategy = strategy
            }
    let balanceSelected = UTxOSelection.selectedBalance result
    let balanceLeftover = UTxOSelection.leftoverBalance result
    monitor $ cover 80
        (assetsRequested `Set.isProperSubsetOf` assetsAvailable)
        "assetsRequested ⊂ assetsAvailable"
    monitor $ cover 50 (Set.size assetsRequested >= 4)
        "size assetsRequested >= 4"
    monitor $ counterexample $ unlines
        [ "balance available:"
        , pretty (Flat balanceAvailable)
        , "balance requested:"
        , pretty (Flat balanceRequested)
        , "balance selected:"
        , pretty (Flat balanceSelected)
        , "balance leftover:"
        , pretty (Flat balanceLeftover)
        ]
    assertWith
        "utxoAvailable `UTxOSelection.isSubSelectionOf` result"
        (utxoAvailable `UTxOSelection.isSubSelectionOf` result)
    assertWith
        "balanceRequested `leq` balanceSelected"
        (balanceRequested `leq` balanceSelected)
    assertWith
        "balanceAvailable == balanceSelected <> balanceLeftover"
        (balanceAvailable == balanceSelected <> balanceLeftover)
  where
    assetsAvailable = TokenBundle.getAssets balanceAvailable
    assetsRequested = TokenBundle.getAssets balanceRequested
    balanceAvailable = UTxOSelection.availableBalance utxoAvailable
    balanceRequested = adjustAllTokenBundleQuantities (`div` 8) $
        cutAssetSetSizeInHalf balanceAvailable

prop_runSelection_UTxO_muchMoreThanEnough
    :: Blind (Large (UTxOIndex InputId))
    -> SelectionStrategy
    -> Property
prop_runSelection_UTxO_muchMoreThanEnough (Blind (Large index)) strategy =
    -- Generation of large UTxO sets takes longer, so limit the number of runs:
    withMaxSuccess 100 $
    checkCoverage $
    monadicIO $ do
        result <- run $ runSelection
            RunSelectionParams
                { selectionLimit = NoLimit
                , utxoAvailable
                , minimumBalance = balanceRequested
                , selectionStrategy = strategy
                }
        let balanceSelected = UTxOSelection.selectedBalance result
        let balanceLeftover = UTxOSelection.leftoverBalance result
        monitor $ cover 80
            (assetsRequested `Set.isProperSubsetOf` assetsAvailable)
            "assetsRequested ⊂ assetsAvailable"
        monitor $ cover 50 (Set.size assetsRequested >= 4)
            "size assetsRequested >= 4"
        monitor $ counterexample $ unlines
            [ "balance available:"
            , pretty (Flat balanceAvailable)
            , "balance requested:"
            , pretty (Flat balanceRequested)
            , "balance selected:"
            , pretty (Flat balanceSelected)
            , "balance leftover:"
            , pretty (Flat balanceLeftover)
            ]
        assertWith
            "utxoAvailable `UTxOSelection.isSubSelectionOf` result"
            (utxoAvailable `UTxOSelection.isSubSelectionOf` result)
        assertWith
            "balanceRequested `leq` balanceSelected"
            (balanceRequested `leq` balanceSelected)
        assertWith
            "balanceAvailable == balanceSelected <> balanceLeftover"
            (balanceAvailable == balanceSelected <> balanceLeftover)
  where
    assetsAvailable = TokenBundle.getAssets balanceAvailable
    assetsRequested = TokenBundle.getAssets balanceRequested
    balanceAvailable = view #balance index
    balanceRequested = adjustAllTokenBundleQuantities (`div` 256) $
        cutAssetSetSizeInHalf balanceAvailable
    utxoAvailable = UTxOSelection.fromIndex index

--------------------------------------------------------------------------------
-- Running a selection (non-empty)
--------------------------------------------------------------------------------

prop_runSelectionNonEmpty :: UTxOSelection InputId -> Property
prop_runSelectionNonEmpty result =
    case (haveLeftover, haveSelected) of
        (False, False) ->
            -- In this case, the available UTxO set was completely empty.
            -- Since there's nothing to select, we must fail with 'Nothing':
            property $ isNothing maybeResultNonEmpty
        (False, True) ->
            -- In this case, we've already selected all entries from the
            -- available UTxO, so there's no more work to do. We need to check
            -- that 'runSelectionNonEmpty' does not expand the selection:
            maybeResultNonEmpty === UTxOSelection.toNonEmpty result
        (True, True) ->
            -- In this case, we've already selected some entries from the
            -- available UTxO, so there's no more work to do. We need to check
            -- that 'runSelectionNonEmpty' does not expand the selection:
            maybeResultNonEmpty === UTxOSelection.toNonEmpty result
        (True, False) ->
            -- This represents the case where 'runSelection' does not select
            -- anything at all, even though we do have at least one UTxO entry
            -- available. This is the only case where 'runSelectionNonEmpty' is
            -- expected to perform extra work: it should select precisely one
            -- entry, and no more:
            checkResultNonEmpty
  where
    haveLeftover = UTxOSelection.leftoverSize result > 0
    haveSelected = UTxOSelection.selectedSize result > 0

    checkResultNonEmpty :: Property
    checkResultNonEmpty = checkSelectedElement &
        fromMaybe (error "Failed to select an entry when one was available")
      where
        checkSelectedElement :: Maybe Property
        checkSelectedElement = do
            resultNonEmpty <- maybeResultNonEmpty
            (i, o) <- matchSingletonList $
                UTxOSelection.selectedList resultNonEmpty
            pure $
                UTxOIndex.insert i o
                    (UTxOSelection.leftoverIndex resultNonEmpty)
                === UTxOSelection.leftoverIndex result

    maybeResultNonEmpty :: Maybe (UTxOSelectionNonEmpty InputId)
    maybeResultNonEmpty = runIdentity $ runSelectionNonEmptyWith
        (Identity <$> mockSelectSingleEntry)
        (result)

mockSelectSingleEntry
    :: UTxOSelection InputId -> Maybe (UTxOSelectionNonEmpty InputId)
mockSelectSingleEntry state =
    selectEntry =<< firstLeftoverEntry state
  where
    firstLeftoverEntry :: UTxOSelection InputId -> Maybe (InputId, TokenBundle)
    firstLeftoverEntry =
        listToMaybe . UTxOIndex.toList . UTxOSelection.leftoverIndex

    selectEntry
        :: (InputId, TokenBundle) -> Maybe (UTxOSelectionNonEmpty InputId)
    selectEntry (i, _b) = UTxOSelection.select i state

--------------------------------------------------------------------------------
-- Running a selection step
--------------------------------------------------------------------------------

data MockSelectionStepData = MockSelectionStepData
    { mockNext :: Maybe Natural
      -- ^ Quantity to be yielded 'by selectQuantity'.
    , mockSelected :: Natural
      -- ^ Quantity already selected.
    , mockMinimum :: Natural
      -- ^ Minimum quantity to select.
    , mockSelectionStrategy :: SelectionStrategy
      -- ^ Which selection strategy to use.
    }
    deriving (Eq, Show)

-- | Runs a single mock selection step.
--
-- If an additional quantity was selected (causing the total selected quantity
-- to increase) then this function returns the updated total selected quantity.
--
-- If no additional quantity was selected, then this function returns 'Nothing'.
--
runMockSelectionStep :: MockSelectionStepData -> Maybe Natural
runMockSelectionStep d =
    runIdentity $ runSelectionStep lens $ mockSelected d
  where
    lens :: SelectionLens Identity Natural Natural
    lens = SelectionLens
        { currentQuantity = id
        , updatedQuantity = id
        , minimumQuantity = mockMinimum d
        , selectQuantity = \s -> pure $ (+ s) <$> mockNext d
        , selectionStrategy = mockSelectionStrategy d
        }

-- Simulates a selection step where there is nothing available to select.
--
-- In this situation, running a selection step should never yield an updated
-- state, regardless of whether or not we've reached the minimum amount.
--
prop_runSelectionStep_supplyExhausted
    :: SelectionStrategy
    -> Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_supplyExhausted
    strategy (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === Nothing
  where
    mockData = MockSelectionStepData {..}
    mockSelected = fromIntegral x
    mockMinimum = fromIntegral y
    mockNext = Nothing
    mockSelectionStrategy = strategy

-- Simulates a selection step where the next quantity to be yielded will not
-- yet allow us to reach the minimum amount.
--
-- In this situation, running a selection step should always succeed,
-- regardless of the selection strategy.
--
prop_runSelectionStep_notYetEnoughToSatisfyMinimum
    :: SelectionStrategy
    -> Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_notYetEnoughToSatisfyMinimum
    strategy (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === fmap (+ mockSelected) mockNext
  where
    p = fromIntegral $ max x y
    q = fromIntegral $ min x y
    mockData = MockSelectionStepData {..}
    mockSelected = p
    mockMinimum = p + q  + 1
    mockNext = Just q
    mockSelectionStrategy = strategy

-- Simulates a selection step where the next quantity to be yielded will allow
-- us to precisely reach the minimum amount (and not exceed it).
--
-- In this situation, running a selection step should always succeed,
-- regardless of the selection strategy.
--
prop_runSelectionStep_preciselyEnoughToSatisfyMinimum
    :: SelectionStrategy
    -> Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_preciselyEnoughToSatisfyMinimum
    strategy (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === fmap (+ mockSelected) mockNext
  where
    p = fromIntegral $ max x y
    q = fromIntegral $ min x y
    mockData = MockSelectionStepData {..}
    mockSelected = p
    mockMinimum = p + q
    mockNext = Just q
    mockSelectionStrategy = strategy

-- Simulates a selection step under the "minimal" selection strategy, where the
-- minimum amount has already reached.
--
-- In this situation, running a selection step should always fail, regardless
-- of the next quantity to be yielded.
--
prop_runSelectionStep_exceedsMinimalTarget
    :: Positive Word8
    -> Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_exceedsMinimalTarget
    (Positive x) (Positive y) (Positive z) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === Nothing
  where
    [next, mockMinimum, mockSelected] = fromIntegral <$> L.sort [x, y, z]
    mockNext = Just next
    mockData = MockSelectionStepData {..}
    mockSelectionStrategy = SelectionStrategyMinimal

-- Simulates a selection step under the "optimal" selection strategy, where the
-- minimum amount has already been reached, and the next quantity to be yielded
-- will take us closer to the target amount without exceeding it.
--
-- In this situation, running a selection step should always succeed.
--
prop_runSelectionStep_getsCloserToOptimalTargetButDoesNotExceedIt
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_getsCloserToOptimalTargetButDoesNotExceedIt
    (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === fmap (+ mockSelected) mockNext
  where
    p = fromIntegral $ max x y
    q = fromIntegral $ min x y
    mockData = MockSelectionStepData {..}
    mockSelected = p
    mockMinimum = p
    mockNext = Just q
    mockSelectionStrategy = SelectionStrategyOptimal

-- Simulates a selection step under the "optimal" selection strategy, where the
-- minimum amount has already been reached, and the next quantity to be yielded
-- will take us closer to the target amount and also exceed it.
--
-- In this situation, running a selection step should always succeed.
--
prop_runSelectionStep_getsCloserToOptimalTargetAndExceedsIt
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_getsCloserToOptimalTargetAndExceedsIt
    (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === fmap (+ mockSelected) mockNext
  where
    p = fromIntegral $ max x y
    q = fromIntegral $ min x y
    mockData = MockSelectionStepData {..}
    mockSelected = (2 * p) - q
    mockMinimum = p
    mockNext = Just ((2 * q) - 1)
    mockSelectionStrategy = SelectionStrategyOptimal

-- Simulates a selection step under the "optimal" selection strategy, where the
-- minimum amount has already been reached, and the next quantity to be yielded
-- will take us further away from the target amount, while also exceeding it.
--
-- In this situation, running a selection step should always fail.
--
prop_runSelectionStep_exceedsOptimalTargetAndGetsFurtherAway
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_exceedsOptimalTargetAndGetsFurtherAway
    (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === Nothing
  where
    p = fromIntegral $ max x y
    q = fromIntegral $ min x y
    mockData = MockSelectionStepData {..}
    mockSelected = (2 * p) - q
    mockMinimum = p
    mockNext = Just ((2 * q) + 1)
    mockSelectionStrategy = SelectionStrategyOptimal

--------------------------------------------------------------------------------
-- Behaviour of selection lenses
--------------------------------------------------------------------------------

prop_assetSelectionLens_givesPriorityToSingletonAssets
    :: Blind (Small (UTxOIndex InputId))
    -> Property
prop_assetSelectionLens_givesPriorityToSingletonAssets (Blind (Small u)) =
    assetCount >= 2 ==> monadicIO $ do
        hasSingletonAsset <- isJust <$>
            run (UTxOIndex.selectRandom u $ WithAssetOnly asset)
        monitor $ cover 20 hasSingletonAsset
            "There is at least one singleton entry that matches"
        monitor $ cover 20 (not hasSingletonAsset)
            "There are no singleton entries that match"
        monitor $ counterexample $ unlines
            ["UTxO index:", pretty $ UTxOIndex.toList u]
        mUpdatedState <- run $ runSelectionStep lens initialState
        case mUpdatedState of
            Nothing -> do
                -- This should never happen: we should always be able to select
                -- _something_ that matches.
                monitor $ counterexample "Error: unable to select any entry"
                assert False
            Just result -> do
                let bundle = NE.head $ snd <$> UTxOSelection.selectedList result
                case F.toList $ TokenBundle.getAssets bundle of
                    [a] -> assertWith
                        "a == asset"
                        (a == asset)
                    _ -> assertWith
                        "not hasSingletonAsset"
                        (not hasSingletonAsset)
  where
    asset = Set.findMin $ UTxOIndex.assets u
    assetCount = Set.size $ UTxOIndex.assets u
    initialState = UTxOSelection.fromIndex u
    lens = assetSelectionLens
        NoLimit SelectionStrategyOptimal (asset, minimumAssetQuantity)
    minimumAssetQuantity = TokenQuantity 1

prop_coinSelectionLens_givesPriorityToCoins
    :: Blind (Small (UTxOIndex InputId))
    -> Property
prop_coinSelectionLens_givesPriorityToCoins (Blind (Small u)) =
    entryCount > 0 ==> monadicIO $ do
        hasCoin <- isJust <$> run (UTxOIndex.selectRandom u WithAdaOnly)
        monitor $ cover 20 hasCoin
            "There is at least one coin"
        monitor $ cover 1 (not hasCoin)
            "There are no coins"
        monitor $ counterexample $ unlines
            ["UTxO index:", pretty $ UTxOIndex.toList u]
        mUpdatedState <- run $ runSelectionStep lens initialState
        case mUpdatedState of
            Nothing -> do
                -- This should never happen: we should always be able to select
                -- _something_ that matches.
                monitor $ counterexample "Error: unable to select any entry"
                assert False
            Just result -> do
                let bundle = NE.head $ snd <$> UTxOSelection.selectedList result
                case F.toList $ TokenBundle.getAssets bundle of
                    [] -> assertWith     "hasCoin" (    hasCoin)
                    _  -> assertWith "not hasCoin" (not hasCoin)
  where
    entryCount = UTxOIndex.size u
    initialState = UTxOSelection.fromIndex u
    lens = coinSelectionLens
        NoLimit SelectionStrategyOptimal minimumCoinQuantity
    minimumCoinQuantity = Coin 1

--------------------------------------------------------------------------------
-- Boundary tests
--------------------------------------------------------------------------------

unit_testBoundaries :: String -> [BoundaryTestData] -> SpecWith ()
unit_testBoundaries title = unitTests title . fmap mkBoundaryTestExpectation

data BoundaryTestData = BoundaryTestData
    { boundaryTestCriteria
        :: BoundaryTestCriteria
    , boundaryTestExpectedResult
        :: BoundaryTestResult
    }
    deriving (Eq, Show)

data BoundaryTestCriteria = BoundaryTestCriteria
    { boundaryTestBundleSizeAssessor
        :: MockAssessTokenBundleSize
    , boundaryTestOutputs
        :: [BoundaryTestEntry]
    , boundaryTestUTxO
        :: [BoundaryTestEntry]
    , boundaryTestSelectionStrategy
        :: SelectionStrategy
    }
    deriving (Eq, Show)

data BoundaryTestResult = BoundaryTestResult
    { boundaryTestInputs
        :: [BoundaryTestEntry]
    , boundaryTestChange
        :: [BoundaryTestEntry]
    }
    deriving (Eq, Show)

type BoundaryTestEntry = (Coin, [(AssetId, TokenQuantity)])

mkBoundaryTestExpectation :: BoundaryTestData -> Expectation
mkBoundaryTestExpectation (BoundaryTestData params expectedResult) = do
    actualResult <- performSelection constraints
        (encodeBoundaryTestCriteria params)
    fmap decodeBoundaryTestResult actualResult `shouldBe` Right expectedResult
  where
    constraints = SelectionConstraints
        { computeMinimumAdaQuantity = computeMinimumAdaQuantityZero
        , computeMinimumCost = computeMinimumCostZero
        , assessTokenBundleSize = unMockAssessTokenBundleSize $
            boundaryTestBundleSizeAssessor params
        , computeSelectionLimit = const NoLimit
        }

encodeBoundaryTestCriteria
    :: BoundaryTestCriteria
    -> SelectionParams Address InputId
encodeBoundaryTestCriteria c = SelectionParams
    { outputsToCover =
        zip
            (dummyAddresses)
            (uncurry TokenBundle.fromFlatList <$> boundaryTestOutputs c)
    , utxoAvailable =
        UTxOSelection.fromIndex
        $ UTxOIndex.fromSequence
        $ zip dummyInputIds
        $ uncurry TokenBundle.fromFlatList <$> boundaryTestUTxO c
    , extraCoinSource =
        Coin 0
    , extraCoinSink =
        Coin 0
    , assetsToMint =
        TokenMap.empty
    , assetsToBurn =
        TokenMap.empty
    , selectionStrategy =
        boundaryTestSelectionStrategy c
    }
  where
    dummyInputIds :: [InputId]
    dummyInputIds = zip dummyTxIns dummyAddresses

    dummyAddresses :: [Address]
    dummyAddresses = [Address (B8.pack $ show x) | x :: Word64 <- [0 ..]]

    dummyTxIns :: [TxIn]
    dummyTxIns = [TxIn (Hash "") x | x <- [0 ..]]

decodeBoundaryTestResult
    :: SelectionResult Address InputId
    -> BoundaryTestResult
decodeBoundaryTestResult r = BoundaryTestResult
    { boundaryTestInputs = L.sort $ NE.toList $
        TokenBundle.toFlatList . snd <$> view #inputsSelected r
    , boundaryTestChange =
        TokenBundle.toFlatList <$> view #changeGenerated r
    }

--------------------------------------------------------------------------------
-- Boundary tests: handling of large token quantities
--------------------------------------------------------------------------------

boundaryTestMatrix_largeTokenQuantities :: [BoundaryTestData]
boundaryTestMatrix_largeTokenQuantities =
    [ boundaryTest_largeTokenQuantities_1
    , boundaryTest_largeTokenQuantities_2
    , boundaryTest_largeTokenQuantities_3
    , boundaryTest_largeTokenQuantities_4
    , boundaryTest_largeTokenQuantities_5
    , boundaryTest_largeTokenQuantities_6
    ]

-- Reach (but do not exceed) the maximum token quantity by selecting inputs
-- with the following quantities:
--
--  - Quantity #1: 1
--  - Quantity #2: maximum token quantity - 1
--
-- We expect no splitting of token bundles.
--
boundaryTest_largeTokenQuantities_1 :: BoundaryTestData
boundaryTest_largeTokenQuantities_1 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    (q1, q2) = (TokenQuantity 1, TokenQuantity.predZero txOutMaxTokenQuantity)
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_500_000, []) ]
    boundaryTestUTxO =
      [ (Coin 1_000_000, [(mockAsset "A", q1)])
      , (Coin 1_000_000, [(mockAsset "A", q2)])
      ]
    boundaryTestInputs =
      [ (Coin 1_000_000, [(mockAsset "A", q1)])
      , (Coin 1_000_000, [(mockAsset "A", q2)])
      ]
    boundaryTestChange =
      [ (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)]) ]

-- Reach (but do not exceed) the maximum token quantity by selecting inputs
-- with the following quantities:
--
--  - Quantity #1: floor   (maximum token quantity / 2)
--  - Quantity #2: ceiling (maximum token quantity / 2)
--
-- We expect no splitting of token bundles.
--
boundaryTest_largeTokenQuantities_2 :: BoundaryTestData
boundaryTest_largeTokenQuantities_2 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    q1 :| [q2] = TokenQuantity.equipartition txOutMaxTokenQuantity (() :| [()])
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_500_000, []) ]
    boundaryTestUTxO =
      [ (Coin 1_000_000, [(mockAsset "A", q1)])
      , (Coin 1_000_000, [(mockAsset "A", q2)])
      ]
    boundaryTestInputs =
      [ (Coin 1_000_000, [(mockAsset "A", q1)])
      , (Coin 1_000_000, [(mockAsset "A", q2)])
      ]
    boundaryTestChange =
      [ (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)]) ]

-- Slightly exceed the maximum token quantity by selecting inputs with the
-- following quantities:
--
--  - Quantity #1: 1
--  - Quantity #2: maximum token quantity
--
-- We expect splitting of change bundles.
--
boundaryTest_largeTokenQuantities_3 :: BoundaryTestData
boundaryTest_largeTokenQuantities_3 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    q1 :| [q2] = TokenQuantity.equipartition
        (TokenQuantity.succ txOutMaxTokenQuantity) (() :| [()])
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_500_000, []) ]
    boundaryTestUTxO =
      [ (Coin 1_000_000, [(mockAsset "A", TokenQuantity 1)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestInputs =
      [ (Coin 1_000_000, [(mockAsset "A", TokenQuantity 1)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestChange =
      [ (Coin 250_000, [(mockAsset "A", q1)])
      , (Coin 250_000, [(mockAsset "A", q2)])
      ]

-- Reach (but do not exceed) exactly twice the maximum token quantity by
-- selecting inputs with the following quantities:
--
--  - Quantity #1: maximum token quantity
--  - Quantity #2: maximum token quantity
--
-- We expect splitting of change bundles.
--
boundaryTest_largeTokenQuantities_4 :: BoundaryTestData
boundaryTest_largeTokenQuantities_4 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_500_000, []) ]
    boundaryTestUTxO =
      [ (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestInputs =
      [ (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestChange =
      [ (Coin 250_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 250_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]

-- In the event that generated change bundles must be split, demonstrate that
-- the change generation algorithm terminates after only a subset of the UTxO
-- has been selected.
--
-- See: https://input-output.atlassian.net/browse/ADP-890
--
boundaryTest_largeTokenQuantities_5 :: BoundaryTestData
boundaryTest_largeTokenQuantities_5 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 2_000_000, []) ]
    boundaryTestUTxO =
      [ (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestInputs =
      [ (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestChange =
      [ (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]

-- In the event that generated change bundles must be split, demonstrate that
-- the change generation algorithm terminates after only a subset of the UTxO
-- has been selected.
--
-- See: https://input-output.atlassian.net/browse/ADP-890
--
boundaryTest_largeTokenQuantities_6 :: BoundaryTestData
boundaryTest_largeTokenQuantities_6 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_000_000, [])
      , (Coin 1_000_000, [])
      ]
    boundaryTestUTxO =
      [ (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestInputs =
      [ (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 1_000_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]
    boundaryTestChange =
      [ (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      , (Coin 500_000, [(mockAsset "A", txOutMaxTokenQuantity)])
      ]

--------------------------------------------------------------------------------
-- Boundary tests: handling of large asset counts
--------------------------------------------------------------------------------

boundaryTestMatrix_largeAssetCounts :: [BoundaryTestData]
boundaryTestMatrix_largeAssetCounts =
    [ boundaryTest_largeAssetCounts_1
    , boundaryTest_largeAssetCounts_2
    , boundaryTest_largeAssetCounts_3
    , boundaryTest_largeAssetCounts_4
    ]

-- Reach (but do not exceed) the maximum per-bundle asset count.
--
-- We expect no splitting of change bundles.
--
boundaryTest_largeAssetCounts_1 :: BoundaryTestData
boundaryTest_largeAssetCounts_1 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUpperLimit 4
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_000_000, []) ]
    boundaryTestUTxO =
      [ (Coin 500_000, [mockAssetQuantity "A" 1])
      , (Coin 500_000, [mockAssetQuantity "B" 1])
      , (Coin 500_000, [mockAssetQuantity "C" 1])
      , (Coin 500_000, [mockAssetQuantity "D" 1])
      ]
    -- Expect that all entries will be selected:
    boundaryTestInputs = boundaryTestUTxO
    boundaryTestChange =
      [ ( Coin 1_000_000
        , [ mockAssetQuantity "A" 1
          , mockAssetQuantity "B" 1
          , mockAssetQuantity "C" 1
          , mockAssetQuantity "D" 1
          ]
        )
      ]

-- Exceed the maximum per-bundle asset count of 3.
--
-- We expect splitting of change bundles.
--
boundaryTest_largeAssetCounts_2 :: BoundaryTestData
boundaryTest_largeAssetCounts_2 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUpperLimit 3
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_000_000, []) ]
    boundaryTestUTxO =
      [ (Coin 500_000, [mockAssetQuantity "A" 1])
      , (Coin 500_000, [mockAssetQuantity "B" 1])
      , (Coin 500_000, [mockAssetQuantity "C" 1])
      , (Coin 500_000, [mockAssetQuantity "D" 1])
      ]
    -- Expect that all entries will be selected:
    boundaryTestInputs = boundaryTestUTxO
    boundaryTestChange =
      [ (Coin 500_000, [mockAssetQuantity "A" 1, mockAssetQuantity "B" 1])
      , (Coin 500_000, [mockAssetQuantity "C" 1, mockAssetQuantity "D" 1])
      ]

-- Exceed the maximum per-bundle asset count of 2.
--
-- We expect splitting of change bundles.
--
boundaryTest_largeAssetCounts_3 :: BoundaryTestData
boundaryTest_largeAssetCounts_3 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUpperLimit 2
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_000_000, []) ]
    boundaryTestUTxO =
      [ (Coin 500_000, [mockAssetQuantity "A" 1])
      , (Coin 500_000, [mockAssetQuantity "B" 1])
      , (Coin 500_000, [mockAssetQuantity "C" 1])
      , (Coin 500_000, [mockAssetQuantity "D" 1])
      ]
    -- Expect that all entries will be selected:
    boundaryTestInputs = boundaryTestUTxO
    boundaryTestChange =
      [ (Coin 500_000, [mockAssetQuantity "A" 1, mockAssetQuantity "B" 1])
      , (Coin 500_000, [mockAssetQuantity "C" 1, mockAssetQuantity "D" 1])
      ]

-- Exceed the maximum per-bundle asset count of 1.
--
-- We expect splitting of change bundles.
--
boundaryTest_largeAssetCounts_4 :: BoundaryTestData
boundaryTest_largeAssetCounts_4 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUpperLimit 1
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1_000_000, []) ]
    boundaryTestUTxO =
      [ (Coin 500_000, [mockAssetQuantity "A" 1])
      , (Coin 500_000, [mockAssetQuantity "B" 1])
      , (Coin 500_000, [mockAssetQuantity "C" 1])
      , (Coin 500_000, [mockAssetQuantity "D" 1])
      ]
    -- Expect that all entries will be selected:
    boundaryTestInputs = boundaryTestUTxO
    boundaryTestChange =
      [ (Coin 250_000, [mockAssetQuantity "A" 1])
      , (Coin 250_000, [mockAssetQuantity "B" 1])
      , (Coin 250_000, [mockAssetQuantity "C" 1])
      , (Coin 250_000, [mockAssetQuantity "D" 1])
      ]

--------------------------------------------------------------------------------
-- Boundary tests: comparison of selection strategies
--------------------------------------------------------------------------------

boundaryTestMatrix_selectionStrategies :: [BoundaryTestData]
boundaryTestMatrix_selectionStrategies =
    [ boundaryTest_selectionStrategies_1_minimal
    , boundaryTest_selectionStrategies_1_optimal
    , boundaryTest_selectionStrategies_2_minimal
    , boundaryTest_selectionStrategies_2_optimal
    , boundaryTest_selectionStrategies_3_minimal
    , boundaryTest_selectionStrategies_3_optimal
    , boundaryTest_selectionStrategies_4_minimal
    , boundaryTest_selectionStrategies_4_optimal
    ]

boundaryTest_selectionStrategies_1_minimal :: BoundaryTestData
boundaryTest_selectionStrategies_1_minimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyMinimal
    boundaryTestOutputs =
      [ (Coin 1, []) ]
    boundaryTestUTxO =
      [ (Coin 1, []), (Coin 1, []) ]
    boundaryTestInputs =
      [ (Coin 1, []) ]
    boundaryTestChange =
      [ (Coin 0, []) ]
      -- Note that a single empty change bundle is expected here, as:
      --    - we attempt to always generate one change output for
      --      each user-specified output.
      --    - the minimum ada quantity is zero.

boundaryTest_selectionStrategies_1_optimal :: BoundaryTestData
boundaryTest_selectionStrategies_1_optimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1, []) ]
    boundaryTestUTxO =
      [ (Coin 1, []), (Coin 1, []) ]
    boundaryTestInputs =
      [ (Coin 1, []), (Coin 1, []) ]
    boundaryTestChange =
      [ (Coin 1, []) ]

boundaryTest_selectionStrategies_2_minimal :: BoundaryTestData
boundaryTest_selectionStrategies_2_minimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyMinimal
    boundaryTestOutputs =
      [ (Coin 1, []) ]
    boundaryTestUTxO =
      [ (Coin 1, [mockAssetQuantity "A" 1])
      , (Coin 1, [mockAssetQuantity "A" 1])
      ]
    boundaryTestInputs =
      [ (Coin 1, [mockAssetQuantity "A" 1]) ]
    boundaryTestChange =
      [ (Coin 0, [mockAssetQuantity "A" 1]) ]

boundaryTest_selectionStrategies_2_optimal :: BoundaryTestData
boundaryTest_selectionStrategies_2_optimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1, []) ]
    boundaryTestUTxO =
      [ (Coin 1, [mockAssetQuantity "A" 1])
      , (Coin 1, [mockAssetQuantity "A" 1])
      ]
    boundaryTestInputs =
      [ (Coin 1, [mockAssetQuantity "A" 1])
      , (Coin 1, [mockAssetQuantity "A" 1])
      ]
    boundaryTestChange =
      [ (Coin 1, [mockAssetQuantity "A" 2]) ]

boundaryTest_selectionStrategies_3_minimal :: BoundaryTestData
boundaryTest_selectionStrategies_3_minimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyMinimal
    boundaryTestOutputs =
      [ (Coin 1, [mockAssetQuantity "A" 1]) ]
    boundaryTestUTxO =
      [ (Coin 1, [mockAssetQuantity "A" 1])
      , (Coin 1, [mockAssetQuantity "A" 1])
      ]
    boundaryTestInputs =
      [ (Coin 1, [mockAssetQuantity "A" 1]) ]
    boundaryTestChange =
      [ (Coin 0, []) ]

boundaryTest_selectionStrategies_3_optimal :: BoundaryTestData
boundaryTest_selectionStrategies_3_optimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 1, [mockAssetQuantity "A" 1]) ]
    boundaryTestUTxO =
      [ (Coin 1, [mockAssetQuantity "A" 1])
      , (Coin 1, [mockAssetQuantity "A" 1])
      ]
    boundaryTestInputs =
      [ (Coin 1, [mockAssetQuantity "A" 1])
      , (Coin 1, [mockAssetQuantity "A" 1])
      ]
    boundaryTestChange =
      [ (Coin 1, [mockAssetQuantity "A" 1]) ]

boundaryTest_selectionStrategies_4_minimal :: BoundaryTestData
boundaryTest_selectionStrategies_4_minimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyMinimal
    boundaryTestOutputs =
      [ (Coin 2, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]
    boundaryTestUTxO =
      [ (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]
    boundaryTestInputs =
      [ (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]
    boundaryTestChange =
      [ (Coin 0, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]

boundaryTest_selectionStrategies_4_optimal :: BoundaryTestData
boundaryTest_selectionStrategies_4_optimal = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = MockAssessTokenBundleSizeUnlimited
    boundaryTestSelectionStrategy = SelectionStrategyOptimal
    boundaryTestOutputs =
      [ (Coin 2, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]
    boundaryTestUTxO =
      [ (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]
    boundaryTestInputs =
      [ (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      , (Coin 1, [mockAssetQuantity "A" 2, mockAssetQuantity "B" 2000])
      ]
    boundaryTestChange =
      [ (Coin 2, [mockAssetQuantity "A" 6, mockAssetQuantity "B" 6000])
      ]

--------------------------------------------------------------------------------
-- Selection constraints
--------------------------------------------------------------------------------

data MockSelectionConstraints = MockSelectionConstraints
    { assessTokenBundleSize
        :: MockAssessTokenBundleSize
    , computeMinimumAdaQuantity
        :: MockComputeMinimumAdaQuantity
    , computeMinimumCost
        :: MockComputeMinimumCost
    , computeSelectionLimit
        :: MockComputeSelectionLimit
    } deriving (Eq, Generic, Show)

genMockSelectionConstraints :: Gen MockSelectionConstraints
genMockSelectionConstraints = MockSelectionConstraints
    <$> genMockAssessTokenBundleSize
    <*> genMockComputeMinimumAdaQuantity
    <*> genMockComputeMinimumCost
    <*> genMockComputeSelectionLimit

shrinkMockSelectionConstraints
    :: MockSelectionConstraints -> [MockSelectionConstraints]
shrinkMockSelectionConstraints = genericRoundRobinShrink
    <@> shrinkMockAssessTokenBundleSize
    <:> shrinkMockComputeMinimumAdaQuantity
    <:> shrinkMockComputeMinimumCost
    <:> shrinkMockComputeSelectionLimit
    <:> Nil

unMockSelectionConstraints
    :: MockSelectionConstraints
    -> SelectionConstraints Address
unMockSelectionConstraints m = SelectionConstraints
    { assessTokenBundleSize =
        unMockAssessTokenBundleSize $ view #assessTokenBundleSize m
    , computeMinimumAdaQuantity =
        unMockComputeMinimumAdaQuantity $ view #computeMinimumAdaQuantity m
    , computeMinimumCost =
        unMockComputeMinimumCost $ view #computeMinimumCost m
    , computeSelectionLimit =
        unMockComputeSelectionLimit $ view #computeSelectionLimit m
    }

--------------------------------------------------------------------------------
-- Computing minimum ada quantities
--------------------------------------------------------------------------------

data MockComputeMinimumAdaQuantity
    = MockComputeMinimumAdaQuantityZero
    | MockComputeMinimumAdaQuantityLinear
    deriving (Eq, Show, Bounded, Enum)

genMockComputeMinimumAdaQuantity :: Gen MockComputeMinimumAdaQuantity
genMockComputeMinimumAdaQuantity = arbitraryBoundedEnum

shrinkMockComputeMinimumAdaQuantity
    :: MockComputeMinimumAdaQuantity -> [MockComputeMinimumAdaQuantity]
shrinkMockComputeMinimumAdaQuantity = \case
    MockComputeMinimumAdaQuantityZero ->
        []
    MockComputeMinimumAdaQuantityLinear ->
        [MockComputeMinimumAdaQuantityZero]

unMockComputeMinimumAdaQuantity
    :: MockComputeMinimumAdaQuantity -> (TokenMap -> Coin)
unMockComputeMinimumAdaQuantity = \case
    MockComputeMinimumAdaQuantityZero ->
        computeMinimumAdaQuantityZero
    MockComputeMinimumAdaQuantityLinear ->
        computeMinimumAdaQuantityLinear

-- | Returns a constant minimum ada quantity of zero.
--
computeMinimumAdaQuantityZero :: TokenMap -> Coin
computeMinimumAdaQuantityZero = const (Coin 0)

-- | A dummy function for calculating the minimum ada quantity to pay for a
--   token map.
--
-- The only property we want this function to have is that is becomes more
-- expensive with the number of unique assets in the map. So, looking at the
-- size of the asset set is enough.
--
computeMinimumAdaQuantityLinear :: TokenMap -> Coin
computeMinimumAdaQuantityLinear m =
    Coin (1 + fromIntegral (TokenMap.size m))

--------------------------------------------------------------------------------
-- Computing minimum costs
--------------------------------------------------------------------------------

data MockComputeMinimumCost
    = MockComputeMinimumCostZero
    | MockComputeMinimumCostLinear
    deriving (Eq, Show, Bounded, Enum)

genMockComputeMinimumCost :: Gen MockComputeMinimumCost
genMockComputeMinimumCost = arbitraryBoundedEnum

shrinkMockComputeMinimumCost
    :: MockComputeMinimumCost -> [MockComputeMinimumCost]
shrinkMockComputeMinimumCost = \case
    MockComputeMinimumCostZero ->
        []
    MockComputeMinimumCostLinear ->
        [MockComputeMinimumCostZero]

unMockComputeMinimumCost
    :: MockComputeMinimumCost -> (SelectionSkeleton Address -> Coin)
unMockComputeMinimumCost = \case
    MockComputeMinimumCostZero ->
        computeMinimumCostZero
    MockComputeMinimumCostLinear ->
        computeMinimumCostLinear

computeMinimumCostZero :: SelectionSkeleton Address -> Coin
computeMinimumCostZero = const $ Coin 0

computeMinimumCostLinear :: SelectionSkeleton Address -> Coin
computeMinimumCostLinear s
    = Coin
    $ fromIntegral
    $ skeletonInputCount s
    + F.length (TokenMap.size . view #tokens . snd <$> skeletonOutputs s)
    + F.sum (Set.size <$> skeletonChange s)

--------------------------------------------------------------------------------
-- Computing selection limits
--------------------------------------------------------------------------------

data MockComputeSelectionLimit
    = MockComputeSelectionLimitNone
    | MockComputeSelectionLimit Int
    deriving (Eq, Show)

genMockComputeSelectionLimit :: Gen MockComputeSelectionLimit
genMockComputeSelectionLimit = oneof
    [ pure MockComputeSelectionLimitNone
    , MockComputeSelectionLimit <$> sized (\n -> choose (1, max 1 n))
    ]

shrinkMockComputeSelectionLimit
    :: MockComputeSelectionLimit -> [MockComputeSelectionLimit]
shrinkMockComputeSelectionLimit = \case
    MockComputeSelectionLimitNone ->
        []
    MockComputeSelectionLimit n ->
        MockComputeSelectionLimit <$> filter (> 0) (shrink n)

unMockComputeSelectionLimit
    :: MockComputeSelectionLimit
    -> ([(Address, TokenBundle)] -> SelectionLimit)
unMockComputeSelectionLimit = \case
    MockComputeSelectionLimitNone ->
        const NoLimit
    MockComputeSelectionLimit n ->
        const $ MaximumInputLimit n

--------------------------------------------------------------------------------
-- Assessing token bundle sizes
--------------------------------------------------------------------------------

data MockAssessTokenBundleSize
    = MockAssessTokenBundleSizeUnlimited
      -- ^ Indicates that there is no limit on a token bundle's size.
    | MockAssessTokenBundleSizeUpperLimit Int
      -- ^ Indicates an inclusive positive upper bound on the number of assets
      -- in a token bundle.
    deriving (Eq, Show)

genMockAssessTokenBundleSize :: Gen MockAssessTokenBundleSize
genMockAssessTokenBundleSize = oneof
    [ pure MockAssessTokenBundleSizeUnlimited
    , MockAssessTokenBundleSizeUpperLimit . getPositive
        <$> arbitrary @(Positive Int)
    ]

shrinkMockAssessTokenBundleSize
    :: MockAssessTokenBundleSize -> [MockAssessTokenBundleSize]
shrinkMockAssessTokenBundleSize = \case
    MockAssessTokenBundleSizeUnlimited ->
        []
    MockAssessTokenBundleSizeUpperLimit n ->
        MockAssessTokenBundleSizeUpperLimit . getPositive
            <$> shrink (Positive n)

unMockAssessTokenBundleSize
    :: MockAssessTokenBundleSize -> (TokenBundle -> TokenBundleSizeAssessment)
unMockAssessTokenBundleSize = \case
    MockAssessTokenBundleSizeUnlimited ->
        const TokenBundleSizeWithinLimit
    MockAssessTokenBundleSizeUpperLimit upperLimit ->
        \bundle ->
            let assetCount = Set.size $ TokenBundle.getAssets bundle in
            case assetCount `compare` upperLimit of
                LT -> TokenBundleSizeWithinLimit
                EQ -> TokenBundleSizeWithinLimit
                GT -> TokenBundleSizeExceedsLimit

mkTokenBundleSizeAssessor
    :: MockAssessTokenBundleSize -> TokenBundleSizeAssessor
mkTokenBundleSizeAssessor =
    TokenBundleSizeAssessor . unMockAssessTokenBundleSize

--------------------------------------------------------------------------------
-- Making change
--------------------------------------------------------------------------------

type MakeChangeData =
    MakeChangeCriteria MockComputeMinimumAdaQuantity MockAssessTokenBundleSize

isValidMakeChangeData :: MakeChangeData -> Bool
isValidMakeChangeData p = (&&)
    (totalOutputValue `leq` totalInputValue)
    (totalOutputCoinValue > Coin 0)
  where
    totalInputValue =
        F.fold (inputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalOutputValue =
        F.fold (outputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSink p)
            <> TokenBundle.fromTokenMap (view #assetsToBurn p)
    totalOutputCoinValue = TokenBundle.getCoin totalOutputValue

genMakeChangeData :: Gen MakeChangeData
genMakeChangeData = flip suchThat isValidMakeChangeData $ do
    outputBundleCount <- choose (0, 15)
    let inputBundleCount = outputBundleCount * 4
    MakeChangeCriteria
        <$> arbitrary
        <*> pure MockAssessTokenBundleSizeUnlimited
        <*> genRequiredCost
        <*> genExtraCoinSource
        <*> genExtraCoinSink
        <*> genTokenBundles inputBundleCount
        <*> genTokenBundles outputBundleCount
        <*> genAssetsToMint
        <*> genAssetsToBurn
  where
    genAssetsToMint :: Gen TokenMap
    genAssetsToMint = genTokenMapSmallRange

    genAssetsToBurn :: Gen TokenMap
    genAssetsToBurn = genTokenMapSmallRange

    genExtraCoinSource :: Gen Coin
    genExtraCoinSource = oneof [pure $ Coin 0, genCoinPositive]

    genExtraCoinSink :: Gen Coin
    genExtraCoinSink = oneof [pure $ Coin 0, genCoinPositive]

    genRequiredCost :: Gen Coin
    genRequiredCost = genCoin

    genTokenBundles :: Int -> Gen (NonEmpty TokenBundle)
    genTokenBundles count = (:|)
        <$> genTokenBundleSmallRangePositive
        <*> replicateM count genTokenBundleSmallRangePositive

makeChangeWith
    :: MakeChangeData
    -> Either UnableToConstructChangeError [TokenBundle]
makeChangeWith p = makeChange p
    { minCoinFor = unMockComputeMinimumAdaQuantity $ minCoinFor p
    , bundleSizeAssessor = mkTokenBundleSizeAssessor $ bundleSizeAssessor p
    }

prop_makeChange_identity
    :: NonEmpty TokenBundle -> Property
prop_makeChange_identity bundles = (===)
    (F.fold <$> makeChange criteria)
    (Right TokenBundle.empty)
  where
    criteria = MakeChangeCriteria
        { minCoinFor = const (Coin 0)
        , requiredCost = Coin 0
        , extraCoinSource = Coin 0
        , extraCoinSink = Coin 0
        , bundleSizeAssessor =
            mkTokenBundleSizeAssessor MockAssessTokenBundleSizeUnlimited
        , inputBundles = bundles
        , outputBundles = bundles
        , assetsToMint = TokenMap.empty
        , assetsToBurn = TokenMap.empty
        }

-- | Tests that 'makeChange' generates the correct number of change bundles.
--
-- In normal circumstances, provided there are no cost constraints in place,
-- the 'makeChange' function is expected to generate a list of change bundles
-- that is equal in length to the number of user-specified outputs.
--
-- However, if any of the generated change bundles are oversized, 'makeChange'
-- is expected to split these bundles into smaller bundles that are not
-- oversized. Therefore, it's possible for 'makeChange' to return a list of
-- change bundles that is longer than the number of user-specified outputs.
--
-- This property tests both scenarios:
--
--    - when change bundles are not split.
--    - when change bundles are split.
--
prop_makeChange_length
    :: MakeChangeData
    -> Property
prop_makeChange_length p =
    case (mChangeUnsplit, mChangeSplit) of
        (Right changeUnsplit, Right changeSplit) ->
            prop changeUnsplit changeSplit
        _ ->
            -- We expect 'makeChange' to always succeed in a zero-cost scenario.
            property False
  where
    prop :: [TokenBundle] -> [TokenBundle] -> Property
    prop changeUnsplit changeSplit =
        checkCoverage $
        cover 50 (changeSplit /= changeUnsplit)
            "able to generate split change" $
        tabulate
            "largest asset set sizes (unsplit change, split change)"
            [ show
                ( getLargestAssetSetSize changeUnsplit
                , getLargestAssetSetSize changeSplit
                )
            ] $
        if (changeSplit == changeUnsplit)
        then
            property (length (outputBundles p) == length changeUnsplit)
        else
            conjoin
                [ property (length (outputBundles p) == length changeUnsplit)
                , property (length (outputBundles p) <  length changeSplit)
                , property (F.fold changeSplit == F.fold changeUnsplit)
                ]

    mChangeUnsplit =
        makeChange zeroCostMakeChangeScenario
            { bundleSizeAssessor =
                mkTokenBundleSizeAssessor MockAssessTokenBundleSizeUnlimited
            }
    mChangeSplit = mChangeUnsplit >>= \changeUnsplit ->
        makeChange zeroCostMakeChangeScenario
            { bundleSizeAssessor = mkTokenBundleSizeAssessor
                $ MockAssessTokenBundleSizeUpperLimit
                $ max 1
                $ (`div` 2)
                $ getLargestAssetSetSize changeUnsplit
            }

    getLargestAssetSetSize :: [TokenBundle] -> Int
    getLargestAssetSetSize = \case
        [] -> 0
        bs -> F.maximum $ fmap (Set.size . TokenBundle.getAssets) bs

    zeroCostMakeChangeScenario = p
        { minCoinFor = computeMinimumAdaQuantityZero
        , requiredCost = Coin 0
        }

prop_makeChange
    :: MakeChangeData
    -> Property
prop_makeChange p =
    checkCoverage $

    -- Inspect the sets of minted and burned assets:
    cover 20 (view #assetsToMint p /= TokenMap.empty)
        "Have some assets to mint" $
    cover 20 (view #assetsToBurn p /= TokenMap.empty)
        "Have some assets to burn" $
    cover 2 (view #assetsToMint p == TokenMap.empty)
        "Have no assets to mint" $
    cover 2 (view #assetsToBurn p == TokenMap.empty)
        "Have no assets to burn" $

    -- Inspect the intersection between minted assets and burned assets:
    cover 2 (someAssetsAreBothMintedAndBurned)
        "Some assets are both minted and burned" $
    cover 2 (noAssetsAreBothMintedAndBurned)
        "No assets are both minted and burned" $

    -- Inspect the intersection between minted assets and spent assets:
    cover 2 (someAssetsAreBothMintedAndSpent)
        "Some assets are both minted and spent" $
    cover 2 (noAssetsAreBothMintedAndSpent)
        "No assets are both minted and spent" $

    -- Inspect the intersection between spent assets and burned assets:
    cover 2 (someAssetsAreBothSpentAndBurned)
        "Some assets are both spent and burned" $
    cover 2 (noAssetsAreBothSpentAndBurned)
        "No assets are both spent and burned" $

    -- Verify that some assets are minted but not spent or burned:
    cover 2 (someAssetsAreMintedButNotSpentOrBurned)
        "Some assets are minted but not spent or burned" $

    case makeChangeWith p of
        Left{} -> disjoin
            [ prop_makeChange_fail_costTooBig p     & label "cost too big"
            , prop_makeChange_fail_minValueTooBig p & label "min value too big"
            ]
        Right change -> conjoin
            [ prop_makeChange_success_delta p change
            , prop_makeChange_success_minValueRespected p change
            ] & label "success"
  where
    assetsSpentByUserSpecifiedOutputs :: TokenMap
    assetsSpentByUserSpecifiedOutputs =
        F.foldMap (view #tokens) (outputBundles p)

    someAssetsAreBothMintedAndBurned :: Bool
    someAssetsAreBothMintedAndBurned
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (view #assetsToMint p)
            (view #assetsToBurn p)

    someAssetsAreBothMintedAndSpent :: Bool
    someAssetsAreBothMintedAndSpent
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (view #assetsToMint p)
            (assetsSpentByUserSpecifiedOutputs)

    someAssetsAreBothSpentAndBurned :: Bool
    someAssetsAreBothSpentAndBurned
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (assetsSpentByUserSpecifiedOutputs)
            (view #assetsToBurn p)

    someAssetsAreMintedButNotSpentOrBurned :: Bool
        = TokenMap.isNotEmpty
        $ assetsMinted `TokenMap.difference` assetsSpentOrBurned
      where
        assetsMinted =
            view #assetsToMint p
        assetsSpentOrBurned =
            view #assetsToBurn p <> assetsSpentByUserSpecifiedOutputs

    noAssetsAreBothMintedAndBurned :: Bool
    noAssetsAreBothMintedAndBurned = not someAssetsAreBothMintedAndBurned

    noAssetsAreBothMintedAndSpent :: Bool
    noAssetsAreBothMintedAndSpent = not someAssetsAreBothMintedAndSpent

    noAssetsAreBothSpentAndBurned :: Bool
    noAssetsAreBothSpentAndBurned = not someAssetsAreBothSpentAndBurned

-- Checks that on successful calls to 'makeChange', the difference between all
-- inputs and all outputs with change is exactly equal to the required cost of
-- the transaction. This property expects the second argument to be the result
-- to 'makeChange' with 'p' as argument.
--
-- See also 'prop_makeChange' as a top-level property driver.
prop_makeChange_success_delta
    :: MakeChangeData
    -> [TokenBundle]
    -> Property
prop_makeChange_success_delta p change =
    let
        totalOutputWithChange = TokenBundle.add
            totalOutputValue
            (F.fold change)

        delta = TokenBundle.unsafeSubtract totalInputValue totalOutputWithChange
    in
        (delta === TokenBundle.fromCoin (view #requiredCost p))
            & counterexample counterExampleText

  where
    counterExampleText :: String
    counterExampleText = unlines
        [ "totalInputValue"
        , pretty (Flat totalInputValue)
        , "totalOutputValue"
        , pretty (Flat totalOutputValue)
        , "required cost"
        , pretty (Flat $ TokenBundle.fromCoin (view #requiredCost p))
        , "assetsToMint"
        , pretty (Flat $ view #assetsToMint p)
        , "assetsToBurn"
        , pretty (Flat $ view #assetsToBurn p)
        , "change"
        , pretty (Flat $ F.fold change)
        , "outputsToCover"
        , pretty (Flat $ F.fold (outputBundles p))
        , "selected:"
        , pretty (Flat $ F.fold (inputBundles p))
        , "totalChangeValue:"
        , pretty totalChangeCoin
        , "totalOutputValue:"
        , pretty totalOutputCoin
        , "totalInputValue:"
        , pretty totalInputCoin
        ]
    totalInputValue =
        F.fold (inputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalInputCoin =
        TokenBundle.getCoin totalInputValue
    totalOutputValue =
        F.fold (outputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSink p)
            <> TokenBundle.fromTokenMap (view #assetsToBurn p)
    totalOutputCoin =
        TokenBundle.getCoin totalOutputValue
    totalChangeCoin =
        TokenBundle.getCoin (F.fold change)

-- Checks that after a successful call to 'makeChange', all generated change
-- outputs satisfy the minimum required coin quantity provided.
--
-- See also `prop_makeChange` as a top-level property driver.
prop_makeChange_success_minValueRespected
    :: MakeChangeData
    -> [TokenBundle]
    -> Property
prop_makeChange_success_minValueRespected p =
    F.foldr ((.&&.) . checkMinValue) (property True)
  where
    minCoinValueFor :: TokenMap -> Coin
    minCoinValueFor = unMockComputeMinimumAdaQuantity (minCoinFor p)

    checkMinValue :: TokenBundle -> Property
    checkMinValue m@TokenBundle{coin,tokens} =
        coin >= minCoinValue
          & counterexample counterexampleText
      where
        counterexampleText = unlines
            [ "bundle:"
            , pretty (Flat m)
            , "minCoinValue:"
            , pretty minCoinValue
            ]
        minCoinValue = minCoinValueFor tokens

-- The 'makeChange' function may fail when the required cost for a transaction
-- is too big. When this occurs, it means that the delta between inputs and
-- outputs (without change) is larger than the required cost.
--
-- See also `prop_makeChange` as a top-level property driver.
prop_makeChange_fail_costTooBig
    :: MakeChangeData
    -> Property
prop_makeChange_fail_costTooBig p =
    let
        deltaCoin = TokenBundle.getCoin $ TokenBundle.unsafeSubtract
            totalInputValue
            totalOutputValue
    in
        deltaCoin < view #requiredCost p
            & counterexample ("delta: " <> pretty deltaCoin)
  where
    totalInputValue =
        F.fold (inputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalOutputValue =
        F.fold (outputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSink p)
            <> TokenBundle.fromTokenMap (view #assetsToBurn p)

-- The 'makeChange' function will fail if there is not enough ada to assign
-- to all the generated change outputs. Indeed, each output must include a
-- minimum quantity of ada.
--
-- See also `prop_makeChange` as a top-level property driver.
prop_makeChange_fail_minValueTooBig
    :: MakeChangeData
    -> Property
prop_makeChange_fail_minValueTooBig p =
    let makeChangeData = p
            { requiredCost = Coin 0
            , minCoinFor = MockComputeMinimumAdaQuantityZero
            }
    in
    case makeChangeWith makeChangeData of
        Left{} ->
            property False & counterexample "makeChange failed with no cost!"
        -- If 'makeChange' failed to generate change, we try to re-run it with
        -- computeMinimumCostZero and noMinValue requirement.
        -- The result _must_ be 'Just'.
        --
        -- From there, we can manually compute the total deposit needed for all
        -- change generated and make sure that there were indeed not enough
        -- coins available to generate all change outputs.
        Right change ->
            conjoin
                [ deltaCoin <
                    totalMinCoinDeposit `Coin.add` view #requiredCost p
                , deltaCoin >=
                    view #requiredCost p
                ]
                & counterexample counterexampleText
          where
            counterexampleText = unlines
                [ "change:"
                , pretty (blockListF (Flat <$> change))
                , "delta:"
                , pretty deltaCoin
                , "totalMinCoinDeposit:"
                , pretty totalMinCoinDeposit
                ]
            deltaCoin = TokenBundle.getCoin $ TokenBundle.unsafeSubtract
                totalInputValue
                totalOutputValue
            minCoinValueFor =
                unMockComputeMinimumAdaQuantity (minCoinFor p)
            totalMinCoinDeposit = F.foldr Coin.add (Coin 0)
                (minCoinValueFor . view #tokens <$> change)
  where
    totalInputValue =
        F.fold (inputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalOutputValue =
        F.fold (outputBundles p)
            <> TokenBundle.fromCoin (view #extraCoinSink p)
            <> TokenBundle.fromTokenMap (view #assetsToBurn p)

unit_makeChange
    :: [Expectation]
unit_makeChange =
    [ makeChange criteria `shouldBe` expectation
    | ( minCoinFor
      , extraCoinSource
      , extraCoinSink
      , i
      , o
      , expectation
      ) <- matrix
    , let criteria = MakeChangeCriteria
              { minCoinFor
              , requiredCost = Coin 0
              , extraCoinSource
              , extraCoinSink
              , bundleSizeAssessor
              , inputBundles = i
              , outputBundles = o
              , assetsToMint = TokenMap.empty
              , assetsToBurn = TokenMap.empty
              }
    ]
  where
    bundleSizeAssessor =
        mkTokenBundleSizeAssessor MockAssessTokenBundleSizeUnlimited
    matrix =
        -- Simple, only ada, should construct a single change output with 1 ada.
        [ ( computeMinimumAdaQuantityZero
          , Coin 0
          , Coin 0
          , b 2 [] :| []
          , b 1 [] :| []
          , Right [b 1 []]
          )

        -- Two outputs, no cost, changes are proportional, no extra assets
        , ( computeMinimumAdaQuantityZero
          , Coin 0
          , Coin 0
          , b 9 [(assetA, 9), (assetB, 6)] :| []
          , b 2 [(assetA, 1)] :| [b 1 [(assetA, 2), (assetB, 3)]]
          , Right
              [ b 4 [(assetA, 2)]
              , b 2 [(assetA, 4), (assetB, 3)]
              ]
          )

        -- Extra non-user-specified assets. Large assets end up in 'large'
        -- bundles and small extra assets in smaller bundles.
        , ( computeMinimumAdaQuantityZero
          , Coin 0
          , Coin 0
          , b 1 [(assetA, 10), (assetC, 1)] :| [b 1 [(assetB, 2), (assetC, 8)]]
          , b 1 [(assetA, 5)] :| [b 1 [(assetB, 1)]]
          , Right
              [ b 0 [(assetA, 5), (assetC, 1)]
              , b 0 [(assetB, 1), (assetC, 8)]
              ]
          )
        ]

    b :: Word64 -> [(AssetId, Natural)] -> TokenBundle
    b c = TokenBundle (Coin.fromWord64 c)
        . TokenMap.fromFlatList
        . fmap (second TokenQuantity)

    assetA :: AssetId
    assetA = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "1")

    assetB :: AssetId
    assetB = AssetId (UnsafeTokenPolicyId $ Hash "B") (UnsafeTokenName "")

    assetC :: AssetId
    assetC = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "2")

--------------------------------------------------------------------------------
-- Collating non-user-specified asset quantities.
--------------------------------------------------------------------------------

prop_collateNonUserSpecifiedAssetQuantities
    :: NonEmpty TokenMap
    -- ^ Token maps of all selected inputs.
    -> Set AssetId
    -- ^ Set of all assets in user-specified outputs.
    -> Property
prop_collateNonUserSpecifiedAssetQuantities inputMaps userSpecifiedAssetIds =
    checkCoverage $
    cover 40 bothSetsNonEmpty
        "both sets non-empty" $
    cover 1 nonUserSpecifiedAssetIdsEmpty
        "non-user-specified asset id set is empty" $
    cover 1 userSpecifiedAssetIdsEmpty
        "user-specified asset id set is empty" $
    cover 0.1 bothSetsEmpty
        "both sets empty" $
    conjoin
        [ actualResult === expectedResult
        , property $
            userSpecifiedAssetIds `Set.disjoint` nonUserSpecifiedAssetIds
        ]
  where
    actualResult :: Map AssetId (NonEmpty TokenQuantity)
    actualResult =
        collateNonUserSpecifiedAssetQuantities inputMaps userSpecifiedAssetIds

    expectedResult :: Map AssetId (NonEmpty TokenQuantity)
    expectedResult = Map.fromSet getQuantitiesForAsset nonUserSpecifiedAssetIds
      where
        getQuantitiesForAsset assetId = NE.fromList $ NE.filter
            (> TokenQuantity 0)
            ((`TokenMap.getQuantity` assetId) <$> inputMaps)

    nonUserSpecifiedAssetIds :: Set AssetId
    nonUserSpecifiedAssetIds =
        TokenMap.getAssets (F.fold inputMaps)
        `Set.difference`
        userSpecifiedAssetIds

    bothSetsEmpty :: Bool
    bothSetsEmpty = (&&)
        (Set.null userSpecifiedAssetIds)
        (Set.null nonUserSpecifiedAssetIds)

    bothSetsNonEmpty :: Bool
    bothSetsNonEmpty = (&&)
        (not $ Set.null userSpecifiedAssetIds)
        (not $ Set.null nonUserSpecifiedAssetIds)

    nonUserSpecifiedAssetIdsEmpty :: Bool
    nonUserSpecifiedAssetIdsEmpty = (&&)
        (not $ Set.null userSpecifiedAssetIds)
        (Set.null nonUserSpecifiedAssetIds)

    userSpecifiedAssetIdsEmpty :: Bool
    userSpecifiedAssetIdsEmpty = (&&)
        (Set.null userSpecifiedAssetIds)
        (not $ Set.null nonUserSpecifiedAssetIds)

data TestDataForCollateNonUserSpecifiedAssetQuantities =
    TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps
            :: NonEmpty TokenMap
        , userSpecifiedAssetIds
            :: Set AssetId
        , expectedResult
            :: Map AssetId (NonEmpty TokenQuantity)
        }
    deriving (Eq, Generic)

unit_collateNonUserSpecifiedAssetQuantities :: Spec
unit_collateNonUserSpecifiedAssetQuantities =
    forM_ (zip [1..] tests) $ \(testNumber :: Int, test) -> do
        let title = "Unit test #" <> show testNumber
        it title $ property $
            collateNonUserSpecifiedAssetQuantities
                (view #selectedInputMaps test)
                (view #userSpecifiedAssetIds test)
                ===
                (view #expectedResult test)
  where
    mkSelectedInputMaps :: [[(ByteString, Natural)]] -> NonEmpty TokenMap
    mkSelectedInputMaps
        = NE.fromList
        . fmap (TokenMap.fromFlatList . fmap (uncurry mockAssetQuantity))

    mkUserSpecifiedAssetIds :: [ByteString] -> Set AssetId
    mkUserSpecifiedAssetIds
        = Set.fromList . fmap mockAsset

    mkExpectedResult
        :: [(ByteString, [Natural])]
        -> Map AssetId (NonEmpty TokenQuantity)
    mkExpectedResult
        = fmap NE.fromList
        . Map.fromList
        . fmap (bimap mockAsset (fmap TokenQuantity))

    tests :: [TestDataForCollateNonUserSpecifiedAssetQuantities]
    tests = [test1, test2, test3, test4, test5, test6, test7, test8]

    test1 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1)]
            , [("A", 2)]
            , [("A", 3)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            []
        , expectedResult = mkExpectedResult
            [ ("A", [1, 2, 3]) ]
        }

    test2 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1)]
            , [("A", 2)]
            , [("A", 3)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            [ "A" ]
        , expectedResult = mkExpectedResult
            []
        }

    test3 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1), ("B", 3)          ]
            , [          ("B", 4), ("C", 5)]
            , [("A", 2),           ("C", 6)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            []
        , expectedResult = mkExpectedResult
            [ ("A", [1, 2])
            , ("B", [3, 4])
            , ("C", [5, 6])
            ]
        }

    test4 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1), ("B", 3)          ]
            , [          ("B", 4), ("C", 5)]
            , [("A", 2),           ("C", 6)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            [ "A" ]
        , expectedResult = mkExpectedResult
            [ ("B", [3, 4])
            , ("C", [5, 6])
            ]
        }

    test5 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1), ("B", 3)          ]
            , [          ("B", 4), ("C", 5)]
            , [("A", 2),           ("C", 6)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            [ "B" ]
        , expectedResult = mkExpectedResult
            [ ("A", [1, 2])
            , ("C", [5, 6])
            ]
        }

    test6 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1), ("B", 3)          ]
            , [          ("B", 4), ("C", 5)]
            , [("A", 2),           ("C", 6)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            [ "C" ]
        , expectedResult = mkExpectedResult
            [ ("A", [1, 2])
            , ("B", [3, 4])
            ]
        }

    test7 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1)          ]
            , [          ("B", 3)]
            , [("A", 2)          ]
            , [          ("B", 4)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            [ "A" ]
        , expectedResult = mkExpectedResult
            [ ("B", [3, 4]) ]
        }

    test8 = TestDataForCollateNonUserSpecifiedAssetQuantities
        { selectedInputMaps = mkSelectedInputMaps
            [ [("A", 1)          ]
            , [          ("B", 3)]
            , [("A", 2)          ]
            , [          ("B", 4)]
            ]
        , userSpecifiedAssetIds = mkUserSpecifiedAssetIds
            [ "B" ]
        , expectedResult = mkExpectedResult
            [ ("A", [1, 2]) ]
        }

--------------------------------------------------------------------------------
-- Assigning coins to change maps
--------------------------------------------------------------------------------

unit_assignCoinsToChangeMaps
    :: [Expectation]
unit_assignCoinsToChangeMaps =
    [ assignCoinsToChangeMaps total minCoinValueFor assets `shouldBe` expectation
    | (total, minCoinValueFor, assets, expectation) <- matrix
    ]
  where
    matrix =
        -- Simple case with a single Ada-only output
        [ ( Coin 1
          , computeMinimumAdaQuantityLinear
          , m 42 [] :| []
          , Right [b 1 []]
          )

        -- Simple case, with a single MA output
        , ( Coin 2
          , computeMinimumAdaQuantityLinear
          , m 42 [(assetA, 1337)] :| []
          , Right [b 2 [(assetA, 1337)]]
          )

        -- Single Ada-only output, but not enough left to create a change
        , ( Coin 1
          , (`Coin.add` Coin 1) . computeMinimumAdaQuantityLinear
          , m 42 [] :| []
          , Right []
          )

        -- Single MA output, but not enough left to create a change
        , ( Coin 1
          , computeMinimumAdaQuantityLinear
          , m 42 [(assetA, 1337)] :| []
          , Left (Coin 1)
          )

        -- Multiple Ada-only change, not enough Ada left to create them all
        , ( Coin 2
          , computeMinimumAdaQuantityLinear
          , NE.fromList
            [ m 1337 []
            , m   14 []
            , m   42 []
            ]
          , Right [b 1 [], b 1 []]
          )

        -- Hybrid Ada & MA, not enough to cover both => Ada change is dropped
        , ( Coin 2
          , computeMinimumAdaQuantityLinear
          , NE.fromList
            [ m 42 []
            , m 14 []
            , m  2 [(assetA, 1337)]
            ]
          , Right [b 2 [(assetA, 1337)]]
          )
        ]

    m :: Word64 -> [(AssetId, Natural)] -> (TokenMap, Coin)
    m c = (, Coin.fromWord64 c)
        . TokenMap.fromFlatList
        . fmap (second TokenQuantity)

    b :: Word64 -> [(AssetId, Natural)] -> TokenBundle
    b c = TokenBundle (Coin.fromWord64 c)
        . TokenMap.fromFlatList
        . fmap (second TokenQuantity)

    assetA :: AssetId
    assetA = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "1")

--------------------------------------------------------------------------------
-- Making change for coins
--------------------------------------------------------------------------------

prop_makeChangeForCoin_sum :: NonEmpty Coin -> Coin -> Property
prop_makeChangeForCoin_sum weights surplus =
    surplus === F.foldr Coin.add (Coin 0) changes
  where
    changes = makeChangeForCoin weights surplus

prop_makeChangeForCoin_length :: NonEmpty Coin -> Coin -> Property
prop_makeChangeForCoin_length weights surplus =
    F.length changes === F.length weights
  where
    changes = makeChangeForCoin weights surplus

unit_makeChangeForCoin
    :: [Expectation]
unit_makeChangeForCoin =
    [ makeChangeForCoin weights surplus `shouldBe` expectation
    | (weights, surplus, expectation) <- matrix
    ]
  where
    matrix =
        [ ( Coin <$> 1 :| [], Coin 1
          , Coin <$> 1 :| []
          )

        , ( Coin <$> 1 :| [2, 3], Coin 12
          , Coin <$> 2 :| [4, 6]
          )

        , ( Coin <$> 1 :| [2, 3], Coin 5
          , Coin <$> 1 :| [2, 2]
          )
        ]

--------------------------------------------------------------------------------
-- Making change for a single non-user-specified asset
--------------------------------------------------------------------------------

prop_makeChangeForNonUserSpecifiedAsset_sum
    :: NonEmpty ()
    -> (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForNonUserSpecifiedAsset_sum n (asset, quantities) =
    F.fold quantities === F.fold ((`TokenMap.getQuantity` asset) <$> changes)
  where
    changes = makeChangeForNonUserSpecifiedAsset n (asset, quantities)

prop_makeChangeForNonUserSpecifiedAsset_order
    :: NonEmpty ()
    -> (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForNonUserSpecifiedAsset_order n assetQuantities =
    property $ inAscendingPartialOrder
        $ makeChangeForNonUserSpecifiedAsset n assetQuantities

prop_makeChangeForNonUserSpecifiedAsset_length
    :: NonEmpty ()
    -> (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForNonUserSpecifiedAsset_length n surplus =
    F.length changes === F.length n
  where
    changes = makeChangeForNonUserSpecifiedAsset n surplus

unit_makeChangeForNonUserSpecifiedAsset
    :: [Expectation]
unit_makeChangeForNonUserSpecifiedAsset =
    [ makeChangeForNonUserSpecifiedAsset
        (mkChangeMapCount changeMapCount) surplus
        `shouldBe` expectation
    | (changeMapCount, surplus, expectation) <- matrix
    ]
  where
    matrix =
        [ ( 2
          , (assetA, q <$> 1 :| [1])
          , m [(assetA, q 1)] :| [m [(assetA, q 1)]]
          )

        , ( 2
          , (assetA, q <$> 1 :| [1, 1])
          , m [(assetA, q 1)] :| [m [(assetA, q 2)]]
          )

        , ( 2
          , (assetA, q <$> 1 :| [])
          , m [(assetA, q 0)] :| [m [(assetA, q 1)]]
          )
        ]

    mkChangeMapCount :: Int -> NonEmpty ()
    mkChangeMapCount n = NE.fromList $ replicate n ()

    q :: Natural -> TokenQuantity
    q = TokenQuantity

    m :: [(AssetId, TokenQuantity)] -> TokenMap
    m = TokenMap.fromFlatList

    assetA :: AssetId
    assetA = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "1")

--------------------------------------------------------------------------------
-- Making change for multiple non-user-specified assets
--------------------------------------------------------------------------------

checkCoverageFor_makeChangeForNonUserSpecifiedAssets
    :: NonEmpty ()
    -> Map AssetId (NonEmpty TokenQuantity)
    -> Property
    -> Property
checkCoverageFor_makeChangeForNonUserSpecifiedAssets n assetQuantityMap prop =
    checkCoverage $

    -- Number of distinct assets:
    cover 1 (Map.size assetQuantityMap == 1)
        "number of distinct assets == 1" $
    cover 50 (Map.size assetQuantityMap >= 2)
        "number of distinct assets >= 2" $
    cover 10 (Map.size assetQuantityMap >= 4)
        "number of distinct assets >= 4" $

    -- Number of change maps:
    cover 1 (length n == 1)
        "number of change maps == 1" $
    cover 50 (length n >= 2)
        "number of change maps >= 2" $
    cover 10 (length n >= 4)
        "number of change maps >= 4" $

    -- Largest number of distinct token quantities for a given asset:
    cover 1 (largestTokenQuantityCount == 1)
        "largest number of token quantities == 1" $
    cover 50 (largestTokenQuantityCount >= 2)
        "largest number of token quantities >= 2" $
    cover 10 (largestTokenQuantityCount >= 4)
        "largest number of token quantities >= 4"

    prop
  where
    largestTokenQuantityCount :: Int
    largestTokenQuantityCount = maximum (length <$> F.toList (assetQuantityMap))

prop_makeChangeForNonUserSpecifiedAssets_length
    :: NonEmpty ()
    -> NonEmpty (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForNonUserSpecifiedAssets_length n assetQuantities =
    checkCoverageFor_makeChangeForNonUserSpecifiedAssets n assetQuantityMap $
    lengthActual === lengthExpected
  where
    assetQuantityMap :: Map AssetId (NonEmpty TokenQuantity)
    assetQuantityMap = Map.fromList (F.toList assetQuantities)

    lengthActual :: Int
    lengthActual = length
        (makeChangeForNonUserSpecifiedAssets n assetQuantityMap)

    lengthExpected :: Int
    lengthExpected = length n

prop_makeChangeForNonUserSpecifiedAssets_order
    :: NonEmpty ()
    -> NonEmpty (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForNonUserSpecifiedAssets_order n assetQuantities =
    checkCoverageFor_makeChangeForNonUserSpecifiedAssets n assetQuantityMap $
    property $ inAscendingPartialOrder result
  where
    assetQuantityMap :: Map AssetId (NonEmpty TokenQuantity)
    assetQuantityMap = Map.fromList (F.toList assetQuantities)

    result :: NonEmpty TokenMap
    result = makeChangeForNonUserSpecifiedAssets n assetQuantityMap

prop_makeChangeForNonUserSpecifiedAssets_sum
    :: NonEmpty ()
    -> NonEmpty (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForNonUserSpecifiedAssets_sum n assetQuantities =
    checkCoverageFor_makeChangeForNonUserSpecifiedAssets n assetQuantityMap $
    sumActual === sumExpected
  where
    assetQuantityMap :: Map AssetId (NonEmpty TokenQuantity)
    assetQuantityMap = Map.fromList (F.toList assetQuantities)

    sumActual :: TokenMap
    sumActual =
        F.fold $ makeChangeForNonUserSpecifiedAssets n assetQuantityMap

    sumExpected :: TokenMap
    sumExpected =
        TokenMap.fromFlatList (Map.toList $ F.fold <$> assetQuantityMap)

data TestDataForMakeChangeForNonUserSpecifiedAssets =
    TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount
            :: NonEmpty ()
        , nonUserSpecifiedAssetQuantities
            :: Map AssetId (NonEmpty TokenQuantity)
        , expectedResult
            :: NonEmpty TokenMap
        }
    deriving (Eq, Generic)

unit_makeChangeForNonUserSpecifiedAssets :: Spec
unit_makeChangeForNonUserSpecifiedAssets =
    forM_ (zip [1..] tests) $ \(testNumber :: Int, test) -> do
        let title = "Unit test #" <> show testNumber
        it title $ property $
            makeChangeForNonUserSpecifiedAssets
                (view #changeMapCount test)
                (view #nonUserSpecifiedAssetQuantities test)
                ===
                (view #expectedResult test)
  where
    mkChangeMapCount :: Int -> NonEmpty ()
    mkChangeMapCount n = NE.fromList $ replicate n ()

    mkNonUserSpecifiedAssetQuantities
        :: [(ByteString, [Natural])]
        -> Map AssetId (NonEmpty TokenQuantity)
    mkNonUserSpecifiedAssetQuantities =
        Map.fromList . fmap (bimap mockAsset (NE.fromList . fmap TokenQuantity))

    mkExpectedResult
        :: [[(ByteString, Natural)]]
        -> NonEmpty TokenMap
    mkExpectedResult
        = NE.fromList
        . fmap (TokenMap.fromFlatList . fmap (uncurry mockAssetQuantity))

    tests :: [TestDataForMakeChangeForNonUserSpecifiedAssets]
    tests = [test1, test2, test3, test4, test5, test6, test7, test8]

    test1 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            1
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [1])
            , ("B", [3, 2, 1])
            ]
        , expectedResult = mkExpectedResult
            [ [("A", 1), ("B", 6)] ]
        }

    test2 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            2
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [1])
            , ("B", [3, 2, 1])
            ]
        , expectedResult = mkExpectedResult
            [ [          ("B", 3)]
            , [("A", 1), ("B", 3)]
            ]
        }

    test3 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            3
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [1])
            , ("B", [3, 2, 1])
            ]
        , expectedResult = mkExpectedResult
            [ [          ("B", 1)]
            , [          ("B", 2)]
            , [("A", 1), ("B", 3)]
            ]
        }

    test4 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            4
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [1])
            , ("B", [3, 2, 1])
            ]
        , expectedResult = mkExpectedResult
            [ [                  ]
            , [          ("B", 1)]
            , [          ("B", 2)]
            , [("A", 1), ("B", 3)]
            ]
        }

    test5 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            1
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [4, 1, 3, 2])
            , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
            ]
        , expectedResult = mkExpectedResult
            [ [("A", 10), ("B", 45)] ]
        }

    test6 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            2
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [4, 1, 3, 2])
            , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
            ]
        , expectedResult = mkExpectedResult
            [ [("A", 4), ("B", 18)]
            , [("A", 6), ("B", 27)]
            ]
        }

    test7 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            4
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [4, 1, 3, 2])
            , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
            ]
        , expectedResult = mkExpectedResult
            [ [("A", 1), ("B",  9)]
            , [("A", 2), ("B",  9)]
            , [("A", 3), ("B", 12)]
            , [("A", 4), ("B", 15)]
            ]
        }

    test8 = TestDataForMakeChangeForNonUserSpecifiedAssets
        { changeMapCount = mkChangeMapCount
            9
        , nonUserSpecifiedAssetQuantities = mkNonUserSpecifiedAssetQuantities
            [ ("A", [4, 1, 3, 2])
            , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
            ]
        , expectedResult = mkExpectedResult
            [ [          ("B",  1)]
            , [          ("B",  2)]
            , [          ("B",  3)]
            , [          ("B",  4)]
            , [          ("B",  5)]
            , [("A", 1), ("B",  6)]
            , [("A", 2), ("B",  7)]
            , [("A", 3), ("B",  8)]
            , [("A", 4), ("B",  9)]
            ]
        }

--------------------------------------------------------------------------------
-- Making change for known assets
--------------------------------------------------------------------------------

prop_makeChangeForUserSpecifiedAsset_sum
    :: NonEmpty TokenMap
    -> (AssetId, TokenQuantity)
    -> Property
prop_makeChangeForUserSpecifiedAsset_sum weights (asset, quantity) =
    if any (`TokenMap.hasQuantity` asset) weights then
        quantity === totalChangeValue
    else
        totalChangeValue === TokenQuantity 0
  where
    changes = makeChangeForUserSpecifiedAsset weights (asset, quantity)
    totalChangeValue = F.fold ((`TokenMap.getQuantity` asset) <$> changes)

prop_makeChangeForUserSpecifiedAsset_length
    :: NonEmpty TokenMap
    -> (AssetId, TokenQuantity)
    -> Property
prop_makeChangeForUserSpecifiedAsset_length weights surplus =
    F.length changes === F.length weights
  where
    changes = makeChangeForUserSpecifiedAsset weights surplus

unit_makeChangeForUserSpecifiedAsset
    :: [Expectation]
unit_makeChangeForUserSpecifiedAsset =
    [ makeChangeForUserSpecifiedAsset weights surplus `shouldBe` expectation
    | (weights, surplus, expectation) <- matrix
    ]
  where
    matrix =
        [ ( m [(assetA, q 1)] :| []
          , (assetA, q 3)
          , m [(assetA, q 3)] :| []
          )

        , ( m [(assetA, q 1)] :| [m [(assetA, q 2), (assetB, q 1)]]
          , (assetA, q 3)
          , m [(assetA, q 1)] :| [m [(assetA, q 2)]]
          )

        , ( m [(assetA, q 1)] :| [m [(assetB, q 1)]]
          , (assetC, q 1)
          , m [(assetA, q 0)] :| [m [(assetA, q 0)]]
          )
        ]

    q :: Natural -> TokenQuantity
    q = TokenQuantity

    m :: [(AssetId, TokenQuantity)] -> TokenMap
    m = TokenMap.fromFlatList

    assetA :: AssetId
    assetA = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "1")

    assetB :: AssetId
    assetB = AssetId (UnsafeTokenPolicyId $ Hash "B") (UnsafeTokenName "")

    assetC :: AssetId
    assetC = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "2")

--------------------------------------------------------------------------------
-- Splitting bundles with excessive asset counts
--------------------------------------------------------------------------------

prop_splitBundleIfAssetCountExcessive_length
    :: Blind (Large TokenBundle) -> Positive Int -> Property
prop_splitBundleIfAssetCountExcessive_length
    (Blind (Large b)) (Positive maxAssetCount) =
        checkCoverage $ property $
        cover 5 (resultLength == 1)
            "length = 1" $
        cover 5 (resultLength >= 2 && resultLength < 8)
            "length >= 2 && length < 8" $
        cover 5 (resultLength >= 8 && resultLength < 16)
            "length >= 8 && length < 16"
        True
  where
    isExcessive = (> maxAssetCount) . Set.size . TokenBundle.getAssets
    result = splitBundleIfAssetCountExcessive b isExcessive
    resultLength = NE.length result

prop_splitBundleIfAssetCountExcessive_maximalSplitting
    :: Blind (Large TokenBundle) -> Property
prop_splitBundleIfAssetCountExcessive_maximalSplitting (Blind (Large b)) =
    checkCoverage $ property $
    cover 5 (assetCount == 0)
        "asset count = 0" $
    cover 5 (assetCount == 1)
        "asset count = 1" $
    cover 5 (assetCount >= 2 && assetCount < 8)
        "asset count >= 2 && asset count < 8" $
    cover 5 (assetCount >= 8 && assetCount < 16)
        "asset count >= 8 && asset count < 16" $
    (.&&.)
        (NE.length result === max 1 assetCount)
        (F.all ((<= 1) . Set.size . TokenBundle.getAssets) result)
  where
    assetCount = Set.size $ TokenBundle.getAssets b
    isExcessive = (> 1) . Set.size . TokenBundle.getAssets
    result = splitBundleIfAssetCountExcessive b isExcessive

prop_splitBundleIfAssetCountExcessive_postCondition
    :: Blind (Large TokenBundle) -> Positive Int -> Property
prop_splitBundleIfAssetCountExcessive_postCondition
    (Blind (Large b)) (Positive maxAssetCount) =
        property $ F.all (not . isExcessive) results
  where
    isExcessive = (> maxAssetCount) . Set.size . TokenBundle.getAssets
    results = splitBundleIfAssetCountExcessive b isExcessive

prop_splitBundleIfAssetCountExcessive_sum
    :: Blind (Large TokenBundle) -> Positive Int -> Property
prop_splitBundleIfAssetCountExcessive_sum
    (Blind (Large b)) (Positive maxAssetCount) =
        F.fold (splitBundleIfAssetCountExcessive b isExcessive) === b
  where
    isExcessive = (> maxAssetCount) . Set.size . TokenBundle.getAssets

prop_splitBundlesWithExcessiveAssetCounts_length
    :: Blind (NonEmpty TokenBundle) -> Positive Int -> Property
prop_splitBundlesWithExcessiveAssetCounts_length
    (Blind input) (Positive maxAssetCount) =
        checkCoverage $ property $
        cover 5 (lengthOutput > lengthInput)
            "length has increased" $
        cover 5 (lengthOutput == lengthInput)
            "length has remained the same" $
        case compare lengthOutput lengthInput of
            GT -> (&&)
                (F.any isExcessive input)
                (F.all (not . isExcessive) output)
            EQ -> (&&)
                (F.all (not . isExcessive) input)
                (input == output)
            LT ->
                error "length has unexpectedly decreased"
  where
    isExcessive =
        (> maxAssetCount) . Set.size . TokenBundle.getAssets
    lengthInput =
        NE.length input
    lengthOutput =
        NE.length output
    output =
        splitBundlesWithExcessiveAssetCounts input isExcessive

prop_splitBundlesWithExcessiveAssetCounts_sum
    :: Blind (NonEmpty TokenBundle) -> Positive Int -> Property
prop_splitBundlesWithExcessiveAssetCounts_sum
    (Blind bundles) (Positive maxAssetCount) = (===)
        (F.fold $ splitBundlesWithExcessiveAssetCounts bundles isExcessive)
        (F.fold bundles)
  where
    isExcessive = (> maxAssetCount) . Set.size . TokenBundle.getAssets

--------------------------------------------------------------------------------
-- Splitting bundles with excessive token quantities
--------------------------------------------------------------------------------

prop_splitBundlesWithExcessiveTokenQuantities_length
    :: NonEmpty TokenBundle -> TokenQuantity -> Property
prop_splitBundlesWithExcessiveTokenQuantities_length input maxQuantityAllowed =
    maxQuantityAllowed > TokenQuantity.zero ==> checkCoverage $ property $
        cover 5 (lengthOutput > lengthInput)
            "length has increased" $
        cover 5 (lengthOutput == lengthInput)
            "length has remained the same" $
        case compare lengthOutput lengthInput of
            GT -> (&&)
                (maxQuantityAllowed <  maxQuantityInput)
                (maxQuantityAllowed >= maxQuantityOutput)
            EQ -> (&&)
                (maxQuantityAllowed >= maxQuantityInput)
                (input == output)
            LT ->
                error "length has unexpectedly decreased"
  where
    lengthInput =
        NE.length input
    lengthOutput =
        NE.length output
    maxQuantityInput =
        F.maximum (TokenMap.maximumQuantity . view #tokens <$> input)
    maxQuantityOutput =
        F.maximum (TokenMap.maximumQuantity . view #tokens <$> output)
    output =
        splitBundlesWithExcessiveTokenQuantities input maxQuantityAllowed

prop_splitBundlesWithExcessiveTokenQuantities_sum
    :: NonEmpty TokenBundle -> TokenQuantity -> Property
prop_splitBundlesWithExcessiveTokenQuantities_sum ms maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        F.fold (splitBundlesWithExcessiveTokenQuantities ms maxQuantity)
            === F.fold ms

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
-- Round-robin processing
--------------------------------------------------------------------------------

data MockRoundRobinState k n = MockRoundRobinState
    { processorLifetimes :: Map k n
    , accumulatedEntries :: [(k, n)]
    } deriving (Eq, Show)

genMockRoundRobinState
    :: forall k n. Ord k => Gen k -> Gen n -> Gen (MockRoundRobinState k n)
genMockRoundRobinState genKey genLifetime = do
    processorCount <- choose (0, 16)
    MockRoundRobinState
        <$> genProcessorLifetimes processorCount
        <*> pure []
  where
    genProcessorLifetimes :: Int -> Gen (Map k n)
    genProcessorLifetimes processorCount =
        Map.fromList <$> replicateM processorCount genProcessorLifetime

    genProcessorLifetime :: Gen (k, n)
    genProcessorLifetime = (,)
        <$> genKey
        <*> genLifetime

shrinkMockRoundRobinState
    :: Ord k
    => (n -> [n])
    -> MockRoundRobinState k n
    -> [MockRoundRobinState k n]
shrinkMockRoundRobinState shrinkLifetime s =
    [ s { processorLifetimes = processorLifetimes' }
    | processorLifetimes' <- shrinkProcessorLifetimes $ processorLifetimes s
    ]
  where
    shrinkProcessorLifetimes
        = fmap Map.fromList
        . shrinkList shrinkProcessorLifetime
        . Map.toList
    shrinkProcessorLifetime (k, n) = (k, ) <$> shrinkLifetime n

runMockRoundRobin
    :: forall k n. (Ord k, Integral n)
    => MockRoundRobinState k n
    -> MockRoundRobinState k n
runMockRoundRobin initialState = runRoundRobin initialState id processors
  where
    processors :: [MockRoundRobinState k n -> Maybe (MockRoundRobinState k n)]
    processors = mkProcessor <$> Map.toList (processorLifetimes initialState)

    mkProcessor
        :: (k, n) -> MockRoundRobinState k n -> Maybe (MockRoundRobinState k n)
    mkProcessor (k, n) s
        | remainingLifetime <= 0 =
            Nothing
        | otherwise = Just $ MockRoundRobinState
            { processorLifetimes = Map.adjust pred k (processorLifetimes s)
            , accumulatedEntries = entry : accumulatedEntries s
            }
      where
        entry :: (k, n)
        entry = (k, n - remainingLifetime)

        remainingLifetime :: n
        remainingLifetime = Map.findWithDefault 0 k (processorLifetimes s)

prop_runRoundRobin_identity
    :: forall state. (Eq state, Show state) => state -> [()] -> Property
prop_runRoundRobin_identity state processors =
    runRoundRobin state id (const Nothing <$ processors) === state

prop_runRoundRobin_iterationCount
    :: forall k n. (Ord k, Integral n)
    => MockRoundRobinState k n
    -> Property
prop_runRoundRobin_iterationCount initialState = (===)
    (toInteger $ length $ accumulatedEntries finalState)
    (F.sum $ toInteger <$> processorLifetimes initialState)
  where
    finalState = runMockRoundRobin initialState

prop_runRoundRobin_iterationOrder
    :: forall k n. (Ord k, Show k, Integral n, Show n)
    => MockRoundRobinState k n
    -> Property
prop_runRoundRobin_iterationOrder initialState =
    sortDescending entries === entries
  where
    finalState = runMockRoundRobin initialState
    entries = swap <$> accumulatedEntries finalState
    sortDescending = L.sortBy (flip compare)

prop_runRoundRobin_generationCount
    :: forall k n. (Ord k, Show k, Integral n, Show n)
    => MockRoundRobinState k n
    -> Property
prop_runRoundRobin_generationCount initialState =
    Map.filter (> 0) (processorLifetimes initialState)
        === generationCounts
  where
    finalState = runMockRoundRobin initialState
    generationCounts :: Map k n
    generationCounts = accumulatedEntries finalState
        & groupByKey
        & fmap (fromIntegral . NE.length)

prop_runRoundRobin_generationOrder
    :: forall k n. (Ord k, Integral n)
    => MockRoundRobinState k n
    -> Property
prop_runRoundRobin_generationOrder initialState = property $
    all (uncurry Set.isSubsetOf)
        $ consecutivePairs
        $ snd <$> Map.toDescList generations
  where
    finalState = runMockRoundRobin initialState
    generations :: Map n (Set k)
    generations = accumulatedEntries finalState
        & fmap swap
        & groupByKey
        & fmap (Set.fromList . F.toList)

--------------------------------------------------------------------------------
-- Selection limits
--------------------------------------------------------------------------------

prop_reduceSelectionLimitBy_coverage_limit :: SelectionLimit -> Property
prop_reduceSelectionLimitBy_coverage_limit limit =
    checkCoverage $
    cover 10 (haveLimit)
        "have limit" $
    cover 10 (not haveLimit)
        "do not have limit" $
    property True
  where
    haveLimit :: Bool
    haveLimit = case limit of
        NoLimit -> False
        MaximumInputLimit _ -> True

prop_reduceSelectionLimitBy_coverage_reduction :: Int -> Property
prop_reduceSelectionLimitBy_coverage_reduction reduction =
    checkCoverage $
    cover 10 (reduction < 0)
        "reduction < 0" $
    cover 1 (reduction == 0)
        "reduction = 0" $
    cover 10 (reduction > 0)
        "reduction > 0" $
    property True

prop_reduceSelectionLimitBy_lessThanOrEqual
    :: SelectionLimit -> Int -> Property
prop_reduceSelectionLimitBy_lessThanOrEqual limit reduction =
    prop_reduceSelectionLimitBy_coverage_limit limit .&&.
    prop_reduceSelectionLimitBy_coverage_reduction reduction .&&.
    limit `reduceSelectionLimitBy` reduction <= limit

prop_reduceSelectionLimitBy_reductionNegative
    :: SelectionLimit -> Negative Int -> Property
prop_reduceSelectionLimitBy_reductionNegative limit (Negative reduction) =
    prop_reduceSelectionLimitBy_coverage_limit limit .&&.
    limit `reduceSelectionLimitBy` reduction == limit

prop_reduceSelectionLimitBy_reductionZero
    :: SelectionLimit -> Property
prop_reduceSelectionLimitBy_reductionZero limit =
    prop_reduceSelectionLimitBy_coverage_limit limit .&&.
    limit `reduceSelectionLimitBy` 0 == limit

prop_reduceSelectionLimitBy_reductionPositive
    :: SelectionLimit -> Positive Int -> Property
prop_reduceSelectionLimitBy_reductionPositive limit (Positive reduction) =
    prop_reduceSelectionLimitBy_coverage_limit limit .&&.
    limit == fmap (+ reduction) (limit `reduceSelectionLimitBy` reduction)

--------------------------------------------------------------------------------
-- Testing utility functions
--------------------------------------------------------------------------------

-- | Behaves the same as the original 'mapMaybe' on list.
prop_mapMaybe_oracle :: NonEmpty Int -> Fun Int (Maybe Int) -> Property
prop_mapMaybe_oracle xs fn =
    Maybe.mapMaybe (applyFun fn) (NE.toList xs)
    ===
    mapMaybe (applyFun fn) xs

--------------------------------------------------------------------------------
-- Testing change map mint/burn functions
--------------------------------------------------------------------------------

-- The total value of the change maps after calling this function increases by
-- the value of the minted tokens exactly. i.e. The value of the change maps
-- after calling this function is equivalent of just adding the value to the
-- change maps.
prop_addMintValueToChangeMaps_value
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap -> Property
prop_addMintValueToChangeMaps_value (assetId, qty) changeMaps =
    F.fold changeMaps <> TokenMap.singleton assetId qty
    ===
    F.fold (addMintValueToChangeMaps (assetId, qty) changeMaps)

-- Add a mint value to the change maps does not change their length (length is
-- determined entirely by the number of outputs to cover).
prop_addMintValueToChangeMaps_length
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap
    -> Property
prop_addMintValueToChangeMaps_length mint changeMaps =
    NE.length changeMaps
    ===
    NE.length (addMintValueToChangeMaps mint changeMaps)

-- Adding a mint value to the change maps preserves the ascending partial order
-- of the change maps.
prop_addMintValueToChangeMaps_order
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap
    -> Property
prop_addMintValueToChangeMaps_order mint changeMapDiffs =
    property
        $ inAscendingPartialOrder
        $ addMintValueToChangeMaps mint changeMaps
  where
    -- A list of change maps already in ascending partial order
    changeMaps = NE.scanl (<>) TokenMap.empty changeMapDiffs

-- The plural of this function is equivalent to calling the singular multiple
-- times. This is an important property because we only test the properties on
-- the singular, but use the plural in our code. If the plural is equivalent to
-- the singular, the properties tested on the singular will hold for the plural.
prop_addMintValuesToChangeMaps :: TokenMap -> NonEmpty TokenMap -> Property
prop_addMintValuesToChangeMaps mints changeMaps =
    F.foldr addMintValueToChangeMaps changeMaps (TokenMap.toFlatList mints)
    ===
    addMintValuesToChangeMaps mints changeMaps

-- The total value of the change maps after calling this function decreases by
-- the value of the burned tokens exactly. i.e. The value of the change maps
-- after calling this function is equivalent of just removing the burned value
-- from the change maps.
prop_removeBurnValueFromChangeMaps_value
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap
    -> Property
prop_removeBurnValueFromChangeMaps_value (assetId, qty) changeMaps =
    F.fold changeMaps `TokenMap.difference` TokenMap.singleton assetId qty
    ===
    F.fold (removeBurnValueFromChangeMaps (assetId, qty) changeMaps)

-- Removing a burned value from the change maps does not change their length
-- (length is determined entirely by the number of outputs to cover).
prop_removeBurnValueFromChangeMaps_length
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap
    -> Property
prop_removeBurnValueFromChangeMaps_length burn changeMaps =
    NE.length changeMaps
    ===
    NE.length (removeBurnValueFromChangeMaps burn changeMaps)

-- Removing a burned value from the change maps preserves the ascending partial
-- order of the change maps.
prop_removeBurnValueFromChangeMaps_order
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap
    -> Property
prop_removeBurnValueFromChangeMaps_order burn changeMapDiffs =
    property
        $ inAscendingPartialOrder
        $ removeBurnValueFromChangeMaps burn changeMaps
  where
    -- A list of change maps already in ascending partial order
    changeMaps = NE.scanl (<>) TokenMap.empty changeMapDiffs

-- The plural of this function is equivalent to calling the singular multiple
-- times. This is an important property because we only test the properties on
-- the singular, but use the plural in our code. If the plural is equivalent to
-- the singular, the properties tested on the singular will hold for the plural.
prop_removeBurnValuesFromChangeMaps :: TokenMap -> NonEmpty TokenMap -> Property
prop_removeBurnValuesFromChangeMaps burns changeMaps =
    F.foldr removeBurnValueFromChangeMaps changeMaps (TokenMap.toFlatList burns)
    ===
    removeBurnValuesFromChangeMaps burns changeMaps

-- reduceTokenQuantities reduces the total value of the token quantity list by
-- the amount it was asked to.
prop_reduceTokenQuantities_value
    :: TokenQuantity -> NonEmpty TokenQuantity -> Property
prop_reduceTokenQuantities_value reduceQty qtys =
    F.fold qtys `TokenQuantity.difference` reduceQty
    ===
    F.fold (reduceTokenQuantities reduceQty qtys)

-- The length of the token quantity list is preserved when reducing quantities.
prop_reduceTokenQuantities_length
    :: TokenQuantity -> NonEmpty TokenQuantity -> Property
prop_reduceTokenQuantities_length reduceQty qtys =
    NE.length qtys
    ===
    NE.length (reduceTokenQuantities reduceQty qtys)

-- If the token quantity list is in ascending order, "reduceTokenQuantities"
-- preserves the order of the list.
prop_reduceTokenQuantities_order
    :: TokenQuantity
    -> NonEmpty TokenQuantity
    -> Property
prop_reduceTokenQuantities_order reduceQty qtyDiffs =
    property
        $ inAscendingOrder
        $ reduceTokenQuantities reduceQty qtys
  where
    -- Returns 'True' if (and only if) the given list is in ascending order.
    inAscendingOrder xs = NE.sort xs == xs

    -- A list of quantities already in ascending order.
    qtys = NE.scanl (<>) TokenQuantity.zero qtyDiffs

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

assertWith :: Monad m => String -> Bool -> PropertyM m ()
assertWith description condition = do
    monitor $ counterexample ("Assertion failed: " <> description)
    assert condition

adjustAllTokenBundleQuantities
    :: (Natural -> Natural) -> TokenBundle -> TokenBundle
adjustAllTokenBundleQuantities f b = uncurry TokenBundle.fromFlatList $ bimap
    (adjustCoin)
    (fmap (fmap adjustTokenQuantity))
    (TokenBundle.toFlatList b)
  where
    adjustCoin :: Coin -> Coin
    adjustCoin = Coin . fromIntegral . f . fromIntegral . unCoin

    adjustTokenQuantity :: TokenQuantity -> TokenQuantity
    adjustTokenQuantity = TokenQuantity . f . unTokenQuantity

adjustAllTokenMapQuantities
    :: (Natural -> Natural) -> TokenMap -> TokenMap
adjustAllTokenMapQuantities f m = view #tokens
    $ adjustAllTokenBundleQuantities f
    $ TokenBundle.fromTokenMap m

cutAssetSetSizeInHalf :: TokenBundle -> TokenBundle
cutAssetSetSizeInHalf = uncurry TokenBundle.fromFlatList
    . second cutListInHalf
    . TokenBundle.toFlatList

cutListInHalf :: [a] -> [a]
cutListInHalf xs = take half xs
  where
    half = length xs `div` 2

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

expectRight :: Either a b -> b
expectRight = \case
    Left _a -> error "Expected right"
    Right b -> b

matchSingletonList :: NonEmpty a -> Maybe a
matchSingletonList = \case
    a :| [] -> Just a
    _   -> Nothing

mockAsset :: ByteString -> AssetId
mockAsset a = AssetId (UnsafeTokenPolicyId $ Hash a) (UnsafeTokenName "1")

mockAssetQuantity :: ByteString -> Natural -> (AssetId, TokenQuantity)
mockAssetQuantity a q = (mockAsset a, TokenQuantity q)

unitTests :: String -> [Expectation] -> SpecWith ()
unitTests lbl cases =
    forM_ (zip [1..] cases) $ \(i, test) ->
        it (lbl <> " example #" <> show @Int i) test

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (AssetCount a) where
    arbitrary = AssetCount <$> arbitrary
    shrink = fmap AssetCount . shrink . unAssetCount

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

instance Arbitrary MakeChangeData where
    arbitrary = genMakeChangeData

instance Arbitrary (MockRoundRobinState TokenName Word8) where
    arbitrary = genMockRoundRobinState genTokenName arbitrary
    shrink = shrinkMockRoundRobinState shrink

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRangePositive
    shrink = shrinkTokenBundleSmallRangePositive

instance Arbitrary (Large TokenBundle) where
    arbitrary = fmap Large $ TokenBundle
        <$> genCoinPositive
        <*> genTokenMapLarge
    -- No shrinking

genTokenMapLarge :: Gen TokenMap
genTokenMapLarge = do
    assetCount <- frequency
        [ (1, pure 0)
        , (1, pure 1)
        , (8, choose (2, 63))
        ]
    TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantityPositive

instance Arbitrary SelectionLimit where
    arbitrary = genSelectionLimit
    shrink = shrinkSelectionLimit

instance Arbitrary TokenMap where
    arbitrary = genTokenMapSmallRange
    shrink = shrinkTokenMap

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantityPositive
    shrink = shrinkTokenQuantityPositive

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut

instance Arbitrary (UTxOSelection InputId) where
    arbitrary = genUTxOSelection
    shrink = shrinkUTxOSelection

newtype Large a = Large
    { getLarge :: a }
    deriving (Eq, Show)

newtype Small a = Small
    { getSmall:: a }
    deriving (Eq, Show)

instance Arbitrary (Large (SelectionParams Address InputId)) where
    arbitrary = Large <$> genSelectionParams
        (genInputIdFunction (arbitrary @Bool))
        (genUTxOIndexLarge)
    shrink = shrinkMapBy Large getLarge shrinkSelectionParams

instance Arbitrary (Small (SelectionParams Address InputId)) where
    arbitrary = Small <$> genSelectionParams
        (genInputIdFunction (arbitrary @Bool))
        (genUTxOIndex)
    shrink = shrinkMapBy Small getSmall shrinkSelectionParams

instance Arbitrary (Large (UTxOIndex InputId)) where
    arbitrary = Large <$> genUTxOIndexLarge
    shrink = shrinkMapBy Large getLarge shrinkUTxOIndex

instance Arbitrary (Small (UTxOIndex InputId)) where
    arbitrary = Small <$> genUTxOIndex
    shrink = shrinkMapBy Small getSmall shrinkUTxOIndex

instance Arbitrary Coin where
    arbitrary = genCoinPositive
    shrink = shrinkCoinPositive

instance Arbitrary MockSelectionConstraints where
    arbitrary = genMockSelectionConstraints
    shrink = shrinkMockSelectionConstraints

instance Arbitrary MockComputeMinimumAdaQuantity where
    arbitrary = genMockComputeMinimumAdaQuantity
    shrink = shrinkMockComputeMinimumAdaQuantity

instance Arbitrary MockComputeMinimumCost where
    arbitrary = genMockComputeMinimumCost
    shrink = shrinkMockComputeMinimumCost

instance Arbitrary SelectionStrategy where
    arbitrary = genSelectionStrategy
    shrink = shrinkSelectionStrategy
