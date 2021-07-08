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

module Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobinSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( inAscendingPartialOrder )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( AssetCount (..)
    , BalanceInsufficientError (..)
    , InsufficientMinCoinValueError (..)
    , MakeChangeCriteria (..)
    , OutputsInsufficientError (..)
    , SelectionCriteria (..)
    , SelectionError (..)
    , SelectionInsufficientError (..)
    , SelectionLens (..)
    , SelectionLimit (..)
    , SelectionResult (..)
    , SelectionSkeleton (..)
    , SelectionState (..)
    , UnableToConstructChangeError (..)
    , addMintValueToChangeMaps
    , addMintValuesToChangeMaps
    , assetSelectionLens
    , assignCoinsToChangeMaps
    , balanceMissing
    , coinSelectionLens
    , collateNonUserSpecifiedAssetQuantities
    , fullBalance
    , groupByKey
    , makeChange
    , makeChangeForCoin
    , makeChangeForNonUserSpecifiedAsset
    , makeChangeForNonUserSpecifiedAssets
    , makeChangeForUserSpecifiedAsset
    , mapMaybe
    , missingOutputAssets
    , performSelection
    , prepareOutputsWith
    , reduceTokenQuantities
    , removeBurnValueFromChangeMaps
    , removeBurnValuesFromChangeMaps
    , runRoundRobin
    , runSelection
    , runSelectionStep
    , splitBundleIfAssetCountExcessive
    , splitBundlesWithExcessiveAssetCounts
    , splitBundlesWithExcessiveTokenQuantities
    , ungroupByKey
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinLargePositive
    , genCoinSmall
    , genCoinSmallPositive
    , shrinkCoinSmallPositive
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange
    , genAssetIdSmallRange
    , genTokenMapSmallRange
    , shrinkAssetIdSmallRange
    , shrinkTokenMapSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameMediumRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmallPositive, shrinkTokenQuantitySmallPositive )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxIn (..)
    , TxOut (..)
    , txOutCoin
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOutSmallRange, shrinkTxOutSmallRange )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( SelectionFilter (..), UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexLarge
    , genUTxOIndexLargeN
    , genUTxOIndexSmall
    , shrinkUTxOIndexSmall
    )
import Control.Monad
    ( forM_, replicateM )
import Data.Bifunctor
    ( bimap, second )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( on, (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Semigroup
    ( mtimesDefault )
import Data.Set
    ( Set )
import Data.Tuple
    ( swap )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( blockListF, pretty )
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
    , Positive (..)
    , Property
    , applyFun
    , arbitraryBoundedEnum
    , arbitrarySizedNatural
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , disjoin
    , frequency
    , generate
    , genericShrink
    , ioProperty
    , label
    , oneof
    , property
    , shrinkIntegral
    , shrinkList
    , sublistOf
    , suchThat
    , tabulate
    , withMaxSuccess
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Classes
    ( eqLaws, ordLaws )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobinSpec" $

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

    parallel $ describe "Ordering of token maps" $ do

        it "prop_AssetCount_TokenMap_placesEmptyMapsFirst" $
            property prop_AssetCount_TokenMap_placesEmptyMapsFirst

    parallel $ describe "Preparing outputs" $ do

        it "prop_prepareOutputWith_twice" $
            property prop_prepareOutputsWith_twice
        it "prop_prepareOutputsWith_length" $
            property prop_prepareOutputsWith_length
        it "prop_prepareOutputsWith_assetsUnchanged" $
            property prop_prepareOutputsWith_assetsUnchanged
        it "prop_prepareOutputsWith_preparedOrExistedBefore" $
            property prop_prepareOutputsWith_preparedOrExistedBefore

    parallel $ describe "Performing a selection" $ do

        it "prop_performSelection_small" $
            property prop_performSelection_small
        it "prop_performSelection_large" $
            property prop_performSelection_large
        it "prop_performSelection_huge" $ ioProperty $ do
            -- The UTxO index is generated outside of the property here to avoid
            -- the cost of re-generating it on every pass. This would still
            -- generate interesting cases since the selection within that large
            -- index is random. Plus, other selection criteria still vary.
            utxoAvailable <- generate (genUTxOIndexLargeN 50000)
            pure $ property $ \minCoin costFor (Large criteria) ->
                let
                    criteria' = Blind $ criteria { utxoAvailable }
                in
                    prop_performSelection minCoin costFor criteria' (const id)
                        & withMaxSuccess 5

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

    parallel $ describe "Running a selection step" $ do

        it "prop_runSelectionStep_supplyExhausted" $
            property prop_runSelectionStep_supplyExhausted
        it "prop_runSelectionStep_notYetEnoughToSatisfyMinimum" $
            property prop_runSelectionStep_notYetEnoughToSatisfyMinimum
        it "prop_runSelectionStep_getsCloserToTargetButDoesNotExceedIt" $
            property prop_runSelectionStep_getsCloserToTargetButDoesNotExceedIt
        it "prop_runSelectionStep_getsCloserToTargetAndExceedsIt" $
            property prop_runSelectionStep_getsCloserToTargetAndExceedsIt
        it "prop_runSelectionStep_exceedsTargetAndGetsFurtherAway" $
            property prop_runSelectionStep_exceedsTargetAndGetsFurtherAway

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

prop_Small_UTxOIndex_coverage :: Small UTxOIndex -> Property
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
        $ cover 60 (entryCount > 16)
            "UTxO set size > 16 entries"
        $ cover 20 (entryCount > 32)
            "UTxO set size > 32 entries"
        True
  where
    assetCount = Set.size $ UTxOIndex.assets index
    entryCount = UTxOIndex.size index

prop_Large_UTxOIndex_coverage :: Large UTxOIndex -> Property
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
        $ cover 40 (isEmptyMap $ NE.head maps)
            "head element is empty map"
        $ cover 40 (not $ isEmptyMap $ NE.head maps)
            "head element is non-empty map"
        $ cover 40 (isEmptyMap $ NE.last maps)
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
-- Preparing outputs
--------------------------------------------------------------------------------

prop_prepareOutputsWith_twice
    :: MinCoinValueFor
    -> NonEmpty TxOut
    -> Property
prop_prepareOutputsWith_twice minCoinValueDef outs =
    once === twice
  where
    minCoinValueFor = mkMinCoinValueFor minCoinValueDef
    (_:once:twice:_) = iterate (prepareOutputsWith minCoinValueFor) outs

prop_prepareOutputsWith_length
    :: MinCoinValueFor
    -> NonEmpty TxOut
    -> Property
prop_prepareOutputsWith_length minCoinValueDef outs =
    F.length (prepareOutputsWith minCoinValueFor outs) === F.length outs
  where
    minCoinValueFor = mkMinCoinValueFor minCoinValueDef

prop_prepareOutputsWith_assetsUnchanged
    :: MinCoinValueFor
    -> NonEmpty TxOut
    -> Property
prop_prepareOutputsWith_assetsUnchanged minCoinValueDef outs =
    (txOutAssets <$> (prepareOutputsWith minCoinValueFor outs))
    ===
    (txOutAssets <$> outs)
  where
    minCoinValueFor = mkMinCoinValueFor minCoinValueDef
    txOutAssets = TokenBundle.getAssets . view #tokens

prop_prepareOutputsWith_preparedOrExistedBefore
    :: MinCoinValueFor
    -> NonEmpty TxOut
    -> Property
prop_prepareOutputsWith_preparedOrExistedBefore minCoinValueDef outs =
    property $ F.all isPreparedOrExistedBefore (NE.zip outs outs')
  where
    minCoinValueFor = mkMinCoinValueFor minCoinValueDef
    outs' = prepareOutputsWith minCoinValueFor outs

    isPreparedOrExistedBefore :: (TxOut, TxOut) -> Bool
    isPreparedOrExistedBefore (before, after)
        | txOutCoin before /= Coin 0 =
            txOutCoin after == txOutCoin before
        | otherwise =
            txOutCoin after == minCoinValueFor (view (#tokens . #tokens) before)

--------------------------------------------------------------------------------
-- Performing a selection
--------------------------------------------------------------------------------

-- | The result of calling 'performSelection'.
--
-- We define this type alias to shorten type signatures.
--
type PerformSelectionResult =
    Either SelectionError (SelectionResult TokenBundle)

genSelectionCriteria :: Gen UTxOIndex -> Gen SelectionCriteria
genSelectionCriteria genUTxOIndex = do
    utxoAvailable <- genUTxOIndex
    outputCount <- max 1 <$>
        choose (1, UTxOIndex.size utxoAvailable `div` 8)
    outputsToCover <- NE.fromList <$>
        replicateM outputCount genTxOutSmallRange
    selectionLimit <- frequency
        [ (5, pure NoLimit)
        , (1, pure $ MaximumInputLimit 0)
        , (1, pure $ MaximumInputLimit (UTxOIndex.size utxoAvailable))
        , (4, MaximumInputLimit <$> choose
            (1, UTxOIndex.size utxoAvailable `div` 8)
          )
        ]
    extraCoinSource <- oneof [ pure Nothing, Just <$> genCoinSmall ]
    (assetsToMint, assetsToBurn) <-
        genAssetsToMintAndBurn utxoAvailable outputsToCover
    pure $ SelectionCriteria
        { outputsToCover
        , utxoAvailable
        , extraCoinSource
        , selectionLimit
        , assetsToMint
        , assetsToBurn
        }
  where
    genAssetsToMintAndBurn
        :: UTxOIndex
        -> NonEmpty TxOut
        -> Gen (TokenMap, TokenMap)
    genAssetsToMintAndBurn utxoAvailable outputsToCover =
        frequency
            [ (95, genForSuccess)
            , ( 5, genForFailureWhereSomeMintedAssetsNotSpentOrBurned)
            ]
      where
        assetsProvidedByUTxO =
            view #tokens $ UTxOIndex.balance utxoAvailable
        assetsSpentByUserSpecifiedOutputs =
            F.foldMap (view (#tokens . #tokens)) outputsToCover

        -- To make a successful coin selection, we must satisfy the following
        -- inequalities:
        --
        -- (assetsInUTxO ∪ assetsToMint) ⊇ (assetsInOutputs ∪ assetsToBurn)
        --                 assetsToMint  ⊆ (assetsInOutputs ∪ assetsToBurn)
        --
        genForSuccess :: Gen (TokenMap, TokenMap)
        genForSuccess = do
            let assetsAvailableToBurn = TokenMap.difference
                    assetsProvidedByUTxO
                    assetsSpentByUserSpecifiedOutputs
            assetsToBurn <- TokenMap.fromFlatList <$>
                sublistOf (TokenMap.toFlatList assetsAvailableToBurn)
            let assetsAvailableToMint = TokenMap.add
                    assetsToBurn
                    assetsSpentByUserSpecifiedOutputs
            assetsToMint <- TokenMap.fromFlatList <$>
                sublistOf (TokenMap.toFlatList assetsAvailableToMint)
            pure (assetsToMint, assetsToBurn)

        -- For this generator, we purposefully violate the following condition:
        --
        -- assetsToMint ⊆ (assetsInOutputs ∪ assetsToBurn)
        --
        -- This allows us to provoke the 'OutputsInsufficient' error.
        --
        genForFailureWhereSomeMintedAssetsNotSpentOrBurned
            :: Gen (TokenMap, TokenMap)
        genForFailureWhereSomeMintedAssetsNotSpentOrBurned = do
            let assetsAvailableToBurn = TokenMap.difference
                    assetsProvidedByUTxO
                    assetsSpentByUserSpecifiedOutputs
            assetsToBurn <- TokenMap.fromFlatList <$>
                sublistOf (TokenMap.toFlatList assetsAvailableToBurn)
            let assetsAvailableToMint = TokenMap.add
                    assetsToBurn
                    assetsSpentByUserSpecifiedOutputs
            -- Here we deliberately mint more than we have spent and burned:
            let assetsToMint = mtimesDefault (2 :: Int) assetsAvailableToMint
            pure (assetsToMint, assetsToBurn)

balanceSufficient :: SelectionCriteria -> Bool
balanceSufficient criteria =
    balanceRequired `leq` balanceAvailable
  where
    SelectionCriteria
        { outputsToCover
        , utxoAvailable
        , extraCoinSource
        , assetsToMint
        , assetsToBurn
        } = criteria
    balanceRequired =
        F.foldMap (view #tokens) outputsToCover
            <> TokenBundle.fromTokenMap assetsToBurn
    balanceAvailable =
        fullBalance utxoAvailable extraCoinSource
            <> TokenBundle.fromTokenMap assetsToMint

prop_performSelection_small
    :: MinCoinValueFor
    -> CostFor
    -> Blind (Small SelectionCriteria)
    -> Property
prop_performSelection_small minCoinValueFor costFor (Blind (Small criteria)) =
    checkCoverage $

    -- Inspect the balance:
    cover 30 (balanceSufficient criteria)
        "balance sufficient" $
    cover 25 (not $ balanceSufficient criteria)
        "balance insufficient" $

    -- Inspect the UTxO and user-specified outputs:
    cover 5 (utxoHasAtLeastOneAsset)
        "UTxO has at least one assest" $
    cover 5 (not outputsHaveAtLeastOneAsset)
        "No assets to cover" $
    cover 2 (outputsHaveAtLeastOneAsset && not utxoHasAtLeastOneAsset)
        "Assets to cover, but no assets in UTxO" $

    -- Inspect the sets of minted and burned assets:
    cover 20 (view #assetsToMint criteria /= TokenMap.empty)
        "Have some assets to mint" $
    cover 20 (view #assetsToBurn criteria /= TokenMap.empty)
        "Have some assets to burn" $
    cover 2 (view #assetsToMint criteria == TokenMap.empty)
        "Have no assets to mint" $
    cover 2 (view #assetsToBurn criteria == TokenMap.empty)
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

    prop_performSelection minCoinValueFor costFor (Blind criteria) $ \result ->
        cover 10 (selectionUnlimited && selectionSufficient result)
            "selection unlimited and sufficient"
        . cover 5 (selectionLimited && selectionSufficient result)
            "selection limited but sufficient"
        . cover 10 (selectionLimited && selectionInsufficient result)
            "selection limited and insufficient"
        . cover 2 (outputsInsufficient result)
            "A portion of the minted assets were not spent or burned"
  where
    utxoHasAtLeastOneAsset = not
        . Set.null
        . UTxOIndex.assets
        $ utxoAvailable criteria

    outputsHaveAtLeastOneAsset =
        not . Set.null $ TokenBundle.getAssets outputTokens
      where
        outputTokens = mconcat
            . F.toList
            . fmap (view #tokens)
            $ outputsToCover criteria

    outputsInsufficient :: PerformSelectionResult -> Bool
    outputsInsufficient = \case
        Left (OutputsInsufficient _) -> True
        _ -> False

    selectionLimited :: Bool
    selectionLimited = case selectionLimit criteria of
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
        Left (SelectionInsufficient _) -> True
        _ -> False

    assetsSpentByUserSpecifiedOutputs :: TokenMap
    assetsSpentByUserSpecifiedOutputs =
        F.foldMap (view (#tokens . #tokens)) (outputsToCover criteria)

    someAssetsAreBothMintedAndBurned :: Bool
    someAssetsAreBothMintedAndBurned
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (view #assetsToMint criteria)
            (view #assetsToBurn criteria)

    someAssetsAreBothMintedAndSpent :: Bool
    someAssetsAreBothMintedAndSpent
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (view #assetsToMint criteria)
            (assetsSpentByUserSpecifiedOutputs)

    someAssetsAreBothSpentAndBurned :: Bool
    someAssetsAreBothSpentAndBurned
        = TokenMap.isNotEmpty
        $ TokenMap.intersection
            (assetsSpentByUserSpecifiedOutputs)
            (view #assetsToBurn criteria)

    noAssetsAreBothMintedAndBurned :: Bool
    noAssetsAreBothMintedAndBurned = not someAssetsAreBothMintedAndBurned

    noAssetsAreBothMintedAndSpent :: Bool
    noAssetsAreBothMintedAndSpent = not someAssetsAreBothMintedAndSpent

    noAssetsAreBothSpentAndBurned :: Bool
    noAssetsAreBothSpentAndBurned = not someAssetsAreBothSpentAndBurned

prop_performSelection_large
    :: MinCoinValueFor
    -> CostFor
    -> Blind (Large SelectionCriteria)
    -> Property
prop_performSelection_large minCoinValueFor costFor (Blind (Large criteria)) =
    -- Generation of large UTxO sets takes longer, so limit the number of runs:
    withMaxSuccess 100 $
    checkCoverage $
    cover 50 (balanceSufficient criteria)
        "balance sufficient" $
    prop_performSelection minCoinValueFor costFor (Blind criteria) (const id)

prop_performSelection
    :: MinCoinValueFor
    -> CostFor
    -> Blind SelectionCriteria
    -> (PerformSelectionResult -> Property -> Property)
    -> Property
prop_performSelection minCoinValueFor costFor (Blind criteria) coverage =
    monadicIO $ do
        monitor $ counterexample $ unlines
            [ "extraCoinSource:"
            , show extraCoinSource
            , "selectionLimit:"
            , show selectionLimit
            , "assetsToMint:"
            , pretty (Flat assetsToMint)
            , "assetsToBurn:"
            , pretty (Flat assetsToBurn)
            ]
        result <- run $ performSelection
            (mkMinCoinValueFor minCoinValueFor)
            (mkCostFor costFor)
            (mkBundleSizeAssessor NoBundleSizeLimit)
            (criteria)
        monitor (coverage result)
        either onFailure onSuccess result
  where
    SelectionCriteria
        { outputsToCover
        , utxoAvailable
        , extraCoinSource
        , selectionLimit
        , assetsToMint
        , assetsToBurn
        } = criteria

    onSuccess result = do
        let totalInputValue =
                balanceSelected
                    <> TokenBundle.fromTokenMap assetsToMint
        let totalOutputValue =
                F.foldMap (view #tokens) outputsCovered
                    <> balanceChange
                    <> TokenBundle.fromTokenMap assetsToBurn
        monitor $ counterexample $ unlines
            [ "available balance:"
            , pretty (Flat balanceAvailable)
            , "required balance:"
            , pretty (Flat balanceRequired)
            , "change balance:"
            , pretty (Flat balanceChange)
            , "cost:"
            , pretty expectedCost
            , "absolute minimum coin quantity:"
            , pretty absoluteMinCoinValue
            , "actual coin delta:"
            , pretty (TokenBundle.getCoin delta)
            , "maximum expected delta:"
            , pretty maximumExpectedDelta
            , "number of outputs:"
            , pretty (length outputsCovered)
            , "number of change outputs:"
            , pretty (length changeGenerated)
            ]
        assert $ balanceSufficient criteria
        assert $ on (==) (view #tokens)
            totalInputValue
            totalOutputValue
        assert $ TokenBundle.getCoin delta >= expectedCost
        assert $ TokenBundle.getCoin delta <= maximumExpectedDelta
        assert $ utxoAvailable
            == UTxOIndex.insertMany inputsSelected utxoRemaining
        assert $ utxoRemaining
            == UTxOIndex.deleteMany (fst <$> inputsSelected) utxoAvailable
        assert $ outputsCovered == NE.toList outputsToCover
        case selectionLimit of
            MaximumInputLimit limit ->
                assert $ NE.length inputsSelected <= limit
            NoLimit ->
                assert True
      where
        absoluteMinCoinValue = mkMinCoinValueFor minCoinValueFor TokenMap.empty
        delta :: TokenBundle
        delta =
            balanceSelected
            `TokenBundle.unsafeSubtract`
            (balanceRequired `TokenBundle.add` balanceChange)
        maximumExpectedDelta =
            expectedCost `addCoin`
            (absoluteMinCoinValue `multiplyCoin`
                (length outputsCovered - length changeGenerated))
        multiplyCoin :: Coin -> Int -> Coin
        multiplyCoin (Coin c) i = Coin $ c * fromIntegral i
        SelectionResult
            { inputsSelected
            , changeGenerated
            , outputsCovered
            , utxoRemaining
            } = result
        skeleton = SelectionSkeleton
            { skeletonInputCount =
                length inputsSelected
            , skeletonOutputs =
                NE.toList outputsToCover
            , skeletonChange =
                fmap (TokenMap.getAssets . view #tokens) changeGenerated
            }
        balanceSelected =
            fullBalance (UTxOIndex.fromSequence inputsSelected) extraCoinSource
        balanceChange =
            F.fold changeGenerated
        expectedCost =
            mkCostFor costFor skeleton

    onFailure = \case
        BalanceInsufficient e ->
            onBalanceInsufficient e
        OutputsInsufficient e ->
            onOutputsInsufficient e
        SelectionInsufficient e ->
            onSelectionInsufficient e
        InsufficientMinCoinValues es ->
            onInsufficientMinCoinValues es
        UnableToConstructChange e ->
            onUnableToConstructChange e

    onBalanceInsufficient e = do
        let balanceAvailable' =
                TokenBundle.add (balanceMissing e) balanceAvailable
        monitor $ counterexample $ unlines
            [ "available balance:"
            , pretty (Flat balanceAvailable)
            , "required balance:"
            , pretty (Flat balanceRequired)
            , "missing balance:"
            , pretty (Flat $ balanceMissing e)
            , "missing + available balance:"
            , pretty (Flat balanceAvailable')
            ]
        assert $ not $ balanceSufficient criteria
        assert $ balanceAvailable == errorBalanceAvailable
        assert $ balanceRequired  == errorBalanceRequired
        assert (balanceRequired `leq` balanceAvailable')
      where
        BalanceInsufficientError errorBalanceAvailable errorBalanceRequired = e

    onSelectionInsufficient e = do
        monitor $ counterexample $ unlines
            [ "required balance:"
            , pretty (Flat errorBalanceRequired)
            , "selected balance:"
            , pretty (Flat errorBalanceSelected)
            ]
        assert $ selectionLimit ==
            MaximumInputLimit (length errorInputsSelected)
        assert $ not (errorBalanceRequired `leq` errorBalanceSelected)
        assert $ balanceRequired == errorBalanceRequired
      where
        SelectionInsufficientError
            errorBalanceRequired errorInputsSelected = e
        errorBalanceSelected =
            F.foldMap (view #tokens . snd) errorInputsSelected

    onOutputsInsufficient e = do
        monitor $ counterexample $ unlines
            [ "assets to mint:"
            , pretty (Flat errorAssetsToMint)
            , "assets to burn:"
            , pretty (Flat errorAssetsToBurn)
            , "requested output assets:"
            , pretty (Flat errorRequestedOutputAssets)
            , "assets minted but not spent or burned:"
            , pretty (Flat $ missingOutputAssets e)
            ]
        assert $ errorAssetsToMint == assetsToMint
        assert $ errorAssetsToBurn == assetsToBurn
        assert
            $ not
            $ errorAssetsToMint
                `leq` (errorRequestedOutputAssets <> errorAssetsToBurn)
      where
        OutputsInsufficientError
            errorAssetsToMint errorAssetsToBurn errorRequestedOutputAssets = e

    onInsufficientMinCoinValues es = do
        monitor $ counterexample $ unlines
            [ show es
            , "expected / actual:"
            , show $ NE.zip
                (expectedMinCoinValue <$> es)
                (actualMinCoinValue <$> es)
            ]
        assert $ all (\e -> expectedMinCoinValue e > actualMinCoinValue e) es
      where
        actualMinCoinValue
            = txOutCoin . outputWithInsufficientAda

    onUnableToConstructChange e = do
        monitor $ counterexample $ show e
        assert (shortfall e > Coin 0)
        let criteria' = criteria { selectionLimit = NoLimit }
        let assessBundleSize =
                mkBundleSizeAssessor NoBundleSizeLimit
        let performSelection' = performSelection
                noMinCoin (const noCost) assessBundleSize criteria'
        run performSelection' >>= \case
            Left e' -> do
                monitor $ counterexample $ unlines
                    [ "Failed to re-run selection with no cost!"
                    , show e'
                    ]
                assert False
            Right{} -> do
                assert True

    balanceRequired  =
      F.foldMap (view #tokens) outputsToCover
          `TokenBundle.add`
              (TokenBundle.fromTokenMap assetsToBurn)
          `TokenBundle.unsafeSubtract`
              (TokenBundle.fromTokenMap assetsToMint)
    balanceAvailable = fullBalance utxoAvailable extraCoinSource

--------------------------------------------------------------------------------
-- Running a selection (without making change)
--------------------------------------------------------------------------------

prop_runSelection_UTxO_empty
    :: Maybe Coin
    -> TokenBundle
    -> Property
prop_runSelection_UTxO_empty extraSource balanceRequested = monadicIO $ do
    SelectionState {selected, leftover} <-
        run $ runSelection NoLimit extraSource UTxOIndex.empty balanceRequested
    let balanceSelected = view #balance selected
    let balanceLeftover = view #balance leftover
    assert $ balanceSelected == TokenBundle.empty
    assert $ balanceLeftover == TokenBundle.empty

prop_runSelection_UTxO_notEnough
    :: Small UTxOIndex
    -> Property
prop_runSelection_UTxO_notEnough (Small index) = monadicIO $ do
    SelectionState {selected, leftover} <-
        run $ runSelection NoLimit Nothing index balanceRequested
    let balanceSelected = view #balance selected
    let balanceLeftover = view #balance leftover
    assert $ balanceSelected == balanceAvailable
    assert $ balanceLeftover == TokenBundle.empty
  where
    balanceAvailable = view #balance index
    balanceRequested = adjustAllQuantities (* 2) balanceAvailable

prop_runSelection_UTxO_exactlyEnough
    :: Maybe Coin
    -> Small UTxOIndex
    -> Property
prop_runSelection_UTxO_exactlyEnough extraSource (Small index) = monadicIO $ do
    SelectionState {selected, leftover} <-
        run $ runSelection NoLimit Nothing index balanceRequested
    let balanceSelected = view #balance selected
    let balanceLeftover = view #balance leftover
    assert $ balanceLeftover == TokenBundle.empty
    if UTxOIndex.null index then
        assert $ balanceSelected == TokenBundle.empty
    else
        assert $ addExtraSource extraSource balanceSelected == balanceRequested
  where
    balanceRequested = case extraSource of
        Nothing -> view #balance index
        Just c -> TokenBundle.add (view #balance index) (TokenBundle.fromCoin c)

prop_runSelection_UTxO_moreThanEnough
    :: Maybe Coin
    -> Small UTxOIndex
    -> Property
prop_runSelection_UTxO_moreThanEnough extraSource (Small index) = monadicIO $ do
    SelectionState {selected, leftover} <-
        run $ runSelection NoLimit extraSource index balanceRequested
    let balanceSelected = view #balance selected
    let balanceLeftover = view #balance leftover
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
    assert $ balanceRequested `leq` addExtraSource extraSource balanceSelected
    assert $ balanceAvailable == balanceSelected <> balanceLeftover
  where
    assetsAvailable = TokenBundle.getAssets balanceAvailable
    assetsRequested = TokenBundle.getAssets balanceRequested
    balanceAvailable = view #balance index
    balanceRequested = adjustAllQuantities (`div` 8) $
        cutAssetSetSizeInHalf balanceAvailable

prop_runSelection_UTxO_muchMoreThanEnough
    :: Maybe Coin
    -> Blind (Large UTxOIndex)
    -> Property
prop_runSelection_UTxO_muchMoreThanEnough extraSource (Blind (Large index)) =
    -- Generation of large UTxO sets takes longer, so limit the number of runs:
    withMaxSuccess 100 $
    checkCoverage $
    monadicIO $ do
        SelectionState {selected, leftover} <-
            run $ runSelection NoLimit extraSource index balanceRequested
        let balanceSelected = view #balance selected
        let balanceLeftover = view #balance leftover
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
        assert $
            balanceRequested `leq` addExtraSource extraSource balanceSelected
        assert $
            balanceAvailable == balanceSelected <> balanceLeftover
  where
    assetsAvailable = TokenBundle.getAssets balanceAvailable
    assetsRequested = TokenBundle.getAssets balanceRequested
    balanceAvailable = view #balance index
    balanceRequested = adjustAllQuantities (`div` 256) $
        cutAssetSetSizeInHalf balanceAvailable

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
    }
    deriving (Eq, Show)

runMockSelectionStep :: MockSelectionStepData -> Maybe Natural
runMockSelectionStep d =
    runIdentity $ runSelectionStep lens $ mockSelected d
  where
    lens :: SelectionLens Identity Natural
    lens = SelectionLens
        { currentQuantity = id
        , minimumQuantity = mockMinimum d
        , selectQuantity = \s -> pure $ (+ s) <$> mockNext d
        }

prop_runSelectionStep_supplyExhausted
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_supplyExhausted
    (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === Nothing
  where
    mockData = MockSelectionStepData {..}
    mockSelected = fromIntegral x
    mockMinimum = fromIntegral y
    mockNext = Nothing

prop_runSelectionStep_notYetEnoughToSatisfyMinimum
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_notYetEnoughToSatisfyMinimum
    (Positive x) (Positive y) =
        counterexample (show mockData) $
        runMockSelectionStep mockData === fmap (+ mockSelected) mockNext
  where
    p = fromIntegral $ max x y
    q = fromIntegral $ min x y
    mockData = MockSelectionStepData {..}
    mockSelected = p
    mockMinimum = p + q  + 1
    mockNext = Just q

prop_runSelectionStep_getsCloserToTargetButDoesNotExceedIt
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_getsCloserToTargetButDoesNotExceedIt
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

prop_runSelectionStep_getsCloserToTargetAndExceedsIt
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_getsCloserToTargetAndExceedsIt
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

prop_runSelectionStep_exceedsTargetAndGetsFurtherAway
    :: Positive Word8
    -> Positive Word8
    -> Property
prop_runSelectionStep_exceedsTargetAndGetsFurtherAway
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

--------------------------------------------------------------------------------
-- Behaviour of selection lenses
--------------------------------------------------------------------------------

prop_assetSelectionLens_givesPriorityToSingletonAssets
    :: Blind (Small UTxOIndex)
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
            Just SelectionState {selected} -> do
                let output = head $ snd <$> UTxOIndex.toList selected
                let bundle = view #tokens output
                case F.toList $ TokenBundle.getAssets bundle of
                    [a] -> assert $ a == asset
                    _   -> assert $ not hasSingletonAsset
  where
    asset = Set.findMin $ UTxOIndex.assets u
    assetCount = Set.size $ UTxOIndex.assets u
    initialState = SelectionState UTxOIndex.empty u
    lens = assetSelectionLens NoLimit (asset, minimumAssetQuantity)
    minimumAssetQuantity = TokenQuantity 1

prop_coinSelectionLens_givesPriorityToCoins
    :: Blind (Small UTxOIndex)
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
            Just SelectionState {selected} -> do
                let output = head $ snd <$> UTxOIndex.toList selected
                let bundle = view #tokens output
                case F.toList $ TokenBundle.getAssets bundle of
                    [] -> assert hasCoin
                    _  -> assert $ not hasCoin
  where
    entryCount = UTxOIndex.size u
    initialState = SelectionState UTxOIndex.empty u
    lens = coinSelectionLens NoLimit Nothing minimumCoinQuantity
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
        :: MockTokenBundleSizeAssessor
    , boundaryTestOutputs
        :: [BoundaryTestEntry]
    , boundaryTestUTxO
        :: [BoundaryTestEntry]
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
mkBoundaryTestExpectation (BoundaryTestData criteria expectedResult) = do
    actualResult <- performSelection
        (noMinCoin)
        (mkCostFor NoCost)
        (mkBundleSizeAssessor $ boundaryTestBundleSizeAssessor criteria)
        (encodeBoundaryTestCriteria criteria)
    fmap decodeBoundaryTestResult actualResult `shouldBe` Right expectedResult

encodeBoundaryTestCriteria :: BoundaryTestCriteria -> SelectionCriteria
encodeBoundaryTestCriteria c = SelectionCriteria
    { outputsToCover = NE.fromList $
        zipWith TxOut
            (dummyAddresses)
            (uncurry TokenBundle.fromFlatList <$> boundaryTestOutputs c)
    , utxoAvailable = UTxOIndex.fromSequence $ zip dummyTxIns $
        zipWith TxOut
            (dummyAddresses)
            (uncurry TokenBundle.fromFlatList <$> boundaryTestUTxO c)
    , selectionLimit =
        NoLimit
    , extraCoinSource =
        Nothing
    , assetsToMint =
        TokenMap.empty
    , assetsToBurn =
        TokenMap.empty
    }
  where
    dummyAddresses :: [Address]
    dummyAddresses = [Address (B8.pack $ show x) | x :: Word64 <- [0 ..]]

    dummyTxIns :: [TxIn]
    dummyTxIns = [TxIn (Hash "") x | x <- [0 ..]]

decodeBoundaryTestResult :: SelectionResult TokenBundle -> BoundaryTestResult
decodeBoundaryTestResult r = BoundaryTestResult
    { boundaryTestInputs = L.sort $ NE.toList $
        TokenBundle.toFlatList . view #tokens . snd <$> view #inputsSelected r
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
    (q1, q2) = (TokenQuantity 1, TokenQuantity.pred txOutMaxTokenQuantity)
    boundaryTestBundleSizeAssessor = NoBundleSizeLimit
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
    boundaryTestBundleSizeAssessor = NoBundleSizeLimit
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
    boundaryTestBundleSizeAssessor = NoBundleSizeLimit
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
    boundaryTestBundleSizeAssessor = NoBundleSizeLimit
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
-- See: https://jira.iohk.io/browse/ADP-890
--
boundaryTest_largeTokenQuantities_5 :: BoundaryTestData
boundaryTest_largeTokenQuantities_5 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = NoBundleSizeLimit
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
-- See: https://jira.iohk.io/browse/ADP-890
--
boundaryTest_largeTokenQuantities_6 :: BoundaryTestData
boundaryTest_largeTokenQuantities_6 = BoundaryTestData
    { boundaryTestCriteria = BoundaryTestCriteria {..}
    , boundaryTestExpectedResult = BoundaryTestResult {..}
    }
  where
    boundaryTestBundleSizeAssessor = NoBundleSizeLimit
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
    boundaryTestBundleSizeAssessor = BundleAssetCountUpperLimit 4
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
    boundaryTestBundleSizeAssessor = BundleAssetCountUpperLimit 3
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
    boundaryTestBundleSizeAssessor = BundleAssetCountUpperLimit 2
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
    boundaryTestBundleSizeAssessor = BundleAssetCountUpperLimit 1
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
-- Making change
--------------------------------------------------------------------------------

data MinCoinValueFor
    = NoMinCoin
    | LinearMinCoin
    deriving (Eq, Show, Bounded, Enum)

mkMinCoinValueFor
    :: MinCoinValueFor
    -> (TokenMap -> Coin)
mkMinCoinValueFor = \case
    NoMinCoin -> noMinCoin
    LinearMinCoin -> linearMinCoin

-- | A dummy function for calculating the minimum ada quantity to pay for a
--   token map.
--
-- The only property we want this function to have is that is becomes more
-- expensive with the number of unique assets in the map. So, looking at the
-- size of the asset set is enough.
--
linearMinCoin :: TokenMap -> Coin
linearMinCoin m =
    Coin (1 + fromIntegral (Set.size (TokenMap.getAssets m)))

noMinCoin :: TokenMap -> Coin
noMinCoin = const (Coin 0)

data CostFor
    = NoCost
    | LinearCost
    deriving (Eq, Show, Bounded, Enum)

mkCostFor
    :: CostFor
    -> (SelectionSkeleton -> Coin)
mkCostFor = \case
    NoCost -> const noCost
    LinearCost -> linearCost

noCost :: Coin
noCost = Coin 0

linearCost :: SelectionSkeleton -> Coin
linearCost s
    = Coin
    $ fromIntegral
    $ skeletonInputCount s
    + F.length (skeletonOutputs s)
    + F.length (skeletonChange s)

type MakeChangeData =
    MakeChangeCriteria MinCoinValueFor MockTokenBundleSizeAssessor

data MockTokenBundleSizeAssessor
    = NoBundleSizeLimit
      -- ^ Indicates that there is no limit on a token bundle's size.
    | BundleAssetCountUpperLimit Int
      -- ^ Indicates an inclusive upper bound on the number of assets in a
      -- token bundle.
    deriving (Eq, Show)

mkBundleSizeAssessor
    :: MockTokenBundleSizeAssessor -> TokenBundleSizeAssessor
mkBundleSizeAssessor m = TokenBundleSizeAssessor $ case m of
    NoBundleSizeLimit ->
        const TokenBundleSizeWithinLimit
    BundleAssetCountUpperLimit upperLimit ->
        \bundle ->
            let assetCount = Set.size $ TokenBundle.getAssets bundle in
            case assetCount `compare` upperLimit of
                LT -> TokenBundleSizeWithinLimit
                EQ -> TokenBundleSizeWithinLimit
                GT -> OutputTokenBundleSizeExceedsLimit

isValidMakeChangeData :: MakeChangeData -> Bool
isValidMakeChangeData p = (&&)
    (totalOutputValue `leq` totalInputValue)
    (totalOutputCoinValue > Coin 0)
  where
    totalInputValue =
        F.fold (inputBundles p)
            <> (F.foldMap TokenBundle.fromCoin (view #extraCoinSource p))
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalOutputValue =
        F.fold (outputBundles p)
            <> TokenBundle.fromTokenMap (view #assetsToBurn p)
    totalOutputCoinValue = TokenBundle.getCoin totalOutputValue

genMakeChangeData :: Gen MakeChangeData
genMakeChangeData = flip suchThat isValidMakeChangeData $ do
    outputBundleCount <- choose (0, 15)
    let inputBundleCount = outputBundleCount * 4

    inputBundles <- genTokenBundles inputBundleCount
    outputBundles <- genTokenBundles outputBundleCount

    (assetsToMint, assetsToBurn) <- genAssetsToMintAndBurn
        (F.foldMap (view #tokens) inputBundles)
        (F.foldMap (view #tokens) outputBundles)

    MakeChangeCriteria
        <$> arbitrary
        <*> pure NoBundleSizeLimit
        <*> genCoinSmall
        <*> oneof [pure Nothing, Just <$> genCoinSmallPositive]
        <*> pure inputBundles
        <*> pure outputBundles
        <*> pure assetsToMint
        <*> pure assetsToBurn
  where
    genAssetsToMintAndBurn :: TokenMap -> TokenMap -> Gen (TokenMap, TokenMap)
    genAssetsToMintAndBurn assetsInInputs assetsInOutputs = do
        mintedAndBurned <- frequency
            -- Here we deliberately introduce the possibility that there is
            -- some overlap between minted and burned assets:
            [ (9, pure TokenMap.empty)
            , (1, genTokenMapSmallRange)
            ]
        assetsToMint <- (<> mintedAndBurned) <$> genAssetsToMint
        assetsToBurn <- (<> mintedAndBurned) <$> genAssetsToBurn
        pure (assetsToMint, assetsToBurn)
      where
        genAssetsToMint :: Gen TokenMap
        genAssetsToMint =
            -- Assets in the outputs but not in the inputs can be minted:
            (assetsInOutputs `TokenMap.difference` assetsInInputs)
                & TokenMap.toFlatList
                & sublistOf
                & fmap TokenMap.fromFlatList

        genAssetsToBurn :: Gen TokenMap
        genAssetsToBurn =
            -- Assets in the inputs but not in the outputs can be burned:
            (assetsInInputs `TokenMap.difference` assetsInOutputs)
                & TokenMap.toFlatList
                & sublistOf
                & fmap TokenMap.fromFlatList

    genTokenBundles :: Int -> Gen (NonEmpty TokenBundle)
    genTokenBundles count = (:|)
        <$> genTokenBundleSmallRangePositive
        <*> replicateM count genTokenBundleSmallRangePositive

makeChangeWith
    :: MakeChangeData
    -> Either UnableToConstructChangeError [TokenBundle]
makeChangeWith p = makeChange p
    { minCoinFor = mkMinCoinValueFor $ minCoinFor p
    , bundleSizeAssessor = mkBundleSizeAssessor $ bundleSizeAssessor p
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
        , extraCoinSource = Nothing
        , bundleSizeAssessor = mkBundleSizeAssessor NoBundleSizeLimit
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
            { bundleSizeAssessor = mkBundleSizeAssessor NoBundleSizeLimit }
    mChangeSplit = mChangeUnsplit >>= \changeUnsplit ->
        makeChange zeroCostMakeChangeScenario
            { bundleSizeAssessor = mkBundleSizeAssessor
                $ BundleAssetCountUpperLimit
                $ max 1
                $ (`div` 2)
                $ getLargestAssetSetSize changeUnsplit
            }

    getLargestAssetSetSize :: [TokenBundle] -> Int
    getLargestAssetSetSize = F.maximum . fmap (Set.size . TokenBundle.getAssets)

    zeroCostMakeChangeScenario = p
        { minCoinFor = noMinCoin
        , requiredCost = noCost
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
            <> F.foldMap TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalInputCoin =
        TokenBundle.getCoin totalInputValue
    totalOutputValue =
        F.fold (outputBundles p)
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
    minCoinValueFor = mkMinCoinValueFor (minCoinFor p)

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
            <> F.foldMap TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalOutputValue =
        F.fold (outputBundles p)
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
    case makeChangeWith p {requiredCost = noCost, minCoinFor = NoMinCoin} of
        Left{} ->
            property False & counterexample "makeChange failed with no cost!"
        -- If 'makeChange' failed to generate change, we try to re-run it with
        -- noCost and noMinValue requirement. The result _must_ be 'Just'.
        --
        -- From there, we can manually compute the total deposit needed for all
        -- change generated and make sure that there were indeed not enough
        -- coins available to generate all change outputs.
        Right change ->
            conjoin
                [ deltaCoin < totalMinCoinDeposit `addCoin` view #requiredCost p
                , deltaCoin >= view #requiredCost p
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
                mkMinCoinValueFor (minCoinFor p)
            totalMinCoinDeposit = F.foldr addCoin (Coin 0)
                (minCoinValueFor . view #tokens <$> change)
  where
    totalInputValue =
        F.fold (inputBundles p)
            <> F.foldMap TokenBundle.fromCoin (view #extraCoinSource p)
            <> TokenBundle.fromTokenMap (view #assetsToMint p)
    totalOutputValue =
        F.fold (outputBundles p)
            <> TokenBundle.fromTokenMap (view #assetsToBurn p)

unit_makeChange
    :: [Expectation]
unit_makeChange =
    [ makeChange criteria `shouldBe` expectation
    | (minCoinFor, requiredCost, extraCoinSource, i, o, expectation) <- matrix
    , let criteria = MakeChangeCriteria
              { minCoinFor
              , requiredCost
              , extraCoinSource
              , bundleSizeAssessor
              , inputBundles = i
              , outputBundles = o
              , assetsToMint = TokenMap.empty
              , assetsToBurn = TokenMap.empty
              }
    ]
  where
    bundleSizeAssessor = mkBundleSizeAssessor NoBundleSizeLimit
    matrix =
        -- Simple, only ada, should construct a single change output with 1 ada.
        [ ( noMinCoin, noCost
          , Nothing
          , b 2 [] :| []
          , b 1 [] :| []
          , Right [b 1 []]
          )

        -- Two outputs, no cost, changes are proportional, no extra assets
        , ( noMinCoin, noCost
          , Nothing
          , b 9 [(assetA, 9), (assetB, 6)] :| []
          , b 2 [(assetA, 1)] :| [b 1 [(assetA, 2), (assetB, 3)]]
          , Right
              [ b 4 [(assetA, 2)]
              , b 2 [(assetA, 4), (assetB, 3)]
              ]
          )

        -- Extra non-user-specified assets. Large assets end up in 'large'
        -- bundles and small extra assets in smaller bundles.
        , ( noMinCoin, noCost
          , Nothing
          , b 1 [(assetA, 10), (assetC, 1)] :| [b 1 [(assetB, 2), (assetC, 8)]]
          , b 1 [(assetA, 5)] :| [b 1 [(assetB, 1)]]
          , Right
              [ b 0 [(assetA, 5), (assetC, 1)]
              , b 0 [(assetB, 1), (assetC, 8)]
              ]
          )
        ]

    b :: Word64 -> [(AssetId, Natural)] -> TokenBundle
    b c = TokenBundle (Coin c) . TokenMap.fromFlatList . fmap (second TokenQuantity)

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
          , linearMinCoin
          , m 42 [] :| []
          , Just [b 1 []]
          )

        -- Simple case, with a single MA output
        , ( Coin 2
          , linearMinCoin
          , m 42 [(assetA, 1337)] :| []
          , Just [b 2 [(assetA, 1337)]]
          )

        -- Single Ada-only output, but not enough left to create a change
        , ( Coin 1
          , (`addCoin` Coin 1) . linearMinCoin
          , m 42 [] :| []
          , Just []
          )

        -- Single MA output, but not enough left to create a change
        , ( Coin 1
          , linearMinCoin
          , m 42 [(assetA, 1337)] :| []
          , Nothing
          )

        -- Multiple Ada-only change, not enough Ada left to create them all
        , ( Coin 2
          , linearMinCoin
          , NE.fromList
            [ m 1337 []
            , m   14 []
            , m   42 []
            ]
          , Just [b 1 [], b 1 []]
          )

        -- Hybrid Ada & MA, not enough to cover both => Ada change is dropped
        , ( Coin 2
          , linearMinCoin
          , NE.fromList
            [ m 42 []
            , m 14 []
            , m  2 [(assetA, 1337)]
            ]
          , Just [b 2 [(assetA, 1337)]]
          )
        ]

    m :: Word64 -> [(AssetId, Natural)] -> (TokenMap, Coin)
    m c = (,Coin c) . TokenMap.fromFlatList . fmap (second TokenQuantity)

    b :: Word64 -> [(AssetId, Natural)] -> TokenBundle
    b c = TokenBundle (Coin c) . TokenMap.fromFlatList . fmap (second TokenQuantity)

    assetA :: AssetId
    assetA = AssetId (UnsafeTokenPolicyId $ Hash "A") (UnsafeTokenName "1")

--------------------------------------------------------------------------------
-- Making change for coins
--------------------------------------------------------------------------------

prop_makeChangeForCoin_sum :: NonEmpty Coin -> Coin -> Property
prop_makeChangeForCoin_sum weights surplus =
    surplus === F.foldr addCoin (Coin 0) changes
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
runMockRoundRobin initialState = runRoundRobin initialState processors
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
    runRoundRobin state (const Nothing <$ processors) === state

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

adjustAllQuantities :: (Natural -> Natural) -> TokenBundle -> TokenBundle
adjustAllQuantities f b = uncurry TokenBundle.fromFlatList $ bimap
    (adjustCoin)
    (fmap (fmap adjustTokenQuantity))
    (TokenBundle.toFlatList b)
  where
    adjustCoin :: Coin -> Coin
    adjustCoin = Coin . fromIntegral . f . fromIntegral . unCoin

    adjustTokenQuantity :: TokenQuantity -> TokenQuantity
    adjustTokenQuantity = TokenQuantity . f . unTokenQuantity

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

addExtraSource :: Maybe Coin -> TokenBundle -> TokenBundle
addExtraSource extraSource =
    TokenBundle.add
        (maybe TokenBundle.empty TokenBundle.fromCoin extraSource)

mockAsset :: ByteString -> AssetId
mockAsset a = AssetId (UnsafeTokenPolicyId $ Hash a) (UnsafeTokenName "1")

mockAssetQuantity :: ByteString -> Natural -> (AssetId, TokenQuantity)
mockAssetQuantity a q = (mockAsset a, TokenQuantity q)

unitTests :: String -> [Expectation] -> SpecWith ()
unitTests lbl cases =
    forM_ (zip [1..] cases) $ \(i, test) ->
        it (lbl <> " example #" <> show @Int i) test

--------------------------------------------------------------------------------
-- Arbitraries
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (AssetCount a) where
    arbitrary = AssetCount <$> arbitrary
    shrink = fmap AssetCount . shrink . unAssetCount

instance Arbitrary AssetId where
    arbitrary = genAssetIdSmallRange
    shrink = shrinkAssetIdSmallRange

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

instance Arbitrary MakeChangeData where
    arbitrary = genMakeChangeData

instance Arbitrary (MockRoundRobinState TokenName Word8) where
    arbitrary = genMockRoundRobinState genTokenNameMediumRange arbitrary
    shrink = shrinkMockRoundRobinState shrink

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRangePositive
    shrink = shrinkTokenBundleSmallRangePositive

instance Arbitrary (Large TokenBundle) where
    arbitrary = fmap Large $ TokenBundle
        <$> genCoinLargePositive
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
        <*> genTokenQuantitySmallPositive

instance Arbitrary TokenMap where
    arbitrary = genTokenMapSmallRange
    shrink = shrinkTokenMapSmallRange

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantitySmallPositive
    shrink = shrinkTokenQuantitySmallPositive

instance Arbitrary TxOut where
    arbitrary = genTxOutSmallRange
    shrink = shrinkTxOutSmallRange

newtype Large a = Large
    { getLarge :: a }
    deriving (Eq, Show)

newtype Small a = Small
    { getSmall:: a }
    deriving (Eq, Show)

instance Arbitrary (Large SelectionCriteria) where
    arbitrary = Large <$> genSelectionCriteria genUTxOIndexLarge
    -- No shrinking

instance Arbitrary (Small SelectionCriteria) where
    arbitrary = Small <$> genSelectionCriteria genUTxOIndexSmall
    -- No shrinking

instance Arbitrary (Large UTxOIndex) where
    arbitrary = Large <$> genUTxOIndexLarge
    -- No shrinking

instance Arbitrary (Small UTxOIndex) where
    arbitrary = Small <$> genUTxOIndexSmall
    shrink = fmap Small . shrinkUTxOIndexSmall . getSmall

instance Arbitrary Coin where
    arbitrary = genCoinSmallPositive
    shrink = shrinkCoinSmallPositive

instance Arbitrary MinCoinValueFor where
    arbitrary = arbitraryBoundedEnum
    shrink = \case
        NoMinCoin -> []
        LinearMinCoin -> [NoMinCoin]

instance Arbitrary CostFor where
    arbitrary = arbitraryBoundedEnum
    shrink = \case
        NoCost -> []
        LinearCost -> [NoCost]
