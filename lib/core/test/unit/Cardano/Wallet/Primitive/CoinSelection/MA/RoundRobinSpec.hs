{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobinSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( BalanceInsufficientError (..)
    , SelectionCriteria (..)
    , SelectionError (..)
    , SelectionInsufficientError (..)
    , SelectionLens (..)
    , SelectionLimit (..)
    , SelectionResult (..)
    , SelectionState (..)
    , groupByKey
    , makeChange
    , makeChangeForSurplusAssets
    , performSelection
    , runRoundRobin
    , runSelection
    , runSelectionStep
    , ungroupByKey
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinSmallPositive, shrinkCoinSmallPositive )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdSmallRange, shrinkAssetIdSmallRange )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameMediumRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmallPositive, shrinkTokenQuantitySmallPositive )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOutSmallRange, shrinkTxOutSmallRange )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexLarge, genUTxOIndexSmall, shrinkUTxOIndexSmall )
import Control.Monad
    ( replicateM )
import Data.Bifunctor
    ( bimap, second )
import Data.Function
    ( (&) )
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
import Data.Set
    ( Set )
import Data.Tuple
    ( swap )
import Data.Word
    ( Word8 )
import Fmt
    ( pretty )
import Numeric.Natural
    ( Natural )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Positive (..)
    , Property
    , checkCoverage
    , choose
    , counterexample
    , cover
    , frequency
    , genericShrink
    , property
    , shrinkList
    , suchThat
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobinSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Coverage" $ do

        it "prop_Small_UTxOIndex_coverage" $
            property prop_Small_UTxOIndex_coverage
        it "prop_Large_UTxOIndex_coverage" $
            property prop_Large_UTxOIndex_coverage

    parallel $ describe "Performing a selection" $ do

        it "prop_performSelection_small" $
            property prop_performSelection_small
        it "prop_performSelection_large" $
            property prop_performSelection_large

    parallel $ describe "Running a selection (without making change)" $ do

        it "prop_runSelection_UTxO_empty" $
            property prop_runSelection_UTxO_empty
        it "prop_runSelection_UTxO_notEnough" $
            property prop_runSelection_UTxO_notEnough
        it "prop_runSelection_UTxO_exactlyEnough" $
            property prop_runSelection_UTxO_exactlyEnough
        it "prop_runSelection_UTxO_extraSourceUsed" $
            property prop_runSelection_UTxO_extraSourceUsed
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
-- Performing a selection
--------------------------------------------------------------------------------

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
    pure $ SelectionCriteria {outputsToCover, utxoAvailable, selectionLimit}

balanceSufficient :: SelectionCriteria -> Bool
balanceSufficient SelectionCriteria {outputsToCover, utxoAvailable} =
    balanceRequired `leq` balanceAvailable
  where
    balanceRequired = F.foldMap (view #tokens) outputsToCover
    balanceAvailable = UTxOIndex.balance utxoAvailable

prop_performSelection_small
    :: Blind (Small SelectionCriteria)
    -> Property
prop_performSelection_small (Blind (Small criteria)) =
    checkCoverage $
    cover 30 (balanceSufficient criteria)
        "balance sufficient" $
    cover 30 (not $ balanceSufficient criteria)
        "balance insufficient" $
    prop_performSelection (Blind criteria)

prop_performSelection_large
    :: Blind (Large SelectionCriteria)
    -> Property
prop_performSelection_large (Blind (Large criteria)) =
    -- Generation of large UTxO sets takes longer, so limit the number of runs:
    withMaxSuccess 100 $
    checkCoverage $
    cover 50 (balanceSufficient criteria)
        "balance sufficient" $
    prop_performSelection (Blind criteria)

prop_performSelection
    :: Blind SelectionCriteria
    -> Property
prop_performSelection (Blind criteria) =
    monadicIO $ do
        result <- run (performSelection criteria)
        monitor $ cover 10 (selectionUnlimited && selectionSufficient result)
            "selection unlimited and sufficient"
        monitor $ cover 10 (selectionLimited && selectionSufficient result)
            "selection limited but sufficient"
        monitor $ cover 10 (selectionLimited && selectionInsufficient result)
            "selection limited and insufficient"
        either onFailure onSuccess result
  where
    SelectionCriteria
        {outputsToCover, utxoAvailable, selectionLimit} = criteria

    onSuccess result = do
        monitor $ counterexample $ unlines
            [ "available balance:"
            , pretty (TokenBundle.Nested balanceAvailable)
            , "required balance:"
            , pretty (TokenBundle.Nested balanceRequired)
            , "selected balance:"
            , pretty (TokenBundle.Nested balanceSelected)
            , "change balance:"
            , pretty (TokenBundle.Nested balanceChange)
            ]
        assert $ balanceSufficient criteria
        assert $ balanceSelected == balanceRequired <> balanceChange
        assert $ utxoAvailable ==
            UTxOIndex.insertMany inputsSelected utxoRemaining
        assert $ utxoRemaining ==
            UTxOIndex.deleteMany (fst <$> inputsSelected) utxoAvailable
        case selectionLimit of
            MaximumInputLimit limit ->
                assert $ NE.length inputsSelected <= limit
            NoLimit ->
                assert True
      where
        SelectionResult
            {inputsSelected, changeGenerated, utxoRemaining} = result
        balanceSelected =
            F.foldMap (view #tokens . snd) inputsSelected
        balanceChange =
            F.fold changeGenerated

    onFailure = \case
        BalanceInsufficient e ->
            onBalanceInsufficient e
        SelectionInsufficient e ->
            onSelectionInsufficient e

    onBalanceInsufficient e = do
        monitor $ counterexample $ unlines
            [ "available balance:"
            , pretty (TokenBundle.Nested balanceAvailable)
            , "required balance:"
            , pretty (TokenBundle.Nested balanceRequired)
            ]
        assert $ not $ balanceSufficient criteria
        assert $ balanceAvailable == errorBalanceAvailable
        assert $ balanceRequired  == errorBalanceRequired
      where
        BalanceInsufficientError errorBalanceAvailable errorBalanceRequired = e

    onSelectionInsufficient e = do
        monitor $ counterexample $ unlines
            [ "required balance:"
            , pretty (TokenBundle.Nested errorBalanceRequired)
            , "selected balance:"
            , pretty (TokenBundle.Nested errorBalanceSelected)
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

    balanceRequired = F.foldMap (view #tokens) outputsToCover
    balanceAvailable = UTxOIndex.balance utxoAvailable

    selectionLimited :: Bool
    selectionLimited = case selectionLimit of
        MaximumInputLimit _ -> True
        NoLimit -> False

    selectionUnlimited :: Bool
    selectionUnlimited = not selectionLimited

    selectionSufficient :: Either SelectionError SelectionResult -> Bool
    selectionSufficient = \case
        Right _ -> True
        _ -> False

    selectionInsufficient :: Either SelectionError SelectionResult -> Bool
    selectionInsufficient = \case
        Left (SelectionInsufficient _) -> True
        _ -> False

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
    :: Small UTxOIndex
    -> Property
prop_runSelection_UTxO_exactlyEnough (Small index) = monadicIO $ do
    SelectionState {selected, leftover} <-
        run $ runSelection NoLimit Nothing index balanceRequested
    let balanceSelected = view #balance selected
    let balanceLeftover = view #balance leftover
    assert $ balanceSelected == balanceRequested
    assert $ balanceLeftover == TokenBundle.empty
  where
    balanceRequested = view #balance index

prop_runSelection_UTxO_extraSourceUsed
    :: Maybe Coin
    -> Small UTxOIndex
    -> Property
prop_runSelection_UTxO_extraSourceUsed extraSource (Small index) =
    monadicIO $ case almostEverything of
        Nothing ->
            assert True
        Just balanceRequested | balanceRequested == TokenBundle.empty -> do
            SelectionState {selected,leftover} <-
                run $ runSelection extraSource index balanceRequested
            let balanceSelected = view #balance selected
            let balanceLeftover = view #balance leftover
            assert $ balanceLeftover == view #balance index
            assert $ balanceSelected == TokenBundle.empty
        Just balanceRequested -> do
            monitor (cover 80 True "sometimes there are Ada")
            SelectionState {selected} <-
                run $ runSelection extraSource index balanceRequested
            let balanceSelected = view #balance selected
            let coinSelected = TokenBundle.coin $
                    addExtraSource extraSource balanceSelected
            monitor $ counterexample $ unlines
                [ "balance selected: " <> show balanceSelected
                ]
            assert $ coinSelected >= TokenBundle.coin balanceRequested
            assert $ balanceSelected /= TokenBundle.empty
  where
    almostEverything = TokenBundle.subtract
        (view #balance index)
        (TokenBundle.fromCoin (Coin 1))

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
        [ "balance available: " <> show balanceAvailable
        , "balance requested: " <> show balanceRequested
        , "balance selected:  " <> show balanceSelected
        , "balance leftover:  " <> show balanceLeftover
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
            [ "balance available: " <> show balanceAvailable
            , "balance requested: " <> show balanceRequested
            , "balance selected:  " <> show balanceSelected
            , "balance leftover:  " <> show balanceLeftover
            ]
        assert $ balanceRequested `leq` addExtraSource extraSource balanceSelected
        assert $ balanceAvailable == balanceSelected <> balanceLeftover
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
        <$> genTokenBundleSmallRangePositive
        <*> replicateM count genTokenBundleSmallRangePositive

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

inAscendingPartialOrder :: (Foldable f, PartialOrd a) => f a -> Bool
inAscendingPartialOrder = all (uncurry leq) . consecutivePairs . F.toList

addExtraSource :: Maybe Coin -> TokenBundle -> TokenBundle
addExtraSource extraSource =
    TokenBundle.add
        (maybe TokenBundle.empty TokenBundle.fromCoin extraSource)

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

instance Arbitrary (MockRoundRobinState TokenName Word8) where
    arbitrary = genMockRoundRobinState genTokenNameMediumRange arbitrary
    shrink = shrinkMockRoundRobinState shrink

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRangePositive
    shrink = shrinkTokenBundleSmallRangePositive

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
