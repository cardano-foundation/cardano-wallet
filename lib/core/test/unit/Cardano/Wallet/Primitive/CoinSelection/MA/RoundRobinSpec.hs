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
    , InsufficientMinCoinValueError (..)
    , SelectionCriteria (..)
    , SelectionError (..)
    , SelectionInsufficientError (..)
    , SelectionLens (..)
    , SelectionLimit (..)
    , SelectionResult (..)
    , SelectionSkeleton (..)
    , SelectionState (..)
    , UnableToConstructChangeError (..)
    , fullBalance
    , groupByKey
    , makeChange
    , makeChangeForCoin
    , makeChangeForKnownAsset
    , makeChangeForUnknownAsset
    , mapMaybe
    , performSelection
    , prepareOutputsWith
    , runRoundRobin
    , runSelection
    , runSelectionStep
    , ungroupByKey
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinSmall, genCoinSmallPositive, shrinkCoinSmallPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdSmallRange
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
    ( TxOut, txOutCoin )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOutSmallRange, shrinkTxOutSmallRange )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexLarge, genUTxOIndexSmall, shrinkUTxOIndexSmall )
import Control.Monad
    ( forM_, replicateM )
import Data.Bifunctor
    ( bimap, second )
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
import Data.Set
    ( Set )
import Data.Tuple
    ( swap )
import Data.Word
    ( Word8 )
import Fmt
    ( blockListF, pretty )
import Numeric.Natural
    ( Natural )
import Safe
    ( tailMay )
import Test.Hspec
    ( Expectation, Spec, SpecWith, describe, it, parallel, shouldBe )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Fun
    , Gen
    , Positive (..)
    , Property
    , applyFun
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , disjoin
    , frequency
    , genericShrink
    , label
    , oneof
    , property
    , shrinkList
    , suchThat
    , withMaxSuccess
    , (.&&.)
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

    parallel $ describe "Making change" $ do

        it "prop_makeChange_identity" $
            property prop_makeChange_identity
        it "prop_makeChange_length" $
            property prop_makeChange_length
        it "prop_makeChange" $
            property prop_makeChange

    parallel $ describe "Making change for coins" $ do
        it "prop_makeChangeForCoin_sum" $
            property prop_makeChangeForCoin_sum
        it "prop_makeChangeForCoin_length" $
            property prop_makeChangeForCoin_length
        unitTests "makeChangeForCoin" unit_makeChangeForCoin

    parallel $ describe "Making change for unknown assets" $ do
        it "prop_makeChangeForUnknownAsset_sum" $
            property prop_makeChangeForUnknownAsset_sum
        it "prop_makeChangeForUnknownAsset_length" $
            property prop_makeChangeForUnknownAsset_length
        unitTests "makeChangeForUnknownAsset" unit_makeChangeForUnknownAsset

    parallel $ describe "Making change for known assets" $ do
        it "prop_makeChangeForKnownAsset_sum" $
            property prop_makeChangeForKnownAsset_sum
        it "prop_makeChangeForKnownAsset_length" $
            property prop_makeChangeForKnownAsset_length
        unitTests "makeChangeForKnownAsset" unit_makeChangeForKnownAsset

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
    isPreparedOrExistedBefore (before, after) =
        if txOutCoin before /= Coin 0
        then txOutCoin after == txOutCoin before
        else txOutCoin after == minCoinValueFor (view (#tokens . #tokens) before)

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
    extraCoinSource <- oneof [ pure Nothing, Just <$> genCoinSmall ]
    pure $ SelectionCriteria
        { outputsToCover, utxoAvailable, extraCoinSource, selectionLimit }

balanceSufficient :: SelectionCriteria -> Bool
balanceSufficient SelectionCriteria{outputsToCover,utxoAvailable,extraCoinSource} =
    balanceRequired `leq` balanceAvailable
  where
    balanceRequired = F.foldMap (view #tokens) outputsToCover
    balanceAvailable = fullBalance utxoAvailable extraCoinSource

prop_performSelection_small
    :: MinCoinValueFor
    -> CostFor
    -> Blind (Small SelectionCriteria)
    -> Property
prop_performSelection_small minCoinValueFor costFor (Blind (Small criteria)) =
    checkCoverage $
    cover 30 (balanceSufficient criteria)
        "balance sufficient" $
    cover 30 (not $ balanceSufficient criteria)
        "balance insufficient" $
    prop_performSelection minCoinValueFor costFor (Blind criteria) $ \result ->
        cover 10 (selectionUnlimited && selectionSufficient result)
            "selection unlimited and sufficient"
        . cover 10 (selectionLimited && selectionSufficient result)
            "selection limited but sufficient"
        . cover 10 (selectionLimited && selectionInsufficient result)
            "selection limited and insufficient"
  where
    selectionLimited :: Bool
    selectionLimited = case selectionLimit criteria of
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
    -> (Either SelectionError SelectionResult -> Property -> Property)
    -> Property
prop_performSelection minCoinValueFor costFor (Blind criteria) coverage =
    monadicIO $ do
        monitor $ counterexample $ unlines
            [ "extraCoinSource: " <> show extraCoinSource
            , "selectionLimit:  " <> show selectionLimit
            ]
        result <- run (performSelection
            (mkMinCoinValueFor minCoinValueFor)
            (mkCostFor costFor)
            criteria)
        monitor (coverage result)
        either onFailure onSuccess result
  where
    SelectionCriteria
        { outputsToCover
        , utxoAvailable
        , extraCoinSource
        , selectionLimit
        } = criteria

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
        let delta = TokenBundle.unsafeSubtract
                balanceSelected
                (balanceRequired <> balanceChange)
        assert $ balanceSufficient criteria
        assert $ on (==) (view #tokens)
            balanceSelected (balanceRequired <> balanceChange)
        assert $ TokenBundle.getCoin delta == expectedCost
        assert $ utxoAvailable
            == UTxOIndex.insertMany inputsSelected utxoRemaining
        assert $ utxoRemaining
            == UTxOIndex.deleteMany (fst <$> inputsSelected) utxoAvailable
        case selectionLimit of
            MaximumInputLimit limit ->
                assert $ NE.length inputsSelected <= limit
            NoLimit ->
                assert True
      where
        SelectionResult
            {inputsSelected, changeGenerated, utxoRemaining} = result
        skeleton = SelectionSkeleton
            { inputsSkeleton =
                UTxOIndex.fromSequence inputsSelected
            , outputsSkeleton =
                outputsToCover
            , changeSkeleton  =
                fmap (TokenMap.getAssets . view #tokens) changeGenerated
            }
        balanceSelected =
            fullBalance (inputsSkeleton skeleton) extraCoinSource
        balanceChange =
            F.fold changeGenerated
        expectedCost =
            mkCostFor costFor skeleton

    onFailure = \case
        BalanceInsufficient e ->
            onBalanceInsufficient e
        SelectionInsufficient e ->
            onSelectionInsufficient e
        InsufficientMinCoinValues es ->
            onInsufficientMinCoinValues es
        UnableToConstructChange e ->
            onUnableToConstructChange e

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
            = txOutCoin . insufficientlyCoveredOutput

    onUnableToConstructChange e = do
        monitor $ counterexample $ show e
        assert (missingCoins e > Coin 0)
        let criteria' = criteria { selectionLimit = NoLimit }
        run (performSelection noMinCoin (const noCost) criteria') >>= \case
            Left e' -> do
                monitor $ counterexample $ unlines
                    [ "Failed to re-run selection with no cost!"
                    , show e'
                    ]
                assert False
            Right{} -> do
                assert True

    balanceRequired  = F.foldMap (view #tokens) outputsToCover
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
        Just c  -> TokenBundle.add (view #balance index) (TokenBundle.fromCoin c)

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
        [ "balance available: " <> pretty (Flat balanceAvailable)
        , "balance requested: " <> pretty (Flat balanceRequested)
        , "balance selected:  " <> pretty (Flat balanceSelected)
        , "balance leftover:  " <> pretty (Flat balanceLeftover)
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

-- | A dummy function for calculating the minimum Ada value to pay for a
-- TokenMap. The only property we want this function to have is that is becomes
-- more expensive with the number of tokens (types) in the map. So, looking at
-- the size of the asset set is enough.
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
linearCost SelectionSkeleton{inputsSkeleton, outputsSkeleton, changeSkeleton}
    = Coin
    $ fromIntegral
    $ UTxOIndex.size inputsSkeleton
    + NE.length outputsSkeleton
    + NE.length changeSkeleton

data MakeChangeData = MakeChangeData
    { inputBundles
        :: NonEmpty TokenBundle
    , extraInputCoins
        :: Maybe Coin
    , outputBundles
        :: NonEmpty TokenBundle
    , cost
        :: Coin
    , minCoinValueDef
        :: MinCoinValueFor
    } deriving (Eq, Show)

isValidMakeChangeData :: MakeChangeData -> Bool
isValidMakeChangeData p = (&&)
    (totalOutputValue `leq` totalInputValue)
    (totalOutputCoinValue > Coin 0)
  where
    totalInputValue = TokenBundle.add
        (F.fold $ inputBundles p)
        (maybe TokenBundle.empty TokenBundle.fromCoin (extraInputCoins p))
    totalOutputValue = F.fold $ outputBundles p
    totalOutputCoinValue = TokenBundle.getCoin totalOutputValue

genMakeChangeData :: Gen MakeChangeData
genMakeChangeData = flip suchThat isValidMakeChangeData $ do
    outputBundleCount <- choose (0, 15)
    let inputBundleCount = outputBundleCount * 4
    MakeChangeData
        <$> genTokenBundles inputBundleCount
        <*> oneof [pure Nothing, Just <$> genCoinSmallPositive]
        <*> genTokenBundles outputBundleCount
        <*> genCoinSmall
        <*> arbitrary
  where
    genTokenBundles :: Int -> Gen (NonEmpty TokenBundle)
    genTokenBundles count = (:|)
        <$> genTokenBundleSmallRangePositive
        <*> replicateM count genTokenBundleSmallRangePositive

makeChangeWith
    :: MakeChangeData
    -> Either UnableToConstructChangeError (NonEmpty TokenBundle)
makeChangeWith p = makeChange
    (mkMinCoinValueFor $ minCoinValueDef p)
    (cost p)
    (extraInputCoins p) (inputBundles p)
    (outputBundles p)

prop_makeChange_identity
    :: NonEmpty TokenBundle -> Property
prop_makeChange_identity bundles = (===)
    (F.fold <$> makeChange (const (Coin 0)) (Coin 0) Nothing bundles bundles)
    (Right TokenBundle.empty)

prop_makeChange_length
    :: MakeChangeData
    -> Property
prop_makeChange_length p =
    case change of
        Left{} -> property False
        Right xs -> length xs === length (outputBundles p)
  where
    change = makeChange noMinCoin noCost
        (extraInputCoins p) (inputBundles p) (outputBundles p)

prop_makeChange
    :: MakeChangeData
    -> Property
prop_makeChange p =
    case makeChangeWith p of
        Left{} -> disjoin
            [ prop_makeChange_fail_costTooBig p     & label "cost too big"
            , prop_makeChange_fail_minValueTooBig p & label "min value too big"
            ]
        Right change -> conjoin
            [ prop_makeChange_success_delta p change
            , prop_makeChange_success_minValueRespected p change
            ] & label "success"

-- Check that on successful calls to `makeChange`, the difference between input
-- and all outputs with change is exactly equal to the required cost of the
-- transaction. This property expects the second argument to be the result to
-- `makeChange` with `p` as argument.
--
-- See also `prop_makeChange` as a top-level property driver.
prop_makeChange_success_delta
    :: MakeChangeData
    -> NonEmpty TokenBundle
    -> Property
prop_makeChange_success_delta p change =
    let
        totalOutputWithChange = TokenBundle.add
            totalOutputValue
            (F.fold change)

        delta = TokenBundle.unsafeSubtract
            totalInputValue
            totalOutputWithChange

        totalChangeCoin =
            TokenBundle.getCoin (F.fold change)
    in
        (delta === TokenBundle.fromCoin (cost p))
            & counterexample ("totalChangeValue: " <> pretty totalChangeCoin)
            & counterexample ("totalOutputValue: " <> pretty totalOutputCoin)
            & counterexample ("totalInputValue:  " <> pretty totalInputCoin)
  where
    totalInputValue = TokenBundle.add
        (F.fold (inputBundles p))
        (maybe TokenBundle.empty TokenBundle.fromCoin (extraInputCoins p))
    totalInputCoin =
        TokenBundle.getCoin totalInputValue
    totalOutputValue =
        F.fold $ outputBundles p
    totalOutputCoin =
        TokenBundle.getCoin totalOutputValue

-- Check that on a successful result of 'makeChange', all change outputs
-- generated satisfy the min coin value set as input.
--
-- See also `prop_makeChange` as a top-level property driver.
prop_makeChange_success_minValueRespected
    :: MakeChangeData
    -> NonEmpty TokenBundle
    -> Property
prop_makeChange_success_minValueRespected p =
    F.foldr ((.&&.) . checkMinValue) (property True)
  where
    minCoinValueFor :: TokenMap -> Coin
    minCoinValueFor = mkMinCoinValueFor (minCoinValueDef p)

    checkMinValue :: TokenBundle -> Property
    checkMinValue m@TokenBundle{coin,tokens} =
        let
            minCoinValue = minCoinValueFor tokens
        in
            coin >= minCoinValue
                & counterexample ("bundle: " <> pretty (Flat m))
                & counterexample ("minCoinValue: " <> pretty minCoinValue)

-- The 'makeChange' function may fail when the required cost for a transaction
-- are too big. When this occurs, it means that the delta between input and
-- output (without change) is larger than the required cost.
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
        deltaCoin < cost p
            & counterexample ("delta: " <> pretty deltaCoin)
  where
    totalInputValue = TokenBundle.add
        (F.fold (inputBundles p))
        (maybe TokenBundle.empty TokenBundle.fromCoin (extraInputCoins p))
    totalOutputValue =
        F.fold $ outputBundles p

-- The 'makeChange' function may fail when there are not enough coins to assign
-- all required change output. Indeed, each output must satisfy a minimum UTxO
-- value, which is paid in Ada.
--
-- See also `prop_makeChange` as a top-level property driver.
prop_makeChange_fail_minValueTooBig
    :: MakeChangeData
    -> Property
prop_makeChange_fail_minValueTooBig p =
    case makeChangeWith (p { cost = noCost, minCoinValueDef = NoMinCoin }) of
        Left{} ->
            property False & counterexample "makeChange failed with no cost!"
        -- If 'makeChange' failed to generate change, we try to re-run it with
        -- noCost and noMinValue requirement. The result _must_ be `Just`.
        --
        -- From there, we can manually compute the total deposit needed for all
        -- change generated and make sure that, there was indeed not enough
        -- coins available to generate all change outputs.
        Right change ->
            let
                deltaCoin = TokenBundle.getCoin $ TokenBundle.unsafeSubtract
                    totalInputValue
                    totalOutputValue

                minCoinValueFor = mkMinCoinValueFor (minCoinValueDef p)

                totalMinCoinDeposit = F.foldr addCoin (Coin 0)
                    (minCoinValueFor . view #tokens <$> change)
            in
                conjoin
                    [ deltaCoin < (totalMinCoinDeposit `addCoin` cost p)
                    , deltaCoin >= cost p
                    ]
                    & counterexample
                        ("change: " <> pretty (blockListF (Flat <$> change)))
                    & counterexample
                        ("delta: " <> pretty deltaCoin)
                    & counterexample
                        ("totalMinCoinDeposit: " <> pretty totalMinCoinDeposit)
  where
    totalInputValue = TokenBundle.add
        (F.fold (inputBundles p))
        (maybe TokenBundle.empty TokenBundle.fromCoin (extraInputCoins p))
    totalOutputValue =
        F.fold $ outputBundles p

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
-- Making change for unknown asset
--------------------------------------------------------------------------------

prop_makeChangeForUnknownAsset_sum
    :: NonEmpty TokenMap
    -> (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForUnknownAsset_sum weights (asset, quantities) =
    F.fold quantities === F.fold ((`TokenMap.getQuantity` asset) <$> changes)
  where
    changes = makeChangeForUnknownAsset weights (asset, quantities)

prop_makeChangeForUnknownAsset_length
    :: NonEmpty TokenMap
    -> (AssetId, NonEmpty TokenQuantity)
    -> Property
prop_makeChangeForUnknownAsset_length weights surplus =
    F.length changes === F.length weights
  where
    changes = makeChangeForUnknownAsset weights surplus

unit_makeChangeForUnknownAsset
    :: [Expectation]
unit_makeChangeForUnknownAsset =
    [ makeChangeForUnknownAsset weights surplus `shouldBe` expectation
    | (weights, surplus, expectation) <- matrix
    ]
  where
    matrix =
        [ ( m [(assetA, q 1)] :| [m [(assetB, q 1)]]
          , (assetC, q <$> 1 :| [1])
          , m [(assetC, q 1)] :| [m [(assetC, q 1)]]
          )

        , ( m [(assetA, q 1)] :| [m [(assetB, q 1)]]
          , (assetC, q <$> 1 :| [1, 1])
          , m [(assetC, q 1)] :| [m [(assetC, q 2)]]
          )

        , ( m [(assetA, q 1)] :| [m [(assetB, q 1)]]
          , (assetC, q <$> 1 :| [])
          , m [(assetC, q 0)] :| [m [(assetC, q 1)]]
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
-- Making change for known asset
--------------------------------------------------------------------------------

prop_makeChangeForKnownAsset_sum
    :: NonEmpty TokenMap
    -> (AssetId, TokenQuantity)
    -> Property
prop_makeChangeForKnownAsset_sum weights (asset, quantity) =
    if any (`TokenMap.hasQuantity` asset) weights then
        quantity === totalChangeValue
    else
        totalChangeValue === TokenQuantity 0
  where
    changes = makeChangeForKnownAsset weights (asset, quantity)
    totalChangeValue = F.fold ((`TokenMap.getQuantity` asset) <$> changes)

prop_makeChangeForKnownAsset_length
    :: NonEmpty TokenMap
    -> (AssetId, TokenQuantity)
    -> Property
prop_makeChangeForKnownAsset_length weights surplus =
    F.length changes === F.length weights
  where
    changes = makeChangeForKnownAsset weights surplus

unit_makeChangeForKnownAsset
    :: [Expectation]
unit_makeChangeForKnownAsset =
    [ makeChangeForKnownAsset weights surplus `shouldBe` expectation
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
