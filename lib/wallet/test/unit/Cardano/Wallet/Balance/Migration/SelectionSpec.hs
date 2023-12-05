{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Balance.Migration.SelectionSpec
    where

import Prelude

import Cardano.Wallet.Balance.Migration.Selection
    ( RewardWithdrawal (..)
    , Selection (..)
    , SelectionCorrectness (..)
    , SelectionError (..)
    , SelectionFullError (..)
    , addValueToOutputs
    , create
    , extend
    , minimizeFee
    , minimizeFeeStep
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( chooseCoin
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..)
    , TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..)
    , TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenName
    ( TokenName (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , TxSize (..)
    , txOutMaxCoin
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    )
import Control.Monad
    ( replicateM
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either
    ( isRight
    )
import Data.Either.Extra
    ( eitherToMaybe
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Semigroup
    ( mtimesDefault
    , stimes
    )
import Data.Word
    ( Word8
    )
import Fmt
    ( pretty
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , cover
    , elements
    , forAllBlind
    , frequency
    , genericShrink
    , oneof
    , property
    , suchThat
    , suchThatMap
    , vectorOf
    , withMaxSuccess
    )
import Test.QuickCheck.Extra
    ( report
    , verify
    )

import qualified Cardano.Wallet.Balance.Migration.Selection as Selection
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describe "Cardano.Wallet.Balance.Migration.SelectionSpec" $

    modifyMaxSuccess (const 1_000) $ do

    describe "Creating selections" $ do

        it "prop_create" $
            property prop_create

    describe "Extending selections" $ do

        it "prop_extend" $
            property prop_extend

    describe "Adding value to outputs" $ do

        it "prop_addValueToOutputs" $
            property prop_addValueToOutputs

    describe "Minimizing fees" $ do

        it "prop_minimizeFee" $
            property prop_minimizeFee
        it "prop_minimizeFeeStep" $
            property prop_minimizeFeeStep

    describe "Constraint calculations" $ do

        it "prop_txOutputCost" $
            property prop_txOutputCost
        it "prop_txOutputSize" $
            property prop_txOutputSize

--------------------------------------------------------------------------------
-- Creating a selection
--------------------------------------------------------------------------------

type MockSelection = Selection MockInputId
type MockSelectionError = SelectionError
type MockSelectionResult = Either MockSelectionError MockSelection

prop_create :: Blind MockTxConstraints -> Property
prop_create (Blind mockConstraints) =
    forAllBlind genInputs $ \inputs ->
    forAllBlind genRewardWithdrawal $ \reward ->
    prop_create_inner mockConstraints inputs reward
  where
    genInputs :: Gen (NonEmpty (MockInputId, TokenBundle))
    genInputs = do
        inputCount <- choose (1, 32)
        (:|)
            <$> genMockInput mockConstraints
            <*> replicateM (inputCount - 1) (genMockInput mockConstraints)

prop_create_inner
    :: MockTxConstraints
    -> NonEmpty (MockInputId, TokenBundle)
    -> RewardWithdrawal
    -> Property
prop_create_inner mockConstraints inputs reward =
    checkCoverage $
    cover 50 (resultIsSelection result)
        "Success" $
    cover 50 (resultHasZeroFeeExcess result)
        "Success with zero fee excess" $
    cover 1 (resultHasInsufficientAda result)
        "Failure due to insufficient ada" $
    cover 1 (resultIsFull result)
        "Failure due to oversized selection" $
    report mockConstraints
        "mockConstraints" $
    case result of
        Left SelectionAdaInsufficient ->
            property True
        Left (SelectionFull e) ->
            property (selectionSizeMaximum e < selectionSizeRequired e)
        Right selection -> makeReports $ testAll
            $ verify
                (correctness == SelectionCorrect)
                "correctness == SelectionCorrect"
            . verify
                (feeExcess selection == feeExcessExpected)
                "feeExcess selection == feeExcessExpected"
          where
            makeReports
                = report correctness
                    "correctness"
                . report (feeExcess selection)
                    "feeExcess"
                . report feeExcessExpected
                    "feeExcessExpected"
            correctness =
                Selection.verify constraints selection
            (feeExcessExpected, _) =
                minimizeFee constraints (feeExcess selection, outputs selection)
  where
    constraints = unMockTxConstraints mockConstraints
    result = create constraints reward inputs

resultIsSelection :: MockSelectionResult -> Bool
resultIsSelection = isRight

resultHasZeroFeeExcess :: MockSelectionResult -> Bool
resultHasZeroFeeExcess = matchRight $ \selection ->
    feeExcess selection == Coin 0

resultHasInsufficientAda :: MockSelectionResult -> Bool
resultHasInsufficientAda = matchLeft $ \case
    SelectionAdaInsufficient -> True
    _ -> False

resultIsFull :: MockSelectionResult -> Bool
resultIsFull = matchLeft $ \case
    SelectionFull _ -> True
    _ -> False

--------------------------------------------------------------------------------
-- Extending a selection
--------------------------------------------------------------------------------

prop_extend :: Blind MockTxConstraints -> Property
prop_extend (Blind mockConstraints) =
    forAllBlind genSelection $ \selection ->
    forAllBlind genExtraInput $ \input ->
    prop_extend_inner mockConstraints selection input
  where
    genSelection :: Gen MockSelection
    genSelection = genSelectionMaybe `suchThatMap` eitherToMaybe
      where
        genSelectionMaybe :: Gen (Either MockSelectionError MockSelection)
        genSelectionMaybe =
            create (unMockTxConstraints mockConstraints)
                <$> genRewardWithdrawal
                <*> genInputs

        genInputs :: Gen (NonEmpty (MockInputId, TokenBundle))
        genInputs = do
            inputCount <- choose (1, 32)
            (:|)
                <$> genMockInput mockConstraints
                <*> replicateM (inputCount - 1) (genMockInput mockConstraints)

    genExtraInput :: Gen (MockInputId, TokenBundle)
    genExtraInput = (,)
        <$> genMockInputId
        <*> oneof
            [ genTokenBundleMixed mockConstraints
              -- In order to increase coverage of error conditions,
              -- deliberately include some large bundles whose ada
              -- quantities are below the minimum:
            , TokenBundle (Coin 0) . F.fold <$>
                replicateM 4 (genTokenMap mockConstraints)
            ]

prop_extend_inner
    :: MockTxConstraints
    -> MockSelection
    -> (MockInputId, TokenBundle)
    -> Property
prop_extend_inner mockConstraints selectionOriginal input =
    checkCoverage $
    cover 40 (resultIsSelection result)
        "Success" $
    cover 10 (resultHasZeroFeeExcess result)
        "Success with zero fee excess" $
    cover 0.1 (resultHasInsufficientAda result)
        "Failure due to insufficient ada" $
    cover 0.1 (resultIsFull result)
        "Failure due to oversized selection" $
    report mockConstraints
        "mockConstraints" $
    case result of
        Left SelectionAdaInsufficient ->
            property True
        Left (SelectionFull e) ->
            property (selectionSizeMaximum e < selectionSizeRequired e)
        Right selection -> makeReports $ testAll
            $ verify
                (correctness == SelectionCorrect)
                "correctness == SelectionCorrect"
            . verify
                (feeExcess selection == feeExcessExpected)
                "feeExcess selection == feeExcessExpected"
          where
            makeReports
                = report correctness
                    "correctness"
                . report (feeExcess selection)
                    "feeExcess"
                . report feeExcessExpected
                    "feeExcessExpected"
            correctness =
                Selection.verify constraints selection
            (feeExcessExpected, _) =
                minimizeFee constraints (feeExcess selection, outputs selection)
  where
    constraints = unMockTxConstraints mockConstraints
    result = extend constraints selectionOriginal input

--------------------------------------------------------------------------------
-- Adding value to outputs
--------------------------------------------------------------------------------

prop_addValueToOutputs :: Blind MockTxConstraints -> Property
prop_addValueToOutputs (Blind mockConstraints) =
    forAllBlind genOutputs $ \outputs ->
    prop_addValueToOutputs_inner mockConstraints outputs
  where
    genOutputs :: Gen (NonEmpty TokenMap)
    genOutputs = do
        -- The upper limit is chosen to be comfortably above the maximum
        -- number of inputs expected in a typical transaction containing
        -- different types of inputs:
        outputCount <- choose (1, 128)
        (:|)
            <$> genTokenMap mockConstraints
            <*> replicateM (outputCount - 1) (genTokenMap mockConstraints)

prop_addValueToOutputs_inner
    :: MockTxConstraints
    -> NonEmpty TokenMap
    -> Property
prop_addValueToOutputs_inner mockConstraints outputs =
    withMaxSuccess 100 $
    checkCoverage $ makeCoverage $ makeReports $ testAll makeTests
  where
    makeTests
        = verify
            (valueAfter == valueBefore)
            "Value is preserved"
        . verify
            (all (txOutputHasValidSizeWithMaxAda constraints) result)
            "All outputs have valid sizes (if ada maximized)"
        . verify
            (all (txOutputHasValidTokenQuantities constraints) result)
            "All outputs have valid token quantities"
    makeCoverage
        = cover 0.1 (length result == 1)
            "length result == 1"
        . cover 8.0 (length result >= 2)
            "length result >= 2"
    makeReports
        = report mockConstraints
            "mockConstraints"
        . report valueBefore
            "valueBefore"
        . report valueAfter
            "valueAfter"
        . report (length outputs)
            "length outputs"
        . report (length result)
            "length result"

    constraints = unMockTxConstraints mockConstraints
    result :: NonEmpty TokenMap
    result = F.foldl'
        (addValueToOutputs constraints . NE.toList)
        (addValueToOutputs constraints [] (NE.head outputs))
        (NE.tail outputs)

    valueBefore
        = F.fold outputs
    valueAfter
        = F.fold result

txOutputHasValidSizeWithMaxAda :: TxConstraints -> TokenMap -> Bool
txOutputHasValidSizeWithMaxAda constraints b =
    txOutputHasValidSize constraints $ TokenBundle txOutMaxCoin b

--------------------------------------------------------------------------------
-- Minimizing fees
--------------------------------------------------------------------------------

prop_minimizeFee :: Blind MockTxConstraints -> Property
prop_minimizeFee (Blind mockConstraints) =
    forAllBlind genFeeExcess $ \feeExcessToMinimize ->
    forAllBlind genOutputs $ \outputs ->
    prop_minimizeFee_inner mockConstraints feeExcessToMinimize outputs
  where
    genFeeExcess :: Gen Coin
    genFeeExcess = chooseCoin (Coin 0, Coin 10_000)

    genOutputs :: Gen (NonEmpty TokenBundle)
    genOutputs = do
        outputCount <- choose (1, 10)
        (:|)
            <$> genTokenBundleMixed mockConstraints
            <*> replicateM
                (outputCount - 1)
                (genTokenBundleMixed mockConstraints)

prop_minimizeFee_inner
    :: MockTxConstraints
    -> Coin
    -> NonEmpty TokenBundle
    -> Property
prop_minimizeFee_inner mockConstraints feeExcessBefore outputsBefore =
    checkCoverage $ makeCoverage $ makeReports $ testAll makeTests
  where
    makeTests
        = verify
            (feeExcessAfter <= feeExcessBefore)
            "feeExcessAfter <= feeExcessBefore"
        . verify
            (feeExcessReduction == feeExcessReductionExpected)
            "feeExcessReduction == feeExcessReductionExpected"
        . verify
            (length outputsAfter == length outputsBefore)
            "length outputsAfter == length outputsBefore"
        . verify
            (feeExcessAfter == feeExcessAfterSecondRun)
            "feeExcessAfter == feeExcessAfterSecondRun (idempotency)"
    makeCoverage
        = cover 50 (feeExcessAfter == Coin 0)
            "feeExcessAfter == 0"
        . cover 50 (totalOutputCostIncrease > Coin 0)
            "totalOutputCostIncrease > 0"
    makeReports
        = report mockConstraints
            "mockConstraints"
        . report feeExcessBefore
            "feeExcessBefore"
        . report feeExcessAfter
            "feeExcessAfter"
        . report feeExcessAfterSecondRun
            "feeExcessAfterSecondRun"
        . report feeExcessReduction
            "feeExcessReduction"
        . report feeExcessReductionExpected
            "feeExcessReductionExpected"
        . report (length outputsBefore)
            "length outputsBefore"
        . report (length outputsAfter)
            "length outputsAfter"

    constraints = unMockTxConstraints mockConstraints

    (feeExcessAfter, outputsAfter) =
        minimizeFee constraints (feeExcessBefore, outputsBefore)
    (feeExcessAfterSecondRun, _) =
        minimizeFee constraints (feeExcessAfter, outputsAfter)

    feeExcessReduction =
        Coin.distance feeExcessBefore feeExcessAfter
    feeExcessReductionExpected =
        totalOutputCostIncrease <> totalOutputAdaIncrease

    totalOutputAdaAfter =
        F.foldMap (view #coin) outputsAfter
    totalOutputAdaBefore =
        F.foldMap (view #coin) outputsBefore
    totalOutputAdaIncrease =
        Coin.distance totalOutputAdaAfter totalOutputAdaBefore

    totalOutputCostAfter =
        F.foldMap (txOutputCost constraints) outputsAfter
    totalOutputCostBefore =
        F.foldMap (txOutputCost constraints) outputsBefore
    totalOutputCostIncrease =
        Coin.distance totalOutputCostBefore totalOutputCostAfter

--------------------------------------------------------------------------------
-- Minimizing fees (a single step)
--------------------------------------------------------------------------------

prop_minimizeFeeStep :: Blind MockTxConstraints -> Property
prop_minimizeFeeStep (Blind mockConstraints) =
    forAllBlind genFeeExcess $ \feeExcessToMinimize ->
    forAllBlind genOutput $ \output ->
    prop_minimizeFeeStep_inner mockConstraints feeExcessToMinimize output
  where
    genFeeExcess :: Gen Coin
    genFeeExcess = chooseCoin (Coin 0, Coin 10_000)

    genOutput :: Gen TokenBundle
    genOutput = genTokenBundleMixed mockConstraints

prop_minimizeFeeStep_inner
    :: MockTxConstraints
    -> Coin
    -> TokenBundle
    -> Property
prop_minimizeFeeStep_inner mockConstraints feeExcessBefore outputBefore =
    checkCoverage $ makeCoverage $ makeReports $ testAll makeTests
  where
    makeTests
        = verify
            (feeExcessAfter <= feeExcessBefore)
            "feeExcessAfter <= feeExcessBefore"
        . verify
            (outputCoinAfter >= outputCoinBefore)
            "outputCoinAfter >= outputCoinBefore"
        . verify
            (outputCostAfter >= outputCostBefore)
            "outputCostAfter >= outputCostBefore"
        . verify
            (feeExcessReduction <> feeExcessAfter == feeExcessBefore)
            "feeExcessReduction <> feeExcessAfter == feeExcessBefore"
        . verify
            (costOfEliminatingFeeExcess >= gainOfEliminatingFeeExcess)
            "costOfEliminatingFeeExcess >= gainOfEliminatingFeeExcess"
    makeCoverage
        = cover 50 (feeExcessAfter == Coin 0)
            "feeExcessAfter == 0"
        . cover 0.01 (feeExcessAfter /= Coin 0)
            "feeExcessAfter /= 0"
        . cover 1 (outputCostIncrease > Coin 0)
            "outputCostIncrease > 0"
    makeReports
        = report mockConstraints
            "mockConstraints"
        . report feeExcessBefore
            "feeExcessBefore"
        . report feeExcessAfter
            "feeExcessAfter"
        . report feeExcessReduction
            "feeExcessReduction"
        . report costOfEliminatingFeeExcess
            "costOfEliminatingFeeExcess"
        . report gainOfEliminatingFeeExcess
            "gainOfEliminatingFeeExcess"
        . report outputCoinBefore
            "outputCoinBefore"
        . report outputCoinAfter
            "outputCoinAfter"
        . report outputCoinIncrease
            "outputCoinIncrease"
        . report outputCostBefore
            "outputCostBefore"
        . report outputCostAfter
            "outputCostAfter"
        . report outputCostIncrease
            "outputCostIncrease"

    constraints = unMockTxConstraints mockConstraints

    (feeExcessAfter, outputAfter) =
        minimizeFeeStep constraints (feeExcessBefore, outputBefore)

    costOfEliminatingFeeExcess = Coin.distance
        (txOutputCoinCost constraints outputCoinAfter)
        (txOutputCoinCost constraints (outputCoinAfter <> feeExcessAfter))
    gainOfEliminatingFeeExcess = Coin.difference
        feeExcessAfter
        costOfEliminatingFeeExcess

    feeExcessReduction =
        Coin.distance feeExcessBefore feeExcessAfter

    outputCoinAfter =
        view #coin outputAfter
    outputCoinBefore =
        view #coin outputBefore
    outputCoinIncrease =
        Coin.distance outputCoinBefore outputCoinAfter
    outputCostAfter =
        txOutputCost constraints outputAfter
    outputCostBefore =
        txOutputCost constraints outputBefore
    outputCostIncrease =
        Coin.distance outputCostBefore outputCostAfter

--------------------------------------------------------------------------------
-- Cost calculations
--------------------------------------------------------------------------------

prop_txOutputCost :: Blind MockTxConstraints -> Property
prop_txOutputCost (Blind mockConstraints) =
    forAllBlind genOutput $ \output ->
    prop_txOutputCost_inner mockConstraints output
  where
    genOutput :: Gen TokenBundle
    genOutput = genTokenBundleMixed mockConstraints

prop_txOutputCost_inner :: MockTxConstraints -> TokenBundle -> Property
prop_txOutputCost_inner mockConstraints output =
    makeReports $ testAll makeTests
  where
    makeTests
        = verify
            ( txOutputCost constraints output <
              txOutputCost constraints outputWithLargerCoin )
            "multiplying a coin by a factor of 10 increases its cost"
        . verify
            ( txOutputCost constraints output <
              txOutputCost constraints outputWithMaxCoin )
            "all coins cost less than the maximum ada quantity"
        . verify
            ( txOutputCostDifference     output outputWithLargerCoin ==
              txOutputCoinCostDifference output outputWithLargerCoin )
            "cost difference is independent of whether bundles are considered"
    makeReports
        = report mockConstraints
            "mockConstraints"
        . report output
            "output"
        . report outputWithLargerCoin
            "outputWithLargerCoin"

    txOutputCostDifference :: TokenBundle -> TokenBundle -> Coin
    txOutputCostDifference out1 out2 = Coin.distance
        (txOutputCost constraints out1)
        (txOutputCost constraints out2)

    txOutputCoinCostDifference :: TokenBundle -> TokenBundle -> Coin
    txOutputCoinCostDifference out1 out2 = Coin.distance
        (txOutputCoinCost constraints (view #coin out1))
        (txOutputCoinCost constraints (view #coin out2))

    constraints =
        unMockTxConstraints mockConstraints
    outputWithLargerCoin = TokenBundle.setCoin output
        $ multiplyCoinByTen
        $ TokenBundle.getCoin output
    outputWithMaxCoin =
        TokenBundle.setCoin output txOutMaxCoin
    multiplyCoinByTen (Coin n) = Coin $ 10 * n

--------------------------------------------------------------------------------
-- Size calculations
--------------------------------------------------------------------------------

prop_txOutputSize :: Blind MockTxConstraints -> Property
prop_txOutputSize (Blind mockConstraints) =
    forAllBlind genOutput $ \output ->
    prop_txOutputSize_inner mockConstraints output
  where
    genOutput :: Gen TokenBundle
    genOutput = genTokenBundleMixed mockConstraints

prop_txOutputSize_inner :: MockTxConstraints -> TokenBundle -> Property
prop_txOutputSize_inner mockConstraints output =
    makeReports $ testAll makeTests
  where
    makeTests
        = verify
            ( txOutputSize constraints output <
              txOutputSize constraints outputWithLargerCoin )
            "multiplying a coin by a factor of 10 increases its size"
        . verify
            ( txOutputSize constraints output <
              txOutputSize constraints outputWithMaxCoin )
            "all coins have a smaller size than the maximum ada quantity"
        . verify
            ( txOutputSizeDifference     outputWithLargerCoin output ==
              txOutputCoinSizeDifference outputWithLargerCoin output )
            "size difference is independent of whether bundles are considered"
    makeReports
        = report mockConstraints
            "mockConstraints"
        . report output
            "output"
        . report outputWithLargerCoin
            "outputWithLargerCoin"

    txOutputSizeDifference :: TokenBundle -> TokenBundle -> TxSize
    txOutputSizeDifference outGreater outSmaller =
        txOutputSize constraints outGreater <\>
        txOutputSize constraints outSmaller

    txOutputCoinSizeDifference :: TokenBundle -> TokenBundle -> TxSize
    txOutputCoinSizeDifference outGreater outSmaller =
        txOutputCoinSize constraints (view #coin outGreater) <\>
        txOutputCoinSize constraints (view #coin outSmaller)

    constraints =
        unMockTxConstraints mockConstraints
    outputWithLargerCoin = TokenBundle.setCoin output
        $ multiplyCoinByTen
        $ TokenBundle.getCoin output
    outputWithMaxCoin =
        TokenBundle.setCoin output txOutMaxCoin
    multiplyCoinByTen (Coin n) = Coin $ 10 * n

--------------------------------------------------------------------------------
-- Mock transaction constraints
--------------------------------------------------------------------------------

data MockTxConstraints = MockTxConstraints
    { mockTxCostFunction
        :: MockTxCostFunction
    , mockTxBaseSize
        :: MockTxBaseSize
    , mockTxInputSize
        :: MockTxInputSize
    , mockTxOutputMaximumSize
        :: MockTxOutputMaximumSize
    , mockTxOutputMaximumTokenQuantity
        :: MockTxOutputMaximumTokenQuantity
    , mockTxOutputMinimumAdaQuantity
        :: MockTxOutputMinimumAdaQuantity
    , mockTxMaximumSize
        :: MockTxMaximumSize
    }
    deriving (Eq, Show)

instance Arbitrary MockTxConstraints where
    arbitrary = genMockTxConstraints

genMockTxConstraints :: Gen MockTxConstraints
genMockTxConstraints = do
    mockTxCostFunction <- genMockTxCostFunction
    mockTxBaseSize <- genMockTxBaseSize
    mockTxInputSize <- genMockTxInputSize
    mockTxOutputMaximumSize <- genMockTxOutputMaximumSize
    mockTxOutputMaximumTokenQuantity <- genMockTxOutputMaximumTokenQuantity
    mockTxOutputMinimumAdaQuantity <- genMockTxOutputMinimumAdaQuantity
    mockTxMaximumSize <- genMockTxMaximumSize
        mockTxBaseSize
        mockTxInputSize
        mockTxOutputMaximumSize
    pure MockTxConstraints {..}

unMockTxConstraints :: MockTxConstraints -> TxConstraints
unMockTxConstraints MockTxConstraints {..} = TxConstraints
    { txBaseCost =
        baseCost mockTxCostFunction
    , txBaseSize =
        unMockTxBaseSize mockTxBaseSize
    , txInputCost =
        mockSizeToCost $ unMockTxInputSize mockTxInputSize
    , txInputSize =
        unMockTxInputSize mockTxInputSize
    , txOutputCost =
        mockSizeToCost . mockOutputSize
    , txOutputSize =
        mockOutputSize
    , txOutputMaximumSize =
        unMockTxOutputMaximumSize mockTxOutputMaximumSize
    , txOutputMaximumTokenQuantity =
        unMockTxOutputMaximumTokenQuantity mockTxOutputMaximumTokenQuantity
    , txOutputMinimumAdaQuantity =
        unMockTxOutputMinimumAdaQuantity mockTxOutputMinimumAdaQuantity
    , txOutputBelowMinimumAdaQuantity =
        unMockTxOutputBelowMinimumAdaQuantity mockTxOutputMinimumAdaQuantity
    , txRewardWithdrawalCost =
        mockSizeToCost . mockRewardWithdrawalSize
    , txRewardWithdrawalSize =
        mockRewardWithdrawalSize
    , txMaximumSize =
        unMockTxMaximumSize mockTxMaximumSize
    }
  where
    mockOutputSize :: TokenBundle -> TxSize
    mockOutputSize (TokenBundle c m) = (<>)
        (TxSize $ fromIntegral $ BS.length $ pretty $ Flat m)
        (mockCoinSize c)

    mockRewardWithdrawalSize :: Coin -> TxSize
    mockRewardWithdrawalSize = \case
        Coin 0 -> TxSize 0
        Coin c -> mockCoinSize (Coin c)

    mockCoinSize :: Coin -> TxSize
    mockCoinSize = TxSize . fromIntegral . length . show

    mockSizeToCost :: TxSize -> Coin
    mockSizeToCost (TxSize s) =
        Coin $ fromIntegral $ fromIntegral a * s
      where
        Coin a = sizeCost mockTxCostFunction

--------------------------------------------------------------------------------
-- Mock transaction costs
--------------------------------------------------------------------------------

data MockTxCostFunction = MockTxCostFunction
    { baseCost :: Coin
    , sizeCost :: Coin
    }
    deriving stock (Eq, Show)

genMockTxCostFunction :: Gen MockTxCostFunction
genMockTxCostFunction = MockTxCostFunction
    <$> chooseCoin (Coin 0, Coin 1_000)
    <*> chooseCoin (Coin 1, Coin 4)

--------------------------------------------------------------------------------
-- Mock base transaction sizes
--------------------------------------------------------------------------------

newtype MockTxBaseSize = MockTxBaseSize
    { unMockTxBaseSize :: TxSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxBaseSize :: Gen MockTxBaseSize
genMockTxBaseSize = MockTxBaseSize <$> genTxSizeRange 0 1_000

--------------------------------------------------------------------------------
-- Mock input sizes
--------------------------------------------------------------------------------

newtype MockTxInputSize = MockTxInputSize
    { unMockTxInputSize :: TxSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxInputSize :: Gen MockTxInputSize
genMockTxInputSize = MockTxInputSize <$> genTxSizeRange 2 4

--------------------------------------------------------------------------------
-- Mock maximum output sizes
--------------------------------------------------------------------------------

newtype MockTxOutputMaximumSize = MockTxOutputMaximumSize
    { unMockTxOutputMaximumSize :: TxSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxOutputMaximumSize :: Gen MockTxOutputMaximumSize
genMockTxOutputMaximumSize =
    -- Chosen so that the upper limit is around twice the unconstrained maximum
    -- size of token bundles generated by 'genTokenBundle'.
    pure $ MockTxOutputMaximumSize $ TxSize 400

--------------------------------------------------------------------------------
-- Mock maximum token quantities
--------------------------------------------------------------------------------

newtype MockTxOutputMaximumTokenQuantity = MockTxOutputMaximumTokenQuantity
    { unMockTxOutputMaximumTokenQuantity :: TokenQuantity }
    deriving stock Eq
    deriving Show via Natural

genMockTxOutputMaximumTokenQuantity :: Gen MockTxOutputMaximumTokenQuantity
genMockTxOutputMaximumTokenQuantity = MockTxOutputMaximumTokenQuantity <$>
    genTokenQuantityRange (TokenQuantity 500) (TokenQuantity 2_000)

--------------------------------------------------------------------------------
-- Mock minimum ada quantities
--------------------------------------------------------------------------------

data MockTxOutputMinimumAdaQuantity = MockTxOutputMinimumAdaQuantity
    { perOutput :: Coin
    , perOutputAsset :: Coin
    }
    deriving (Eq, Show)

unMockTxOutputMinimumAdaQuantity
    :: MockTxOutputMinimumAdaQuantity
    -> (Address -> TokenMap -> Coin)
unMockTxOutputMinimumAdaQuantity mock _addr m =
    let assetCount = TokenMap.size m in
    perOutput mock
        <> mtimesDefault assetCount (perOutputAsset mock)

unMockTxOutputBelowMinimumAdaQuantity
    :: MockTxOutputMinimumAdaQuantity
    -> (Address -> TokenBundle -> Bool)
unMockTxOutputBelowMinimumAdaQuantity mock addr b =
    view #coin b < unMockTxOutputMinimumAdaQuantity mock addr (view #tokens b)

genMockTxOutputMinimumAdaQuantity :: Gen MockTxOutputMinimumAdaQuantity
genMockTxOutputMinimumAdaQuantity = MockTxOutputMinimumAdaQuantity
    <$> chooseCoin (Coin 4, Coin 8)
    <*> chooseCoin (Coin 1, Coin 2)

-- Addresses are currently never used within the mock minimum ada quantity
-- calculation. However, 'unMockTxOutputMinimumAdaQuantity' still requires an
-- address. For convenience, we define a dummy address that produces an error
-- if it is used unexpectedly.
--
-- See 'unMockTxOutputMinimumAdaQuantity'.
--
dummyAddress :: Address
dummyAddress = error "dummyAddress"

--------------------------------------------------------------------------------
-- Mock maximum transaction sizes
--------------------------------------------------------------------------------

newtype MockTxMaximumSize = MockTxMaximumSize
    { unMockTxMaximumSize :: TxSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxMaximumSize
    :: MockTxBaseSize
    -> MockTxInputSize
    -> MockTxOutputMaximumSize
    -> Gen MockTxMaximumSize
genMockTxMaximumSize mockTxBaseSize mockTxInputSize mockTxOutputMaximumSize =
    pure $ genInner 4
  where
    genInner :: Int -> MockTxMaximumSize
    genInner multiplier = MockTxMaximumSize $ mconcat
        [ unMockTxBaseSize mockTxBaseSize
        , stimes multiplier (unMockTxInputSize mockTxInputSize)
        , stimes multiplier (unMockTxOutputMaximumSize mockTxOutputMaximumSize)
        ]

--------------------------------------------------------------------------------
-- Generating inputs
--------------------------------------------------------------------------------

newtype MockInputId = MockInputId
    { unMockInputId :: ByteString }
    deriving (Eq, Ord)

instance Show MockInputId where
    show = show . T.decodeUtf8 . convertToBase Base16 . unMockInputId

genMockInput :: MockTxConstraints -> Gen (MockInputId, TokenBundle)
genMockInput mockConstraints = (,)
    <$> genMockInputId
    <*> genTokenBundleMixed mockConstraints

shrinkMockInput :: (MockInputId, TokenBundle) -> [(MockInputId, TokenBundle)]
shrinkMockInput (inputId, TokenBundle c m)
    | c /= Coin 0, m /= mempty =
        [(inputId, TokenBundle c mempty)]
    | c /= Coin 0, m == mempty =
        [(inputId, TokenBundle (Coin 0) mempty)]
    | otherwise =
        []

genMockInputId :: Gen MockInputId
genMockInputId = MockInputId . BS.pack <$>
    vectorOf 16 (choose (minBound @Word8, maxBound @Word8))

--------------------------------------------------------------------------------
-- Generating coins, token bundles, token maps, and token quantities
--------------------------------------------------------------------------------

genCoinAboveMinimumAdaQuantity :: MockTxConstraints -> Gen Coin
genCoinAboveMinimumAdaQuantity mockConstraints =
    chooseCoin (lo, hi)
  where
    constraints = unMockTxConstraints mockConstraints
    lo = txOutputMinimumAdaQuantity constraints dummyAddress TokenMap.empty
    hi = lo `scaleCoin` 1_000

genCoinBelowMinimumAdaQuantity :: MockTxConstraints -> Gen Coin
genCoinBelowMinimumAdaQuantity mockConstraints =
    chooseCoin (lo, hi)
  where
    constraints = unMockTxConstraints mockConstraints
    lo = Coin 1
    hi = Coin.difference
        (txOutputMinimumAdaQuantity constraints dummyAddress TokenMap.empty)
        (Coin 1)

genTokenBundleMixed :: MockTxConstraints -> Gen TokenBundle
genTokenBundleMixed mockConstraints =
    genInner `suchThat` txOutputHasValidSize constraints
  where
    constraints = unMockTxConstraints mockConstraints

    genInner :: Gen TokenBundle
    genInner = frequency $ fmap (\g -> g mockConstraints) <$> distribution

    distribution :: [(Int, MockTxConstraints -> Gen TokenBundle)]
    distribution =
        [ (10, genCoinBelowMinimumAdaQuantity <&> fmap TokenBundle.fromCoin)
        , (40, genCoinAboveMinimumAdaQuantity <&> fmap TokenBundle.fromCoin)
        , (40, genTokenBundleWithMinimumAdaQuantity)
        , (10, genTokenBundleAboveMinimumAdaQuantity)
        ]

genTokenBundleWithMinimumAdaQuantity :: MockTxConstraints -> Gen TokenBundle
genTokenBundleWithMinimumAdaQuantity mockConstraints = do
    m <- genTokenMap mockConstraints
    let minAda = txOutputMinimumAdaQuantity constraints dummyAddress m
    pure $ TokenBundle minAda m
  where
    constraints = unMockTxConstraints mockConstraints

genTokenBundleAboveMinimumAdaQuantity :: MockTxConstraints -> Gen TokenBundle
genTokenBundleAboveMinimumAdaQuantity mockConstraints = do
    m <- genTokenMap mockConstraints
    let minAda = txOutputMinimumAdaQuantity constraints dummyAddress m
    c <- chooseCoin (minAda <> Coin 1, minAda `scaleCoin` 1_000)
    pure $ TokenBundle c m
  where
    constraints = unMockTxConstraints mockConstraints

genTokenMap :: MockTxConstraints -> Gen TokenMap
genTokenMap mockConstraints =
    genInner
        `suchThat`
            (txOutputHasValidSize constraints . (TokenBundle txOutMaxCoin))
        `suchThat`
            (txOutputHasValidTokenQuantities constraints)
  where
    constraints = unMockTxConstraints mockConstraints

    genInner :: Gen TokenMap
    genInner = do
        assetCount <- choose (1, 4)
        TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity

    genAssetQuantity :: Gen (AssetId, TokenQuantity)
    genAssetQuantity = (,)
        <$> genAssetId
        <*> genTokenQuantityRange
            (TokenQuantity 1)
            (txOutputMaximumTokenQuantity constraints)

    genAssetId :: Gen AssetId
    genAssetId = elements mockAssetIds

genTokenQuantityRange :: TokenQuantity -> TokenQuantity -> Gen TokenQuantity
genTokenQuantityRange (TokenQuantity a) (TokenQuantity b) =
    TokenQuantity . fromIntegral @Integer
        <$> choose (fromIntegral a, fromIntegral b)

mockAssetIds :: [AssetId]
mockAssetIds =
    [ AssetId i n
    | i <- UnsafeTokenPolicyId . Hash . B8.singleton <$> ['0' .. '3']
    , n <- UnsafeTokenName . B8.singleton <$> ['0' .. '3']
    ]

--------------------------------------------------------------------------------
-- Generating reward withdrawals
--------------------------------------------------------------------------------

genRewardWithdrawal :: Gen RewardWithdrawal
genRewardWithdrawal = RewardWithdrawal <$> oneof
    [ pure (Coin 0)
    , chooseCoin (Coin 1, Coin 1_000_000)
    ]

--------------------------------------------------------------------------------
-- Generating transaction sizes
--------------------------------------------------------------------------------

genTxSizeRange :: Natural -> Natural -> Gen TxSize
genTxSizeRange minSize maxSize =
    TxSize . fromIntegral @Integer @Natural <$>
        choose (fromIntegral minSize, fromIntegral maxSize)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

--------------------------------------------------------------------------------
-- Internal types and functions
--------------------------------------------------------------------------------

-- | Tests a collection of properties defined with 'verify'.
--
-- Example:
--
-- >>> testAll (verify c1 "cond1" . verify c2 "cond2" . verify c3 "cond3")
--
testAll :: (Property -> Property) -> Property
testAll properties = properties $ property True

matchLeft :: (e -> Bool) -> Either e a -> Bool
matchLeft f result = case result of
    Right _ -> False
    Left x -> f x

matchRight :: (a -> Bool) -> Either e a -> Bool
matchRight f result = case result of
    Right x -> f x
    Left _ -> False

scaleCoin :: Coin -> Int -> Coin
scaleCoin (Coin c) s = Coin $ c * fromIntegral s
