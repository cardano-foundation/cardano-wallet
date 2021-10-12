{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.CoinSelectionSpec
    where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( ComputeMinimumCollateralParams (..)
    , Selection
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionCorrectness (..)
    , SelectionError (..)
    , SelectionParams (..)
    , computeMinimumCollateral
    , performSelection
    , prepareOutputsWith
    , selectionCollateralRequired
    , selectionDeltaAllAssets
    , selectionHasValidSurplus
    , selectionMinimumCost
    , toBalanceConstraintsParams
    , verifySelection
    )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit, SelectionSkeleton )
import Cardano.Wallet.Primitive.CoinSelection.Balance.Gen
    ( genSelectionSkeleton, shrinkSelectionSkeleton )
import Cardano.Wallet.Primitive.CoinSelection.BalanceSpec
    ( MockAssessTokenBundleSize
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
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut (..), txOutCoin )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOut, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, shrinkUTxO )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection, shrinkUTxOSelection )
import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isLeft, isRight )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , conjoin
    , cover
    , genericShrink
    , listOf
    , property
    , scale
    , shrink
    , shrinkList
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Extra
    ( Pretty (..)
    , chooseNatural
    , liftShrink8
    , liftShrink9
    , report
    , shrinkNatural
    )
import Test.QuickCheck.Monadic
    ( monadicIO, run )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelectionSpec" $ do

    parallel $ describe "Performing selections" $ do

        it "prop_performSelection_onSuccess_isCorrect" $
            prop_performSelection_onSuccess
            prop_performSelection_onSuccess_isCorrect
        it "prop_performSelection_onSuccess_hasValidSurplus" $
            prop_performSelection_onSuccess
            prop_performSelection_onSuccess_hasValidSurplus

    parallel $ describe "Constructing balance constraints and parameters" $ do

        it "prop_toBalanceConstraintsParams_computeMinimumCost" $
            property prop_toBalanceConstraintsParams_computeMinimumCost
        it "prop_toBalanceConstraintsParams_computeSelectionLimit" $
            property prop_toBalanceConstraintsParams_computeSelectionLimit

    parallel $ describe "Preparing outputs" $ do

        it "prop_prepareOutputsWith_twice" $
            property prop_prepareOutputsWith_twice
        it "prop_prepareOutputsWith_length" $
            property prop_prepareOutputsWith_length
        it "prop_prepareOutputsWith_assetsUnchanged" $
            property prop_prepareOutputsWith_assetsUnchanged
        it "prop_prepareOutputsWith_preparedOrExistedBefore" $
            property prop_prepareOutputsWith_preparedOrExistedBefore

    parallel $ describe "Computing minimum collateral amounts" $ do

        unitTests_computeMinimumCollateral

--------------------------------------------------------------------------------
-- Performing selections
--------------------------------------------------------------------------------

type PerformSelectionProperty =
    Pretty MockSelectionConstraints ->
    Pretty SelectionParams ->
    Property

type PerformSelectionPropertyInner =
    SelectionConstraints ->
    SelectionParams ->
    Either SelectionError Selection ->
    Property

type PerformSelectionPropertyOnSuccess =
    SelectionConstraints ->
    SelectionParams ->
    Selection ->
    Property

prop_performSelection_with
    :: PerformSelectionPropertyInner
    -> PerformSelectionProperty
prop_performSelection_with mkProperty (Pretty mockConstraints) (Pretty params) =
    monadicIO $ do
        result <- run $ runExceptT $ performSelection constraints params
        pure $ conjoin
            [ prop_performSelection_coverage constraints params result
            , mkProperty constraints params result
            ]
  where
    constraints = unMockSelectionConstraints mockConstraints

prop_performSelection_coverage :: PerformSelectionPropertyInner
prop_performSelection_coverage _constraints params result =
    checkCoverage $
    cover 10 (isLeft result)
        "failure" $
    cover 10 (isRight result)
        "success" $
    cover 10 (selectionCollateralRequired params)
        "collateral required: yes" $
    cover 10 (not $ selectionCollateralRequired params)
        "collateral required: no" $
    case result of
        Left e ->
            cover 2.0 (isBalanceError e)
                "failure: balance" $
            cover 2.0 (isCollateralError e)
                "failure: collateral" $
            cover 0.5 (isOutputError e)
                "failure: output" $
            property True
        Right _ ->
            property True
  where
    isBalanceError :: SelectionError -> Bool
    isBalanceError = \case
        SelectionBalanceError _ -> True
        _ -> False
    isCollateralError :: SelectionError -> Bool
    isCollateralError = \case
        SelectionCollateralError _ -> True
        _ -> False
    isOutputError :: SelectionError -> Bool
    isOutputError = \case
        SelectionOutputError _ -> True
        _ -> False

prop_performSelection_onSuccess
    :: PerformSelectionPropertyOnSuccess -> Property
prop_performSelection_onSuccess onSuccess = property $
    prop_performSelection_with $ \constraints params ->
        either (const $ property True) (onSuccess constraints params)

prop_performSelection_onSuccess_isCorrect
    :: PerformSelectionPropertyOnSuccess
prop_performSelection_onSuccess_isCorrect cs ps selection =
    Pretty (verifySelection cs ps selection) === Pretty SelectionCorrect

prop_performSelection_onSuccess_hasValidSurplus
    :: PerformSelectionPropertyOnSuccess
prop_performSelection_onSuccess_hasValidSurplus cs ps selection =
    report (selectionDeltaAllAssets selection)
        "selectionDelta" $
    report (selectionMinimumCost cs ps selection)
        "selectionMinimumCost" $
    selectionHasValidSurplus cs ps selection

--------------------------------------------------------------------------------
-- Construction of balance constraints and parameters
--------------------------------------------------------------------------------

-- Tests that function 'toBalanceConstraintsParams' applies the correct
-- transformation to the 'computeMinimumCost' function.
--
prop_toBalanceConstraintsParams_computeMinimumCost
    :: MockSelectionConstraints
    -> SelectionParams
    -> SelectionSkeleton
    -> Property
prop_toBalanceConstraintsParams_computeMinimumCost
    mockConstraints params skeleton =
        checkCoverage $
        cover 10 (selectionCollateralRequired params)
            "collateral required: yes" $
        cover 10 (not (selectionCollateralRequired params))
            "collateral required: no" $
        cover 10 (costOriginal < costAdjusted)
            "cost (original) < cost (adjusted)" $
        report costOriginal
            "cost (original)" $
        report costAdjusted
            "cost (adjusted)" $
        if selectionCollateralRequired params
        then
            conjoin
                [ costOriginal <= costAdjusted
                -- Here we apply a transformation that is the *inverse* of
                -- the transformation within 'toBalanceConstraintsParams':
                , costOriginal ==
                    ( computeMinimumCostAdjusted
                    . over #skeletonInputCount
                        (subtract maximumCollateralInputCount)
                    $ skeleton
                    )
                ]
        else
            costOriginal === costAdjusted
  where
    constraints :: SelectionConstraints
    constraints = unMockSelectionConstraints mockConstraints

    maximumCollateralInputCount :: Int
    maximumCollateralInputCount = constraints ^. #maximumCollateralInputCount

    computeMinimumCostOriginal :: SelectionSkeleton -> Coin
    computeMinimumCostOriginal = constraints ^. #computeMinimumCost

    computeMinimumCostAdjusted :: SelectionSkeleton -> Coin
    computeMinimumCostAdjusted =
        toBalanceConstraintsParams (constraints, params)
            & fst & view #computeMinimumCost

    costOriginal :: Coin
    costOriginal = computeMinimumCostOriginal skeleton

    costAdjusted :: Coin
    costAdjusted = computeMinimumCostAdjusted skeleton

-- Tests that function 'toBalanceConstraintsParams' applies the correct
-- transformation to the 'computeSelectionLimit' function.
--
prop_toBalanceConstraintsParams_computeSelectionLimit
    :: MockSelectionConstraints
    -> SelectionParams
    -> Property
prop_toBalanceConstraintsParams_computeSelectionLimit mockConstraints params =
    checkCoverage $
    cover 10 (selectionCollateralRequired params)
        "collateral required: yes" $
    cover 10 (not (selectionCollateralRequired params))
        "collateral required: no" $
    cover 10 (selectionLimitOriginal > selectionLimitAdjusted)
        "selection limit (original) > selection limit (adjusted)" $
    report selectionLimitOriginal
        "selection limit (original)" $
    report selectionLimitAdjusted
        "selection limit (adjusted)" $
    if selectionCollateralRequired params
    then
        conjoin
            [ selectionLimitOriginal >= selectionLimitAdjusted
            -- Here we apply a transformation that is the *inverse* of
            -- the transformation within 'toBalanceConstraintsParams':
            , selectionLimitOriginal ==
                (selectionLimitAdjusted <&> (+ maximumCollateralInputCount))
            ]
    else
        selectionLimitOriginal === selectionLimitAdjusted
  where
    constraints :: SelectionConstraints
    constraints = unMockSelectionConstraints mockConstraints

    maximumCollateralInputCount :: Int
    maximumCollateralInputCount = constraints ^. #maximumCollateralInputCount

    computeSelectionLimitOriginal :: [TxOut] -> SelectionLimit
    computeSelectionLimitOriginal = constraints ^. #computeSelectionLimit

    computeSelectionLimitAdjusted :: [TxOut] -> SelectionLimit
    computeSelectionLimitAdjusted =
        toBalanceConstraintsParams (constraints, params)
            & fst & view #computeSelectionLimit

    selectionLimitOriginal :: SelectionLimit
    selectionLimitOriginal = computeSelectionLimitOriginal
        (params ^. #outputsToCover)

    selectionLimitAdjusted :: SelectionLimit
    selectionLimitAdjusted = computeSelectionLimitAdjusted
        (params ^. #outputsToCover)

--------------------------------------------------------------------------------
-- Preparing outputs
--------------------------------------------------------------------------------

prop_prepareOutputsWith_twice
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_twice minCoinValueDef outs =
    once === twice
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    (_:once:twice:_) = iterate (prepareOutputsWith minCoinValueFor) outs

prop_prepareOutputsWith_length
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_length minCoinValueDef outs =
    F.length (prepareOutputsWith minCoinValueFor outs) === F.length outs
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef

prop_prepareOutputsWith_assetsUnchanged
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_assetsUnchanged minCoinValueDef outs =
    (txOutAssets <$> (prepareOutputsWith minCoinValueFor outs))
    ===
    (txOutAssets <$> outs)
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    txOutAssets = TokenBundle.getAssets . view #tokens

prop_prepareOutputsWith_preparedOrExistedBefore
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_preparedOrExistedBefore minCoinValueDef outs =
    property $ F.all isPreparedOrExistedBefore (zip outs outs')
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    outs' = prepareOutputsWith minCoinValueFor outs

    isPreparedOrExistedBefore :: (TxOut, TxOut) -> Bool
    isPreparedOrExistedBefore (before, after)
        | txOutCoin before /= Coin 0 =
            txOutCoin after == txOutCoin before
        | otherwise =
            txOutCoin after == minCoinValueFor (view (#tokens . #tokens) before)

--------------------------------------------------------------------------------
-- Computing minimum collateral amounts
--------------------------------------------------------------------------------

unitTests_computeMinimumCollateral :: Spec
unitTests_computeMinimumCollateral = unitTests
    "unitTests_computeMinimumCollateral"
    (computeMinimumCollateral)
    (mkTest <$> tests)
  where
    mkTest (minimumCollateralPercentage, transactionFee, minimumCollateral) =
        UnitTestData
            { params = ComputeMinimumCollateralParams
                { minimumCollateralPercentage
                , transactionFee
                }
            , result = minimumCollateral
            }
    -- We compute the minimum collateral amount by multiplying the minimum
    -- collateral percentage (a protocol parameter) with the estimated
    -- transaction fee (derived from the ada surplus of the selection).
    --
    -- However, the result of this multiplication may be non-integral.
    -- In the event that the result is non-integral, we always round up.
    tests =
        --( Min, Tx     , Min     )
        --(   %, Fee    , Required)
        --(----, -------, --------)
        [ (   0, Coin  0, Coin   0)
        , (   0, Coin 10, Coin   0)
        , (  90, Coin 10, Coin   9)
        , (  91, Coin 10, Coin  10) -- result is non-integral so we round up
        , (  99, Coin 10, Coin  10) -- result is non-integral so we round up
        , ( 100, Coin 10, Coin  10)
        , ( 990, Coin 10, Coin  99)
        , ( 991, Coin 10, Coin 100) -- result is non-integral so we round up
        , ( 999, Coin 10, Coin 100) -- result is non-integral so we round up
        , (1000, Coin 10, Coin 100)
        ]

--------------------------------------------------------------------------------
-- Selection constraints
--------------------------------------------------------------------------------

data MockSelectionConstraints = MockSelectionConstraints
    { assessTokenBundleSize
        :: MockAssessTokenBundleSize
    , certificateDepositAmount
        :: Coin
    , computeMinimumAdaQuantity
        :: MockComputeMinimumAdaQuantity
    , computeMinimumCost
        :: MockComputeMinimumCost
    , computeSelectionLimit
        :: MockComputeSelectionLimit
    , maximumCollateralInputCount
        :: Int
    , minimumCollateralPercentage
        :: Natural
    , utxoSuitableForCollateral
        :: MockUTxOSuitableForCollateral
    }
    deriving (Eq, Generic, Show)

genMockSelectionConstraints :: Gen MockSelectionConstraints
genMockSelectionConstraints = MockSelectionConstraints
    <$> genMockAssessTokenBundleSize
    <*> genCertificateDepositAmount
    <*> genMockComputeMinimumAdaQuantity
    <*> genMockComputeMinimumCost
    <*> genMockComputeSelectionLimit
    <*> genMaximumCollateralInputCount
    <*> genMinimumCollateralPercentage
    <*> genMockUTxOSuitableForCollateral

shrinkMockSelectionConstraints
    :: MockSelectionConstraints -> [MockSelectionConstraints]
shrinkMockSelectionConstraints =
    shrinkMapBy toMock unMock $ liftShrink8
        shrinkMockAssessTokenBundleSize
        shrinkCertificateDepositAmount
        shrinkMockComputeMinimumAdaQuantity
        shrinkMockComputeMinimumCost
        shrinkMockComputeSelectionLimit
        shrinkMaximumCollateralInputCount
        shrinkMinimumCollateralPercentage
        shrinkMockUTxOSuitableForCollateral
  where
    unMock (MockSelectionConstraints a b c d e f g h) = (a, b, c, d, e, f, g, h)
    toMock (a, b, c, d, e, f, g, h) = (MockSelectionConstraints a b c d e f g h)

unMockSelectionConstraints :: MockSelectionConstraints -> SelectionConstraints
unMockSelectionConstraints m = SelectionConstraints
    { assessTokenBundleSize =
        unMockAssessTokenBundleSize $ view #assessTokenBundleSize m
    , certificateDepositAmount =
        view #certificateDepositAmount m
    , computeMinimumAdaQuantity =
        unMockComputeMinimumAdaQuantity $ view #computeMinimumAdaQuantity m
    , computeMinimumCost =
        unMockComputeMinimumCost $ view #computeMinimumCost m
    , computeSelectionLimit =
        unMockComputeSelectionLimit $ view #computeSelectionLimit m
    , maximumCollateralInputCount =
        view #maximumCollateralInputCount m
    , minimumCollateralPercentage =
        view #minimumCollateralPercentage m
    , utxoSuitableForCollateral =
        unMockUTxOSuitableForCollateral $ view #utxoSuitableForCollateral m
    }

--------------------------------------------------------------------------------
-- Certificate deposit amounts
--------------------------------------------------------------------------------

genCertificateDepositAmount :: Gen Coin
genCertificateDepositAmount = genCoinPositive

shrinkCertificateDepositAmount :: Coin -> [Coin]
shrinkCertificateDepositAmount = shrinkCoinPositive

--------------------------------------------------------------------------------
-- Maximum collateral input counts
--------------------------------------------------------------------------------

genMaximumCollateralInputCount :: Gen Int
genMaximumCollateralInputCount = choose (1, 5)

shrinkMaximumCollateralInputCount :: Int -> [Int]
shrinkMaximumCollateralInputCount = shrink

--------------------------------------------------------------------------------
-- Minimum collateral percentages
--------------------------------------------------------------------------------

genMinimumCollateralPercentage :: Gen Natural
genMinimumCollateralPercentage = chooseNatural (0, 1000)

shrinkMinimumCollateralPercentage :: Natural -> [Natural]
shrinkMinimumCollateralPercentage = shrinkNatural

--------------------------------------------------------------------------------
-- Determining suitability of UTxOs for use as collateral
--------------------------------------------------------------------------------

data MockUTxOSuitableForCollateral
    = MockUTxOSuitableForCollateralNothing
      -- ^ Indicates that no UTxOs are suitable for use as collateral
    | MockUTxOSuitableForCollateralPureAda
      -- ^ Indicates that all pure ada UTxOs are suitable for use as collateral
    deriving (Bounded, Enum, Eq, Generic, Show)

genMockUTxOSuitableForCollateral :: Gen MockUTxOSuitableForCollateral
genMockUTxOSuitableForCollateral = arbitraryBoundedEnum

shrinkMockUTxOSuitableForCollateral
    :: MockUTxOSuitableForCollateral -> [MockUTxOSuitableForCollateral]
shrinkMockUTxOSuitableForCollateral = genericShrink

unMockUTxOSuitableForCollateral
    :: MockUTxOSuitableForCollateral -> ((TxIn, TxOut) -> Maybe Coin)
unMockUTxOSuitableForCollateral = \case
    MockUTxOSuitableForCollateralNothing ->
        const Nothing
    MockUTxOSuitableForCollateralPureAda ->
        \(_i, o) -> TokenBundle.toCoin $ view #tokens o

--------------------------------------------------------------------------------
-- Selection parameters
--------------------------------------------------------------------------------

genSelectionParams :: Gen SelectionParams
genSelectionParams = SelectionParams
    <$> genAssetsToBurn
    <*> genAssetsToMint
    <*> genOutputsToCover
    <*> genRewardWithdrawal
    <*> genCertificateDepositsTaken
    <*> genCertificateDepositsReturned
    <*> genCollateralRequirement
    <*> genUTxOAvailableForCollateral
    <*> genUTxOAvailableForInputs

shrinkSelectionParams :: SelectionParams -> [SelectionParams]
shrinkSelectionParams =
    shrinkMapBy ofTuple toTuple $ liftShrink9
        shrinkAssetsToBurn
        shrinkAssetsToMint
        shrinkOutputsToCover
        shrinkRewardWithdrawal
        shrinkCerticateDepositsTaken
        shrinkCerticateDepositsReturned
        shrinkCollateralRequirement
        shrinkUTxOAvailableForCollateral
        shrinkUTxOAvailableForInputs
  where
    toTuple (SelectionParams a b c d e f g h i) = (a, b, c, d, e, f, g, h, i)
    ofTuple (a, b, c, d, e, f, g, h, i) = (SelectionParams a b c d e f g h i)

--------------------------------------------------------------------------------
-- Assets to mint and burn
--------------------------------------------------------------------------------

genAssetsToMint :: Gen TokenMap
genAssetsToMint = genTokenMap

genAssetsToBurn :: Gen TokenMap
genAssetsToBurn = scale (`mod` 4) genTokenMap

shrinkAssetsToMint :: TokenMap -> [TokenMap]
shrinkAssetsToMint = shrinkTokenMap

shrinkAssetsToBurn :: TokenMap -> [TokenMap]
shrinkAssetsToBurn = shrinkTokenMap

--------------------------------------------------------------------------------
-- Outputs to cover
--------------------------------------------------------------------------------

genOutputsToCover :: Gen [TxOut]
genOutputsToCover = scale (`mod` 4) $ listOf (scale (* 8) genTxOut)

shrinkOutputsToCover :: [TxOut] -> [[TxOut]]
shrinkOutputsToCover = shrinkList shrinkTxOut

--------------------------------------------------------------------------------
-- Reward withdrawals
--------------------------------------------------------------------------------

genRewardWithdrawal :: Gen Coin
genRewardWithdrawal = genCoin

shrinkRewardWithdrawal :: Coin -> [Coin]
shrinkRewardWithdrawal = shrinkCoin

--------------------------------------------------------------------------------
-- Certificate deposits taken and returned
--------------------------------------------------------------------------------

genCertificateDepositsTaken :: Gen Natural
genCertificateDepositsTaken = chooseNatural (0, 3)

genCertificateDepositsReturned :: Gen Natural
genCertificateDepositsReturned = chooseNatural (0, 3)

shrinkCerticateDepositsTaken :: Natural -> [Natural]
shrinkCerticateDepositsTaken = shrinkNatural

shrinkCerticateDepositsReturned :: Natural -> [Natural]
shrinkCerticateDepositsReturned = shrinkNatural

--------------------------------------------------------------------------------
-- Collateral requirements
--------------------------------------------------------------------------------

genCollateralRequirement :: Gen SelectionCollateralRequirement
genCollateralRequirement = arbitraryBoundedEnum

shrinkCollateralRequirement
    :: SelectionCollateralRequirement -> [SelectionCollateralRequirement]
shrinkCollateralRequirement = genericShrink

--------------------------------------------------------------------------------
-- UTxO available for inputs and collateral
--------------------------------------------------------------------------------

genUTxOAvailableForCollateral :: Gen UTxO
genUTxOAvailableForCollateral = genUTxO

genUTxOAvailableForInputs :: Gen UTxOSelection
genUTxOAvailableForInputs = genUTxOSelection

shrinkUTxOAvailableForCollateral :: UTxO -> [UTxO]
shrinkUTxOAvailableForCollateral = shrinkUTxO

shrinkUTxOAvailableForInputs :: UTxOSelection -> [UTxOSelection]
shrinkUTxOAvailableForInputs = shrinkUTxOSelection

--------------------------------------------------------------------------------
-- Unit test support
--------------------------------------------------------------------------------

data UnitTestData params result = UnitTestData
    { params :: params
    , result :: result
    }
    deriving (Eq, Generic, Show)

unitTests
    :: (Eq result, Show result)
    => String
    -> (params -> result)
    -> [UnitTestData params result]
    -> Spec
unitTests title f unitTestData =
    describe title $
    forM_ (zip testNumbers unitTestData) $
        \(testNumber :: Int, test) -> do
            let subtitle = "Unit test #" <> show testNumber
            it subtitle $
                let resultExpected = view #result test in
                let resultActual = f (view #params test) in
                property $ Pretty resultExpected === Pretty resultActual
  where
    testNumbers :: [Int]
    testNumbers = [1 ..]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary MockSelectionConstraints where
    arbitrary = genMockSelectionConstraints
    shrink = shrinkMockSelectionConstraints

instance Arbitrary SelectionParams where
    arbitrary = genSelectionParams
    shrink = shrinkSelectionParams

instance Arbitrary SelectionSkeleton where
    arbitrary = genSelectionSkeleton
    shrink = shrinkSelectionSkeleton
