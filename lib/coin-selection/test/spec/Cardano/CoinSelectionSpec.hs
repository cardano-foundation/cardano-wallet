{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.CoinSelectionSpec
    where

import Prelude

import Cardano.CoinSelection
    ( ComputeMinimumCollateralParams (..)
    , Selection
    , SelectionCollateralError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionParams (..)
    , VerificationResult (..)
    , computeMinimumCollateral
    , performSelection
    , selectionCollateralRequired
    , toBalanceConstraintsParams
    , verifySelection
    , verifySelectionError
    )
import Cardano.CoinSelection.Balance
    ( SelectionSkeleton )
import Cardano.CoinSelection.Balance.Gen
    ( genSelectionSkeleton
    , genSelectionStrategy
    , shrinkSelectionSkeleton
    , shrinkSelectionStrategy
    )
import Cardano.CoinSelection.BalanceSpec
    ( MockAssessTokenBundleSize
    , MockComputeMinimumAdaQuantity
    , MockComputeMinimumCost
    , TestAddress (..)
    , TestSelectionContext
    , TestUTxO
    , genMockAssessTokenBundleSize
    , genMockComputeMinimumAdaQuantity
    , genMockComputeMinimumCost
    , shrinkMockAssessTokenBundleSize
    , shrinkMockComputeMinimumAdaQuantity
    , shrinkMockComputeMinimumCost
    , unMockAssessTokenBundleSize
    , unMockComputeMinimumAdaQuantity
    , unMockComputeMinimumCost
    )
import Cardano.CoinSelection.UTxOSelection
    ( UTxOSelection )
import Cardano.CoinSelection.UTxOSelection.Gen
    ( genUTxOSelection, shrinkUTxOSelection )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxTokenQuantity )
import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.IntCast
    ( intCast )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word64 )
import Generics.SOP
    ( NP (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable (..)
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , conjoin
    , cover
    , elements
    , frequency
    , genericShrink
    , listOf
    , property
    , scale
    , shrink
    , shrinkList
    , suchThat
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Extra
    ( Pretty (..)
    , chooseNatural
    , genMapWith
    , genericRoundRobinShrink
    , report
    , shrinkMapWith
    , shrinkNatural
    , (<:>)
    , (<@>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, run )

import qualified Cardano.CoinSelection.Balance as Balance
import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

spec :: Spec
spec = describe "Cardano.CoinSelectionSpec" $ do

    describe "Performing selections" $ do

        it "prop_performSelection" $
            property prop_performSelection

    describe "Constructing balance constraints and parameters" $ do

        it "prop_toBalanceConstraintsParams_computeMinimumCost" $
            property prop_toBalanceConstraintsParams_computeMinimumCost

    describe "Computing minimum collateral amounts" $ do

        unitTests_computeMinimumCollateral

--------------------------------------------------------------------------------
-- Performing selections
--------------------------------------------------------------------------------

prop_performSelection
    :: Pretty MockSelectionConstraints
    -> Pretty (SelectionParams TestSelectionContext)
    -> Property
prop_performSelection (Pretty mockConstraints) (Pretty params) =
    monadicIO $
    prop_performSelection_inner constraints params <$>
    run (runExceptT $ performSelection constraints params)
  where
    constraints = unMockSelectionConstraints mockConstraints

prop_performSelection_inner
    :: SelectionConstraints TestSelectionContext
    -> SelectionParams TestSelectionContext
    -> Either
        (SelectionError TestSelectionContext)
        (Selection TestSelectionContext)
    -> Property
prop_performSelection_inner constraints params result =
    checkCoverage $
    prop_performSelection_coverage params result $
    case result of
        Right selection ->
            report selection "selection" $
            Pretty (verifySelection constraints params selection) ===
            Pretty VerificationSuccess
        Left e ->
            report e "selection error" $
            Pretty (verifySelectionError constraints params e) ===
            Pretty VerificationSuccess

prop_performSelection_coverage
    :: Testable property
    => SelectionParams TestSelectionContext
    -> Either
        (SelectionError TestSelectionContext)
        (Selection TestSelectionContext)
    -> property
    -> Property
prop_performSelection_coverage params r innerProperty =
    cover 20
        (selectionCollateralRequired params)
        "selectionCollateralRequired params" $
    cover 20
        (not $ selectionCollateralRequired params)
        "not $ selectionCollateralRequired params" $
    cover 20
        (isSelection r)
        "isSelection r" $
    cover 0.1
        (isSelectionBalanceError_BalanceInsufficient r)
        "isSelectionBalanceError_BalanceInsufficient" $
    cover 0.1
        (isSelectionBalanceError_UnableToConstructChange r)
        "isSelectionBalanceError_UnableToConstructChange" $
    cover 0.1
        (isSelectionBalanceError_EmptyUTxO r)
        "isSelectionBalanceError_EmptyUTxO" $
    cover 0.1
        (isSelectionCollateralError r)
        "isSelectionCollateralError" $
    property innerProperty
  where
    isSelection = isRight
    isSelectionBalanceError_BalanceInsufficient = \case
        Left (SelectionBalanceErrorOf Balance.BalanceInsufficient {})
            -> True; _ -> False
    isSelectionBalanceError_UnableToConstructChange = \case
        Left (SelectionBalanceErrorOf Balance.UnableToConstructChange {})
            -> True; _ -> False
    isSelectionBalanceError_EmptyUTxO = \case
        Left (SelectionBalanceErrorOf Balance.EmptyUTxO {})
            -> True; _ -> False
    isSelectionCollateralError = \case
        Left (SelectionCollateralErrorOf _)
            -> True; _ -> False

    -- Provides an exhaustiveness check for all possible constructors of
    -- the 'SelectionError' type.
    --
    -- If the compiler indicates that the pattern match is non-exhaustive,
    -- please update the pattern match and then revise the coverage checks
    -- above to make sure that they are also exhaustive.
    --
    _checkExhaustivenessForSelectionError :: ()
    _checkExhaustivenessForSelectionError = case undefined of
        SelectionBalanceErrorOf e -> case e of
            Balance.BalanceInsufficient {} -> ()
            Balance.UnableToConstructChange {} -> ()
            Balance.EmptyUTxO {} -> ()
        SelectionCollateralErrorOf e -> case e of
            SelectionCollateralError {} -> ()

--------------------------------------------------------------------------------
-- Construction of balance constraints and parameters
--------------------------------------------------------------------------------

-- Tests that function 'toBalanceConstraintsParams' applies the correct
-- transformation to the 'computeMinimumCost' function.
--
prop_toBalanceConstraintsParams_computeMinimumCost
    :: MockSelectionConstraints
    -> SelectionParams TestSelectionContext
    -> SelectionSkeleton TestSelectionContext
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
    constraints :: SelectionConstraints TestSelectionContext
    constraints = unMockSelectionConstraints mockConstraints

    maximumCollateralInputCount :: Int
    maximumCollateralInputCount = constraints ^. #maximumCollateralInputCount

    computeMinimumCostOriginal
        :: SelectionSkeleton TestSelectionContext -> Coin
    computeMinimumCostOriginal = constraints ^. #computeMinimumCost

    computeMinimumCostAdjusted
        :: SelectionSkeleton TestSelectionContext -> Coin
    computeMinimumCostAdjusted =
        toBalanceConstraintsParams (constraints, params)
            & fst & view #computeMinimumCost

    costOriginal :: Coin
    costOriginal = computeMinimumCostOriginal skeleton

    costAdjusted :: Coin
    costAdjusted = computeMinimumCostAdjusted skeleton

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
        --(  Min, Tx     , Min     )
        --(    %, Fee    , Required)
        --(-----, -------, --------)
        [ (    0, Coin  0, Coin   0)
        , (    0, Coin 10, Coin   0)
        , (   90, Coin 10, Coin   9)
        , (   91, Coin 10, Coin  10) -- result is non-integral so we round up
        , (   99, Coin 10, Coin  10) -- result is non-integral so we round up
        , (  100, Coin 10, Coin  10)
        , (  990, Coin 10, Coin  99)
        , (  991, Coin 10, Coin 100) -- result is non-integral so we round up
        , (  999, Coin 10, Coin 100) -- result is non-integral so we round up
        , (1_000, Coin 10, Coin 100)
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
    , maximumCollateralInputCount
        :: Int
    , minimumCollateralPercentage
        :: Natural
    , maximumOutputAdaQuantity
        :: Coin
    , maximumOutputTokenQuantity
        :: TokenQuantity
    }
    deriving (Eq, Generic, Show)

genMockSelectionConstraints :: Gen MockSelectionConstraints
genMockSelectionConstraints = MockSelectionConstraints
    <$> genMockAssessTokenBundleSize
    <*> genMockComputeMinimumAdaQuantity
    <*> genMockComputeMinimumCost
    <*> genMaximumCollateralInputCount
    <*> genMinimumCollateralPercentage
    <*> genMaximumOutputAdaQuantity
    <*> genMaximumOutputTokenQuantity

shrinkMockSelectionConstraints
    :: MockSelectionConstraints -> [MockSelectionConstraints]
shrinkMockSelectionConstraints = genericRoundRobinShrink
    <@> shrinkMockAssessTokenBundleSize
    <:> shrinkMockComputeMinimumAdaQuantity
    <:> shrinkMockComputeMinimumCost
    <:> shrinkMaximumCollateralInputCount
    <:> shrinkMinimumCollateralPercentage
    <:> shrinkMaximumOutputAdaQuantity
    <:> shrinkMaximumOutputTokenQuantity
    <:> Nil

unMockSelectionConstraints
    :: MockSelectionConstraints -> SelectionConstraints TestSelectionContext
unMockSelectionConstraints m = SelectionConstraints
    { assessTokenBundleSize =
        unMockAssessTokenBundleSize $ view #assessTokenBundleSize m
    , computeMinimumAdaQuantity =
        unMockComputeMinimumAdaQuantity $ view #computeMinimumAdaQuantity m
    , isBelowMinimumAdaQuantity =
        unMockIsBelowMinimumAdaQuantity $ view #computeMinimumAdaQuantity m
    , computeMinimumCost =
        unMockComputeMinimumCost $ view #computeMinimumCost m
    , maximumCollateralInputCount =
        view #maximumCollateralInputCount m
    , minimumCollateralPercentage =
        view #minimumCollateralPercentage m
    , maximumOutputAdaQuantity =
        view #maximumOutputAdaQuantity m
    , maximumOutputTokenQuantity =
        view #maximumOutputTokenQuantity m
    , maximumLengthChangeAddress =
        TestAddress 0x0
    , nullAddress =
        TestAddress 0x0
    }

--------------------------------------------------------------------------------
-- Minimum ada quantities
--------------------------------------------------------------------------------

unMockIsBelowMinimumAdaQuantity
    :: MockComputeMinimumAdaQuantity -> TestAddress -> TokenBundle -> Bool
unMockIsBelowMinimumAdaQuantity mock addr b =
    view #coin b < unMockComputeMinimumAdaQuantity mock addr (view #tokens b)

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
genMinimumCollateralPercentage = chooseNatural (0, 1_000)

shrinkMinimumCollateralPercentage :: Natural -> [Natural]
shrinkMinimumCollateralPercentage = shrinkNatural

--------------------------------------------------------------------------------
-- Maximum token quantities
--------------------------------------------------------------------------------

genMaximumOutputAdaQuantity :: Gen Coin
genMaximumOutputAdaQuantity = pure testMaximumOutputAdaQuantity

genMaximumOutputTokenQuantity :: Gen TokenQuantity
genMaximumOutputTokenQuantity = pure testMaximumOutputTokenQuantity

shrinkMaximumOutputAdaQuantity :: Coin -> [Coin]
shrinkMaximumOutputAdaQuantity = const []

shrinkMaximumOutputTokenQuantity :: TokenQuantity -> [TokenQuantity]
shrinkMaximumOutputTokenQuantity = const []

-- | Specifies the largest ada quantity that can appear in the token bundle
--   of an output.
--
-- For the moment, we use the same constant that is used in the wallet. In
-- future, we can improve our test coverage by allowing this value to vary.
--
testMaximumOutputAdaQuantity :: Coin
testMaximumOutputAdaQuantity = Coin 45_000_000_000_000_000

-- | Specifies the largest non-ada quantity that can appear in the token bundle
--   of an output.
--
-- For the moment, we use the same constant that is used in the wallet. In
-- future, we can improve our test coverage by allowing this value to vary.
--
testMaximumOutputTokenQuantity :: TokenQuantity
testMaximumOutputTokenQuantity = TokenQuantity $ intCast $ maxBound @Word64

--------------------------------------------------------------------------------
-- Selection parameters
--------------------------------------------------------------------------------

genSelectionParams :: Gen (SelectionParams TestSelectionContext)
genSelectionParams = SelectionParams
    <$> genAssetsToBurn
    <*> genAssetsToMint
    <*> genExtraCoinIn
    <*> genExtraCoinOut
    <*> genOutputsToCover
    <*> genCollateralRequirement
    <*> genUTxOAvailableForCollateral
    <*> genUTxOAvailableForInputs
    <*> genSelectionStrategy

shrinkSelectionParams
    :: SelectionParams TestSelectionContext
    -> [SelectionParams TestSelectionContext]
shrinkSelectionParams = genericRoundRobinShrink
    <@> shrinkAssetsToBurn
    <:> shrinkAssetsToMint
    <:> shrinkExtraCoinIn
    <:> shrinkExtraCoinOut
    <:> shrinkOutputsToCover
    <:> shrinkCollateralRequirement
    <:> shrinkUTxOAvailableForCollateral
    <:> shrinkUTxOAvailableForInputs
    <:> shrinkSelectionStrategy
    <:> Nil

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
-- Extra coin in and out
--------------------------------------------------------------------------------

genCoinMostly0 :: Gen Coin
genCoinMostly0 = frequency
    [ (70, pure $ Coin 0)
    , (30, genCoin)
    ]

genExtraCoinIn :: Gen Coin
genExtraCoinIn = genCoin

genExtraCoinOut :: Gen Coin
genExtraCoinOut = genCoinMostly0

shrinkExtraCoinIn :: Coin -> [Coin]
shrinkExtraCoinIn = shrinkCoin

shrinkExtraCoinOut :: Coin -> [Coin]
shrinkExtraCoinOut = shrinkCoin

--------------------------------------------------------------------------------
-- Outputs to cover
--------------------------------------------------------------------------------

genOutputsToCover :: Gen [(TestAddress, TokenBundle)]
genOutputsToCover = do
    count <- choose (1, 4)
    vectorOf count genOutputToCover
  where
    genOutputToCover :: Gen (TestAddress, TokenBundle)
    genOutputToCover = frequency
        [ (49, scale (`mod` 8) genOutput)
        , (01, genOutputWith genTokenQuantityThatMayExceedLimit)
        ]
      where
        genOutput = (,)
            <$> arbitrary @TestAddress
            <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin

    genOutputWith :: Gen TokenQuantity -> Gen (TestAddress, TokenBundle)
    genOutputWith genTokenQuantityFn = (,)
        <$> arbitrary @TestAddress
        <*> genTokenBundleWith genTokenQuantityFn

    genTokenBundleWith :: Gen TokenQuantity -> Gen TokenBundle
    genTokenBundleWith genTokenQuantityFn = TokenBundle
        <$> genCoinPositive
        <*> genTokenMapWith genTokenQuantityFn

    genTokenMapWith :: Gen TokenQuantity -> Gen TokenMap
    genTokenMapWith genTokenQuantityFn = do
        assetIds <- listOf genAssetId
        quantities <- vectorOf (length assetIds) genTokenQuantityFn
        pure $ TokenMap.fromFlatList (assetIds `zip` quantities)

    genTokenQuantityThatMayExceedLimit :: Gen TokenQuantity
    genTokenQuantityThatMayExceedLimit = TokenQuantity <$>
        elements
            [ 1
            , limit `div` 4
            , limit `div` 2
            , limit - 2
            , limit - 1
            , limit
            , limit + 1
            , limit + 2
            ]
      where
        limit :: Natural
        limit = unTokenQuantity txOutMaxTokenQuantity

shrinkOutputsToCover
    :: [(TestAddress, TokenBundle)] -> [[(TestAddress, TokenBundle)]]
shrinkOutputsToCover = shrinkList shrinkOutput
  where
    shrinkOutput = genericRoundRobinShrink
        <@> shrink @TestAddress
        <:> (filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)
        <:> Nil

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

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

genUTxOAvailableForCollateral :: Gen (Map TestUTxO Coin)
genUTxOAvailableForCollateral = genMapWith (arbitrary @TestUTxO) genCoinPositive

genUTxOAvailableForInputs :: Gen (UTxOSelection TestUTxO)
genUTxOAvailableForInputs = frequency
    [ (24, genUTxOSelection (arbitrary @TestUTxO))
    , (01, pure UTxOSelection.empty)
    ]

shrinkUTxOAvailableForCollateral
    :: Map TestUTxO Coin -> [Map TestUTxO Coin]
shrinkUTxOAvailableForCollateral =
    shrinkMapWith (shrink @TestUTxO) shrinkCoinPositive

shrinkUTxOAvailableForInputs
    :: UTxOSelection TestUTxO -> [UTxOSelection TestUTxO]
shrinkUTxOAvailableForInputs = shrinkUTxOSelection (shrink @TestUTxO)

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

instance Arbitrary (SelectionParams TestSelectionContext) where
    arbitrary = genSelectionParams
    shrink = shrinkSelectionParams

instance Arbitrary (SelectionSkeleton TestSelectionContext) where
    arbitrary = genSelectionSkeleton (arbitrary @TestAddress)
    shrink = shrinkSelectionSkeleton (shrink @TestAddress)
