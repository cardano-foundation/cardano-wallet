{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.InternalSpec
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal
    ( ComputeMinimumCollateralParams (..)
    , Selection
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOutputError (..)
    , SelectionParams (..)
    , VerificationResult (..)
    , computeMinimumCollateral
    , performSelection
    , prepareOutputsWith
    , selectionCollateralRequired
    , toBalanceConstraintsParams
    , verifySelection
    , verifySelectionError
    )
import Cardano.Wallet.CoinSelection.Internal.Balance
    ( SelectionLimit, SelectionSkeleton )
import Cardano.Wallet.CoinSelection.Internal.Balance.Gen
    ( genSelectionSkeleton
    , genSelectionStrategy
    , shrinkSelectionSkeleton
    , shrinkSelectionStrategy
    )
import Cardano.Wallet.CoinSelection.Internal.BalanceSpec
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
import Cardano.Wallet.CoinSelection.Internal.Collateral
    ( SelectionCollateralError (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress, shrinkAddress )
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
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, txOutMaxTokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection, shrinkUTxOSelection )
import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.Map.Strict
    ( Map )
import Generics.SOP
    ( NP (..) )
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
    , genSized2
    , genericRoundRobinShrink
    , report
    , shrinkMapWith
    , shrinkNatural
    , (<:>)
    , (<@>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, run )

import qualified Cardano.Wallet.CoinSelection.Internal.Balance as Balance
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Data.Foldable as F

-- TODO: ADP-1448:
--
-- Replace this type synonym with a type parameter on types that use it.
--
type InputId = (TxIn, Address)

spec :: Spec
spec = describe "Cardano.Wallet.CoinSelection.InternalSpec" $ do

    parallel $ describe "Performing selections" $ do

        it "prop_performSelection" $
            property prop_performSelection

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

prop_performSelection
    :: Pretty MockSelectionConstraints
    -> Pretty (SelectionParams InputId)
    -> Property
prop_performSelection (Pretty mockConstraints) (Pretty params) =
    monadicIO $
    prop_performSelection_inner constraints params <$>
    run (runExceptT $ performSelection constraints params)
  where
    constraints = unMockSelectionConstraints mockConstraints

prop_performSelection_inner
    :: SelectionConstraints
    -> SelectionParams InputId
    -> Either (SelectionError InputId) (Selection InputId)
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
    => SelectionParams InputId
    -> Either (SelectionError InputId) (Selection InputId)
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
        (isSelectionBalanceError_SelectionLimitReached r)
        "isSelectionBalanceError_SelectionLimitReached" $
    cover 0.1
        (isSelectionBalanceError_InsufficientMinCoinValues r)
        "isSelectionBalanceError_InsufficientMinCoinValues" $
    cover 0.1
        (isSelectionBalanceError_UnableToConstructChange r)
        "isSelectionBalanceError_UnableToConstructChange" $
    cover 0.1
        (isSelectionBalanceError_EmptyUTxO r)
        "isSelectionBalanceError_EmptyUTxO" $
    cover 0.1
        (isSelectionCollateralError r)
        "isSelectionCollateralError" $
    cover 0.1
        (isSelectionOutputError_SelectionOutputSizeExceedsLimit r)
        "isSelectionOutputError_SelectionOutputSizeExceedsLimit" $
    cover 0.1
        (isSelectionOutputError_SelectionOutputTokenQuantityExceedsLimit r)
        "isSelectionOutputError_SelectionOutputTokenQuantityExceedsLimit" $
    property innerProperty
  where
    isSelection = isRight
    isSelectionBalanceError_BalanceInsufficient = \case
        Left (SelectionBalanceErrorOf Balance.BalanceInsufficient {})
            -> True; _ -> False
    isSelectionBalanceError_SelectionLimitReached = \case
        Left (SelectionBalanceErrorOf Balance.SelectionLimitReached {})
            -> True; _ -> False
    isSelectionBalanceError_InsufficientMinCoinValues = \case
        Left (SelectionBalanceErrorOf Balance.InsufficientMinCoinValues {})
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
    isSelectionOutputError_SelectionOutputSizeExceedsLimit = \case
        Left (SelectionOutputErrorOf SelectionOutputSizeExceedsLimit {})
            -> True; _ -> False
    isSelectionOutputError_SelectionOutputTokenQuantityExceedsLimit = \case
        Left (SelectionOutputErrorOf SelectionOutputTokenQuantityExceedsLimit {})
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
            Balance.SelectionLimitReached {} -> ()
            Balance.InsufficientMinCoinValues {} -> ()
            Balance.UnableToConstructChange {} -> ()
            Balance.EmptyUTxO {} -> ()
        SelectionCollateralErrorOf e -> case e of
            SelectionCollateralError {} -> ()
        SelectionOutputErrorOf e -> case e of
            SelectionOutputSizeExceedsLimit {} -> ()
            SelectionOutputTokenQuantityExceedsLimit {} -> ()

--------------------------------------------------------------------------------
-- Construction of balance constraints and parameters
--------------------------------------------------------------------------------

-- Tests that function 'toBalanceConstraintsParams' applies the correct
-- transformation to the 'computeMinimumCost' function.
--
prop_toBalanceConstraintsParams_computeMinimumCost
    :: MockSelectionConstraints
    -> SelectionParams InputId
    -> SelectionSkeleton Address
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

    computeMinimumCostOriginal :: SelectionSkeleton Address -> Coin
    computeMinimumCostOriginal = constraints ^. #computeMinimumCost

    computeMinimumCostAdjusted :: SelectionSkeleton Address -> Coin
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
    -> SelectionParams InputId
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

    computeSelectionLimitOriginal :: [(Address, TokenBundle)] -> SelectionLimit
    computeSelectionLimitOriginal = constraints ^. #computeSelectionLimit

    computeSelectionLimitAdjusted :: [(Address, TokenBundle)] -> SelectionLimit
    computeSelectionLimitAdjusted =
        toBalanceConstraintsParams (constraints, params)
            & fst & view #computeSelectionLimit

    selectionLimitOriginal :: SelectionLimit
    selectionLimitOriginal = computeSelectionLimitOriginal $
        params ^. #outputsToCover

    selectionLimitAdjusted :: SelectionLimit
    selectionLimitAdjusted = computeSelectionLimitAdjusted $
        params ^. #outputsToCover

--------------------------------------------------------------------------------
-- Preparing outputs
--------------------------------------------------------------------------------

prop_prepareOutputsWith_twice
    :: MockComputeMinimumAdaQuantity
    -> [(Address, TokenBundle)]
    -> Property
prop_prepareOutputsWith_twice minCoinValueDef outs =
    once === twice
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    (_:once:twice:_) = iterate (prepareOutputsWith minCoinValueFor) outs

prop_prepareOutputsWith_length
    :: MockComputeMinimumAdaQuantity
    -> [(Address, TokenBundle)]
    -> Property
prop_prepareOutputsWith_length minCoinValueDef outs =
    F.length (prepareOutputsWith minCoinValueFor outs) === F.length outs
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef

prop_prepareOutputsWith_assetsUnchanged
    :: MockComputeMinimumAdaQuantity
    -> [(Address, TokenBundle)]
    -> Property
prop_prepareOutputsWith_assetsUnchanged minCoinValueDef outs =
    (outputAssets <$> (prepareOutputsWith minCoinValueFor outs))
    ===
    (outputAssets <$> outs)
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    outputAssets = TokenBundle.getAssets . snd

prop_prepareOutputsWith_preparedOrExistedBefore
    :: MockComputeMinimumAdaQuantity
    -> [(Address, TokenBundle)]
    -> Property
prop_prepareOutputsWith_preparedOrExistedBefore minCoinValueDef outs =
    property $ F.all isPreparedOrExistedBefore (zip outs outs')
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    outs' = prepareOutputsWith minCoinValueFor outs

    isPreparedOrExistedBefore
        :: ((Address, TokenBundle), (Address, TokenBundle)) -> Bool
    isPreparedOrExistedBefore (before, after)
        | outputCoin before /= Coin 0 =
            outputCoin after == outputCoin before
        | otherwise =
            outputCoin after == minCoinValueFor (view #tokens $ snd before)
      where
        outputCoin = view #coin . snd

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

shrinkMockSelectionConstraints
    :: MockSelectionConstraints -> [MockSelectionConstraints]
shrinkMockSelectionConstraints = genericRoundRobinShrink
    <@> shrinkMockAssessTokenBundleSize
    <:> shrinkCertificateDepositAmount
    <:> shrinkMockComputeMinimumAdaQuantity
    <:> shrinkMockComputeMinimumCost
    <:> shrinkMockComputeSelectionLimit
    <:> shrinkMaximumCollateralInputCount
    <:> shrinkMinimumCollateralPercentage
    <:> Nil

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
-- Selection parameters
--------------------------------------------------------------------------------

genSelectionParams :: Gen (SelectionParams InputId)
genSelectionParams = SelectionParams
    <$> genAssetsToBurn
    <*> genAssetsToMint
    <*> genExtraCoinIn
    <*> genExtraCoinOut
    <*> genOutputsToCover
    <*> genRewardWithdrawal
    <*> genCertificateDepositsTaken
    <*> genCertificateDepositsReturned
    <*> genCollateralRequirement
    <*> genUTxOAvailableForCollateral
    <*> genUTxOAvailableForInputs
    <*> genSelectionStrategy

shrinkSelectionParams :: SelectionParams InputId -> [SelectionParams InputId]
shrinkSelectionParams = genericRoundRobinShrink
    <@> shrinkAssetsToBurn
    <:> shrinkAssetsToMint
    <:> shrinkExtraCoinIn
    <:> shrinkExtraCoinOut
    <:> shrinkOutputsToCover
    <:> shrinkRewardWithdrawal
    <:> shrinkCerticateDepositsTaken
    <:> shrinkCerticateDepositsReturned
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
genExtraCoinIn = genCoinMostly0

genExtraCoinOut :: Gen Coin
genExtraCoinOut = genCoinMostly0

shrinkExtraCoinIn :: Coin -> [Coin]
shrinkExtraCoinIn = shrinkCoin

shrinkExtraCoinOut :: Coin -> [Coin]
shrinkExtraCoinOut = shrinkCoin

--------------------------------------------------------------------------------
-- Outputs to cover
--------------------------------------------------------------------------------

genOutputsToCover :: Gen [(Address, TokenBundle)]
genOutputsToCover = do
    count <- choose (1, 4)
    vectorOf count genOutputToCover
  where
    genOutputToCover :: Gen (Address, TokenBundle)
    genOutputToCover = frequency
        [ (49, scale (`mod` 8) genOutput)
        , (01, genOutputWith genTokenQuantityThatMayExceedLimit)
        ]
      where
        genOutput = (,)
            <$> genAddress
            <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin

    genOutputWith :: Gen TokenQuantity -> Gen (Address, TokenBundle)
    genOutputWith genTokenQuantityFn = (,)
        <$> genAddress
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

shrinkOutputsToCover :: [(Address, TokenBundle)] -> [[(Address, TokenBundle)]]
shrinkOutputsToCover = shrinkList shrinkOutput
  where
    shrinkOutput = genericRoundRobinShrink
        <@> shrinkAddress
        <:> (filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)
        <:> Nil

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

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

genUTxOAvailableForCollateral :: Gen (Map InputId Coin)
genUTxOAvailableForCollateral = genMapWith genInputId genCoinPositive
  where
    genInputId :: Gen InputId
    genInputId = genSized2 genTxIn genAddress

genUTxOAvailableForInputs :: Gen (UTxOSelection InputId)
genUTxOAvailableForInputs = frequency
    [ (49, genUTxOSelection)
    , (01, pure UTxOSelection.empty)
    ]

shrinkUTxOAvailableForCollateral :: Map InputId Coin -> [Map InputId Coin]
shrinkUTxOAvailableForCollateral =
    shrinkMapWith shrinkInputId shrinkCoinPositive
  where
    shrinkInputId :: InputId -> [InputId]
    shrinkInputId = genericRoundRobinShrink
        <@> shrinkTxIn
        <:> shrinkAddress
        <:> Nil

shrinkUTxOAvailableForInputs :: UTxOSelection InputId -> [UTxOSelection InputId]
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

instance Arbitrary Address where
    arbitrary = genAddress
    shrink = shrinkAddress

instance Arbitrary MockSelectionConstraints where
    arbitrary = genMockSelectionConstraints
    shrink = shrinkMockSelectionConstraints

instance Arbitrary (SelectionParams InputId) where
    arbitrary = genSelectionParams
    shrink = shrinkSelectionParams

instance Arbitrary (SelectionSkeleton Address) where
    arbitrary = genSelectionSkeleton
    shrink = shrinkSelectionSkeleton
