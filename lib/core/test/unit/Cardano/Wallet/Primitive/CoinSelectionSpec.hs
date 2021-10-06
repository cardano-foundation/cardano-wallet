{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.CoinSelectionSpec
    where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionParams (..)
    , prepareOutputsWith
    )
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
import Data.Generics.Internal.VL.Lens
    ( view )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Gen
    , Property
    , arbitraryBoundedEnum
    , choose
    , genericShrink
    , listOf
    , property
    , shrink
    , shrinkList
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Extra
    ( chooseNatural, liftShrink8, liftShrink9, shrinkNatural )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelectionSpec" $

    parallel $ describe "Preparing outputs" $ do

        it "prop_prepareOutputsWith_twice" $
            property prop_prepareOutputsWith_twice
        it "prop_prepareOutputsWith_length" $
            property prop_prepareOutputsWith_length
        it "prop_prepareOutputsWith_assetsUnchanged" $
            property prop_prepareOutputsWith_assetsUnchanged
        it "prop_prepareOutputsWith_preparedOrExistedBefore" $
            property prop_prepareOutputsWith_preparedOrExistedBefore

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
genAssetsToBurn = genTokenMap

shrinkAssetsToMint :: TokenMap -> [TokenMap]
shrinkAssetsToMint = shrinkTokenMap

shrinkAssetsToBurn :: TokenMap -> [TokenMap]
shrinkAssetsToBurn = shrinkTokenMap

--------------------------------------------------------------------------------
-- Outputs to cover
--------------------------------------------------------------------------------

genOutputsToCover :: Gen [TxOut]
genOutputsToCover = listOf genTxOut

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
