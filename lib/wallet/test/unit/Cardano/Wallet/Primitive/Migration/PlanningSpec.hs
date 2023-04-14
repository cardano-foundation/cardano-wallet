{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Migration.PlanningSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Migration.Planning
    ( CategorizedUTxO (..)
    , MigrationPlan (..)
    , UTxOEntryCategory (..)
    , categorizeUTxOEntries
    , categorizeUTxOEntry
    , createPlan
    , uncategorizeUTxOEntries
    )
import Cardano.Wallet.Primitive.Migration.Selection
    ( RewardWithdrawal (..), Selection (..) )
import Cardano.Wallet.Primitive.Migration.SelectionSpec
    ( MockInputId
    , MockTxConstraints (..)
    , genMockInput
    , genRewardWithdrawal
    , genTokenBundleMixed
    , shrinkMockInput
    , testAll
    , unMockTxConstraints
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Control.Monad
    ( replicateM )
import Data.Either
    ( isLeft, isRight )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Set
    ( Set )
import Fmt
    ( padLeftF, pretty )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Blind (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , cover
    , forAllBlind
    , forAllShrink
    , property
    , shrinkList
    , tabulate
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Extra
    ( report, verify )
import Test.Utils.Pretty
    ( Pretty (..) )

import qualified Cardano.Wallet.Primitive.Migration.Selection as Selection
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Migration.PlanningSpec" $

    modifyMaxSuccess (const 1_000) $ do

    describe "Creating migration plans" $ do

        describe "Empty migrations" $
            it "prop_createPlan_empty" $
                property prop_createPlan_empty

        describe "Small migrations" $
            it "prop_createPlan_small" $
                property prop_createPlan_small

        describe "Large migrations" $
            it "prop_createPlan_large" $
                property prop_createPlan_large

        describe "Giant migrations" $
            it "prop_createPlan_giant" $
                property prop_createPlan_giant

    describe "Categorizing UTxO entries" $ do

        it "prop_categorizeUTxOEntries" $
            property prop_categorizeUTxOEntries
        it "prop_categorizeUTxOEntry" $
            property prop_categorizeUTxOEntry

--------------------------------------------------------------------------------
-- Creating migration plans
--------------------------------------------------------------------------------

prop_createPlan_empty :: Blind MockTxConstraints -> Property
prop_createPlan_empty (Blind mockConstraints) =
    withMaxSuccess 1 $
    prop_createPlan (0, 0) mockConstraints

prop_createPlan_small :: Blind MockTxConstraints -> Property
prop_createPlan_small (Blind mockConstraints) =
    withMaxSuccess 100 $
    prop_createPlan (1, 100) mockConstraints

prop_createPlan_large :: Blind MockTxConstraints -> Property
prop_createPlan_large (Blind mockConstraints) =
    withMaxSuccess 10 $
    prop_createPlan (1_000, 1_000) mockConstraints

prop_createPlan_giant :: Blind MockTxConstraints -> Property
prop_createPlan_giant (Blind mockConstraints) =
    withMaxSuccess 1 $
    prop_createPlan (10_000, 10_000) mockConstraints

prop_createPlan :: (Int, Int) -> MockTxConstraints -> Property
prop_createPlan inputCountRange mockConstraints =
    forAllBlind genInputs $ \inputs ->
    forAllBlind genRewardWithdrawal $ \reward ->
    prop_createPlan_inner mockConstraints inputs reward
  where
    genInputs :: Gen [(MockInputId, TokenBundle)]
    genInputs = do
        mockInputCount <- choose inputCountRange
        replicateM mockInputCount (genMockInput mockConstraints)

prop_createPlan_inner
    :: MockTxConstraints
    -> [(MockInputId, TokenBundle)]
    -> RewardWithdrawal
    -> Property
prop_createPlan_inner mockConstraints inputs reward =
    makeReports $ makeStatistics $ testAll makeTests
  where
    makeTests
        = verify
            (inputIdsAll == Set.union inputIdsSelected inputIdsNotSelected)
            "inputs are preserved (union)"
        . verify
            (Set.empty == Set.intersection inputIdsSelected inputIdsNotSelected)
            "inputs are preserved (intersection)"
        . verify
            (totalInputAda >= totalOutputAda)
            "ada is consumed and not created"
        . verify
            (totalInputTokenBalance == totalOutputTokenBalance)
            "balance of non-ada tokens is preserved"
        . verify
            (totalFee result == totalFeeExpected)
            "total fee is correct"
        . verify
            (rewardWithdrawalCount <= 1)
            "at most one transaction has reward withdrawal"
        . verify
            (rewardWithdrawalAmount == rewardWithdrawalExpected)
            "reward withdrawal amount correct"
        . verify
            (null (supporters (unselected result)))
            "every supporter is selected"

    makeReports
        = report mockConstraints
            "mockConstraints"
        . report (length $ supporters categorizedUTxO)
            "count of supporters available"
        . report (length $ supporters $ unselected result)
            "count of supporters not selected"
        . report (length $ freeriders categorizedUTxO)
            "count of freeriders available"
        . report (length $ freeriders $ unselected result)
            "count of freeriders not selected"
        . report (length $ ignorables categorizedUTxO)
            "count of ignorables available"
        . report (length $ ignorables $ unselected result)
            "count of ignorables not selected"
        . report rewardWithdrawalCount
            "count of reward withdrawals"
        . report totalInputAda
            "total input ada"
        . report totalOutputAda
            "total output ada"
        . report totalFeeExpected
            "total fee expected"
        . report (totalFee result)
            "total fee actual"
        . report totalInputTokenBalance
            "total input token balance"
        . report totalOutputTokenBalance
            "total output token balance"

    makeStatistics
        = tabulate "Number of transactions required"
            [transactionCount]
        . tabulate "Mean number of inputs per transaction"
            [meanTransactionInputCount]
        . tabulate "Mean number of outputs per transaction"
            [meanTransactionOutputCount]
        . tabulate "Percentage of supporters selected"
            [percentageSelected supporters]
        . tabulate "Percentage of freeriders selected"
            [percentageSelected freeriders]
        . tabulate "Percentage of ignorables selected"
            [percentageSelected ignorables]

    transactionCount = pretty $ mconcat
        [ "["
        , padLeftF 3 '0' (10 * selectionCountDiv10)
        , " – "
        , padLeftF 3 '0' (10 * (selectionCountDiv10 + 1) - 1)
        , "]"
        ]
      where
        selectionCountDiv10 = selectionCount `div` 10

    meanTransactionInputCount = pretty $ mconcat
        [ "["
        , padLeftF 3 '0' (10 * meanTxInputCountDiv10)
        , " – "
        , padLeftF 3 '0' (10 * (meanTxInputCountDiv10 + 1) - 1)
        , "]"
        ]
      where
        meanTxInputCountDiv10 = meanTxInputCount `div` 10
        meanTxInputCount :: Int
        meanTxInputCount
            | selectionCount == 0 =
                0
            | otherwise =
                totalSelectedInputCount `div` selectionCount
        totalSelectedInputCount :: Int
        totalSelectedInputCount =
            L.sum $ L.length . view #inputIds <$> selections result

    meanTransactionOutputCount = pretty $
        padLeftF 3 ' ' meanTxOutputCount
      where
        meanTxOutputCount :: Int
        meanTxOutputCount
            | selectionCount == 0 =
                0
            | otherwise =
                totalSelectedOutputCount `div` selectionCount
        totalSelectedOutputCount :: Int
        totalSelectedOutputCount =
            L.sum $ L.length . view #outputs <$> selections result

    percentageSelected category = pretty $
        padLeftF 3 ' ' percentage <> "%"
      where
        percentage :: Int
        percentage
            | entriesAvailable == 0 =
                100
            | otherwise =
                100 - ((entriesNotSelected * 100) `div` entriesAvailable)

        entriesAvailable :: Int
        entriesAvailable = length $ category categorizedUTxO
        entriesNotSelected :: Int
        entriesNotSelected = length $ category $ unselected result

    constraints = unMockTxConstraints mockConstraints
    result = createPlan constraints categorizedUTxO reward

    categorizedUTxO = categorizeUTxOEntries constraints inputs

    inputIdsAll :: Set MockInputId
    inputIdsAll = Set.fromList (fst <$> inputs)

    inputIdsSelected :: Set MockInputId
    inputIdsSelected = Set.fromList
        [ i
        | s <- selections result
        , i <- NE.toList (view #inputIds s)
        ]

    inputIdsNotSelected :: Set MockInputId
    inputIdsNotSelected = Set.fromList
        $ fmap fst
        $ uncategorizeUTxOEntries
        $ unselected result

    rewardWithdrawalCount =
        length $ filter (> Coin 0) (rewardWithdrawal <$> selections result)
    rewardWithdrawalAmount =
        RewardWithdrawal $ F.foldMap rewardWithdrawal (selections result)
    rewardWithdrawalExpected
        | selectionCount == 0 =
            RewardWithdrawal $ Coin 0
        | otherwise =
            reward

    selectionCount = length (selections result)

    totalFeeExpected :: Coin
    totalFeeExpected
        | not (null (selections result)) =
            Coin.distance totalInputAda totalOutputAda
        | otherwise =
            Coin 0

    totalInputAda :: Coin
    totalInputAda = mconcat
        [ F.foldMap (view #coin . view #inputBalance) (selections result)
        , unRewardWithdrawal reward
        ]

    totalOutputAda :: Coin
    totalOutputAda =
        F.foldMap (view #coin . F.fold . view #outputs) (selections result)

    totalInputTokenBalance :: TokenMap
    totalInputTokenBalance =
        F.foldMap (view #tokens . view #inputBalance) (selections result)

    totalOutputTokenBalance :: TokenMap
    totalOutputTokenBalance =
        F.foldMap (view #tokens . F.fold . view #outputs) (selections result)

--------------------------------------------------------------------------------
-- Categorizing multiple UTxO entries
--------------------------------------------------------------------------------

prop_categorizeUTxOEntries :: Blind MockTxConstraints -> Property
prop_categorizeUTxOEntries (Blind mockConstraints) =
    forAllShrink genEntries (shrinkList shrinkMockInput) prop
  where
    prop :: [(MockInputId, TokenBundle)] -> Property
    prop entries = (===)
        (Pretty $ L.sortOn fst $ uncategorizeUTxOEntries categorizedEntries)
        (Pretty $ L.sortOn fst entries)
      where
        categorizedEntries = categorizeUTxOEntries constraints entries
        constraints = unMockTxConstraints mockConstraints

    genEntries :: Gen [(MockInputId, TokenBundle)]
    genEntries = do
        mockEntryCount <- choose (1, 100)
        replicateM mockEntryCount (genMockInput mockConstraints)

--------------------------------------------------------------------------------
-- Categorizing individual UTxO entries
--------------------------------------------------------------------------------

prop_categorizeUTxOEntry :: Blind MockTxConstraints -> Property
prop_categorizeUTxOEntry (Blind mockConstraints) =
    forAllBlind (genTokenBundleMixed mockConstraints) prop
  where
    prop :: TokenBundle -> Property
    prop entry =
        checkCoverage $
        cover 5 (result == Supporter) "Supporter" $
        cover 5 (result == Freerider) "Freerider" $
        cover 5 (result == Ignorable) "Ignorable" $
        report mockConstraints "mockConstraints" $
        property
            $ selectionCreateExpectation
            $ Selection.create constraints
                (RewardWithdrawal $ Coin 0) [((), entry)]
      where
        constraints = unMockTxConstraints mockConstraints
        result = categorizeUTxOEntry constraints entry
        selectionCreateExpectation = case result of
            Supporter -> isRight
            Freerider -> isLeft
            Ignorable -> isLeft
