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
    , Pretty (..)
    , conjoinMap
    , counterexampleMap
    , genCoinRange
    , genMockInput
    , genTokenBundleMixed
    , shrinkMockInput
    , unMockTxConstraints
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
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
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Gen
    , Property
    , checkCoverage
    , choose
    , counterexample
    , cover
    , forAll
    , forAllShrink
    , oneof
    , property
    , shrinkList
    , tabulate
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Migration.Selection as Selection
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Migration.PlanningSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Creating migration plans" $ do

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

    parallel $ describe "Categorizing UTxO entries" $ do

        it "prop_categorizeUTxOEntries" $
            property prop_categorizeUTxOEntries
        it "prop_categorizeUTxOEntry" $
            property prop_categorizeUTxOEntry

--------------------------------------------------------------------------------
-- Creating migration plans
--------------------------------------------------------------------------------

prop_createPlan_empty :: Pretty MockTxConstraints -> Property
prop_createPlan_empty (Pretty mockConstraints) =
    withMaxSuccess 1 $
    prop_createPlan (0, 0) mockConstraints

prop_createPlan_small :: Pretty MockTxConstraints -> Property
prop_createPlan_small (Pretty mockConstraints) =
    withMaxSuccess 100 $
    prop_createPlan (1, 100) mockConstraints

prop_createPlan_large :: Pretty MockTxConstraints -> Property
prop_createPlan_large (Pretty mockConstraints) =
    withMaxSuccess 10 $
    prop_createPlan (1_000, 1_000) mockConstraints

prop_createPlan_giant :: Pretty MockTxConstraints -> Property
prop_createPlan_giant (Pretty mockConstraints) =
    withMaxSuccess 1 $
    prop_createPlan (10_000, 10_000) mockConstraints

prop_createPlan :: (Int, Int) -> MockTxConstraints -> Property
prop_createPlan inputCountRange mockConstraints =
    forAll genInputs $ \inputs ->
    forAll genRewardWithdrawal $ \reward ->
    prop_createPlan_inner mockConstraints inputs reward
  where
    genInputs :: Gen [(MockInputId, TokenBundle)]
    genInputs = do
        mockInputCount <- choose inputCountRange
        replicateM mockInputCount (genMockInput mockConstraints)
    genRewardWithdrawal :: Gen RewardWithdrawal
    genRewardWithdrawal = RewardWithdrawal <$> oneof
        [ pure (Coin 0)
        , genCoinRange (Coin 1) (Coin 1_000_000)
        ]

prop_createPlan_inner
    :: MockTxConstraints
    -> [(MockInputId, TokenBundle)]
    -> RewardWithdrawal
    -> Property
prop_createPlan_inner mockConstraints inputs reward =
    tabulate "Number of transactions required"
        [transactionCount] $
    tabulate "Mean number of inputs per transaction"
        [meanTransactionInputCount] $
    tabulate "Mean number of outputs per transaction"
        [meanTransactionOutputCount] $
    tabulate "Percentage of supporters selected"
        [percentageSelected supporters] $
    tabulate "Percentage of freeriders selected"
        [percentageSelected freeriders] $
    tabulate "Percentage of ignorables selected"
        [percentageSelected ignorables] $
    counterexample counterexampleText $
    conjoinMap
        [ ( "inputs are not preserved (union)"
          , inputIdsAll == Set.union inputIdsSelected inputIdsNotSelected )
        , ( "inputs are not preserved (intersection)"
          , Set.empty == Set.intersection inputIdsSelected inputIdsNotSelected )
        , ( "total fee is incorrect"
          , totalFee result == totalFeeExpected )
        , ( "more than one transaction has reward withdrawal"
          , rewardWithdrawalCount <= 1 )
        , ( "reward withdrawal amount incorrect"
          , rewardWithdrawalAmount == rewardWithdrawalExpected )
        , ( "one or more supporters not selected"
          , null (supporters (unselected result)) )
        ]
  where
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
    totalFeeExpected = F.foldMap fee (selections result)

    counterexampleText = counterexampleMap
        [ ( "mockConstraints"
          , show mockConstraints )
        , ( "count of supporters available"
          , show (length $ supporters categorizedUTxO) )
        , ( "count of supporters not selected"
          , show (length $ supporters $ unselected result) )
        , ( "count of freeriders available"
          , show (length $ freeriders categorizedUTxO) )
        , ( "count of freeriders not selected"
          , show (length $ freeriders $ unselected result) )
        , ( "count of ignorables available"
          , show (length $ ignorables categorizedUTxO) )
        , ( "count of ignorables not selected"
          , show (length $ ignorables $ unselected result) )
        , ( "count of reward withdrawals"
          , show rewardWithdrawalCount )
        ]

--------------------------------------------------------------------------------
-- Categorizing multiple UTxO entries
--------------------------------------------------------------------------------

prop_categorizeUTxOEntries :: Pretty MockTxConstraints -> Property
prop_categorizeUTxOEntries (Pretty mockConstraints) =
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

prop_categorizeUTxOEntry :: MockTxConstraints -> Property
prop_categorizeUTxOEntry mockConstraints =
    forAll (genTokenBundleMixed mockConstraints) prop
  where
    prop :: TokenBundle -> Property
    prop entry =
        checkCoverage $
        cover 5 (result == Supporter) "Supporter" $
        cover 5 (result == Freerider) "Freerider" $
        cover 5 (result == Ignorable) "Ignorable" $
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

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin
