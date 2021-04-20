{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
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
    , genMockInputAdaOnly
    , genMockTxConstraints
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
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , counterexample
    , cover
    , label
    , oneof
    , property
    , shrinkList
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

data ArgsForCreatePlan = ArgsForCreatePlan
    { mockConstraints :: MockTxConstraints
    , mockInputs :: [(MockInputId, TokenBundle)]
    , mockRewardWithdrawal :: Coin
    }
    deriving (Eq, Show)

newtype Small a = Small { unSmall :: a }
    deriving (Eq, Show)

newtype Large a = Large { unLarge :: a }
    deriving (Eq, Show)

newtype Giant a = Giant { unGiant :: a }
    deriving (Eq, Show)

instance Arbitrary (Small ArgsForCreatePlan) where
    arbitrary = Small <$> genArgsForCreatePlan
        (0, 100) genMockInput

instance Arbitrary (Large ArgsForCreatePlan) where
    arbitrary = Large <$> genArgsForCreatePlan
        (1_000, 1_000) genMockInput

instance Arbitrary (Giant ArgsForCreatePlan) where
    arbitrary = Giant <$> genArgsForCreatePlan
        (100_000, 100_000) genMockInputAdaOnly

prop_createPlan_small :: Blind (Small ArgsForCreatePlan) -> Property
prop_createPlan_small (Blind (Small args)) =
    withMaxSuccess 100 $
    prop_createPlan args

prop_createPlan_large :: Blind (Large ArgsForCreatePlan) -> Property
prop_createPlan_large (Blind (Large args)) =
    withMaxSuccess 10 $
    prop_createPlan args

prop_createPlan_giant :: Blind (Giant ArgsForCreatePlan) -> Property
prop_createPlan_giant (Blind (Giant args)) =
    withMaxSuccess 1 $
    prop_createPlan args

genArgsForCreatePlan
    :: (Int, Int)
    -- ^ Input count range
    -> (MockTxConstraints -> Gen (MockInputId, TokenBundle))
    -- ^ Genenator for inputs
    -> Gen ArgsForCreatePlan
genArgsForCreatePlan (inputCountMin, inputCountMax) genInput = do
    mockConstraints <- genMockTxConstraints
    mockInputCount <- choose (inputCountMin, inputCountMax)
    mockInputs <- replicateM mockInputCount (genInput mockConstraints)
    mockRewardWithdrawal <- oneof
        [ pure (Coin 0)
        , genCoinRange (Coin 1) (Coin 1_000_000)
        ]
    pure ArgsForCreatePlan
        { mockConstraints
        , mockInputs
        , mockRewardWithdrawal
        }

prop_createPlan :: ArgsForCreatePlan -> Property
prop_createPlan mockArgs =
    label labelTransactionCount $
    label labelMeanTransactionInputCount $
    label labelMeanTransactionOutputCount $
    label (labelNotSelectedPercentage "freeriders" freeriders) $
    label (labelNotSelectedPercentage "supporters" supporters) $
    label (labelNotSelectedPercentage "ignorables" ignorables) $

    counterexample counterexampleText $
    conjoinMap
        [ ( "inputs are not preserved"
          , inputIdsAll == Set.union inputIdsSelected inputIdsNotSelected )
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
    labelTransactionCount = pretty $ mconcat
        [ "number of transactions required: ["
        , padLeftF 3 '0' (10 * selectionCountDiv10)
        , " – "
        , padLeftF 3 '0' (10 * (selectionCountDiv10 + 1) - 1)
        , "]"
        ]
      where
        selectionCountDiv10 = selectionCount `div` 10

    labelMeanTransactionInputCount = pretty $ mconcat
        [ "mean number of inputs per transaction: ["
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

    labelMeanTransactionOutputCount = pretty $ mconcat
        [ "mean number of outputs per transaction: "
        , padLeftF 3 '0' meanTxOutputCount
        ]
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

    labelNotSelectedPercentage categoryName category = pretty $ mconcat
        [ categoryName
        , " not selected: "
        , maybe
            ("no entries available")
            (\p -> padLeftF 3 '0' p <> "%")
            (percentage)
        ]
      where
        percentage
            | entriesAvailable == 0 =
                Nothing
            | otherwise =
                Just $ (entriesNotSelected * 100) `div` entriesAvailable

        entriesAvailable :: Int
        entriesAvailable = length $ category categorizedUTxO
        entriesNotSelected :: Int
        entriesNotSelected = length $ category $ unselected result

    ArgsForCreatePlan
        { mockConstraints
        , mockInputs
        , mockRewardWithdrawal
        } = mockArgs

    constraints = unMockTxConstraints mockConstraints
    result = createPlan constraints categorizedUTxO
        (RewardWithdrawal mockRewardWithdrawal)

    categorizedUTxO = categorizeUTxOEntries constraints mockInputs

    inputIdsAll :: Set MockInputId
    inputIdsAll = Set.fromList (fst <$> mockInputs)

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
        F.foldMap rewardWithdrawal (selections result)
    rewardWithdrawalExpected
        | selectionCount == 0 =
            Coin 0
        | otherwise =
            mockRewardWithdrawal

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

data ArgsForCategorizeUTxOEntries = ArgsForCategorizeUTxOEntries
    { mockConstraints :: MockTxConstraints
    , mockEntries :: [(MockInputId, TokenBundle)]
    }
    deriving (Eq, Generic, Show)

instance Arbitrary ArgsForCategorizeUTxOEntries where
    arbitrary = genArgsForCategorizeUTxOEntries
    shrink = shrinkArgsForCategorizeUTxOEntries

genArgsForCategorizeUTxOEntries :: Gen ArgsForCategorizeUTxOEntries
genArgsForCategorizeUTxOEntries = do
    mockConstraints <- genMockTxConstraints
    mockEntryCount <- choose (0, 100)
    mockEntries <- replicateM mockEntryCount (genMockInput mockConstraints)
    pure ArgsForCategorizeUTxOEntries {mockConstraints, mockEntries}

shrinkArgsForCategorizeUTxOEntries
    :: ArgsForCategorizeUTxOEntries -> [ArgsForCategorizeUTxOEntries]
shrinkArgsForCategorizeUTxOEntries args = do
    mockEntriesShrunk <- shrinkList shrinkMockInput (mockEntries args)
    pure ArgsForCategorizeUTxOEntries
        { mockConstraints = view #mockConstraints args
        , mockEntries = mockEntriesShrunk
        }

prop_categorizeUTxOEntries :: Pretty ArgsForCategorizeUTxOEntries -> Property
prop_categorizeUTxOEntries args =
    Pretty (L.sortOn fst (uncategorizeUTxOEntries categorizedEntries))
        === Pretty (L.sortOn fst mockEntries)
  where
    categorizedEntries = categorizeUTxOEntries constraints mockEntries
    Pretty ArgsForCategorizeUTxOEntries
        { mockConstraints
        , mockEntries
        } = args
    constraints = unMockTxConstraints mockConstraints

--------------------------------------------------------------------------------
-- Categorizing individual UTxO entries
--------------------------------------------------------------------------------

data ArgsForCategorizeUTxOEntry = ArgsForCategorizeUTxOEntry
    { mockConstraints :: MockTxConstraints
    , mockEntry :: TokenBundle
    }
    deriving (Eq, Show)

instance Arbitrary ArgsForCategorizeUTxOEntry where
    arbitrary = genArgsForCategorizeUTxOEntry

genArgsForCategorizeUTxOEntry :: Gen ArgsForCategorizeUTxOEntry
genArgsForCategorizeUTxOEntry = do
    mockConstraints <- genMockTxConstraints
    mockEntry <- genTokenBundleMixed mockConstraints
    pure ArgsForCategorizeUTxOEntry {..}

prop_categorizeUTxOEntry :: ArgsForCategorizeUTxOEntry -> Property
prop_categorizeUTxOEntry mockArgs =
    checkCoverage $
    cover 5 (result == Supporter) "Supporter" $
    cover 5 (result == Freerider) "Freerider" $
    cover 5 (result == Ignorable) "Ignorable" $
    property
        $ selectionCreateExpectation
        $ Selection.create constraints
            (RewardWithdrawal $ Coin 0) [((), mockEntry)]
  where
    ArgsForCategorizeUTxOEntry
        { mockConstraints
        , mockEntry
        } = mockArgs
    constraints = unMockTxConstraints mockConstraints
    result = categorizeUTxOEntry constraints mockEntry
    selectionCreateExpectation = case result of
        Supporter -> isRight
        Freerider -> isLeft
        Ignorable -> isLeft

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin
