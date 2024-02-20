{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeqSpec
    ( spec
    ) where

import Prelude hiding
    ( seq
    )

import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq
    )
import Data.Function
    ( (&)
    )
import GHC.Generics
    ( Generic
    )
import Safe
    ( tailMay
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary
    , Fun
    , Function
    , Gen
    , Property
    , applyFun
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , cover
    , genericShrink
    , listOf
    , property
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Classes
    ( bifoldableLaws
    , bifunctorLaws
    , eqLaws
    , foldableLaws
    , functorLaws
    , showLaws
    )
import Test.Utils.Laws
    ( testLawsMany
    )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "Instances" $ do

        testLawsMany @(StateDeltaSeq TestState TestDelta)
            [ eqLaws
            , showLaws
            ]
        testLawsMany @(StateDeltaSeq TestState)
            [ foldableLaws
            , functorLaws
            ]
        testLawsMany @StateDeltaSeq
            [ bifoldableLaws
            , bifunctorLaws
            ]

    describe "Constructors" $ do

        describe "fromState" $ do
            it "prop_fromState_isValid" $
                prop_fromState_isValid
                    & property
            it "prop_fromState_headState" $
                prop_fromState_headState
                    & property
            it "prop_fromState_lastState" $
                prop_fromState_lastState
                    & property
            it "prop_fromState_length" $
                prop_fromState_length
                    & property

        describe "fromStateDeltas" $ do
            it "prop_fromStateDeltas_isValid" $
                prop_fromStateDeltas_isValid
                    & property
            it "prop_fromStateDeltas_headState" $
                prop_fromStateDeltas_headState
                    & property
            it "prop_fromStateDeltas_lastState" $
                prop_fromStateDeltas_lastState
                    & property
            it "prop_fromStateDeltas_length" $
                prop_fromStateDeltas_length
                    & property

    describe "Conversions" $ do

        describe "toTransitionList" $ do
            it "prop_toTransitionList_consecutivePairs" $
                prop_toTransitionList_consecutivePairs
                    & property
            it "prop_toTransitionList_countTransitions" $
                prop_toTransitionList_countTransitions
                    & property
            it "prop_toTransitionList_length" $
                prop_toTransitionList_length
                    & property
            it "prop_toTransitionList_nextState" $
                prop_toTransitionList_nextState
                    & property
            it "prop_toTransitionList_toDeltaList" $
                prop_toTransitionList_toDeltaList
                    & property
            it "prop_toTransitionList_toStateList_initials" $
                prop_toTransitionList_toStateList_initials
                    & property
            it "prop_toTransitionList_toStateList_finals" $
                prop_toTransitionList_toStateList_finals
                    & property

    describe "Counts" $ do

        describe "countEmptyTransitionsWhere" $ do
            it "prop_countEmptyTransitionsWhere_coverage" $
                prop_countEmptyTransitionsWhere_coverage
                    & property

    describe "Extension" $ do

        describe "applyDeltas" $ do
            it "prop_applyDeltas_headState" $
                prop_applyDeltas_headState
                    & property
            it "prop_applyDeltas_length" $
                prop_applyDeltas_length
                    & property

    describe "Shrinking" $ do

        describe "dropEmptyTransitions" $ do
            it "prop_dropEmptyTransitions_toStateList" $
                prop_dropEmptyTransitions_toStateList
                    & property

        describe "dropEmptyTransitionWhere" $ do
            it "prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere" $
                prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere
                    & property
            it "prop_dropEmptyTransitionWhere_isValid" $
                prop_dropEmptyTransitionWhere_isValid
                    & property
            it "prop_dropEmptyTransitionWhere_headState" $
                prop_dropEmptyTransitionWhere_headState
                    & property
            it "prop_dropEmptyTransitionWhere_lastState" $
                prop_dropEmptyTransitionWhere_lastState
                    & property
            it "prop_dropEmptyTransitionWhere_length" $
                prop_dropEmptyTransitionWhere_length
                    & property

        describe "dropEmptyTransitionsWhere" $ do
            it "prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere" $
                prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere
                    & property
            it "prop_dropEmptyTransitionsWhere_isValid" $
                prop_dropEmptyTransitionsWhere_isValid
                    & property
            it "prop_dropEmptyTransitionsWhere_headState" $
                prop_dropEmptyTransitionsWhere_headState
                    & property
            it "prop_dropEmptyTransitionsWhere_lastState" $
                prop_dropEmptyTransitionsWhere_lastState
                    & property
            it "prop_dropEmptyTransitionsWhere_length" $
                prop_dropEmptyTransitionsWhere_length
                    & property

        describe "prefixes" $ do
            it "prop_prefixes_head" $
                prop_prefixes_head
                    & property
            it "prop_prefixes_last" $
                prop_prefixes_last
                    & property
            it "prop_prefixes_length" $
                prop_prefixes_length
                    & property
            it "prop_prefixes_isPrefixOf" $
                prop_prefixes_isPrefixOf
                    & property
            it "prop_prefixes_isValid" $
                prop_prefixes_isValid
                    & property

        describe "suffixes" $ do
            it "prop_suffixes_head" $
                prop_suffixes_head
                    & property
            it "prop_suffixes_last" $
                prop_suffixes_last
                    & property
            it "prop_suffixes_length" $
                prop_suffixes_length
                    & property
            it "prop_suffixes_isSuffixOf" $
                prop_suffixes_isSuffixOf
                    & property
            it "prop_suffixes_isValid" $
                prop_suffixes_isValid
                    & property

--------------------------------------------------------------------------------
-- fromState
--------------------------------------------------------------------------------

prop_fromState_isValid :: TestState -> Property
prop_fromState_isValid state =
    Seq.isValid applyTestDelta (Seq.fromState state)
    === True

prop_fromState_headState :: TestState -> Property
prop_fromState_headState state =
    Seq.headState (Seq.fromState state)
    === state

prop_fromState_lastState :: TestState -> Property
prop_fromState_lastState state =
    Seq.lastState (Seq.fromState state)
    === state

prop_fromState_length :: TestState -> Property
prop_fromState_length state =
    length (Seq.fromState state)
    === 0

--------------------------------------------------------------------------------
-- fromStateDeltas
--------------------------------------------------------------------------------

prop_fromStateDeltas_isValid :: TestState -> [TestDelta] -> Property
prop_fromStateDeltas_isValid state deltas =
    Seq.isValid applyTestDelta (Seq.fromStateDeltas applyTestDelta state deltas)
    === True

prop_fromStateDeltas_headState :: TestState -> [TestDelta] -> Property
prop_fromStateDeltas_headState state deltas =
    Seq.headState (Seq.fromStateDeltas applyTestDelta state deltas)
    === state

prop_fromStateDeltas_lastState :: TestState -> [TestDelta] -> Property
prop_fromStateDeltas_lastState state deltas =
    Seq.lastState (Seq.fromStateDeltas applyTestDelta state deltas)
    === F.foldl' applyTestDelta state deltas

prop_fromStateDeltas_length :: TestState -> [TestDelta] -> Property
prop_fromStateDeltas_length state deltas =
    length (Seq.fromStateDeltas applyTestDelta state deltas)
    === length deltas

--------------------------------------------------------------------------------
-- toTransitionList
--------------------------------------------------------------------------------

prop_toTransitionList_consecutivePairs
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_consecutivePairs (TestStateDeltaSeq seq) =
    all (\((_, _, sf), (si, _, _)) -> sf == si)
        (consecutivePairs (Seq.toTransitionList seq))
    === True

prop_toTransitionList_countTransitions
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_countTransitions (TestStateDeltaSeq seq) =
    length (Seq.toTransitionList seq)
    === Seq.countTransitions seq

prop_toTransitionList_length
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_length (TestStateDeltaSeq seq) =
    length (Seq.toTransitionList seq)
    === length seq

prop_toTransitionList_nextState
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_nextState (TestStateDeltaSeq seq) =
    all (\(si, d, sf) -> applyTestDelta si d == sf) (Seq.toTransitionList seq)
    === True

prop_toTransitionList_toDeltaList
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_toDeltaList (TestStateDeltaSeq seq) =
    fmap (\(_, d, _) -> d) (Seq.toTransitionList seq)
    === Seq.toDeltaList seq

prop_toTransitionList_toStateList_initials
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_toStateList_initials (TestStateDeltaSeq seq) =
    fmap (\(si, _, _) -> si) (Seq.toTransitionList seq)
    === NE.take (length seq) (Seq.toStateList seq)

prop_toTransitionList_toStateList_finals
    :: TestStateDeltaSeq -> Property
prop_toTransitionList_toStateList_finals (TestStateDeltaSeq seq) =
    fmap (\(_, _, sf) -> sf) (Seq.toTransitionList seq)
    === NE.drop 1 (Seq.toStateList seq)

--------------------------------------------------------------------------------
-- applyDeltas
--------------------------------------------------------------------------------

prop_applyDeltas_headState
    :: TestStateDeltaSeq -> [TestDelta] -> Property
prop_applyDeltas_headState (TestStateDeltaSeq seq) deltas =
    Seq.headState (Seq.applyDeltas applyTestDelta deltas seq)
    === Seq.headState seq

prop_applyDeltas_length
    :: TestStateDeltaSeq -> [TestDelta] -> Property
prop_applyDeltas_length (TestStateDeltaSeq seq) deltas =
    length (Seq.applyDeltas applyTestDelta deltas seq)
    === length seq + length deltas

--------------------------------------------------------------------------------
-- countEmptyTransitionsWhere
--------------------------------------------------------------------------------

prop_countEmptyTransitionsWhere_coverage
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_countEmptyTransitionsWhere_coverage
    (TestStateDeltaSeq seq) (applyFun -> f) =
        checkCoverage $
        cover 10
            (strictlyIncreasing [0, matchCount, emptyCount, length seq])
            "strictlyIncreasing [0, matchCount, emptyCount, length seq]" $
        property True
  where
    emptyCount = Seq.countEmptyTransitions seq
    matchCount = Seq.countEmptyTransitionsWhere f seq

--------------------------------------------------------------------------------
-- dropEmptyTransitions
--------------------------------------------------------------------------------

prop_dropEmptyTransitions_toStateList
    :: TestStateDeltaSeq -> Property
prop_dropEmptyTransitions_toStateList (TestStateDeltaSeq seq) =
        NE.toList (Seq.toStateList $ Seq.dropEmptyTransitions seq)
        === removeConsecutiveDuplicates (NE.toList $ Seq.toStateList seq)

--------------------------------------------------------------------------------
-- dropEmptyTransitionWhere
--------------------------------------------------------------------------------

prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all ((== pred emptyTransitionCount) . Seq.countEmptyTransitionsWhere f)
            (Seq.dropEmptyTransitionWhere f seq)
        === True
  where
    emptyTransitionCount = Seq.countEmptyTransitionsWhere f seq

prop_dropEmptyTransitionWhere_isValid
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_isValid
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all (Seq.isValid applyTestDelta) (Seq.dropEmptyTransitionWhere f seq)
        === True

prop_dropEmptyTransitionWhere_headState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_headState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all ((== Seq.headState seq) . Seq.headState)
            (Seq.dropEmptyTransitionWhere f seq)
        === True

prop_dropEmptyTransitionWhere_lastState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_lastState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all ((== Seq.lastState seq) . Seq.lastState)
            (Seq.dropEmptyTransitionWhere f seq)
        === True

prop_dropEmptyTransitionWhere_length
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_length
    (TestStateDeltaSeq seq) (applyFun -> f) =
        length (Seq.dropEmptyTransitionWhere f seq)
        === Seq.countEmptyTransitionsWhere f seq

--------------------------------------------------------------------------------
-- dropEmptyTransitionsWhere
--------------------------------------------------------------------------------

prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.countEmptyTransitionsWhere f (Seq.dropEmptyTransitionsWhere f seq)
        === 0

prop_dropEmptyTransitionsWhere_isValid
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_isValid
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.isValid applyTestDelta (Seq.dropEmptyTransitionsWhere f seq)
        === True

prop_dropEmptyTransitionsWhere_headState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_headState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.headState (Seq.dropEmptyTransitionsWhere f seq)
        === Seq.headState seq

prop_dropEmptyTransitionsWhere_lastState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_lastState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.lastState (Seq.dropEmptyTransitionsWhere f seq)
        === Seq.lastState seq

prop_dropEmptyTransitionsWhere_length
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_length
    (TestStateDeltaSeq seq) (applyFun -> f) =
        length (Seq.dropEmptyTransitionsWhere f seq)
            + Seq.countEmptyTransitionsWhere f seq
        === length seq

--------------------------------------------------------------------------------
-- prefixes
--------------------------------------------------------------------------------

prop_prefixes_head
    :: TestStateDeltaSeq -> Property
prop_prefixes_head (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.prefixes seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.head ss === Seq.fromState (Seq.headState seq)

prop_prefixes_last
    :: TestStateDeltaSeq -> Property
prop_prefixes_last (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.prefixes seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            Just (NE.last ss) === Seq.dropLast seq

prop_prefixes_length
    :: TestStateDeltaSeq -> Property
prop_prefixes_length (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.prefixes seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.length ss === length seq

prop_prefixes_isPrefixOf
    :: TestStateDeltaSeq -> Property
prop_prefixes_isPrefixOf (TestStateDeltaSeq seq) =
    all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.prefixes seq))
    === True

prop_prefixes_isValid
    :: TestStateDeltaSeq -> Property
prop_prefixes_isValid (TestStateDeltaSeq seq) =
    all (Seq.isValid applyTestDelta) (Seq.prefixes seq)
    === True

--------------------------------------------------------------------------------
-- suffixes
--------------------------------------------------------------------------------

prop_suffixes_head
    :: TestStateDeltaSeq -> Property
prop_suffixes_head (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.suffixes seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.head ss === Seq.fromState (Seq.lastState seq)

prop_suffixes_last
    :: TestStateDeltaSeq -> Property
prop_suffixes_last (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.suffixes seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            Just (NE.last ss) === Seq.dropHead seq

prop_suffixes_length
    :: TestStateDeltaSeq -> Property
prop_suffixes_length (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.suffixes seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.length ss === length seq

prop_suffixes_isSuffixOf
    :: TestStateDeltaSeq -> Property
prop_suffixes_isSuffixOf (TestStateDeltaSeq seq) =
    all (uncurry Seq.isSuffixOf) (consecutivePairs (Seq.suffixes seq))
    === True

prop_suffixes_isValid
    :: TestStateDeltaSeq -> Property
prop_suffixes_isValid (TestStateDeltaSeq seq) =
    all (Seq.isValid applyTestDelta) (Seq.suffixes seq)
    === True

--------------------------------------------------------------------------------
-- Test states
--------------------------------------------------------------------------------

newtype TestState = TestState {unTestState :: Int}
    deriving (Eq, Generic, Show)

genTestState :: Gen TestState
genTestState = TestState <$> choose (0, 3)

shrinkTestState :: TestState -> [TestState]
shrinkTestState = shrinkMapBy TestState unTestState shrink

--------------------------------------------------------------------------------
-- Test delta functions
--------------------------------------------------------------------------------

data TestDeltaFn
    = Add
    | Sub
    | Mul
    deriving (Bounded, Enum, Eq, Generic, Show)

applyTestDeltaFn :: TestDeltaFn -> (Int -> Int -> Int)
applyTestDeltaFn = \case
    Add -> (+)
    Sub -> (-)
    Mul -> (*)

genTestDeltaFn :: Gen TestDeltaFn
genTestDeltaFn = arbitraryBoundedEnum

shrinkTestDeltaFn :: TestDeltaFn -> [TestDeltaFn]
shrinkTestDeltaFn = genericShrink

--------------------------------------------------------------------------------
-- Test deltas
--------------------------------------------------------------------------------

data TestDelta = TestDelta TestDeltaFn Int
    deriving (Eq, Generic, Show)

applyTestDelta :: TestState -> TestDelta -> TestState
applyTestDelta (TestState i) (TestDelta fn j) =
    TestState $ applyTestDeltaFn fn i j

genTestDelta :: Gen TestDelta
genTestDelta = TestDelta
    <$> genTestDeltaFn
    <*> choose (0, 3)

shrinkTestDelta :: TestDelta -> [TestDelta]
shrinkTestDelta = genericShrink

--------------------------------------------------------------------------------
-- Test state delta sequences
--------------------------------------------------------------------------------

newtype TestStateDeltaSeq = TestStateDeltaSeq
    {unTestStateDeltaSeq :: StateDeltaSeq TestState TestDelta}
    deriving (Eq, Show)

genTestStateDeltaSeq :: Gen TestStateDeltaSeq
genTestStateDeltaSeq =
    fmap TestStateDeltaSeq . Seq.fromStateDeltas applyTestDelta
        <$> genTestState
        <*> listOf genTestDelta

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary s, Arbitrary d) => Arbitrary (StateDeltaSeq s d) where
    arbitrary = Seq.fromStateDeltasUnchecked
        <$> arbitrary @s
        <*> arbitrary @([(d, s)])

instance Arbitrary TestStateDeltaSeq where
    arbitrary = genTestStateDeltaSeq

instance Arbitrary TestDeltaFn where
    arbitrary = genTestDeltaFn
    shrink = shrinkTestDeltaFn

instance Arbitrary TestState where
    arbitrary = genTestState
    shrink = shrinkTestState

instance Arbitrary TestDelta where
    arbitrary = genTestDelta
    shrink = shrinkTestDelta

deriving anyclass instance CoArbitrary TestDelta
deriving anyclass instance CoArbitrary TestDeltaFn

deriving anyclass instance Function TestDelta
deriving anyclass instance Function TestDeltaFn

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

removeConsecutiveDuplicates :: (Foldable f, Eq a) => f a -> [a]
removeConsecutiveDuplicates = loop . F.toList
  where
    loop = \case
        [ ] -> [ ]
        [a] -> [a]
        (a1 : a2 : as)
            | a1 == a2 -> loop (a2 : as)
            | otherwise -> a1 : loop (a2 : as)

strictlyIncreasing :: (Foldable f, Ord a) => f a -> Bool
strictlyIncreasing as = all (uncurry (<)) (consecutivePairs as)
