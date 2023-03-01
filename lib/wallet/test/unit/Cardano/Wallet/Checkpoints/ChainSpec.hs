{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Checkpoints.ChainSpec (
    spec,
) where

import Prelude

import Cardano.Wallet.Checkpoints.Chain
    ( Chain, DeltaChain (..), bootChain, member, origin, summary, tip )
import Data.Delta
    ( Delta (..) )
import Data.List
    ( sort )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , counterexample
    , elements
    , forAll
    , property
    , (===)
    , (==>)
    )

import Data.Foldable
    ( foldl' )
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "checkpoints chain" $ do
        describe "append transition" $ do
            it "add a new transition" $
                property prop_appendTransition_addsNewTransition
            it "doesn't change the old data" $
                property prop_appendTransition_preservesOldData
        describe "rollback to" $ do
            it "removes all transitions after the given timestamp" $
                property prop_rollbackTo_removesTransitions
            it "doesn't remove other transitions" $
                property prop_rollbackTo_preservesOtherTransitions
        describe "collapse transitions" $ do
            it "removes all transitions at timestamps in the given set" $
                property prop_collapseTransitions_removesTransitions
            it "preserve the summary" $
                property prop_collapseTransitions_preservesSummary
            it "doesn't remove other transitions" $
                property prop_collapsesTransitions_preservesOtherTransitions

type Chain' = Chain Int [Int]

genChain :: Gen (Chain', [(Int, [Int])])
genChain = do
    timestamps <- genTimestamps
    transitions <- genTransitions
    let xs = zip timestamps transitions
        chain = foldl'
            (\c (t, ts) -> apply (AppendTransition t ts) c)
            (bootChain 0) xs
    return (chain , (0, []) : xs)
  where
    genTimestamps :: Gen [Int]
    genTimestamps = fmap sort $ fmap getPositive <$> arbitrary
    genTransitions :: Gen [[Int]]
    genTransitions = arbitrary

prop_appendTransition_addsNewTransition :: Property
prop_appendTransition_addsNewTransition =
    forAll genChain $ \(chain, _) -> do
        let (t, _) = tip chain
        forAll arbitrary $ \timestamp -> do
            forAll arbitrary $ \transition ->
                counterexample (show (chain, timestamp,transition)) $ do
                    let newChain =
                            apply (AppendTransition timestamp transition) chain
                    timestamp > t ==> tip newChain === (timestamp, transition)

prop_appendTransition_preservesOldData :: Property
prop_appendTransition_preservesOldData =
    forAll genChain $ \(chain, old) -> do
        forAll arbitrary $ \timestamp -> do
            forAll arbitrary $ \transition -> do
                let newChain = apply (AppendTransition timestamp transition) chain
                forAll (elements old) $ \(oldTimestamp, _) -> do
                    member oldTimestamp newChain === member oldTimestamp chain

prop_collapseTransitions_removesTransitions :: Property
prop_collapseTransitions_removesTransitions =
    forAll genChain $ \(chain, _old) -> do
        forAll arbitrary $ \(NonEmpty timestamps) -> do
            let newChain = apply (CollapseTransitions $ Set.fromList timestamps) chain
            forAll (elements timestamps) $ \oldTimestamp -> do
                counterexample (show (chain, timestamps, newChain)) $
                    oldTimestamp > origin chain ==> member oldTimestamp newChain === False

prop_collapseTransitions_preservesSummary :: Property
prop_collapseTransitions_preservesSummary =
    forAll genChain $ \(chain, _old) -> do
        forAll arbitrary $ \timestamps -> do
            let newChain = apply (CollapseTransitions $ Set.fromList timestamps) chain
            counterexample (show (chain, timestamps, newChain)) $
                summary newChain ===
                    summary (apply (RollbackTo $ fst (tip newChain)) chain)

prop_collapsesTransitions_preservesOtherTransitions :: Property
prop_collapsesTransitions_preservesOtherTransitions =
    forAll genChain $ \(chain, old) -> do
        forAll arbitrary $ \timestamps -> do
            let newChain = apply (CollapseTransitions $ Set.fromList timestamps) chain
            forAll (elements old) $ \(oldTimestamp, _) ->
                oldTimestamp `notElem` timestamps
                    ==> member oldTimestamp newChain === True

prop_rollbackTo_removesTransitions :: Property
prop_rollbackTo_removesTransitions =
    forAll genChain $ \(chain, old) -> do
        forAll arbitrary $ \timestamp -> do
            let newChain = apply (RollbackTo timestamp) chain
            forAll (elements old) $ \(oldTimestamp, _) ->
                    counterexample (show (chain, timestamp,oldTimestamp,  newChain))
                        $ oldTimestamp > timestamp && oldTimestamp > origin chain
                            ==> member oldTimestamp newChain === False

prop_rollbackTo_preservesOtherTransitions :: Property
prop_rollbackTo_preservesOtherTransitions =
    forAll genChain $ \(chain, old) -> do
        forAll arbitrary $ \timestamp -> do
            let newChain = apply (RollbackTo timestamp) chain
            forAll (elements old) $ \(oldTimestamp, _) -> do
                counterexample (show (chain, timestamp,oldTimestamp,  newChain))
                    $ oldTimestamp <= timestamp
                        ==>  member oldTimestamp newChain === True
