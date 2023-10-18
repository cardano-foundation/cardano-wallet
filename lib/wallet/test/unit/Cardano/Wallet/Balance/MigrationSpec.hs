{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Balance.MigrationSpec
    where

import Prelude

import Cardano.Wallet.Balance.Migration
    ( RewardWithdrawal (..), createPlan )
import Cardano.Wallet.Balance.Migration.Planning
    ( categorizeUTxO, uncategorizeUTxO )
import Cardano.Wallet.Balance.Migration.SelectionSpec
    ( MockTxConstraints
    , genRewardWithdrawal
    , genTokenBundleMixed
    , testAll
    , unMockTxConstraints
    )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxInLargeRange )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad
    ( replicateM )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Blind (..), Gen, Property, choose, forAllBlind, property )
import Test.QuickCheck.Extra
    ( verify )

import qualified Cardano.Wallet.Balance.Migration.Planning as Planning
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec =
    describe "Cardano.Wallet.Balance.MigrationSpec" $

        describe "Creating migration plans (with concrete wallet types)" $ do

            it "prop_createPlan_equivalent" $
                property prop_createPlan_equivalent

--------------------------------------------------------------------------------
-- Creating migration plans (with concrete wallet types)
--------------------------------------------------------------------------------

-- This property test is really just a simple sanity check to ensure that it's
-- possible to create migration plans through the public interface, using
-- concrete wallet types such as 'UTxO', 'TxIn', and 'TxOut'.
--
-- As such, this test does not do anything beyond establishing that the results
-- of calling the following functions are equivalent:
--
--  - Migration         .createPlan (uses concrete wallet types)
--  - Migration.Planning.createPlan (uses abstract types)
--
-- For a more detailed test of 'createPlan' (with abstract types) see
-- 'PlanningSpec.prop_createPlan'.
--
prop_createPlan_equivalent :: Blind MockTxConstraints -> Property
prop_createPlan_equivalent (Blind mockConstraints) =
    forAllBlind genUTxO $ \utxo ->
    forAllBlind genRewardWithdrawal $ \reward ->
    prop_createPlan_equivalent_inner mockConstraints utxo reward
  where
    genUTxO :: Gen UTxO
    genUTxO = do
        entryCount <- choose (0, 64)
        UTxO . Map.fromList <$> replicateM entryCount genUTxOEntry
      where
        genUTxOEntry :: Gen (TxIn, TxOut)
        genUTxOEntry = (,) <$> genTxIn <*> genTxOut
          where
            genTxIn :: Gen TxIn
            genTxIn = genTxInLargeRange

            genTxOut :: Gen TxOut
            genTxOut = TxOut
                <$> genAddress
                <*> genTokenBundleMixed mockConstraints

prop_createPlan_equivalent_inner
    :: MockTxConstraints
    -> UTxO
    -> RewardWithdrawal
    -> Property
prop_createPlan_equivalent_inner mockConstraints utxo reward = testAll
    $ verify
        (totalFeeConcrete == totalFeeAbstract)
        "totalFeeConcrete == totalFeeAbstract"
    . verify
        (selectionsConcrete == selectionsAbstract)
        "selectionsConcrete == selectionsAbstract"
    . verify
        (unselectedConcrete == unselectedAbstract)
        "unselectedConcrete == unselectedAbstract"
    . verify
        (utxoEmpty == utxoIntersect utxoSelected utxoNotSelected)
        "utxoEmpty == utxoIntersect utxoSelected utxoNotSelected"
    . verify
        (utxo == utxoUnion utxoSelected utxoNotSelected)
        "utxo == utxoUnion utxoSelected utxoNotSelected"
  where
    totalFeeConcrete = view #totalFee planConcrete
    totalFeeAbstract = view #totalFee planAbstract

    selectionsConcrete = view #selections planConcrete
    selectionsAbstract = view #selections planAbstract

    unselectedConcrete = view #unselected planConcrete
    unselectedAbstract = view #unselected planAbstract
        & uncategorizeUTxO

    planConcrete = createPlan constraints utxo reward
    planAbstract = Planning.createPlan
        constraints (categorizeUTxO constraints utxo) reward

    constraints = unMockTxConstraints mockConstraints

    utxoEmpty :: UTxO
    utxoEmpty = UTxO Map.empty

    utxoIntersect :: UTxO -> UTxO -> UTxO
    utxoIntersect (UTxO u1) (UTxO u2) = UTxO $ Map.intersection u1 u2

    utxoUnion :: UTxO -> UTxO -> UTxO
    utxoUnion (UTxO u1) (UTxO u2) = UTxO $ Map.union u1 u2

    utxoSelected :: UTxO
    utxoSelected = planConcrete
        & view #selections
        & fmap (NE.toList . view #inputIds)
        & mconcat
        & Map.fromList
        & UTxO

    utxoNotSelected :: UTxO
    utxoNotSelected = planConcrete
        & view #unselected
