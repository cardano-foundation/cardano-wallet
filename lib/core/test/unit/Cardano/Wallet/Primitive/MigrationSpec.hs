{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.MigrationSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Migration
    ( RewardWithdrawal (..), createPlan )
import Cardano.Wallet.Primitive.Migration.Planning
    ( categorizeUTxO, uncategorizeUTxO )
import Cardano.Wallet.Primitive.Migration.SelectionSpec
    ( MockTxConstraints
    , genCoinRange
    , genMockTxConstraints
    , genTokenBundleMixed
    , unMockTxConstraints
    )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddressSmallRange )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxInLargeRange )
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
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , choose
    , conjoin
    , oneof
    , property
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Migration.Planning as Planning
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.MigrationSpec" $

    parallel $
        describe "Creating migration plans (with concrete wallet types)" $ do

            it "prop_createPlan_equivalent" $
                property prop_createPlan_equivalent

--------------------------------------------------------------------------------
-- Creating migration plans (with concrete wallet types)
--------------------------------------------------------------------------------

data ArgsForCreatePlan = ArgsForCreatePlan
    { mockConstraints :: MockTxConstraints
    , mockUTxO :: UTxO
    , mockRewardWithdrawal :: RewardWithdrawal
    }
    deriving (Eq, Show)

instance Arbitrary ArgsForCreatePlan where
    arbitrary = genArgsForCreatePlan

genArgsForCreatePlan :: Gen ArgsForCreatePlan
genArgsForCreatePlan = do
    mockConstraints <- genMockTxConstraints
    mockRewardWithdrawal <- RewardWithdrawal <$> oneof
        [ pure (Coin 0)
        , genCoinRange (Coin 1) (Coin 1_000_000)
        ]
    entryCount <- choose (0, 64)
    mockUTxO <- UTxO . Map.fromList <$>
        replicateM entryCount (genUTxOEntry mockConstraints)
    pure ArgsForCreatePlan
        { mockConstraints
        , mockUTxO
        , mockRewardWithdrawal
        }
  where
    genUTxOEntry :: MockTxConstraints -> Gen (TxIn, TxOut)
    genUTxOEntry constraints = (,) <$> genTxIn <*> genTxOut constraints

    genTxIn :: Gen TxIn
    genTxIn = genTxInLargeRange

    genTxOut :: MockTxConstraints -> Gen TxOut
    genTxOut constraints = TxOut
        <$> genAddressSmallRange
        <*> genTokenBundleMixed constraints

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
prop_createPlan_equivalent :: Blind ArgsForCreatePlan -> Property
prop_createPlan_equivalent args =
    conjoin
        [ (===)
            (view #totalFee planWithConcreteTypes)
            (view #totalFee planWithAbstractTypes)
        , (===)
            (view #selections planWithConcreteTypes)
            (view #selections planWithAbstractTypes)
        , (===)
            (view #unselected planWithConcreteTypes)
            (view #unselected planWithAbstractTypes & uncategorizeUTxO)
        , (===)
            (utxoEmpty)
            (utxoIntersect utxoSelected utxoNotSelected)
        , (===)
            (mockUTxO)
            (utxoUnion utxoSelected utxoNotSelected)
        ]
  where
    planWithConcreteTypes = createPlan
        constraints mockUTxO mockRewardWithdrawal
    planWithAbstractTypes = Planning.createPlan
        constraints (categorizeUTxO constraints mockUTxO) mockRewardWithdrawal

    Blind ArgsForCreatePlan
        { mockConstraints
        , mockUTxO
        , mockRewardWithdrawal
        } = args
    constraints = unMockTxConstraints mockConstraints

    utxoEmpty :: UTxO
    utxoEmpty = UTxO Map.empty

    utxoIntersect :: UTxO -> UTxO -> UTxO
    utxoIntersect (UTxO u1) (UTxO u2) = UTxO $ Map.intersection u1 u2

    utxoUnion :: UTxO -> UTxO -> UTxO
    utxoUnion (UTxO u1) (UTxO u2) = UTxO $ Map.union u1 u2

    utxoSelected :: UTxO
    utxoSelected = planWithConcreteTypes
        & view #selections
        & fmap (NE.toList . view #inputIds)
        & mconcat
        & Map.fromList
        & UTxO

    utxoNotSelected :: UTxO
    utxoNotSelected = planWithConcreteTypes
        & view #unselected
