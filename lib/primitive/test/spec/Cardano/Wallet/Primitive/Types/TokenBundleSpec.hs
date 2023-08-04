{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.TokenBundleSpec
  ( spec
  )
where

import Cardano.Numeric.Util
  ( inAscendingPartialOrder
  )
import Cardano.Wallet.Primitive.Types.TokenBundle
  ( Lexicographic (..)
  , TokenBundle (..)
  )
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
  ( genTokenBundlePartition
  , genTokenBundleSmallRange
  , shrinkTokenBundleSmallRange
  )
import Cardano.Wallet.Primitive.Types.TokenMap qualified as TokenMap
import Cardano.Wallet.Primitive.Types.TokenQuantity
  ( TokenQuantity (..)
  )
import Cardano.Wallet.Primitive.Types.TokenQuantity qualified as TokenQuantity
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
  ( genTokenQuantityPositive
  , shrinkTokenQuantityPositive
  )
import Data.Foldable qualified as F
import Data.Function
  ( (&)
  )
import Data.Ratio
  ( (%)
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  )
import Test.Hspec.Core.QuickCheck
  ( modifyMaxSuccess
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Property
  , checkCoverage
  , cover
  , forAll
  , property
  , (===)
  , (==>)
  )
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Classes
  ( eqLaws
  , monoidLaws
  , ordLaws
  , semigroupLaws
  , semigroupMonoidLaws
  )
import Test.QuickCheck.Classes.Monoid.GCD
  ( gcdMonoidLaws
  , leftGCDMonoidLaws
  , overlappingGCDMonoidLaws
  , rightGCDMonoidLaws
  )
import Test.QuickCheck.Classes.Monoid.Monus
  ( monusLaws
  )
import Test.QuickCheck.Classes.Monoid.Null
  ( monoidNullLaws
  )
import Test.QuickCheck.Classes.Semigroup.Cancellative
  ( commutativeLaws
  , leftReductiveLaws
  , reductiveLaws
  , rightReductiveLaws
  )
import Test.Utils.Laws
  ( testLawsMany
  )
import Test.Utils.Laws.PartialOrd
  ( partialOrdLaws
  )
import Prelude hiding
  ( subtract
  )

spec :: Spec
spec =
  describe "Token bundle properties"
    $ modifyMaxSuccess (const 1000)
    $ do
      describe "Class instances obey laws" $ do
        testLawsMany @TokenBundle
          [ commutativeLaws
          , eqLaws
          , gcdMonoidLaws
          , leftGCDMonoidLaws
          , leftReductiveLaws
          , monoidLaws
          , monoidNullLaws
          , monusLaws
          , overlappingGCDMonoidLaws
          , partialOrdLaws
          , reductiveLaws
          , rightGCDMonoidLaws
          , rightReductiveLaws
          , semigroupLaws
          , semigroupMonoidLaws
          ]
        testLawsMany @(Lexicographic TokenBundle)
          [ eqLaws
          , ordLaws
          ]

      describe "Partitioning quantities with an upper bound" $ do
        it "prop_equipartitionQuantitiesWithUpperBound_length"
          $ property prop_equipartitionQuantitiesWithUpperBound_length
        it "prop_equipartitionQuantitiesWithUpperBound_order"
          $ property prop_equipartitionQuantitiesWithUpperBound_order
        it "prop_equipartitionQuantitiesWithUpperBound_sum"
          $ property prop_equipartitionQuantitiesWithUpperBound_sum

      describe "Generating partitions" $ do
        it "prop_genTokenBundlePartition_fold"
          $ prop_genTokenBundlePartition_fold
          & property
        it "prop_genTokenBundlePartition_length"
          $ prop_genTokenBundlePartition_length
          & property
        it "prop_genTokenBundlePartition_nonPositive"
          $ prop_genTokenBundlePartition_nonPositive
          & property

      describe "Behaviour"
        $ it "toCoin only returns when token bundle has only ADA"
        $ property prop_toCoin_onlyAda

--------------------------------------------------------------------------------
-- Partitioning quantities according to an upper bound
--------------------------------------------------------------------------------

-- | Computes the number of parts that 'equipartitionQuantitiesWithUpperBound'
--   should return.
equipartitionQuantitiesWithUpperBound_expectedLength
  :: TokenBundle -> TokenQuantity -> Int
equipartitionQuantitiesWithUpperBound_expectedLength
  (TokenBundle _ m)
  (TokenQuantity maxQuantity) =
    max 1 $ ceiling $ currentMaxQuantity % maxQuantity
    where
      TokenQuantity currentMaxQuantity = TokenMap.maximumQuantity m

prop_equipartitionQuantitiesWithUpperBound_length
  :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_length m maxQuantity =
  maxQuantity > TokenQuantity.zero ==>
    length (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)
      === equipartitionQuantitiesWithUpperBound_expectedLength
        m
        maxQuantity

prop_equipartitionQuantitiesWithUpperBound_order
  :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_order m maxQuantity =
  maxQuantity > TokenQuantity.zero ==>
    inAscendingPartialOrder
      (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)

prop_equipartitionQuantitiesWithUpperBound_sum
  :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_sum m maxQuantity =
  maxQuantity > TokenQuantity.zero ==>
    F.fold (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)
      === m

--------------------------------------------------------------------------------
-- Generating partitions
--------------------------------------------------------------------------------

prop_genTokenBundlePartition_fold
  :: TokenBundle -> QC.Positive (QC.Small Int) -> Property
prop_genTokenBundlePartition_fold m (QC.Positive (QC.Small i)) =
  forAll (genTokenBundlePartition m i) $ (=== m) . F.fold

prop_genTokenBundlePartition_length
  :: TokenBundle -> QC.Positive (QC.Small Int) -> Property
prop_genTokenBundlePartition_length m (QC.Positive (QC.Small i)) =
  forAll (genTokenBundlePartition m i) $ (=== i) . F.length

prop_genTokenBundlePartition_nonPositive
  :: TokenBundle -> QC.NonPositive (QC.Small Int) -> Property
prop_genTokenBundlePartition_nonPositive m (QC.NonPositive (QC.Small i)) =
  forAll (genTokenBundlePartition m i) (=== pure m)

--------------------------------------------------------------------------------
-- Behavioural properties
-------------------------------------------------------------------------------

prop_toCoin_onlyAda :: TokenBundle -> Property
prop_toCoin_onlyAda bundle =
  let
    result = TokenBundle.toCoin bundle
  in
    checkCoverage
      $ cover
        30
        (not (TokenBundle.isCoin bundle))
        "Token bundle has at least 1 non-ada asset"
      $ cover
        30
        (TokenBundle.isCoin bundle)
        "Token bundle has no non-ada assets"
      $ if TokenBundle.isCoin bundle
        then result === Just (TokenBundle.coin bundle)
        else result === Nothing

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TokenBundle where
  arbitrary = genTokenBundleSmallRange
  shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenQuantity where
  arbitrary = genTokenQuantityPositive
  shrink = shrinkTokenQuantityPositive

deriving instance Arbitrary (Lexicographic TokenBundle)
