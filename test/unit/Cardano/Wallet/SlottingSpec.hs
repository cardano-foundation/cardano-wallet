{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.SlottingSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Slotting
    ( EpochIndex (..)
    , LocalSlotIndex (..)
    , SlotId (..)
    , addSlots
    , isValidSlotId
    , slotDiff
    , slotNext
    , slotPrev
    , slotsPerEpoch
    )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, (===), (==>) )

spec :: Spec
spec = do
    describe "Generators are valid" $ do
        it "Arbitrary SlotId" $ property isValidSlotId

    describe "Basic slot arithmetic" $ do
        it "slotNext . slotPrev = id"
            (property propNextSlotPrevSlot)

        it "slotNext always increments the SlotId"
            (property propNextIncrements)

        it "slotPrev decrements the SlotId"
            (property propPrevDecrements)

        it "addSlots results in correct difference"
            (property propAddSlotsDiff)

        it "addSlots 0 == id"
            (property propAddSlotsId)

propNextSlotPrevSlot :: SlotId -> Property
propNextSlotPrevSlot sl = slotPrev (slotNext sl) === Just sl

propNextIncrements :: SlotId -> Property
propNextIncrements sl = slotDiff (slotNext sl) sl === 1

propPrevDecrements :: SlotId -> Property
propPrevDecrements sl =
    sl > SlotId 0 0 ==> (slotDiff sl <$> slotPrev sl) === Just 1

propAddSlotsDiff :: (Natural, SlotId) -> Property
propAddSlotsDiff (n, sl) =
    slotDiff (addSlots n sl) sl === fromIntegral n

propAddSlotsId :: SlotId -> Property
propAddSlotsId sl = addSlots 0 sl === sl


{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary SlotId where
    arbitrary = SlotId <$> arbitrary <*> arbitrary

instance Arbitrary EpochIndex where
    arbitrary = EpochIndex <$> arbitrary

instance Arbitrary LocalSlotIndex where
    arbitrary =
        LocalSlotIndex <$> choose (0, fromIntegral slotsPerEpoch - 1)

instance Arbitrary Natural where
    arbitrary = fromIntegral
        <$> choose (0 :: Int, 4 * (fromIntegral slotsPerEpoch))
