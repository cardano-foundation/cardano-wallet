{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.SlottingSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Slotting
    ( SlotCount
    , SlotId (..)
    , addSlots
    , isValidSlotId
    , slotDiff
    , slotNext
    , slotPrev
    , slotsPerEpoch
    )
import Cardano.Wallet.SlottingOrphans
    ()

import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, checkCoverage, choose, property, (==>) )

spec :: Spec
spec = do
    describe "Generators are valid" $ do
        it "Arbitrary SlotId" $ property isValidSlotId

    describe "Basic slot arithmetic" $ do
        it "slotNext . slotPrev = id"
            (checkCoverage propNextSlotPrevSlot)

        it "slotNext always increments the SlotId"
            (checkCoverage propNextIncrements)

        it "slotPrev decrements the SlotId"
            (checkCoverage propPrevDecrements)

        it "addSlots results in correct difference"
            (checkCoverage propAddSlotsDiff)

        it "addSlots 0 == id"
            (checkCoverage propAddSlotsId)

propNextSlotPrevSlot :: SlotId -> Property
propNextSlotPrevSlot sl = property $ slotPrev (slotNext sl) == Just sl

propNextIncrements :: SlotId -> Property
propNextIncrements sl = property $ slotDiff (slotNext sl) sl == 1

propPrevDecrements :: SlotId -> Property
propPrevDecrements sl = property $
    sl > SlotId 0 0 ==> (slotDiff sl <$> slotPrev sl) == Just 1

instance Arbitrary SlotCount where
    arbitrary
        = fromIntegral <$> choose (0 :: Int, 4 * (fromIntegral slotsPerEpoch))

propAddSlotsDiff :: (SlotCount, SlotId) -> Property
propAddSlotsDiff (n, sl) = property $
    slotDiff (addSlots n sl) sl == fromIntegral n

propAddSlotsId :: SlotId -> Property
propAddSlotsId sl = property $ addSlots 0 sl == sl
