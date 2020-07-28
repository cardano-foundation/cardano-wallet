{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Primitive.SlottingSpec
    ( spec
    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.Gen
    ( genActiveSlotCoefficient, shrinkActiveSlotCoefficient )
import Cardano.Wallet.Primitive.Slotting
    ( Qry
    , SlotParameters
    , endTimeOfEpoch
    , epochOf
    , firstSlotInEpoch
    , flatSlot
    , fromFlatSlot
    , singleEraInterpreter
    , slotParams
    , slotRangeFromTimeRange
    , slotRangeFromTimeRange'
    , slotStartTime
    , startTime
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient
    , EpochLength (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , Hash (..)
    , Range (..)
    , SlotId (..)
    , SlotId (epochNumber)
    , SlotLength (..)
    , StartTime (..)
    )
import Data.Functor.Identity
    ( runIdentity )
import Data.Quantity
    ( Quantity (..) )
import Data.Time
    ( UTCTime )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, withMaxSuccess, (===) )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Utils.Time
    ( genUniformTime )

spec :: Spec
spec = do
    describe "slotting" $ do
        describe "runQuery NEW singleEraInterpreter == OLD . fromFlatSlot" $ do
            it "epochOf and epochNumber"
                $  property $ legacySlottingTest (\_ s -> epochNumber s) epochOf

            it "startTime and slotStartTime"
                $ property $ legacySlottingTest slotStartTime startTime

            it "slotRangeFromTimeRange and slotRangeFromTimeRange'"
                $ withMaxSuccess 10000 $ property $ \gp timeRange -> do
                    -- NOTE: The old impementation breaks for large times /
                    -- slotNos. After only generating SlotLengths of 1s or
                    -- bigger, it should hopefully always work.
                    let sp = slotParams gp
                    let res = runIdentity $ singleEraInterpreter gp
                            (slotRangeFromTimeRange timeRange)

                    let legacy = slotRangeFromTimeRange' sp timeRange

                    let res' = fmap (fromFlatSlot (getEpochLength gp)
                            . unSlotNo) <$> res
                    res' === legacy

            it "(firstSlotInEpoch e) vs (SlotId e 0) "
                $ withMaxSuccess 10000 $ property $ \gp e -> do
                    let res = runIdentity $ singleEraInterpreter gp
                            (firstSlotInEpoch e)
                    let legacy = SlotNo $ flatSlot (getEpochLength gp) $ SlotId e 0

                    res === legacy

        it "endTimeOfEpoch e == (startTime =<< firstSlotInEpoch (e + 1)) \
           \ (always true useing singleEraInterpreter)"
            $ withMaxSuccess 10000 $ property $ \gp e -> do
                let run = runIdentity . singleEraInterpreter gp
                run (endTimeOfEpoch e)
                    === run (startTime =<< firstSlotInEpoch (e + 1))
legacySlottingTest
    :: (Eq a, Show a)
    => (SlotParameters -> SlotId -> a)
    -> (SlotNo -> Qry a)
    -> GenesisParameters
    -> SlotNo
    -> Property
legacySlottingTest legacyImpl newImpl gp slotNo = withMaxSuccess 10000 $ do
    let res = runIdentity $ singleEraInterpreter gp (newImpl slotNo)
    let legacy = legacyImpl (slotParams gp) $ fromFlatSlot
            (getEpochLength gp)
            (unSlotNo slotNo)
    res === legacy

instance Arbitrary SlotNo where
    -- Don't generate /too/ large slots
    arbitrary = SlotNo . fromIntegral <$> (arbitrary @Word32)
    shrink (SlotNo x) = map SlotNo $ shrink x

instance Arbitrary EpochNo where
    -- Don't generate /too/ large numbers
    arbitrary = EpochNo . fromIntegral <$> (arbitrary @Word32)
    shrink _ = []

instance Arbitrary GenesisParameters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlotLength where
    arbitrary = SlotLength . fromRational . toRational <$> choose (1,10::Double)
    shrink _ = []

instance Arbitrary (Hash "Genesis") where
    arbitrary = return $ Hash "Genesis Hash"
    shrink _ = []

instance Arbitrary StartTime where
    arbitrary = StartTime <$> genUniformTime
    shrink _ = []

instance Arbitrary EpochLength where
    arbitrary = EpochLength <$> choose (2,100000)
    shrink _ = []

instance Arbitrary ActiveSlotCoefficient where
    arbitrary = genActiveSlotCoefficient
    shrink = shrinkActiveSlotCoefficient

instance Arbitrary (Quantity "block" Word32) where
    arbitrary = Quantity <$> choose (1,100000)
    shrink (Quantity x) = map Quantity $ shrink x

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
    arbitrary =
        makeRangeValid . uncurry Range <$> arbitrary
    shrink (Range p q) =
        makeRangeValid . uncurry Range <$> shrink (p, q)

-- Ensures that the start of a range is not greater than its end.
makeRangeValid :: Ord a => Range a -> Range a
makeRangeValid = \case
    Range (Just p) (Just q) -> Range (Just $ min p q) (Just $ max p q)
    r -> r

instance Arbitrary UTCTime where
    arbitrary = genUniformTime
