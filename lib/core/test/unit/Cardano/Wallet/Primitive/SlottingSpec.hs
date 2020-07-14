{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
    ( epochOf, fromFlatSlot, runQuery, singleEraInterpreter )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient
    , EpochLength (..)
    , GenesisParameters (..)
    , Hash (..)
    , SlotId (epochNumber)
    , SlotLength (..)
    , StartTime (..)
    )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), choose, property, (===) )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Utils.Time
    ( genUniformTime )

spec :: Spec
spec = do
    describe "slotting" $ do
        it "runQuery epochNo singleEraInterpreter == epochNumber . fromFlatSlot"
            $ property $ \gp slotNo -> do
                let run q = runQuery q (singleEraInterpreter gp)
                let Right res = run (epochOf slotNo)
                let legacy = epochNumber $ fromFlatSlot
                        (getEpochLength gp)
                        (unSlotNo slotNo)
                res === legacy

instance Arbitrary SlotNo where
    -- Don't generate /too/ large slots
    arbitrary = SlotNo . fromIntegral <$> (arbitrary @Word32)
    shrink (SlotNo x) = map SlotNo $ shrink x

instance Arbitrary GenesisParameters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlotLength where
    arbitrary = SlotLength . fromRational . toRational <$> choose (0.1,10::Double)
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
