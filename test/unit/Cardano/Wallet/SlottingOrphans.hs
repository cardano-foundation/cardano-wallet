{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.SlottingOrphans
    where

import Prelude

import Test.QuickCheck
    ( Arbitrary (..), choose )

import Cardano.Wallet.Slotting
    ( EpochIndex (..), LocalSlotIndex (..), SlotId (..), slotsPerEpoch )

instance Arbitrary SlotId where
    arbitrary = SlotId <$> arbitrary <*> arbitrary

instance Arbitrary EpochIndex where
    arbitrary = EpochIndex <$> arbitrary

instance Arbitrary LocalSlotIndex where
    arbitrary = LocalSlotIndex <$> choose (0, fromIntegral slotsPerEpoch - 1)
