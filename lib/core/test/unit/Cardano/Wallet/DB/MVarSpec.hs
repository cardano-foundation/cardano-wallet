{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DBSpec
    ( DummyTarget, dbPropertyTests, withDB )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..), SeqState (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )
import Control.DeepSeq
    ( NFData )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..) )

import qualified Cardano.Wallet.DB.MVar as MVar

spec :: Spec
spec = withDB @(SeqState DummyTarget) MVar.newDBLayer $
    describe "MVar" dbPropertyTests

newtype DummyStateMVar = DummyStateMVar Int
    deriving (Show, Eq)

instance Arbitrary DummyStateMVar where
    shrink _ = []
    arbitrary = DummyStateMVar <$> arbitrary

deriving instance NFData DummyStateMVar

instance IsOurs DummyStateMVar where
    isOurs _ num = (True, num)

instance Arbitrary (Wallet DummyStateMVar DummyTarget) where
    shrink _ = []
    arbitrary = initWallet block0 <$> arbitrary
      where
        block0 :: BlockHeader
        block0 = BlockHeader (SlotId 0 0) (Hash "block0") (Hash "genesis")
