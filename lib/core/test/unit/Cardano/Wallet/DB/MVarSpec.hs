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
    ( dbPropertyTests, withDB )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, block0 )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
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
