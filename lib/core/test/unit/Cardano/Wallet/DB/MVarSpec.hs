{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.DBSpec
    ( DummyTarget, dbPropertyTests, withDB )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..), SeqState (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Control.DeepSeq
    ( NFData )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..) )

spec :: Spec
spec = withDB (newDBLayer :: IO (DBLayer IO (SeqState DummyTarget) DummyTarget)) $
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
    arbitrary = initWallet <$> arbitrary
