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
    ( DummyTarget, dbPropertyTests )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Control.DeepSeq
    ( NFData )
import Test.Hspec
    ( Spec )
import Test.QuickCheck
    ( Arbitrary (..) )

spec :: Spec
spec = dbPropertyTests (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget))

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
