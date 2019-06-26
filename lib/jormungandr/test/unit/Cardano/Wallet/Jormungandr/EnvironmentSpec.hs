{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.EnvironmentSpec where

import Prelude

import Cardano.Wallet.Jormungandr.Environment
    ( Network )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..), arbitraryBoundedEnum, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )

spec :: Spec
spec = do
    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @Network

{-------------------------------------------------------------------------------
                               Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary Network where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink
