{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Environment.HttpBridgeSpec where

import Prelude

import Cardano.Environment.HttpBridge
    ( Network )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
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
    arbitrary = genericArbitrary
    shrink = genericShrink
