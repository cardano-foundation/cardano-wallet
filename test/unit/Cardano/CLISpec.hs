{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLISpec
    (spec
    ) where

import Prelude

import Cardano.CLI
    ( Network, Port )
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
        textRoundtrip $ Proxy @(Port "test")

instance Arbitrary Network where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Port "test") where
    arbitrary = genericArbitrary
    shrink = genericShrink
