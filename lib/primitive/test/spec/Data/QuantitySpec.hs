{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.QuantitySpec
    ( spec
    ) where

import Prelude

import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Test.Hspec
    ( Spec
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    )
import Test.Text.Roundtrip
    ( textRoundtrip
    )

import qualified Data.Aeson as Aeson

spec :: Spec
spec = do
    textRoundtrip (Proxy @(Quantity "bytes" Int))

    it "fail to parse from JSON if unit doesn't match" $ do
        let msg =
                "Error in $: failed to parse quantified value. Expected \
                \value in 'bytes' (e.g. { \"unit\": \"bytes\", \"quantity\"\
                \: ... }) but got something else."
        Aeson.eitherDecode "{\"unit\":\"patate\",\"quantity\":14}"
            `shouldBe`
            (Left @String @(Quantity "bytes" Int) msg)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Quantity u a) where
    shrink (Quantity a) = Quantity <$> shrink a
    arbitrary = Quantity <$> arbitrary
