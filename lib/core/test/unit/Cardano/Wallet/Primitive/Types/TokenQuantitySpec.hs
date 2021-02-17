{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TokenQuantitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityMixed, shrinkTokenQuantityMixed )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Typeable
    ( Typeable )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, (===), (==>) )
import Test.QuickCheck.Classes
    ( eqLaws
    , monoidLaws
    , ordLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showReadLaws
    )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Char as Char
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Test.Utils.Roundtrip as JsonRoundtrip

spec :: Spec
spec =
    describe "Token quantity properties" $
    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @TokenQuantity
            [ eqLaws
            , monoidLaws
            , ordLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]

    parallel $ describe "Operations" $ do

        it "prop_pred_succ" $
            property prop_pred_succ
        it "prop_succ_pred" $
            property prop_succ_pred

    parallel $ describe "JSON serialization" $ do

        describe "Roundtrip tests" $ do
            testJson $ Proxy @TokenQuantity

    parallel $ describe "Text serialization" $ do

        describe "Roundtrip tests" $ do
            textRoundtrip $ Proxy @TokenQuantity
        it "prop_toText_noQuotes" $ do
            property prop_toText_noQuotes

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

prop_pred_succ :: TokenQuantity -> Property
prop_pred_succ q = q > TokenQuantity.zero ==>
    TokenQuantity.succ (TokenQuantity.pred q) === q

prop_succ_pred :: TokenQuantity -> Property
prop_succ_pred q =
    TokenQuantity.pred (TokenQuantity.succ q) === q

--------------------------------------------------------------------------------
-- JSON serialization
--------------------------------------------------------------------------------

testJson
    :: (Arbitrary a, ToJSON a, FromJSON a, Typeable a) => Proxy a -> Spec
testJson = JsonRoundtrip.jsonRoundtripAndGolden testJsonDataDirectory

testJsonDataDirectory :: FilePath
testJsonDataDirectory =
    ($(getTestData) </> "Cardano" </> "Wallet" </> "Primitive" </> "Types")

--------------------------------------------------------------------------------
-- Text serialization
--------------------------------------------------------------------------------

prop_toText_noQuotes :: TokenQuantity -> Property
prop_toText_noQuotes q = property $ case text of
    c : cs ->
        Char.isDigit c || c == '-' && F.all Char.isDigit cs
    [] ->
        error "Unexpected empty string."
  where
    text = T.unpack $ toText q

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TokenQuantity where
    -- We test with token quantities of a variety of magnitudes to ensure that
    -- roundtrip serialization works even with large values, both positive and
    -- negative.
    arbitrary = genTokenQuantityMixed
    shrink = shrinkTokenQuantityMixed
