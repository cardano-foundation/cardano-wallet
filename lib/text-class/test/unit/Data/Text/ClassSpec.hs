{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.ClassSpec
    ( spec
    ) where

import Prelude

import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), UnicodeString (..), property, (===) )

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Int" $ do
        it "fromText \"14\"" $
            fromText @Int "14" === pure 14
        it "fromText \"-42\"" $
            fromText @Int "-42" === pure (-42)
        it "toText 42" $
            toText @Int 42 === "42"
        it "toText -14" $
            toText @Int (-14) === "-14"
        it "fromText \"patate\"" $
            let err = "input does not start with a digit"
            in fromText @Int "patate" === Left (TextDecodingError err)
        it "fromText . toText === pure"
            $ property $ \(i :: Int) -> (fromText . toText) i === pure i

    describe "Text" $ do
        it "fromText \"patate\"" $
            fromText @Text "patate" === pure "patate"
        it "toText \"patate\"" $
            toText @Text "patate" === "patate"
        it "fromText . toText === pure" $
            property $ \(t :: Text) -> (fromText . toText) t === pure t

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary Text where
    shrink = map (T.pack . getUnicodeString) . shrink . UnicodeString . T.unpack
    arbitrary = T.pack . getUnicodeString <$> arbitrary
