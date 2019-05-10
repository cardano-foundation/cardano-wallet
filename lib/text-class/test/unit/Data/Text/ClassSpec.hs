{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.ClassSpec
    ( spec
    ) where

import Prelude

import Data.Foldable
    ( toList )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..), fromTextMaybe )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , UnicodeString (..)
    , choose
    , classify
    , elements
    , property
    , vectorOf
    , (===)
    )

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
            let err =
                    "Int is an integer number between "
                    <> show (minBound @Int)
                    <> " and "
                    <> show (maxBound @Int)
                    <> "."
            in fromText @Int "patate" === Left (TextDecodingError err)
        it "fromText . toText === pure"
            $ property $ \(i :: Int) -> (fromText . toText) i === pure i
        it "fromText ~ fromTextMaybe" $
            property $ \(Digits t) ->
                classify (isNothing (fromTextMaybe @Int t)) "invalid" $
                classify ((compare 0 <$> fromTextMaybe @Int t) == Just GT) "valid negative" $
                toList (fromTextMaybe @Int t) === toList (fromText t)

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

newtype Digits = Digits { getDigits :: Text } deriving Show

instance Arbitrary Digits where
    shrink = map (Digits . T.pack) . shrink . T.unpack . getDigits
    arbitrary = Digits . T.pack <$> do
        n <- choose (0,10)
        str <- vectorOf n (elements ('x':['0'..'9']))
        sign <- elements ["", "-", "+"]
        pure (sign ++ str)
