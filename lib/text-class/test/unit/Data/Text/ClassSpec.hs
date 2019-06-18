{-# LANGUAGE DeriveGeneric #-}
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
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextMaybe
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , UnicodeString (..)
    , arbitraryBoundedEnum
    , choose
    , classify
    , elements
    , genericShrink
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

    describe "BoundedEnum" $ do
        it "fromTextToBoundedEnum s (toTextFromBoundedEnum s a) == Right a" $
            property $ \(a :: TestBoundedEnum) (s :: CaseStyle) ->
                fromTextToBoundedEnum s (toTextFromBoundedEnum s a) === Right a
        it "CamelCase" $
            toTextFromBoundedEnum CamelCase FooBarBaz === "fooBarBaz"
        it "PascalCase" $
            toTextFromBoundedEnum PascalCase FooBarBaz === "FooBarBaz"
        it "KebabLowerCase" $
            toTextFromBoundedEnum KebabLowerCase FooBarBaz === "foo-bar-baz"
        it "SnakeLowerCase" $
            toTextFromBoundedEnum SnakeLowerCase FooBarBaz === "foo_bar_baz"
        it "SnakeUpperCase" $
            toTextFromBoundedEnum SnakeUpperCase FooBarBaz === "FOO_BAR_BAZ"
        it "SpacedLowerCase" $
            toTextFromBoundedEnum SpacedLowerCase FooBarBaz === "foo bar baz"

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary CaseStyle where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

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

data TestBoundedEnum
    = A
      -- ^ 1 char
    | AB
      -- ^ 2 chars
    | ABC
      -- ^ 3 chars
    | Foo
      -- ^ 1 word
    | FooBar
      -- ^ 2 words
    | FooBarBaz
      -- ^ 3 words
    deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance Arbitrary TestBoundedEnum where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink
