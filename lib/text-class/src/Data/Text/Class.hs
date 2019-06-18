{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Extend the 'Data.Text' module with an extra abstraction to encode and decode
-- values safely to and from 'Text'. It's very similar to 'FromJSON' and
-- 'ToJSON' from 'Data.Aeson'.

module Data.Text.Class
    ( -- * Producing and consuming text from arbitrary types
      ToText (..)
    , FromText (..)
    , TextDecodingError(..)
    , fromTextMaybe
      -- * Producing and consuming text from bounded enumeration types
    , CaseStyle (..)
    , toTextFromBoundedEnum
    , fromTextToBoundedEnum
    ) where

import Prelude

import Control.Monad
    ( unless )
import Data.Bifunctor
    ( first )
import Data.List.Extra
    ( enumerate )
import Data.Maybe
    ( listToMaybe )
import Data.Text
    ( Text )
import Data.Text.Read
    ( decimal, signed )
import Fmt
    ( Buildable )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Casing as Casing

-- | Defines a textual encoding for a type.
class ToText a where
    -- | Encode the specified value as text.
    toText :: a -> Text

-- | Defines a textual decoding for a type.
class FromText a where
    -- | Decode the specified text as a value.
    fromText :: Text -> Either TextDecodingError a

-- | Indicates an error that occurred while decoding from text.
newtype TextDecodingError = TextDecodingError
    { getTextDecodingError :: String }
    deriving stock (Eq, Show)
    deriving newtype Buildable

instance FromText Text where
    fromText = pure

instance ToText Text where
    toText = id

instance FromText Int where
    fromText t = do
        (parsedValue, unconsumedInput) <- first (const err) $ signed decimal t
        unless (T.null unconsumedInput) $ Left err
        pure parsedValue
      where
        err = TextDecodingError $
            "Int is an integer number between "
                <> show (minBound @Int)
                <> " and "
                <> show (maxBound @Int)
                <> "."

instance ToText Int where
    toText = T.pack . show

instance FromText Natural where
    fromText t = do
        (parsedValue, unconsumedInput) <- first (const err) $ decimal t
        unless (T.null unconsumedInput) $ Left err
        pure parsedValue
      where
        err = TextDecodingError "Expecting natural number"

instance ToText Natural where
    toText = T.pack . show

-- | Decode the specified text with a 'Maybe' result type.
fromTextMaybe :: FromText a => Text -> Maybe a
fromTextMaybe = either (const Nothing) Just . fromText

-- | Represents a case style for multi-word strings.
data CaseStyle
    = CamelCase
      -- ^ A string in the style of "doNotRepeatYourself"
    | PascalCase
      -- ^ A string in the style of "DoNotRepeatYourself"
    | KebabLowerCase
      -- ^ A string in the style of "do-not-repeat-yourself"
    | SnakeLowerCase
      -- ^ A string in the style of "do_not_repeat_yourself"
    | SnakeUpperCase
      -- ^ A string in the style of "DO_NOT_REPEAT_YOURSELF"
    | SpacedLowerCase
      -- ^ A string in the style of "do not repeat yourself"
    deriving (Bounded, Enum, Eq, Generic, Show)

-- | Converts the given value to text, according to the specified 'CaseStyle'.
--
-- This function guarantees to satisfy the following property:
--
-- > fromTextToBoundedEnum s (toTextFromBoundedEnum s a) == Right a
--
toTextFromBoundedEnum
    :: forall a . (Bounded a, Enum a, Show a)
    => CaseStyle -> a -> Text
toTextFromBoundedEnum cs = T.pack . toCaseStyle cs . Casing.fromHumps . show

-- | Parses the given text to a value, according to the specified 'CaseStyle'.
--
-- This function guarantees to satisfy the following property:
--
-- > fromTextToBoundedEnum s (toTextFromBoundedEnum s a) == Right a
--
fromTextToBoundedEnum
    :: forall a . (Bounded a, Enum a, Show a)
    => CaseStyle -> Text -> Either TextDecodingError a
fromTextToBoundedEnum cs t =
    case matchingValue of
        Just mv -> Right mv
        Nothing -> Left $ TextDecodingError $ "Error: "
            <> "Unable to decode the given value: "
            <> show t
            <> ". Please specify one of the following values: "
            <> T.unpack (T.intercalate ", " allValuesInRequiredCase)
            <> "."
  where
    allValuesInPascalCase = toTextFromBoundedEnum PascalCase <$> enumerate @a
    allValuesInRequiredCase = toTextFromBoundedEnum cs <$> enumerate @a
    inputInPascalCase = T.pack $ Casing.toPascal $ fromCaseStyle cs $ T.unpack t
    matchingValue = fmap (toEnum . snd) $ listToMaybe $
        filter ((== inputInPascalCase) . fst) $
            allValuesInPascalCase `zip` [0 :: Int ..]

toCaseStyle :: CaseStyle -> Casing.Identifier String -> String
toCaseStyle = \case
    CamelCase       -> Casing.toCamel
    PascalCase      -> Casing.toPascal
    KebabLowerCase  -> Casing.toKebab
    SnakeLowerCase  -> Casing.toQuietSnake
    SnakeUpperCase  -> Casing.toScreamingSnake
    SpacedLowerCase -> fmap C.toLower <$> Casing.toWords

fromCaseStyle :: CaseStyle -> String -> Casing.Identifier String
fromCaseStyle = \case
    CamelCase       -> Casing.fromHumps
    PascalCase      -> Casing.fromHumps
    KebabLowerCase  -> Casing.fromKebab
    SnakeLowerCase  -> Casing.fromSnake
    SnakeUpperCase  -> Casing.fromSnake
    SpacedLowerCase -> Casing.fromWords
