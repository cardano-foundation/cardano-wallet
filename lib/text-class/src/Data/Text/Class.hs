{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extend the 'Data.Text' module with an extra abstraction to encode and decode
-- values safely to and from 'Text'. It's very similar to 'FromJSON' and
-- 'ToJSON' from 'Data.Aeson'.
module Data.Text.Class
    ( -- * Producing and consuming text from arbitrary types
      ToText (..)
    , FromText (..)
    , TextDecodingError (..)
    , fromTextMaybe

      -- * Producing and consuming text from bounded enumeration types
    , CaseStyle (..)
    , toTextFromBoundedEnum
    , fromTextToBoundedEnum

      -- * Helpers
    , showT
    ) where

import Prelude

import Control.Monad
    ( unless
    , (<=<)
    )
import Data.Bifunctor
    ( bimap
    , first
    )
import Data.List
    ( find
    )
import Data.List.Extra
    ( enumerate
    )
import Data.Maybe
    ( listToMaybe
    )
import Data.Text
    ( Text
    )
import Data.Text.Read
    ( decimal
    , signed
    )
import Data.Time.Clock
    ( NominalDiffTime
    )
import Data.Word
    ( Word32
    , Word64
    )
import Data.Word.Odd
    ( Word31
    )
import Formatting
    ( builder
    , sformat
    )
import Formatting.Buildable
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Text.Read
    ( readEither
    )

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Text.Casing as Casing

{-------------------------------------------------------------------------------
                                     Types
-------------------------------------------------------------------------------}

-- | Defines a textual encoding for a type.
class ToText a where
    -- | Encode the specified value as text.
    toText :: a -> Text
    default toText :: Buildable a => a -> Text
    toText = sformat builder . build

-- | Defines a textual decoding for a type.
class FromText a where
    -- | Decode the specified text as a value.
    fromText :: Text -> Either TextDecodingError a

-- | Indicates an error that occurred while decoding from text.
newtype TextDecodingError = TextDecodingError
    {getTextDecodingError :: String}
    deriving stock (Eq, Show)
    deriving newtype (Buildable)

-- | Decode the specified text with a 'Maybe' result type.
fromTextMaybe :: FromText a => Text -> Maybe a
fromTextMaybe = either (const Nothing) Just . fromText

{-------------------------------------------------------------------------------
                                   Instances
-------------------------------------------------------------------------------}

instance FromText String where
    fromText = pure . T.unpack

instance ToText String where
    toText = T.pack

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
        err =
            TextDecodingError
                $ "Int is an integer number between "
                    <> show (minBound @Int)
                    <> " and "
                    <> show (maxBound @Int)
                    <> "."

instance ToText Int where
    toText = intToText

instance FromText Natural where
    fromText t = do
        (parsedValue, unconsumedInput) <- first (const err) $ decimal t
        unless (T.null unconsumedInput) $ Left err
        pure parsedValue
      where
        err = TextDecodingError "Expecting natural number"

instance ToText Natural where
    toText = intToText

instance FromText Word32 where
    fromText =
        validate <=< (fmap fromIntegral . fromText @Natural)
      where
        validate x
            | (x >= (minBound @Word32)) && (x <= (maxBound @Word32)) =
                return x
            | otherwise =
                Left $ TextDecodingError "Word32 is out of bounds"

instance FromText Integer where
    fromText t = do
        (parsedValue, unconsumedInput) <- first (const err) $ signed decimal t
        unless (T.null unconsumedInput) $ Left err
        pure parsedValue
      where
        err = TextDecodingError "Expecting integer"

instance ToText Integer where
    toText = intToText

instance FromText Double where
    fromText = first (const err) . readEither . T.unpack
      where
        err = TextDecodingError "Expecting floating number"

instance ToText Double where
    toText = realFloatToText

instance ToText Word64 where
    toText = intToText

instance ToText Word32 where
    toText = intToText

instance ToText Word31 where
    toText = intToText

instance ToText NominalDiffTime where
    toText = T.pack . show

-- Note: This parser doesn't allow fractional or negative durations.
instance FromText NominalDiffTime where
    fromText t = case T.splitOn "s" t of
        [v, ""] -> bimap (const err) (fromIntegral @Natural) (fromText v)
        _ -> Left err
      where
        err =
            TextDecodingError
                $ unwords
                    [ "Cannot parse given time duration."
                    , "Values must be given as whole positive seconds, and must"
                    , "finish with \"s\". For example: \"3s\", \"3600s\", \"42s\"."
                    ]

realFloatToText :: RealFloat a => a -> T.Text
realFloatToText = TL.toStrict . B.toLazyText . B.realFloat

intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . B.toLazyText . B.decimal

{-------------------------------------------------------------------------------
                            Formatting enums as text
-------------------------------------------------------------------------------}

-- | Represents a case style for multi-word strings.
data CaseStyle
    = -- | A string in the style of "doNotRepeatYourself"
      CamelCase
    | -- | A string in the style of "DoNotRepeatYourself"
      PascalCase
    | -- | A string in the style of "do-not-repeat-yourself"
      KebabLowerCase
    | -- | A string in the style of "do_not_repeat_yourself"
      SnakeLowerCase
    | -- | A string in the style of "DO_NOT_REPEAT_YOURSELF"
      SnakeUpperCase
    | -- | A string in the style of "do not repeat yourself"
      SpacedLowerCase
    deriving (Bounded, Enum, Eq, Generic, Show)

-- | Converts the given value to text, according to the specified 'CaseStyle'.
--
-- This function guarantees to satisfy the following property:
--
-- > fromTextToBoundedEnum s (toTextFromBoundedEnum s a) == Right a
toTextFromBoundedEnum
    :: forall a
     . (Bounded a, Enum a, Show a)
    => CaseStyle
    -> a
    -> Text
toTextFromBoundedEnum cs = T.pack . toCaseStyle cs . Casing.fromHumps . show

-- | Parses the given text to a value, according to the specified 'CaseStyle'.
--
-- This function guarantees to satisfy the following property:
--
-- > fromTextToBoundedEnum s (toTextFromBoundedEnum s a) == Right a
fromTextToBoundedEnum
    :: forall a
     . (Bounded a, Enum a, Show a)
    => CaseStyle
    -> Text
    -> Either TextDecodingError a
fromTextToBoundedEnum cs t =
    case matchingValue of
        Just mv -> Right mv
        Nothing ->
            Left
                $ TextDecodingError
                $ mempty
                    <> "Unable to decode the given text value. "
                    <> "Please specify one of the following values: "
                    <> T.unpack (T.intercalate ", " allValuesInRequiredCase)
                    <> "."
  where
    allValuesInPascalCase = toTextFromBoundedEnum PascalCase <$> enumerate @a
    allValuesInRequiredCase = toTextFromBoundedEnum cs <$> enumerate @a
    inputInPascalCase =
        T.pack . Casing.toPascal <$> fromCaseStyle cs (T.unpack t)
    matchingValue =
        fmap (toEnum . snd)
            $ find ((== inputInPascalCase) . fst)
            $ (Just <$> allValuesInPascalCase) `zip` [0 :: Int ..]

toCaseStyle :: CaseStyle -> Casing.Identifier String -> String
toCaseStyle = \case
    CamelCase -> Casing.toCamel
    PascalCase -> Casing.toPascal
    KebabLowerCase -> Casing.toKebab
    SnakeLowerCase -> Casing.toQuietSnake
    SnakeUpperCase -> Casing.toScreamingSnake
    SpacedLowerCase -> fmap C.toLower <$> Casing.toWords

fromCaseStyle :: CaseStyle -> String -> Maybe (Casing.Identifier String)
fromCaseStyle = \case
    CamelCase -> fmap Casing.fromHumps . ensureFirstCharLowerCase
    PascalCase -> fmap Casing.fromHumps . ensureFirstCharUpperCase
    KebabLowerCase -> fmap Casing.fromKebab . ensureAllLowerCase
    SnakeLowerCase -> fmap Casing.fromSnake . ensureAllLowerCase
    SnakeUpperCase -> fmap Casing.fromSnake . ensureAllUpperCase
    SpacedLowerCase -> fmap Casing.fromWords . ensureAllLowerCase
  where
    ensureAllLowerCase s =
        if any C.isUpper s then Nothing else Just s
    ensureAllUpperCase s =
        if any C.isLower s then Nothing else Just s
    ensureFirstCharLowerCase s =
        (\c -> if C.isUpper c then Nothing else Just s) =<< listToMaybe s
    ensureFirstCharUpperCase s =
        (\c -> if C.isLower c then Nothing else Just s) =<< listToMaybe s

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- | Show a data-type through its 'ToText' instance
showT :: ToText a => a -> String
showT = T.unpack . toText
