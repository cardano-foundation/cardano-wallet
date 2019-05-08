{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Extend the 'Data.Text' module with an extra abstraction to encode and decode
-- values safely to and from 'Text'. It's very similar to 'FromJSON' and
-- 'ToJSON' from 'Data.Aeson'.

module Data.Text.Class
    ( ToText (..)
    , FromText (..)
    , TextDecodingError(..)
    ) where

import Prelude

import Control.Monad
    ( unless )
import Data.Bifunctor
    ( first )
import Data.Text
    ( Text )
import Data.Text.Read
    ( decimal, signed )
import Fmt
    ( Buildable )

import qualified Data.Text as T

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
                <> show (fromEnum $ minBound @Int)
                <> " and "
                <> show (fromEnum $ maxBound @Int)
                <> "."

instance ToText Int where
    toText = T.pack . show
