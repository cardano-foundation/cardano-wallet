-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Utility functions for converting time values to and from text.
module Data.Time.Text
    ( -- * Conversion to and from text
      utcTimeToText
    , utcTimeFromText

      -- * Time format specification
    , TimeFormat (..)

      -- * Time formats
    , iso8601BasicUtc
    , iso8601BasicLocal
    , iso8601ExtendedUtc
    , iso8601ExtendedLocal

      -- * Time format families
    , iso8601
    , iso8601Basic
    , iso8601Extended
    ) where

import Prelude

import Control.Applicative
    ( (<|>)
    )
import Control.Monad
    ( join
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    , parseTimeM
    )

import qualified Data.Text as T

-- | Convert the specified time value to text, using the specified time format.
utcTimeToText :: TimeFormat -> UTCTime -> Text
utcTimeToText f = T.pack . formatTime defaultTimeLocale (timeFormatPattern f)

-- | Attempt to use each of the specified time formats to parse the given text.
--   Returns a time value that corresponds to the first matching format, or
--   'Nothing' if none of the formats matched.
utcTimeFromText :: [TimeFormat] -> Text -> Maybe UTCTime
utcTimeFromText fs t =
    foldr (<|>) Nothing
        $ flip (parseTimeM False defaultTimeLocale) (T.unpack t) . timeFormatPattern
            <$> fs

-- | Represents a particular way of representing a moment in time in text.
data TimeFormat = TimeFormat
    { timeFormatName :: String
    , timeFormatPattern :: String
    }
    deriving (Eq)

-- | Represents the ISO 8601 family of formats.
iso8601 :: [TimeFormat]
iso8601 = join [iso8601Basic, iso8601Extended]

-- | Represents the ISO 8601 basic family of formats.
iso8601Basic :: [TimeFormat]
iso8601Basic = [iso8601BasicUtc, iso8601BasicLocal]

-- | Represents the ISO 8601 extended family of formats.
iso8601Extended :: [TimeFormat]
iso8601Extended = [iso8601ExtendedUtc, iso8601ExtendedLocal]

-- | Represents the ISO 8601 basic format (UTC).
iso8601BasicUtc :: TimeFormat
iso8601BasicUtc =
    TimeFormat "ISO 8601 Basic UTC" "%Y%m%dT%H%M%S%QZ"

-- | Represents the ISO 8601 basic format (with local timezone).
iso8601BasicLocal :: TimeFormat
iso8601BasicLocal =
    TimeFormat "ISO 8601 Basic Local" "%Y%m%dT%H%M%S%Q%z"

-- | Represents the ISO 8601 extended format (UTC).
iso8601ExtendedUtc :: TimeFormat
iso8601ExtendedUtc =
    TimeFormat "ISO 8601 Extended UTC" "%Y-%m-%dT%H:%M:%S%QZ"

-- | Represents the ISO 8601 extended format (with local timezone).
iso8601ExtendedLocal :: TimeFormat
iso8601ExtendedLocal =
    TimeFormat "ISO 8601 Extended Local" "%Y-%m-%dT%H:%M:%S%Q%z"
