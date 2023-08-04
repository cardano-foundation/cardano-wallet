module Data.Time.TextSpec
  ( spec
  )
where

import Control.Monad
  ( forM_
  )
import Data.List
  ( (\\)
  )
import Data.Maybe
  ( isJust
  , maybeToList
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Data.Time.Text
  ( TimeFormat (..)
  , iso8601
  , iso8601BasicLocal
  , iso8601BasicUtc
  , iso8601ExtendedLocal
  , iso8601ExtendedUtc
  , utcTimeFromText
  , utcTimeToText
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  )
import Test.QuickCheck
  ( property
  , (.&&.)
  , (===)
  )
import Test.Utils.Time
  ( getUniformTime
  )
import Prelude

spec :: Spec
spec = describe "Conversion of UTC time values to and from text" $ do
  describe "Roundtrip conversion to and from text succeeds for all formats"
    $ forM_ allSupportedFormats
    $ \tf ->
      it (timeFormatName tf) $ property $ \t ->
        utcTimeFromText [tf] (utcTimeToText tf $ getUniformTime t)
          `shouldBe` Just (getUniformTime t)

  describe "Parsing valid strings succeeds for all formats"
    $ forM_ allSupportedFormats
    $ \tf -> describe (timeFormatName tf)
      $ forM_ (validForFormat tf)
      $ \es -> describe (title es)
        $ forM_ (examples es)
        $ \e ->
          it (T.unpack e)
            $ property
            $ utcTimeFromText [tf] e
            `shouldSatisfy` isJust

  describe "Parsing invalid strings fails for all formats"
    $ forM_ allSupportedFormats
    $ \tf -> describe (timeFormatName tf)
      $ forM_ (invalidForFormat tf)
      $ \es -> describe (title es)
        $ forM_ (examples es)
        $ \e ->
          it (T.unpack e)
            $ property
            $ utcTimeFromText [tf] e === Nothing

  describe "Equivalent times are decoded equivalently." $ do
    describe "Times with identical dates" $ do
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T12:00:00+01:00"
        "2008-08-08T11:00:00Z"
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T12:00:00+08:00"
        "2008-08-08T04:00:00Z"
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T12:00:00-01:00"
        "2008-08-08T13:00:00Z"
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T12:00:00-08:00"
        "2008-08-08T20:00:00Z"

    describe "Times with different dates" $ do
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T00:00:00+01:00"
        "2008-08-07T23:00:00Z"
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T00:00:00+08:00"
        "2008-08-07T16:00:00Z"
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T23:00:00-01:00"
        "2008-08-09T00:00:00Z"
      ensureTimesDecodeEquivalently
        iso8601
        "2008-08-08T23:00:00-08:00"
        "2008-08-09T07:00:00Z"

-- | Checks that the specified "Text' values can both be decoded according to
--   the specified formats, and that the resultant values are equal.
ensureTimesDecodeEquivalently :: [TimeFormat] -> Text -> Text -> Spec
ensureTimesDecodeEquivalently tf t1 t2 =
  it testTitle
    $ property
    $ (r1 `shouldBe` r2)
    .&&. (r1 `shouldSatisfy` isJust)
    .&&. (r2 `shouldSatisfy` isJust)
  where
    r1 = utcTimeFromText tf t1
    r2 = utcTimeFromText tf t2
    testTitle =
      mempty
        <> "Equivalent times are decoded equivalently: "
        <> show (t1, t2)

-- | Represents a set of example input strings.
data ExampleSet = ExampleSet
  { title :: String
  , examples :: [Text]
  }
  deriving (Eq)

-- | A list of all formats supported by `utcTimeFromText` and `utcTimeToText`.
allSupportedFormats :: [TimeFormat]
allSupportedFormats =
  [ iso8601BasicUtc
  , iso8601BasicLocal
  , iso8601ExtendedUtc
  , iso8601ExtendedLocal
  ]

-- | Return all known valid examples for the given time format. The examples
--   returned should all parse successfully when `utcTimeFromText` is called
--   with the given format.
validForFormat :: TimeFormat -> [ExampleSet]
validForFormat format =
  maybeToList
    $ lookup
      format
      [ (iso8601BasicUtc, validIso8601BasicUtc)
      , (iso8601BasicLocal, validIso8601BasicLocal)
      , (iso8601ExtendedUtc, validIso8601ExtendedUtc)
      , (iso8601ExtendedLocal, validIso8601ExtendedLocal)
      ]

-- | Return all known invalid examples for the given time format. The examples
--   returned should all fail to parse when `utcTimeFromText` is called with
--   the given format.
invalidForFormat :: TimeFormat -> [ExampleSet]
invalidForFormat format = allExamples \\ validForFormat format

-- | The list of all example time strings. Whether or not a given string is
--   valid depends on the particular list of formats that is passed to the
--   `utcTimeFromText` function.
allExamples :: [ExampleSet]
allExamples =
  [ validIso8601BasicUtc
  , validIso8601BasicLocal
  , validIso8601ExtendedUtc
  , validIso8601ExtendedLocal
  , nonTimeExamples
  , iso8601BasicWithoutTimes
  , iso8601BasicWithoutTimezones
  , iso8601BasicWithInvalidTimezones
  , iso8601BasicWithInvalidDateTimeSeparators
  , iso8601BasicWithMissingDateTimeSeparators
  , iso8601ExtendedWithoutTimes
  , iso8601ExtendedWithoutTimezones
  , iso8601ExtendedWithInvalidTimezones
  , iso8601ExtendedWithInvalidDateTimeSeparators
  , iso8601ExtendedWithMissingDateTimeSeparators
  ]

validIso8601BasicUtc :: ExampleSet
validIso8601BasicUtc =
  ExampleSet
    "Valid ISO 8601 basic UTC times"
    [ "20080915T155300Z"
    , "20080915T155300.1Z"
    , "20080915T155300.12Z"
    ]

validIso8601BasicLocal :: ExampleSet
validIso8601BasicLocal =
  ExampleSet
    "Valid ISO 8601 basic local times"
    [ "20080915T155300+0000"
    , "20080915T155300+0800"
    , "20080915T155300-0800"
    , "20080915T155300.1+0000"
    , "20080915T155300.1+0800"
    , "20080915T155300.1-0800"
    , "20080915T155300.12+0000"
    , "20080915T155300.12+0800"
    , "20080915T155300.12-0800"
    ]

validIso8601ExtendedUtc :: ExampleSet
validIso8601ExtendedUtc =
  ExampleSet
    "Valid ISO 8601 extended UTC times"
    [ "2008-09-15T15:53:00Z"
    , "2008-09-15T15:53:00.1Z"
    , "2008-09-15T15:53:00.12Z"
    ]

validIso8601ExtendedLocal :: ExampleSet
validIso8601ExtendedLocal =
  ExampleSet
    "Valid ISO 8601 extended local times"
    [ "2008-09-15T15:53:00+00:00"
    , "2008-09-15T15:53:00+08:00"
    , "2008-09-15T15:53:00-08:00"
    , "2008-09-15T15:53:00.1+00:00"
    , "2008-09-15T15:53:00.1+08:00"
    , "2008-09-15T15:53:00.1-08:00"
    , "2008-09-15T15:53:00.12+00:00"
    , "2008-09-15T15:53:00.12+08:00"
    , "2008-09-15T15:53:00.12-08:00"
    ]

nonTimeExamples :: ExampleSet
nonTimeExamples =
  ExampleSet
    "Non-time examples"
    [ ""
    , "w"
    , "wibble"
    ]

iso8601BasicWithoutTimes :: ExampleSet
iso8601BasicWithoutTimes =
  ExampleSet
    "ISO 8601 basic format without times"
    [ "2008"
    , "200809"
    , "20080915"
    ]

iso8601BasicWithoutTimezones :: ExampleSet
iso8601BasicWithoutTimezones =
  ExampleSet
    "ISO 8601 basic format without timezones"
    [ "20080915T155300"
    , "20080915T155300.1"
    , "20080915T155300.12"
    ]

iso8601BasicWithInvalidTimezones :: ExampleSet
iso8601BasicWithInvalidTimezones =
  ExampleSet
    "ISO 8601 basic format with invalid timezones"
    [ "20080915T155300A"
    , "20080915T155300.1A"
    , "20080915T155300.12A"
    ]

iso8601BasicWithInvalidDateTimeSeparators :: ExampleSet
iso8601BasicWithInvalidDateTimeSeparators =
  ExampleSet
    "ISO 8601 basic format with invalid datetime separators"
    [ "20080915S155300Z"
    , "20080915S155300.1Z"
    , "20080915S155300.12Z"
    ]

iso8601BasicWithMissingDateTimeSeparators :: ExampleSet
iso8601BasicWithMissingDateTimeSeparators =
  ExampleSet
    "ISO 8601 basic format with missing datetime separators"
    [ "20080915155300Z"
    , "20080915155300.1Z"
    , "20080915155300.12Z"
    ]

iso8601ExtendedWithoutTimes :: ExampleSet
iso8601ExtendedWithoutTimes =
  ExampleSet
    "ISO 8601 extended format without times"
    [ "2008"
    , "2008-09"
    , "2008-09-15"
    ]

iso8601ExtendedWithoutTimezones :: ExampleSet
iso8601ExtendedWithoutTimezones =
  ExampleSet
    "ISO 8601 extended format without timezones"
    [ "2008-09-15T15:53:00"
    , "2008-09-15T15:53:00.1"
    , "2008-09-15T15:53:00.12"
    ]

iso8601ExtendedWithInvalidTimezones :: ExampleSet
iso8601ExtendedWithInvalidTimezones =
  ExampleSet
    "ISO 8601 extended format with invalid timezones"
    [ "2008-09-15T15:53:00A"
    , "2008-09-15T15:53:00.1A"
    , "2008-09-15T15:53:00.12A"
    ]

iso8601ExtendedWithInvalidDateTimeSeparators :: ExampleSet
iso8601ExtendedWithInvalidDateTimeSeparators =
  ExampleSet
    "ISO 8601 extended format with invalid date-time separators"
    [ "2008-09-15S15:53:00Z"
    , "2008-09-15S15:53:00.1Z"
    , "2008-09-15S15:53:00.12Z"
    ]

iso8601ExtendedWithMissingDateTimeSeparators :: ExampleSet
iso8601ExtendedWithMissingDateTimeSeparators =
  ExampleSet
    "ISO 8601 extended format with missing date-time separators"
    [ "2008-09-1515:53:00Z"
    , "2008-09-1515:53:00.1Z"
    , "2008-09-1515:53:00.12Z"
    ]
