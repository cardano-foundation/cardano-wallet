{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Small module for writing text-based golden tests.
--
-- In contrast to `hspec-golden-aeson` this module works with manually provided
-- values instead of QuickCheck, and with text instead of JSON.
--
-- If the tests are run with the OVERWRITE_GOLDENS environment variable set,
-- they will all fail, and write the current values to disk.
--
-- If some goldens are os-dependent (like some optparse-applicative help text
-- using getProgName), you can create a windows specific variant by appending
-- ".win" to the golden file name.
--
-- This seems to work for now with just two windows specific goldens, but if
-- there are more, it might be more convenient if windows goldens always are
-- separate, as OVERWRITE_GOLDENS will never currently write the .win version.
module Test.Hspec.Goldens
    ( Settings (..)
    , textGolden
    )
    where

import Prelude

import Data.Either.Combinators
    ( rightToMaybe
    )
import Data.Text
    ( Text
    )
import Fmt
    ( Builder
    , fmt
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import System.IO.Error
    ( ioeGetErrorType
    , isDoesNotExistErrorType
    )
import Test.Hspec
    ( Expectation
    , expectationFailure
    , shouldBe
    )
import Test.Utils.Platform
    ( isWindows
    )
import UnliftIO.Exception
    ( IOException
    , try
    , tryJust
    )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Settings = Settings
    { goldenDirectory :: FilePath
    , postProcess :: Text -> Text
    }

-- | Wrapper for text which is easier to read in HSpec failure messages.
newtype GoldenText = GoldenText { getGoldenText :: Text } deriving Eq

instance Show GoldenText where
    show = T.unpack . getGoldenText

textGolden
    :: Settings
    -> String -- ^ Filename for the test
    -> Text  -- ^ Value to compare with the golden file
    -> Expectation
textGolden settings title value =
    (,) <$> lookupEnv "OVERWRITE_GOLDENS" <*> readGolden >>= \case
        (Nothing, (_, Just expected)) ->
            GoldenText (postProcess settings value) `shouldBe` expected
        (Just _overwrite, (f, Just _expected)) ->
            writeGoldenAndFail f value "Overwriting goldens"
        (_, (f, Nothing)) ->
            writeGoldenAndFail f value "No existing golden file found"

  where
    golden = goldenDirectory settings </> title
    -- If running on windows, we will use the windows-specific golden first,
    -- if it exists.
    goldenWin = if isWindows then Just (golden <.> "win") else Nothing

    -- | Gets the contents of a golden text file, if it exists, and returns the
    -- path of the actual file which was read.
    readGolden :: IO (FilePath, Maybe GoldenText)
    readGolden = fmap (fmap GoldenText) <$> case goldenWin of
        Just win -> readTextFile win >>= \case
            Just text -> pure (win, Just text)
            Nothing -> (golden,) <$> readTextFile golden
        Nothing -> (golden,) <$> readTextFile golden

    readTextFile :: FilePath -> IO (Maybe Text)
    readTextFile = fmap rightToMaybe . tryJust handler . TIO.readFile
      where
        handler e
            | isDoesNotExistErrorType (ioeGetErrorType e) = Just e
            | otherwise = Nothing

    writeGoldenAndFail
        :: FilePath
        -> Text
        -> String -- ^ Error message prefix on failure
        -> IO ()
    writeGoldenAndFail f text errMsg =
        try (TIO.writeFile f text') >>= expectationFailure . fmt . msg
      where
        text' = postProcess settings text

        msg :: Either IOException () -> Builder
        msg res = errMsg|+"... "+|case res of
            Right () ->
                "Now written to disk. Please check for correctness and commit."
            Left err ->
                "Unable to write the new value to disk because of:\n"+||err||+""
