{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
-- there are more, it might be more conventient if windows goldens always are
-- separate, as OVERWRITE_GOLDENS will never currently write the .win version.
module Test.Hspec.Goldens
    ( Settings (..)
    , textGolden
    )
    where

import Prelude

import Control.Applicative
    ( (<|>) )
import Control.Exception
    ( try )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import System.Info
    ( os )
import Test.Hspec

import qualified Data.Text.IO as TIO

newtype Settings = Settings
    { goldenDirectory :: FilePath
    }

textGolden
    :: Settings
    -> String -- ^ Filename for the test
    -> Text  -- ^ Value to compare with the golden file
    -> Expectation
textGolden settings title value = do
    let f = goldenDirectory settings </> title
    overwrite <- isJust <$> lookupEnv "OVERWRITE_GOLDENS"
    readGolden f >>= \case
        Right expected | not overwrite -> value `shouldBe` expected
                       | otherwise     -> writeGoldenAndFail f value "Overwriting goldens"
        Left _ -> do
            writeGoldenAndFail f value "No existing golden file found"

  where
    -- If running on windows, will try to read the windows-specific golden
    -- first.
    readGolden :: FilePath -> IO (Either IOError Text)
    readGolden f
        | os == "mingw32" = try $ TIO.readFile (f <> ".win") <|> TIO.readFile f
        | otherwise       = try $ TIO.readFile f

    -- NOTE: Not the most elegant error handling, but using e.g. ExceptT seemed
    -- to get unnecessarily complicated and not interplay with the IO `shouldBe`
    -- expectation.
    writeGoldenAndFail
        :: FilePath
        -> Text
        -> String -- ^ Error message prefix on failure
        -> IO ()
    writeGoldenAndFail f v errMsg = do
        try @IOError (TIO.writeFile f v) >>= \case
            Right () -> expectationFailure $ mconcat
                [ errMsg
                , "... "
                , "Now written to disk. Please check for correctness and "
                , "commit."
                ]
            Left writeError -> expectationFailure $ mconcat
                [ errMsg
                , "... "
                , "Unable to write the new value to disk because of:\n"
                , show writeError
                ]
