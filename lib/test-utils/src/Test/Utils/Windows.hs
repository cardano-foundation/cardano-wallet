{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Utility function for making test suites pass on Windows.

module Test.Utils.Windows
    ( skipOnWindows
    , pendingOnWindows
    , pendingOnWine
    , whenWindows
    , isWindows
    , nullFileName
    , getIsWine
    ) where

import Prelude

import Control.Monad
    ( when )
import System.Exit
    ( ExitCode (..) )
import System.Info
    ( os )
import System.Process
    ( readProcessWithExitCode )
import Test.Hspec.Core.Spec
    ( ResultStatus (..), pendingWith )
import Test.Hspec.Expectations
    ( Expectation, HasCallStack )
import UnliftIO.Exception
    ( IOException, handle, throwIO )

skipOnWindows :: HasCallStack => String -> Expectation
skipOnWindows _reason = whenWindows $ throwIO Success

pendingOnWindows :: HasCallStack => String -> Expectation
pendingOnWindows reason = whenWindows $ pendingWith reason

pendingOnWine :: HasCallStack => String -> Expectation
pendingOnWine reason = whenWindows $ do
    wine <- getIsWine
    when wine $ pendingWith reason

whenWindows :: IO () -> IO ()
whenWindows = when isWindows

isWindows :: Bool
isWindows = os == "mingw32"

-- | Use the presence of @winepath.exe@ to detect when running tests under Wine.
getIsWine :: IO Bool
getIsWine = handle (\(_ :: IOException) -> pure False) $ do
    (code, _, _) <- readProcessWithExitCode "winepath" ["--version"] mempty
    pure (code == ExitSuccess)

nullFileName :: FilePath
nullFileName = if isWindows then "NUL" else "/dev/null"
