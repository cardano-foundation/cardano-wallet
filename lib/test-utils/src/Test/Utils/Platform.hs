{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Utility function for making test suites pass on difficult platforms.

module Test.Utils.Platform
    ( -- * Skipping tests
      skipOnWindows
    , pendingOnWindows
    , pendingOnWine
    , pendingOnMacOS

    -- * OS detection
    , whenWindows
    , isWindows
    , isMacOS
    , getIsWine

    -- * Cross-platform compatibility
    , nullFileName
    ) where

import Prelude

import Control.Monad
    ( when )
import System.Exit
    ( ExitCode (..) )
import System.Info
    ( os )
import Test.Hspec.Core.Spec
    ( ResultStatus (..), pendingWith )
import Test.Hspec.Expectations
    ( Expectation )
import UnliftIO.Exception
    ( IOException, handle, throwIO )
import UnliftIO.Process
    ( readProcessWithExitCode )

skipOnWindows :: String -> Expectation
skipOnWindows _reason = whenWindows $ throwIO Success

pendingOnWindows :: String -> Expectation
pendingOnWindows reason = whenWindows $ pendingWith reason

pendingOnWine :: String -> Expectation
pendingOnWine reason = whenWindows $ do
    wine <- getIsWine
    when wine $ pendingWith reason

-- | Mark test pending if running on macOS
pendingOnMacOS :: String -> Expectation
pendingOnMacOS reason = when isMacOS $ pendingWith reason

isWindows, isMacOS :: Bool
isWindows = os == "mingw32"
isMacOS = os == "darwin"

whenWindows :: IO () -> IO ()
whenWindows = when isWindows

-- | Use the presence of @winepath.exe@ to detect when running tests under Wine.
getIsWine :: IO Bool
getIsWine = handle (\(_ :: IOException) -> pure False) $ do
    (code, _, _) <- readProcessWithExitCode "winepath" ["--version"] mempty
    pure (code == ExitSuccess)

nullFileName :: FilePath
nullFileName = if isWindows then "NUL" else "/dev/null"
