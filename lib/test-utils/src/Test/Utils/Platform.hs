{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
    , skipWhenRoot

      -- * OS detection
    , whenWindows
    , isWindows
    , isMacOS
    , getIsWine
    , runningAsRoot

      -- * Cross-platform compatibility
    , nullFileName
    ) where

import Control.Monad
    ( when
    )
import System.Exit
    ( ExitCode (..)
    )
import System.Info
    ( os
    )
import Test.Hspec.Core.Spec
    ( ResultStatus (..)
    , pendingWith
    )
import Test.Hspec.Expectations
    ( Expectation
    , HasCallStack
    )
import UnliftIO.Exception
    ( IOException
    , handle
    , throwIO
    )
import UnliftIO.Process
    ( readProcessWithExitCode
    )
import Prelude

skipOnWindows :: HasCallStack => String -> Expectation
skipOnWindows _reason = whenWindows $ throwIO Success

pendingOnWindows :: HasCallStack => String -> Expectation
pendingOnWindows reason = whenWindows $ pendingWith reason

pendingOnWine :: HasCallStack => String -> Expectation
pendingOnWine reason = whenWindows $ do
    wine <- getIsWine
    when wine $ pendingWith reason

-- | Mark test pending if running on macOS
pendingOnMacOS :: HasCallStack => String -> Expectation
pendingOnMacOS reason = when isMacOS $ pendingWith reason

-- | Mark a test pending when the process runs as root (effective uid 0).
-- Some assertions — e.g. that binding a privileged port is denied — only
-- hold for an unprivileged user, but CI self-hosted runners often run as
-- root, where the privileged operation instead succeeds.
skipWhenRoot :: HasCallStack => String -> Expectation
skipWhenRoot reason = do
    root <- runningAsRoot
    when root $ pendingWith reason

-- | Whether the current process has effective user id 0. Determined via
-- @id -u@ to avoid a @unix@ dependency and stay cross-platform; always
-- 'False' on Windows.
runningAsRoot :: IO Bool
runningAsRoot
    | isWindows = pure False
    | otherwise =
        handle (\(_ :: IOException) -> pure False) $ do
            (code, out, _) <- readProcessWithExitCode "id" ["-u"] ""
            pure (code == ExitSuccess && words out == ["0"])

isWindows, isMacOS :: Bool
isWindows = os == "mingw32"
isMacOS = os == "darwin"

whenWindows :: IO () -> IO ()
whenWindows = when isWindows

-- | Use the presence of @winepath.exe@ to detect when running tests under Wine.
getIsWine :: IO Bool
getIsWine = handle (\(_ :: IOException) -> pure False) $ do
    (code, _, _) <-
        readProcessWithExitCode "winepath" ["--version"] mempty
    pure (code == ExitSuccess)

nullFileName :: FilePath
nullFileName = if isWindows then "NUL" else "/dev/null"
