{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Utility function for making test suites pass on Windows.

module Test.Utils.Windows
    ( skipOnWindows
    , pendingOnWindows
    , whenWindows
    , isWindows
    ) where

import Prelude

import Control.Exception
    ( throwIO )
import Control.Monad
    ( when )
import System.Info
    ( os )
import Test.Hspec.Core.Spec
    ( ResultStatus (..), pendingWith )
import Test.Hspec.Expectations
    ( Expectation, HasCallStack )

skipOnWindows :: HasCallStack => String -> Expectation
skipOnWindows _reason = whenWindows $ throwIO Success

pendingOnWindows :: HasCallStack => String -> Expectation
pendingOnWindows reason = whenWindows $ pendingWith reason

whenWindows :: IO () -> IO ()
whenWindows = when isWindows

isWindows :: Bool
isWindows = os == "mingw32"
