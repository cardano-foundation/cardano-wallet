{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Utility function for making test suites pass on Windows.

module Test.Utils.Windows
    ( skipOnWindows
    ) where

import Prelude

import Control.Exception
    ( throwIO )
import Control.Monad
    ( when )
import System.Info
    ( os )
import Test.Hspec.Core.Spec
    ( ResultStatus (..) )
import Test.Hspec.Expectations
    ( Expectation, HasCallStack )

skipOnWindows :: HasCallStack => String -> Expectation
skipOnWindows _reason = when (os == "mingw32") $ throwIO Success
