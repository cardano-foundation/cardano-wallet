-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Utility function for making test suites pass on Darwin/macOS.

module Test.Utils.Darwin
    ( pendingOnMacOS
    ) where

import Prelude

import Control.Monad
    ( when )
import System.Info
    ( os )
import Test.Hspec.Core.Spec
    ( pendingWith )
import Test.Hspec.Expectations
    ( Expectation, HasCallStack )

-- | Mark test pending if running on macOS
pendingOnMacOS :: HasCallStack => String -> Expectation
pendingOnMacOS reason = when isDarwin $ pendingWith reason

isDarwin :: Bool
isDarwin = os == "darwin"
