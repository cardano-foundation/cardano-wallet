-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Utility function for finding the package test data directory.

module Test.Utils.Paths
    ( getTestData
    ) where

import Prelude

import Data.FileEmbed
    ( makeRelativeToProject )
import Language.Haskell.TH.Syntax
    ( Exp, Q, liftData )
import System.FilePath
    ( (</>) )

-- | A TH function to get the test data directory. It combines the current
-- source file location and cabal file to locate the package directory in such a
-- way that works in both the package build and ghci.
getTestData :: Q Exp
getTestData = makeRelativeToProject ("test" </> "data") >>= liftData
