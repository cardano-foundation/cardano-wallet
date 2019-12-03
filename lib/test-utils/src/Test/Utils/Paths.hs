-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Utility function for finding the package test data directory.

module Test.Utils.Paths
    ( getTestData
    ) where

import Prelude

import Control.Monad.IO.Class
    ( liftIO )
import Data.FileEmbed
    ( makeRelativeToProject )
import Language.Haskell.TH.Syntax
    ( Exp, Q, liftData )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )

-- | A TH function to get the test data directory.
--
-- It combines the current source file location and cabal file to locate the
-- package directory in such a way that works in both the stack/cabal package
-- build and ghci.
--
-- For the Nix build, rather than baking in a path that starts with @/build@, it
-- makes the test data path relative to the current directory.
getTestData :: Q Exp
getTestData = do
    let relPath = "test" </> "data"
    absPath <- makeRelativeToProject relPath

    -- This environment variable indicates we are building under nix.
    nixBuildDir <- liftIO $ lookupEnv "NIX_BUILD_TOP"

    liftData $ maybe absPath (const relPath) nixBuildDir
