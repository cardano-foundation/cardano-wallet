-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Utility function for finding the package test data directory.
module Test.Utils.Paths
    ( getTestData
    , getTestDataPath
    , inNixBuild
    ) where

import Prelude

import Control.Monad.IO.Class
    ( liftIO
    )
import Data.FileEmbed
    ( makeRelativeToProject
    )
import Language.Haskell.TH.Syntax
    ( Exp
    , Q
    , liftData
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( (</>)
    )

-- | A TH function to get the test data directory.
--
-- It combines the current source file location and cabal file to locate the
-- package directory in such a way that works in both the stack/cabal package
-- build and ghci.
--
-- For the Nix build, rather than baking in a path that starts with @/build@, it
-- makes the test data path relative to the current directory.
getTestData :: Q Exp
getTestData = getTestDataPath ("test" </> "data")

-- | A variant of 'getTestData' which lets you specify the test data 'FilePath'
-- relative to the package root directory.
getTestDataPath :: FilePath -> Q Exp
getTestDataPath relPath = do
    absPath <- makeRelativeToProject relPath
    useRel <- liftIO inNixBuild
    liftData (if useRel then relPath else absPath)

-- | Infer from environment variables whether we are running within a Nix build
-- (and not just a nix-shell).
inNixBuild :: IO Bool
inNixBuild = do
    let testEnv = fmap (maybe False (not . null)) . lookupEnv
    haveNixBuildDir <- testEnv "NIX_BUILD_TOP"
    inNixShell <- testEnv "IN_NIX_SHELL"
    pure (haveNixBuildDir && not inNixShell)
