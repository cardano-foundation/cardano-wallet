{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.Git.TH
    ( gitRevParseHEAD
    ) where

import Prelude

import Control.Exception
    ( SomeException
    , try
    )
import Language.Haskell.TH
    ( Exp (..)
    , Lit (..)
    , Q
    , runIO
    )
import System.Environment
    ( lookupEnv
    )
import System.Exit
    ( ExitCode (..)
    )
import System.Process
    ( readProcessWithExitCode
    )

gitRevParseHEAD :: Q Exp
gitRevParseHEAD =
    LitE . StringL <$> runIO findGitRev
  where
    findGitRev :: IO String
    findGitRev = do
        envRev <- lookupEnv "GITREV"
        maybe runGitRevParse pure envRev

    runGitRevParse :: IO String
    runGitRevParse = do
        result <- try @SomeException $
            readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
        case result of
            Right (ExitSuccess, revision, _) -> pure revision
            _ -> pure "unknown revision"
