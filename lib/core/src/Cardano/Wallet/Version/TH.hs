-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Template Haskell function for getting the git revision from the local
-- repo. This is a separate module due to the GHC stage restriction.

module Cardano.Wallet.Version.TH
    ( gitRevFromGit
    ) where

import Prelude

import Language.Haskell.TH
    ( Exp (..), Lit (..), Q, runIO )
import System.Exit
    ( ExitCode (..) )
import System.IO.Error
    ( ioeGetErrorType, isDoesNotExistErrorType )
import UnliftIO.Exception
    ( handleJust )
import UnliftIO.Process
    ( readProcessWithExitCode )

-- | Git revision found by running @git rev-parse@. If @git@ could not be
-- executed, then this will be an empty string.
gitRevFromGit :: Q Exp
gitRevFromGit = LitE . StringL <$> runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse = handleJust missingGit (const $ pure "") $ do
        (exitCode, output, _) <-
            readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
        pure $ case exitCode of
            ExitSuccess -> output
            _           -> ""
    missingGit e =
        if isDoesNotExistErrorType (ioeGetErrorType e)
        then Just ()
        else Nothing
