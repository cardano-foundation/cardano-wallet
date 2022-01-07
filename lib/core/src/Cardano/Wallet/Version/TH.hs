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

import Control.Exception
    ( catch )
import Language.Haskell.TH
    ( Exp (..), Lit (..), Q, runIO )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( hPutStrLn, stderr )
import System.IO.Error
    ( isDoesNotExistError )
import UnliftIO.Process
    ( readProcessWithExitCode )

-- | Git revision found by running @git rev-parse@. If @git@ could not be
-- executed, then this will be an empty string.
gitRevFromGit :: Q Exp
gitRevFromGit = LitE . StringL <$> runIO runGitRevParse
    where
        runGitRevParse :: IO String
        runGitRevParse = do
            (exitCode, output, errorMessage) <- readProcessWithExitCode_ "git" ["rev-parse", "--verify", "HEAD"] ""
            case exitCode of
                ExitSuccess -> pure output
                ExitFailure _ -> do
                    hPutStrLn stderr $ "WARNING: " ++ errorMessage
                    pure ""

        readProcessWithExitCode_ :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
        readProcessWithExitCode_ cmd args input =
            catch (readProcessWithExitCode cmd args input) $ \e ->
                if isDoesNotExistError e
                    then return (ExitFailure 127, "", show e)
                    else return (ExitFailure 999, "", show e)
