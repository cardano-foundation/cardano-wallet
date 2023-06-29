{-# LANGUAGE LambdaCase #-}

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

import Fmt
    ( fmt
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import Language.Haskell.TH
    ( Exp (..)
    , Lit (..)
    , Q
    , runIO
    )
import System.Exit
    ( ExitCode (..)
    )
import System.IO
    ( hPutStrLn
    , stderr
    )
import System.IO.Error
    ( isDoesNotExistError
    )
import UnliftIO.Exception
    ( handle
    )
import UnliftIO.Process
    ( readProcessWithExitCode
    )

-- | Git revision found by running @git rev-parse@. If @git@ could not be
-- executed, then this will be an empty string.
gitRevFromGit :: Q Exp
gitRevFromGit = LitE . StringL <$> runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse =
        run "git" ["rev-parse", "--verify", "HEAD"] >>= \case
            Right output -> pure output
            Left errorMessage -> do
                -- This message will appear in the build logs
                hPutStrLn stderr $ "WARNING (gitRevFromGit): " ++ errorMessage
                pure ""

    run :: FilePath -> [String] -> IO (Either String String)
    run cmd args = handleProcess $ readProcessWithExitCode cmd args ""
      where
        handleProcess = handle (pure . Left . errMsg) . fmap handleExitCode

        handleExitCode = \case
            (ExitSuccess, output, _) -> Right output
            (ExitFailure code, _, err) ->
                Left
                    $ fmt
                    $ cmd'
                    |+ " exited with status "
                    +| code
                    |+ ": "
                    +| err
                    |+ ""

        cmd' = unwords (cmd : args)

        errMsg e =
            if isDoesNotExistError e
                then fmt ("Could not find " +| cmd |+ ": " +|| e ||+ "")
                else show e
