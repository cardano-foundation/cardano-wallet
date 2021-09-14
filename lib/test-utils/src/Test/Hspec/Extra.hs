{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Helper functions for testing.
--

module Test.Hspec.Extra
    ( aroundAll
    , it
    , itWithCustomTimeout
    , flakyBecauseOf
    , parallel

    -- * Custom test suite runner
    , HspecWrapper
    , hspecMain
    , hspecMain'
    , getDefaultConfig
    -- ** Internals
    , configWithExecutionTimes
    , setEnvParser
    ) where

import Prelude

import Control.Monad
    ( void, (<=<) )
import Data.List
    ( elemIndex )
import Options.Applicative
    ( Parser
    , ParserInfo (..)
    , ReadM
    , eitherReader
    , execParser
    , failureCode
    , forwardOptions
    , help
    , info
    , long
    , many
    , metavar
    , option
    , short
    , strArgument
    )
import Say
    ( sayString )
import System.Environment
    ( lookupEnv, withArgs )
import Test.Hspec
    ( ActionWith
    , Expectation
    , HasCallStack
    , Spec
    , SpecWith
    , afterAll
    , beforeAll
    , beforeWith
    , pendingWith
    , specify
    )
import Test.Hspec.Core.Runner
    ( Config (..), Summary, defaultConfig, evaluateSummary, hspecWithResult )
import Test.HUnit.Lang
    ( HUnitFailure (..), assertFailure, formatFailureReason )
import Test.Utils.Env
    ( withAddedEnv )
import Test.Utils.Platform
    ( isWindows )
import Test.Utils.Resource
    ( unBracket )
import Test.Utils.Startup
    ( withLineBuffering )
import UnliftIO.Async
    ( race )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( catch, throwIO )
import UnliftIO.MVar
    ( newEmptyMVar, tryPutMVar, tryTakeMVar )

import qualified Test.Hspec as Hspec

-- | Run a 'bracket' resource acquisition function around all the specs. The
-- resource is allocated just before the first test case and released
-- immediately after the last test case.
--
-- Each test is given the resource as a function parameter.
aroundAll
    :: forall a. HasCallStack
    => (ActionWith a -> IO ())
    -> SpecWith a
    -> Spec
aroundAll acquire =
    beforeAll (unBracket acquire) . afterAll snd . beforeWith fst

-- | Add execution timing information to test output.
--
configWithExecutionTimes :: Config -> Config
configWithExecutionTimes config = config
    { configPrintCpuTime = True
      -- Prints the total elapsed CPU time for the entire test suite.
    , configPrintSlowItems = Just 10
      -- Prints a list of the slowest tests in descending order of
      -- elapsed CPU time.
    , configTimes = True
      -- Appends the elapsed CPU time to the end of each individual test.
    }

-- | A drop-in replacement for 'it' that'll automatically retry a scenario once
-- if it fails, to cope with potentially flaky tests, if the environment
-- variable @TESTS_RETRY_FAILED@ is set.
--
-- It also has a timeout of 10 minutes.
it :: HasCallStack => String -> ActionWith ctx -> SpecWith ctx
it = itWithCustomTimeout (60*minutes)
  where
    minutes = 10

-- | Like @it@ but with a custom timeout, testing of the function possible.
itWithCustomTimeout
    :: HasCallStack
    => Int -- ^ Timeout in seconds.
    -> String
    -> ActionWith ctx
    -> SpecWith ctx
itWithCustomTimeout sec title action = specify title $ \ctx -> do
    e <- newEmptyMVar
    shouldRetry <- maybe False (not . null) <$> lookupEnv "TESTS_RETRY_FAILED"
    let action' = if shouldRetry
        then actionWithRetry (void . tryPutMVar e)
        else action
    race (timer >> tryTakeMVar e) (action' ctx) >>= \case
       Left Nothing ->
           assertFailure $ "timed out in " <> show sec <> " seconds"
       Left (Just firstException) ->
           throwIO firstException
       Right () ->
           pure ()
  where
    timer = threadDelay (sec * 1000 * 1000)

    -- Run the action, if it fails try again. If it fails again, report the
    -- original exception.
    actionWithRetry save ctx = action ctx
        `catch` (\e -> save e >> reportFirst e >> action ctx
        `catch` (\f -> reportSecond e f >> throwIO e))

    reportFirst (HUnitFailure _ reason) = do
        report (formatFailureReason reason)
        report "Retrying failed test."
    reportSecond (HUnitFailure _ reason1) (HUnitFailure _ reason2)
        | reason1 == reason2 = report "Test failed again in the same way."
        | otherwise = do
              report (formatFailureReason reason2)
              report "Test failed again; will report the first error."

    report = mapM_ (sayString . ("retry: " ++)) . lines

-- | Mark a test pending because of flakiness, with given reason. Unless the
-- RUN_FLAKY_TESTS environment variable is set.
flakyBecauseOf :: String -> Expectation
flakyBecauseOf ticketOrReason =
    lookupEnv "RUN_FLAKY_TESTS" >>= \case
        Just _ -> return ()
        Nothing -> pendingWith $ "Flaky: " <> ticketOrReason

-- | Like Hspec's parallel, except on Windows.
parallel :: SpecWith a -> SpecWith a
parallel
    | isWindows = id
    | otherwise = Hspec.parallel

{-------------------------------------------------------------------------------
                             Test suite runner main
-------------------------------------------------------------------------------}

-- | Main function for running a test suite using 'getDefaultConfig'.
hspecMain :: Spec -> IO ()
hspecMain = hspecMain' getDefaultConfig

-- | An IO action that runs around 'hspecWith'.
type HspecWrapper a = IO Summary -> IO a

-- | Main function for running a test suite. Like 'Test.Hspec.hspec', except it
-- allows for a custom action to modify the environment and configuration before
-- passing control over to Hspec.
hspecMain' :: IO (HspecWrapper a, Config) -> Spec -> IO a
hspecMain' getConfig spec = withLineBuffering $ do
    (wrapper, config) <- getConfig
    wrapper $ hspecWithResult config spec

-- | Our custom Hspec wrapper. It adds the @--env@ option for setting
-- environment variables, and prints the tests which took the longest time after
-- finishing the test suite.
getDefaultConfig :: IO (HspecWrapper (), Config)
getDefaultConfig = do
    (env, args) <- execParser setEnvParser
    pure ( evaluateSummary <=< withArgs args . withAddedEnv env
         , configWithExecutionTimes defaultConfig)

-- | A CLI arguments parser which handles setting environment variables.
setEnvParser :: ParserInfo ([(String, String)], [String])
setEnvParser = info ((,) <$> many setEnvOpt <*> restArgs) $
    forwardOptions <> failureCode 89
  where
    setEnvOpt :: Parser (String, String)
    setEnvOpt = option readSetEnv
            (  long "env"
            <> short 'e'
            <> metavar "NAME=VALUE"
            <> help "Export the given environment variable to the test suite" )

    readSetEnv :: ReadM (String, String)
    readSetEnv = eitherReader $ \arg -> case elemIndex '=' arg of
        Just i | i > 0 -> Right (take i arg, drop (i+1) arg)
        _ -> Left "does not match syntax NAME=VALUE"

    restArgs :: Parser [String]
    restArgs = many $ strArgument (metavar "HSPEC-ARGS...")
