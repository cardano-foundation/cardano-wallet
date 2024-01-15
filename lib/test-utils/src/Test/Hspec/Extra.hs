{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Helper functions for testing.
module Test.Hspec.Extra
    ( aroundAll
    , it
    , itN
    , itNT
    , xit
    , itWithCustomTimeout
    , parallel
    , counterexample
    , appendFailureReason

      -- * Custom test suite runner
    , HspecWrapper
    , hspecMain
    , hspecMain'
    , getDefaultConfig

      -- ** Internals
    , configWithExecutionTimes
    , setEnvParser

      -- * Metrics
    , HasMetrics (..)
    , NoMetrics (..)
    ) where

import Prelude

import Control.Monad
    ( (<=<)
    )
import Control.Monad.IO.Class
    ( MonadIO
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO
    )
import Data.List
    ( elemIndex
    )
import Data.Time
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    , getCurrentTime
    )
import "optparse-applicative" Options.Applicative
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
import System.Environment
    ( lookupEnv
    , withArgs
    )
import Test.Hspec
    ( ActionWith
    , HasCallStack
    , Spec
    , SpecWith
    , afterAll
    , beforeAll
    , beforeWith
    , specify
    )
import Test.Hspec.Core.Runner
    ( Config (..)
    , Summary
    , defaultConfig
    , evaluateSummary
    , hspecWithResult
    )
import Test.HUnit.Lang
    ( FailureReason (..)
    , HUnitFailure (..)
    , assertFailure
    )
import Test.Utils.Env
    ( withAddedEnv
    )
import Test.Utils.Platform
    ( isWindows
    )
import Test.Utils.Resource
    ( unBracket
    )
import Test.Utils.Startup
    ( withLineBuffering
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( catch
    , throwIO
    )

import qualified Test.Hspec as Hspec

-- | Run a 'bracket' resource acquisition function around all the specs. The
-- resource is allocated just before the first test case and released
-- immediately after the last test case.
--
-- Each test is given the resource as a function parameter.
aroundAll
    :: forall a
     . HasCallStack
    => (ActionWith a -> IO ())
    -> SpecWith a
    -> Spec
aroundAll acquire =
    beforeAll (unBracket acquire) . afterAll snd . beforeWith fst

-- | Add execution timing information to test output.
configWithExecutionTimes :: Config -> Config
configWithExecutionTimes config =
    config
        { configPrintCpuTime = True
        , -- Prints the total elapsed CPU time for the entire test suite.
          configPrintSlowItems = Just 10
        , -- Prints a list of the slowest tests in descending order of
          -- elapsed CPU time.
          configTimes = True
          -- Appends the elapsed CPU time to the end of each individual test.
        }

-- | A drop-in replacement for 'it' that'll automatically retry a
-- scenario 5 times if it fails.
-- To cope with potentially flaky tests!
-- Works only if the environment variable @TESTS_RETRY_FAILED@ is set.
--
-- It also has a timeout of 10 minutes.
it
    :: (HasCallStack, HasMetrics ctx)
    => String
    -> ActionWith ctx
    -> SpecWith ctx
it = itN 5

-- | A drop-in replacement for 'it' that'll automatically retry a
-- scenario n times if it fails with a timeout of 10 minutes.
itN
    :: (HasCallStack, HasMetrics ctx)
    => Int -- ^ Number of retries
    -> String -- ^ Title of the test
    -> ActionWith ctx -- ^ Action to run
    -> SpecWith ctx
itN n = itNT n 10

-- | A drop-in replacement for 'it' that'll automatically retry a
-- scenario n times with a timeout of t minutes if it fails.
itNT
    :: (HasCallStack, HasMetrics ctx)
    => Int -- ^ Number of retries
    -> Int -- ^ Timeout in minutes
    -> String -- ^ Title of the test
    -> ActionWith ctx -- ^ Action to run
    -> SpecWith ctx
itNT n t = itWithCustomTimeout n (t * 60)

-- |
-- Changing `it` to `xit` marks the corresponding spec item as pending.
--
-- This can be used to temporarily disable a spec item.
xit :: (HasCallStack, HasMetrics ctx) => String -> ActionWith ctx -> SpecWith ctx
xit label action = Hspec.before_ Hspec.pending $ it label action

class HasMetrics ctx where
    putTimeout :: ctx -> String -> NominalDiffTime -> IO ()
    putFailure :: ctx -> String -> HUnitFailure -> NominalDiffTime -> IO ()
    putSuccess :: ctx -> String -> NominalDiffTime -> IO ()

data NoMetrics = NoMetrics

instance HasMetrics NoMetrics where
    putTimeout _ _ _ = pure ()
    putFailure _ _ _ _ = pure ()
    putSuccess _ _ _ = pure ()

diffTime :: UTCTime -> IO NominalDiffTime
diffTime start = do
    end <- getCurrentTime
    pure $ diffUTCTime end start

data Result = Timeout Int | Success | Failure HUnitFailure

addResult :: Result -> Result -> Result
addResult Success _ = Success
addResult _ Success = Success
addResult (Failure f) _ = Failure f
addResult _ (Failure f) = Failure f
addResult (Timeout t) (Timeout t') = Timeout (t + t')

timedAction
    :: HasMetrics ctx
    => [Int]
    -- ^ Timeouts in seconds
    -> String
    -- ^ Title of the test
    -> (ctx -> IO ())
    -- ^ Action to run
    -> ctx
    -- ^ Context
    -> IO Result
timedAction = go
  where
    go :: HasMetrics ctx => [Int] -> String -> (ctx -> IO ()) -> ctx -> IO Result
    go [] _ _ _ = pure (Timeout 0)
    go (timeout : timeouts) title action'' ctx' = do
        start <- getCurrentTime
        let
            next = go timeouts title action'' ctx'
            timer = do
                threadDelay (timeout * 1000 * 1000)
                putTimeout ctx' title (fromIntegral timeout)
            run = do
                r <- race timer $ do
                    action'' ctx'
                    diff <- diffTime start
                    putSuccess ctx' title diff
                case r of
                    Left _ ->
                        addResult (Timeout timeout) <$> next
                    Right _ -> pure Success
        run `catch` \e@(_ :: HUnitFailure) -> do
            diff <- diffTime start
            putFailure ctx' title e diff
            addResult (Failure e) <$> go timeouts title action'' ctx'

-- | Like @it@ but with a custom timeout, and more than once try
-- testing of the function possible.
itWithCustomTimeout
    :: forall ctx
     . HasMetrics ctx
    => HasCallStack
    => Int
    -- ^ Times to retry.
    -> Int
    -- ^ Timeout in seconds.
    -> String
    -- ^ Title of the test
    -> ActionWith ctx
    -- ^ Action to run
    -> SpecWith ctx
    -- ^ Resulting spec
itWithCustomTimeout times sec title action = specify title $ \ctx -> do
    shouldRetry <- do
        me <- lookupEnv "TESTS_RETRY_FAILED"
        case me of
            Nothing -> pure [sec]
            Just "" -> pure [sec]
            Just _ -> pure $ replicate times sec
    timedAction shouldRetry title action ctx >>= \case
        Success -> pure ()
        Failure e -> throwIO e
        Timeout t -> assertFailure $ "timed out in " <> show t <> " seconds"

-- | Like Hspec's parallel, except on Windows.
parallel :: SpecWith a -> SpecWith a
parallel
    | isWindows = id
    | otherwise = Hspec.parallel

-- | Can be used to add context to a @HUnitFailure@.
--
-- >>> counterexample (show response) (0 `shouldBe` 3)
-- >>>  (Status {statusCode = 200, statusMessage = "OK"},Right [])
-- >>>        expected: 3
-- >>>         but got: 0
counterexample
    :: (MonadIO m, MonadUnliftIO m, HasCallStack)
    => String
    -> m a
    -> m a
counterexample msg = (`catch` (throwIO . appendFailureReason msg))

appendFailureReason :: String -> HUnitFailure -> HUnitFailure
appendFailureReason message = wrap
  where
    wrap :: HUnitFailure -> HUnitFailure
    wrap (HUnitFailure mloc reason) = HUnitFailure mloc (addMessageTo reason)

    addMessageTo :: FailureReason -> FailureReason
    addMessageTo (Reason reason) = Reason $ addMessage reason
    addMessageTo (ExpectedButGot preface expected actual) =
        ExpectedButGot (Just $ maybe message addMessage preface) expected actual

    addMessage = (++ "\n" ++ message)

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
    pure
        ( evaluateSummary <=< withArgs args . withAddedEnv env
        , configWithExecutionTimes defaultConfig
        )

-- | A CLI arguments parser which handles setting environment variables.
setEnvParser :: ParserInfo ([(String, String)], [String])
setEnvParser =
    info ((,) <$> many setEnvOpt <*> restArgs)
        $ forwardOptions <> failureCode 89
  where
    setEnvOpt :: Parser (String, String)
    setEnvOpt =
        option
            readSetEnv
            ( long "env"
                <> short 'e'
                <> metavar "NAME=VALUE"
                <> help "Export the given environment variable to the test suite"
            )

    readSetEnv :: ReadM (String, String)
    readSetEnv = eitherReader $ \arg -> case elemIndex '=' arg of
        Just i | i > 0 -> Right (take i arg, drop (i + 1) arg)
        _ -> Left "does not match syntax NAME=VALUE"

    restArgs :: Parser [String]
    restArgs = many $ strArgument (metavar "HSPEC-ARGS...")
