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
    ) where

import Prelude

import Control.Monad
    ( void )
import Say
    ( sayString )
import System.Environment
    ( lookupEnv )
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
import Test.HUnit.Lang
    ( HUnitFailure (..), assertFailure, formatFailureReason )
import Test.Utils.Resource
    ( unBracket )
import Test.Utils.Windows
    ( isWindows )
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
