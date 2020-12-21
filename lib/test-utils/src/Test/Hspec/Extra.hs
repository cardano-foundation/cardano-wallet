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
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
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
import UnliftIO.Async
    ( async, race, wait )
import UnliftIO.Exception
    ( catch, finally, throwIO )
import UnliftIO.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryTakeMVar )

-- | Run a 'bracket' resource acquisition function around all the specs. The
-- bracket opens before the first test case and closes after the last test case.
--
-- It works by actually spawning a new thread responsible for the resource
-- acquisition, passing the resource along to the parent threads via a shared
-- MVar. Then, there's a bit of logic to synchronize both threads and make sure
-- that:
--
-- a) The 'Resource Owner' thread is terminated when the main thread is done
--    with the resource.
--
-- b) The 'Main Thread' only exists when the resource owner has released the
--    resource. Exiting the main thread before the 'Resource Owner' has
--    released the resource could left a hanging resource open. This is
--    particularly annoying when the resource is a running process!
--
--     Main Thread            Resource Owner
--          x
--          |         Spawn
--          |----------------------->x
--          |                        |
--          |                        |-- Acquire resource
--          |     Send Resource      |
--          |<-----------------------|
--          |                        |
--          |                        |
--         ...                      ... Await main thread signal
--          |                        |
--          |                        |
--          |      Send Signal       |
--          |----------------------->|
--          |                        |
--          |                       ... Release resource
--          |      Send Done         |
--          |<-----------------------|
--          |                       Exit
--          |
--         Exit
--
aroundAll
    :: forall a.
       (HasCallStack)
    => (ActionWith a -> IO ())
    -> SpecWith a
    -> Spec
aroundAll acquire =
    beforeAll setup . afterAll snd . beforeWith (pure . fst)
  where
    setup :: IO (a, IO ())
    setup = do
        resource <- newEmptyMVar
        release  <- newEmptyMVar
        done     <- newEmptyMVar

        pid <- async $ do
            acquire $ \a -> do
                putMVar resource a
                await release
            unlock done

        race (wait pid) (takeMVar resource) >>= \case
            Left _ ->
                throwIO $ userError "aroundAll: failed to setup"
            Right a -> pure $ (a,) $ do
                unlock release
                await done


-- | A drop-in replacement for 'it' that'll automatically retry a scenario once
-- if it fails, to cope with potentially flaky tests.
--
-- It also has a timeout of 10 minutes.
it :: HasCallStack => String -> ActionWith ctx -> SpecWith ctx
it = itWithCustomTimeout (60*minutes)
  where
    minutes = 10

-- | Like @it@ but with a custom timeout, which makes it realistic to test.
itWithCustomTimeout
    :: HasCallStack
    => Int -- ^ Timeout in seconds.
    -> String
    -> ActionWith ctx
    -> SpecWith ctx
itWithCustomTimeout sec title action = specify title $ \ctx -> do
    e <- newEmptyMVar
    race (timer >> tryTakeMVar e) (action' (void . tryPutMVar e) ctx) >>= \case
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
    action' save ctx = action ctx
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

-- | Some helper to help readability on the thread synchronization above.
await :: MVar () -> IO ()
await = takeMVar

-- | Some helper to help readability on the thread synchronization above.
unlock  :: MVar () -> IO ()
unlock = flip putMVar ()

-- | Mark a test pending because of flakiness, with given reason. Unless the
-- RUN_FLAKY_TESTS environment variable is set.
flakyBecauseOf :: String -> Expectation
flakyBecauseOf ticketOrReason =
    lookupEnv "RUN_FLAKY_TESTS" >>= \case
        Just _ -> return ()
        Nothing -> pendingWith $ "Flaky: " <> ticketOrReason
