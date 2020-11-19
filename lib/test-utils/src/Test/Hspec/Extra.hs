{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Concurrent.Async
    ( AsyncCancelled, async, race, wait )
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( SomeException, catch, throwIO )
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
    , expectationFailure
    , pendingWith
    , specify
    )

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
it = itWithCustomTimeout (10*minute)
  where
    minute = 60

-- | Like @it@ but with a custom timeout, which makes it realistic to test.
itWithCustomTimeout
    :: HasCallStack
    => Int -- ^ Timeout in seconds.
    -> String
    -> ActionWith ctx
    -> SpecWith ctx
itWithCustomTimeout sec title action = specify title $ \ctx -> timeout sec $ do
    action ctx
        `catch` (\(_ :: AsyncCancelled) -> return ())
        `catch` (\(e :: SomeException)  -> action ctx
        `catch` (\(_ :: SomeException)  -> throwIO e))
  where
    timeout t act =
        race (threadDelay (micro t)) act >>= \case
            Right () ->
                return ()
            Left () ->
                expectationFailure $  "timed out in " <> show t <> " seconds"
      where
        micro = (* 1000) . (* 1000)

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
