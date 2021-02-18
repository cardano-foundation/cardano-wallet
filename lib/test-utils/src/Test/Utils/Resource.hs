{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- A helper function for using the bracket pattern in code.
--

module Test.Utils.Resource
    ( unBracket
    ) where

import Prelude

import Control.Monad.IO.Unlift
    ( MonadUnliftIO )
import GHC.Stack
    ( HasCallStack )
import UnliftIO.Async
    ( async, race, waitCatch )
import UnliftIO.Exception
    ( finally, throwIO, throwString )
import UnliftIO.Memoize
    ( memoizeMVar, runMemoized )
import UnliftIO.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )

-- | Decompose a bracket pattern resource acquisition function into two separate
-- functions: "allocate" and "release".
--
-- It almost goes without saying that you should always call "release" after
-- "allocate", otherwise bad things will happen.
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
unBracket
    :: forall m a. (HasCallStack, MonadUnliftIO m)
    => ((a -> m ()) -> m ())
    -> m (m a, m ())
unBracket withResource = do
    allocated <- newEmptyMVar
    released  <- newEmptyMVar
    done      <- newEmptyMVar

    let cont a = do
            putMVar allocated a
            await released

    release <- memoizeMVar $ do
        unlock released
        await done

    allocate <- memoizeMVar $ do
        pid <- async $ withResource cont `finally` unlock done
        race (waitCatch pid) (takeMVar allocated) >>= \case
            Left (Left e) -> throwIO e
            Left (Right ()) -> throwString "aroundAll: failed to setup"
            Right a -> pure a

    pure (runMemoized allocate, runMemoized release)

  where
    await :: MVar () -> m ()
    await = takeMVar

    unlock :: MVar () -> m ()
    unlock = flip putMVar ()
