{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Helper functions for testing.
--

module Test.Hspec.Extra
    ( aroundAll
    ) where

import Prelude

import Control.Concurrent.Async
    ( async, race, wait )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Test.Hspec
    ( ActionWith
    , HasCallStack
    , Spec
    , SpecWith
    , afterAll
    , beforeAll
    , beforeWith
    )

-- | Run a 'bracket' resource acquisition function around all the specs. The
-- bracket opens before the first test case and closes after the last test case.
aroundAll
    :: forall a.
       (HasCallStack)
    => (ActionWith a -> IO ())
    -> SpecWith a
    -> Spec
aroundAll with = beforeAll setup . afterAll fst . beforeWith (pure . snd)
  where
    setup :: IO (IO (), a)
    setup = do
        avar <- newEmptyMVar
        finished <- newEmptyMVar
        pid <- async $ with $ \a -> do
            putMVar avar a
            takeMVar finished
        race (wait pid) (takeMVar avar) >>= \case
            Left _ -> throwIO $ userError "aroundAll: failed to setup"
            Right a -> pure (putMVar finished (), a)
