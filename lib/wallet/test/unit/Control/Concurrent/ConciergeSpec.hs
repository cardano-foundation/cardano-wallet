{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Concurrent.ConciergeSpec
    ( spec
    ) where

import Prelude

import Control.Concurrent.Concierge
    ( atomicallyWithLifted
    , newConcierge
    )
import Control.Monad.Class.MonadFork
    ( forkIO
    )
import Control.Monad.Class.MonadSay
    ( say
    )
import Control.Monad.Class.MonadThrow
    ( throwIO
    , try
    )
import Control.Monad.Class.MonadTimer
    ( threadDelay
    )
import Control.Monad.IOSim
    ( IOSim
    , runSimTrace
    , selectTraceEventsSay
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Property
    , (===)
    )

spec :: Spec
spec =
    describe "Control.Concurrent.Concierge" $ do
        it
            "Atomic operations do not interleave"
            unit_atomic

        it
            "throwIO releases lock"
            unit_release_lock

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}

-- | Deterministic test for atomicity.
-- We have to compare a program run that interleaves against one that is atomic.
unit_atomic :: Bool
unit_atomic =
    ("ABAB" == sayings testInterleave) && ("AABB" == sayings testAtomic)
  where
    sayings :: (forall s. IOSim s a) -> String
    sayings x = concat . selectTraceEventsSay $ runSimTrace x

    testAtomic = do
        concierge <- newConcierge
        test $ atomicallyWithLifted id concierge ()
    testInterleave = test id

    test :: (forall a. IOSim s a -> IOSim s a) -> IOSim s ()
    test atomically = do
        _ <- forkIO $ atomically (threadDelay 1 >> action "B")
        atomically $ action "A"
        threadDelay 4

    action :: String -> IOSim s ()
    action s = say s >> threadDelay 2 >> say s

unit_release_lock :: Property
unit_release_lock = ["A"] === selectTraceEventsSay (runSimTrace test)
  where
    test :: IOSim s ()
    test = do
        concierge <- newConcierge
        let atomically = atomicallyWithLifted id concierge ()
        _ <- try @_ @IOError $ atomically $ throwIO $ userError "X"
        atomically $ say "A"
