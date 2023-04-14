{-# LANGUAGE RankNTypes #-}
module Control.Concurrent.ConciergeSpec
    ( spec
    ) where

import Prelude

import Control.Concurrent.Concierge
    ( atomicallyWithLifted, newConcierge )
import Control.Monad.Class.MonadFork
    ( forkIO )
import Control.Monad.Class.MonadSay
    ( say )
import Control.Monad.Class.MonadTimer
    ( threadDelay )
import Control.Monad.IOSim
    ( IOSim, runSimTrace, selectTraceEventsSay )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT, catchE, runExceptT, throwE )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, (===) )

spec :: Spec
spec = do
    describe "Control.Concurrent.Concierge" $ do
        it "Atomic operations do not interleave"
            unit_atomic

        it "throwE in ExceptT releases lock"
            unit_exceptT_release_lock

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
-- | Deterministic test for atomicity.
-- We have to compare a program run that interleaves
-- against one that is atomic.
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
        _ <- forkIO $ atomically (delay 1 >> action "B")
        atomically $ action "A"
        delay 4

    action :: String -> IOSim s ()
    action s = say s >> delay 2 >> say s

    delay :: Int -> IOSim s ()
    delay n = threadDelay (fromIntegral n*0.1)

-- | Check that using 'throwE' in the 'ExceptE' monad releases the lock
unit_exceptT_release_lock :: Property
unit_exceptT_release_lock =
    ["A"] === selectTraceEventsSay (runSimTrace $ runExceptT test)
  where
    liftE :: IOSim s a -> ExceptT String (IOSim s) a
    liftE = lift

    test :: ExceptT String (IOSim s) ()
    test = do
        concierge <- liftE newConcierge
        let atomically = atomicallyWithLifted liftE concierge ()
        _ <- tryE $ atomically $ throwE "X"
        atomically $ liftE $ say "A"

-- not exported in  transformers <= 0.5.6
tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE action = (Right <$> action) `catchE` (pure . Left)
