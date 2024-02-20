{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.RegistrySpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..)
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , Worker
    , WorkerLog
    , empty
    , register
    , workerThread
    )
import Control.Exception.Base
    ( AsyncException (..)
    , asyncExceptionFromException
    )
import Control.Monad
    ( replicateM
    , void
    )
import Control.Tracer
    ( Tracer
    )
import Crypto.Hash
    ( hash
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , Property
    , generate
    , property
    )
import Test.QuickCheck.Monadic
    ( monadicIO
    , run
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Concurrent
    ( threadDelay
    , throwTo
    )
import UnliftIO.Exception
    ( SomeException (..)
    , throwIO
    )
import UnliftIO.MVar
    ( modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , swapMVar
    , takeMVar
    , tryTakeMVar
    )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Workers" $ do
        it "Executes task and stops gracefully when done"
            workerRunsTaskAndExits
        it "Stops gracefully when receiving an async exception"
            workerFailsGracefullyAsyncE
        it "Stops gracefully when 'main' fails"
            workerFailsGracefullyMain
        it "Stops gracefully when 'before' fails"
            workerFailsGracefullyBefore
        it "Stops gracefully when 'acquire' fails"
            workerFailsGracefullyAcquire
        it "Does not return until 'before' is completed"
            (property workerIsUsableAfterBefore)

{-------------------------------------------------------------------------------
                                  Tests
-------------------------------------------------------------------------------}

-- | Workers are given a 'workerTask', and ends as soon as their task is over.
workerRunsTaskAndExits
    :: IO ()
workerRunsTaskAndExits = do
    mvar <- newMVar (42 :: Int)
    workerTest $ defaultWorkerTest
        { _workerMain = \_ _ ->
            void $ swapMVar mvar 14

        , _workerAssertion = \res -> do
            res `shouldBe` WorkerIsDone
            takeMVar mvar `shouldReturn` 14
        }

-- | When receiving an async exception, the worker action stops and 'onExit' is
-- called.
workerFailsGracefullyAsyncE
    :: IO ()
workerFailsGracefullyAsyncE =
    workerTest $ defaultWorkerTest
        { _workerMain = \_ _ ->
            threadDelay maxBound

        , _workerConcurrently = \worker ->
            throwTo (workerThread worker) UserInterrupt

        , _workerAssertion = \case
            WorkerWasInterrupted someE ->
                case asyncExceptionFromException someE of
                    Just e ->
                        e `shouldBe` UserInterrupt
                    Nothing ->
                        fail "expected worker to stop with async exception"
            _ ->
                fail "expected worker to stop with exception"
        }

-- | When 'acquire' throws an exception, the worker terminates gracefully
workerFailsGracefullyAcquire
    :: IO ()
workerFailsGracefullyAcquire = do
    mvar <- newMVar (42 :: Int)
    workerTest $ defaultWorkerTest
        { _workerBefore = \_ _ ->
            void $ swapMVar mvar 14

        , _workerMain = \_ _ ->
            void $ swapMVar mvar 14

        , _workerAssertion = \res -> do
            res `shouldBe` WorkerNotStarted
            tryTakeMVar mvar `shouldReturn` (Just 42)

        , _workerAcquire =
            error "oops"
        }

-- | When 'before' throws an exception, the worker terminates gracefully
workerFailsGracefullyBefore
    :: IO ()
workerFailsGracefullyBefore = do
    mvar <- newMVar (42 :: Int)
    workerTest $ defaultWorkerTest
        { _workerBefore = \_ _ ->
            throwIO UserInterrupt

        , _workerMain = \_ _ ->
            void $ swapMVar mvar 14

        , _workerAssertion = \res -> do
            res `shouldBe` WorkerNotStarted
            tryTakeMVar mvar `shouldReturn` (Just 42)
        }

-- | When 'main' throws an exception, the worker terminates gracefully
workerFailsGracefullyMain
    :: IO ()
workerFailsGracefullyMain = do
    workerTest $ defaultWorkerTest
        { _workerMain = \_ _ ->
            throwIO UserInterrupt

        , _workerAssertion = \res -> do
            res `shouldBe` WorkerWasInterrupted (SomeException UserInterrupt)
        }

-- | Worker is not usable before the 'before' action has completed.
--
-- We run two actions concurrently:
--
--    - _workerBefore (which runs inside the worker thread)
--    - _workerConcurrently (which runs insides the main thread)
--
-- Both actions appends a string to a list. Though, _workerBefore does it after
-- a little delay. Yet, regardless of the delay, we expect "before" to always
-- be present before "concurrently".
workerIsUsableAfterBefore
    :: Delay
    -> Property
workerIsUsableAfterBefore (Delay delay) = monadicIO $ do
    actions <- run $ newMVar ([] :: [String])
    done <- run newEmptyMVar
    run $ workerTest $ defaultWorkerTest
        { _workerBefore = \_ _ -> do
            threadDelay delay
            modifyMVar_ actions (\xs -> return $ "before":xs)

        , _workerMain = \_ _ ->
            takeMVar done

        , _workerConcurrently = \_ -> do
            modifyMVar_ actions (\xs -> return $ "concurrently":xs)
            putMVar done ()

        , _workerAssertion = \res -> do
            res `shouldBe` WorkerIsDone
            tryTakeMVar actions `shouldReturn` Just ["concurrently", "before"]

        , _workerTimeout =
            100 * delay
        }

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

data DummyCtx = DummyCtx
    (Tracer IO (WorkerLog WalletId Text))
    DummyResource
    deriving (Generic)

data DummyResource = DummyResource deriving (Generic)

instance HasWorkerCtx DummyResource DummyCtx where
    type WorkerCtx DummyCtx = DummyCtx
    type WorkerMsg DummyCtx = Text
    type WorkerKey DummyCtx = WalletId
    hoistResource _ _ = id

-- A reasonably 'long' delay to test asynchronous race conditions, in us
newtype Delay = Delay Int deriving Show

data WorkerTest ctx res = WorkerTest
    { _workerBefore :: WorkerCtx ctx -> WalletId -> IO ()
        -- ^ A task to execute before the main worker's task. See 'workerBefore'

    , _workerMain :: WorkerCtx ctx -> WalletId -> IO ()
        -- ^ A main task to execute, see 'workerMain'

    , _workerAcquire :: (res -> IO ()) -> IO ()
        -- ^ How the worker acquires its resource

    , _workerConcurrently :: Worker WalletId res -> IO ()
        -- ^ An action to perform after the worker has been created,
        -- concurrently in the main thread.

    , _workerAssertion :: WorkerResult -> IO ()
        -- ^ Assertion to run after the wallet has exited

    , _workerTimeout :: Int
        -- ^ Timeout in us after which the worker is killed
    }

-- | A default setup to make above tests less noisy.
defaultWorkerTest :: WorkerTest DummyCtx DummyResource
defaultWorkerTest = WorkerTest
    { _workerBefore = \_ _ -> pure ()
    , _workerMain = \_ _ -> pure ()
    , _workerAcquire = \cb -> cb DummyResource
    , _workerConcurrently = \_ -> pure ()
    , _workerAssertion = \_ -> fail "defaultWorkerTest"
    , _workerTimeout = 250*1000 -- 250ms
    }

data WorkerResult
    = WorkerNotStarted
    | WorkerIsDone
    | WorkerWasInterrupted SomeException
    deriving (Show)

instance Eq WorkerResult where
    WorkerNotStarted == WorkerNotStarted = True
    WorkerIsDone == WorkerIsDone = True
    WorkerWasInterrupted e == WorkerWasInterrupted e' = show e == show e'
    _ == _ = False

workerTest
    :: WorkerTest DummyCtx DummyResource
    -> IO ()
workerTest (WorkerTest before main acquire concurrently assertion timeout) = do
    onExit <- newEmptyMVar
    wid <- generate arbitrary
    let ctx = DummyCtx nullTracer DummyResource
    let config = MkWorker
            { workerBefore = before
            , workerMain = main
            , workerAfter = \_ -> putMVar onExit
            , workerAcquire = acquire
            }
    registry <- empty
    register registry ctx wid config >>= \case
        Nothing -> assertion WorkerNotStarted
        Just worker -> do
            concurrently worker
            race (threadDelay timeout) (takeMVar onExit) >>= \case
                Right (Right ()) -> assertion WorkerIsDone
                Right (Left e) -> assertion (WorkerWasInterrupted e)
                Left _ -> fail "expected worker to stop but hasn't"

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary Delay where
    shrink (Delay n) =
        [ Delay n'
        | n' <- shrink n
        , n' > 10*1000 -- 10ms
        ]
    arbitrary = do
        Positive n <- arbitrary
        pure $ Delay (n * 1000) -- n ~ [1, 100], so n * 1000 ~ [1ms, 100ms]
