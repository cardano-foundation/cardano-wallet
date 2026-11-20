{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.RegistrySpec
    ( spec
    ) where

import Cardano.DB.Sqlite
    ( ErrConnectionClosed (..)
    , SqliteContext (..)
    , noAutoMigrations
    , noManualMigration
    , withSqliteContextFile
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..)
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , Worker
    , WorkerLog
    , WorkerRegistry
    , drain
    , empty
    , lookup
    , register
    , unregister
    , workerId
    , workerResource
    , workerThread
    )
import Cardano.Wallet.Tracing.Trace
    ( nullTracer
    )
import Control.Exception.Base
    ( AsyncException (..)
    , asyncExceptionFromException
    )
import Control.Monad
    ( forever
    , replicateM
    , void
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( Tracer
    )
import Cryptography.Hash.Core
    ( hash
    )
import Data.Int
    ( Int64
    )
import Data.Maybe
    ( isNothing
    )
import Data.Text
    ( Text
    )
import Database.Persist.Sql
    ( PersistValue (..)
    , Single (..)
    , SqlPersistT
    , rawExecute
    , rawSql
    )
import GHC.Conc
    ( BlockReason (..)
    , ThreadStatus (..)
    , threadStatus
    )
import GHC.Generics
    ( Generic
    )
import System.FilePath
    ( (</>)
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
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
    ( async
    , asyncThreadId
    , race
    , wait
    )
import UnliftIO.Concurrent
    ( ThreadId
    , threadDelay
    , throwTo
    )
import UnliftIO.Exception
    ( SomeException (..)
    , bracket
    , displayException
    , finally
    , fromException
    , throwIO
    , try
    , uninterruptibleMask_
    )
import UnliftIO.MVar
    ( MVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , swapMVar
    , takeMVar
    , tryPutMVar
    , tryReadMVar
    , tryTakeMVar
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )
import Prelude hiding
    ( lookup
    )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Workers" $ do
        it
            "Executes task and stops gracefully when done"
            workerRunsTaskAndExits
        it
            "Stops gracefully when receiving an async exception"
            workerFailsGracefullyAsyncE
        it
            "Stops gracefully when 'main' fails"
            workerFailsGracefullyMain
        it
            "Stops gracefully when 'before' fails"
            workerFailsGracefullyBefore
        it
            "Stops gracefully when 'acquire' fails"
            workerFailsGracefullyAcquire
        it
            "Does not return until 'before' is completed"
            (property workerIsUsableAfterBefore)
        it
            "Keeps its database open for a running query when 'main' fails"
            (runningQueryOutlivesWorker workerDies)
        it
            "Keeps its database open for a running query when unregistered"
            (runningQueryOutlivesWorker workerIsUnregistered)
    describe "drain" $ do
        it
            "is idempotent and leaves the registry empty"
            drainIsIdempotentAndEmpty
        it
            "waits for resource release and worker finalizers"
            drainWaitsForReleaseAndFinalizers
        it
            "races with worker self-exit without leftover entries"
            drainRacesWithSelfExit
        it
            "unregister terminates only the selected worker"
            unregisterDoesNotTerminateAnotherWorker

{-------------------------------------------------------------------------------
                                  Tests
-------------------------------------------------------------------------------}

-- | Workers are given a 'workerTask', and ends as soon as their task is over.
workerRunsTaskAndExits
    :: IO ()
workerRunsTaskAndExits = do
    mvar <- newMVar (42 :: Int)
    workerTest
        $ defaultWorkerTest
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
    workerTest
        $ defaultWorkerTest
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
    workerTest
        $ defaultWorkerTest
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
    workerTest
        $ defaultWorkerTest
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
    workerTest
        $ defaultWorkerTest
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
    run
        $ workerTest
        $ defaultWorkerTest
            { _workerBefore = \_ _ -> do
                threadDelay delay
                modifyMVar_ actions (\xs -> return $ "before" : xs)
            , _workerMain = \_ _ ->
                takeMVar done
            , _workerConcurrently = \_ -> do
                modifyMVar_ actions (\xs -> return $ "concurrently" : xs)
                putMVar done ()
            , _workerAssertion = \res -> do
                res `shouldBe` WorkerIsDone
                tryTakeMVar actions `shouldReturn` Just ["concurrently", "before"]
            , _workerTimeout =
                100 * delay
            }

-- | Drain of an empty registry is harmless, and draining workers leaves
-- no membership.
drainIsIdempotentAndEmpty :: IO ()
drainIsIdempotentAndEmpty = do
    registry <- empty
    let ctx = DummyCtx nullTracer DummyResource
    drain registry
    drain registry
    wids <- replicateM 3 (generate arbitrary)
    released <- mapM (const newEmptyMVar) wids
    finalized <- mapM (const newEmptyMVar) wids
    mapM_
        (registerDelayed registry ctx (200 * 1000))
        (zip3 wids released finalized)
    drain registry
    drain registry
    mapM_ (assertAbsent registry) wids
    mapM_ assertAlreadyFilled released
    mapM_ assertAlreadyFilled finalized

-- | Drain must not return until every delayed release and worker-after
-- action has completed. A drain that only kills threads fails here.
drainWaitsForReleaseAndFinalizers :: IO ()
drainWaitsForReleaseAndFinalizers = do
    registry <- empty
    let ctx = DummyCtx nullTracer DummyResource
    wids <- replicateM 3 (generate arbitrary)
    released <- mapM (const newEmptyMVar) wids
    finalized <- mapM (const newEmptyMVar) wids
    mapM_
        (registerDelayed registry ctx (200 * 1000))
        (zip3 wids released finalized)
    drain registry
    mapM_ assertAlreadyFilled released
    mapM_ assertAlreadyFilled finalized
    mapM_ (assertAbsent registry) wids

-- | Worker self-exit must win against drain's stale snapshot: drain has
-- already listed the worker, the worker then deletes itself, and drain
-- still calls unregister. A membership-error on that Nothing branch
-- must go red; repetition without this barrier is not the race.
drainRacesWithSelfExit :: IO ()
drainRacesWithSelfExit = go 20
  where
    go :: Int -> IO ()
    go 0 =
        fail "could not sample blockerId < targetId"
    go n = do
        blockerId <- generate arbitrary
        targetId <- generate arbitrary
        if blockerId < targetId
            then drainRacesWithSelfExitOrdered blockerId targetId
            else go (n - 1)

drainRacesWithSelfExitOrdered
    :: WalletId
    -> WalletId
    -> IO ()
drainRacesWithSelfExitOrdered blockerId targetId = do
    registry <- empty
    let ctx = DummyCtx nullTracer DummyResource
    releaseBlocker <- newEmptyMVar
    allowTargetExit <- newEmptyMVar
    blockerHeld <- newEmptyMVar
    targetReady <- newEmptyMVar
    blockerReleased <- newEmptyMVar
    targetReleased <- newEmptyMVar
    blockerFinalized <- newEmptyMVar
    targetFinalized <- newEmptyMVar
    void
        $ mustRegister
            registry
            ctx
            blockerId
            (delayedAcquire blockerReleased 0)
            ( \_ _ ->
                uninterruptibleMask_ $ do
                    putMVar blockerHeld ()
                    takeMVar releaseBlocker
            )
            (\_ _ -> putMVar blockerFinalized ())
    void
        $ mustRegister
            registry
            ctx
            targetId
            (delayedAcquire targetReleased 0)
            ( \_ _ -> do
                putMVar targetReady ()
                takeMVar allowTargetExit
            )
            (\_ _ -> putMVar targetFinalized ())
    takeMVar blockerHeld
    takeMVar targetReady
    let releaseWorkers = do
            void $ tryPutMVar allowTargetExit ()
            void $ tryPutMVar releaseBlocker ()
    drainA <- async $ drain registry
    let body = do
            waitUntilAbsent
                registry
                blockerId
                "blocker after drain began unregister"
            putMVar allowTargetExit ()
            assertFilled
                "target self-exit finalizer"
                targetFinalized
            assertAbsent registry targetId
            putMVar releaseBlocker ()
            race (threadDelay (2 * 1000 * 1000)) (wait drainA) >>= \case
                Left _ ->
                    fail "drain timed out after caused self-exit"
                Right () -> pure ()
            assertAbsent registry blockerId
            assertAbsent registry targetId
            assertFilled "blocker resource release" blockerReleased
            assertFilled "target resource release" targetReleased
    body
        `finally` ( do
                        releaseWorkers
                        void
                            $ race
                                (threadDelay (500 * 1000))
                                (wait drainA)
                  )

-- | Selected unregister must not terminate a different worker.
unregisterDoesNotTerminateAnotherWorker :: IO ()
unregisterDoesNotTerminateAnotherWorker = do
    registry <- empty
    let ctx = DummyCtx nullTracer DummyResource
    widKeep <- generate arbitrary
    widDrop <- generate arbitrary
    keepReleased <- newEmptyMVar
    dropReleased <- newEmptyMVar
    keepFinalized <- newEmptyMVar
    dropFinalized <- newEmptyMVar
    keep <-
        mustRegister
            registry
            ctx
            widKeep
            (delayedAcquire keepReleased 0)
            (\_ _ -> forever $ threadDelay maxBound)
            (\_ _ -> putMVar keepFinalized ())
    void
        $ mustRegister
            registry
            ctx
            widDrop
            (delayedAcquire dropReleased 0)
            (\_ _ -> forever $ threadDelay maxBound)
            (\_ _ -> putMVar dropFinalized ())
    unregister registry widDrop
    remaining <- lookup registry widKeep
    fmap workerId remaining `shouldBe` Just (workerId keep)
    fmap workerThread remaining
        `shouldBe` Just (workerThread keep)
    drain registry
    assertAbsent registry widKeep
    assertAbsent registry widDrop

-- | Stop a worker started by 'registerDbWorker' by letting its 'main' throw.
workerDies
    :: MVar ()
    -> WorkerRegistry WalletId SqliteContext
    -> WalletId
    -> IO ()
workerDies die _ _ = putMVar die ()

-- | Stop a worker the way 'drain' does.
workerIsUnregistered
    :: MVar ()
    -> WorkerRegistry WalletId SqliteContext
    -> WalletId
    -> IO ()
workerIsUnregistered _ = unregister

-- | A worker whose resource is a file-backed 'SqliteContext' stops while a
-- request is running a query on that database and another query is waiting
-- for its turn. The waiting query is refused, the worker closes the
-- database only after the running query has committed, the request's next
-- query fails with 'ErrConnectionClosed', the worker leaves the registry,
-- and the database of another worker keeps answering.
runningQueryOutlivesWorker
    :: ( MVar ()
         -> WorkerRegistry WalletId SqliteContext
         -> WalletId
         -> IO ()
       )
    -- ^ Stops the worker, given the 'MVar' that its 'main' waits on.
    -> IO ()
runningQueryOutlivesWorker stop =
    withSystemTempDirectory "registry" $ \dir -> do
        registry <- empty
        widStop <- generate arbitrary
        widKeep <- generate arbitrary
        dieStop <- newEmptyMVar
        dieKeep <- newEmptyMVar
        releasedStop <- newEmptyMVar
        releasedKeep <- newEmptyMVar
        let dbStop = dir </> "stop.sqlite"
        stopped <-
            registerDbWorker registry dbStop widStop dieStop releasedStop
        kept <-
            registerDbWorker
                registry
                (dir </> "keep.sqlite")
                widKeep
                dieKeep
                releasedKeep
        inQuery <- newEmptyMVar
        resume <- newEmptyMVar
        let db = workerResource stopped
        request <- async $ do
            first <- try $ runQuery db $ do
                insertRow 1
                liftIO $ putMVar inQuery () >> takeMVar resume
                insertRow 2
            second <- try $ runQuery db $ insertRow 3
            pure (first, second)
        let body = do
                takeMVar inQuery
                waiting <- startWaitingQuery db
                stop dieStop registry widStop
                -- The waiting query is refused once the worker has started
                -- to close the database, so the request's second query
                -- starts after that.
                refused <-
                    race (threadDelay 5_000_000) (takeMVar waiting)
                releasedEarly <- tryReadMVar releasedStop
                putMVar resume ()
                (first, second) <- wait request
                case first of
                    Left (e :: SomeException) ->
                        expectationFailure
                            $ "the running query failed: "
                                <> displayException e
                    Right () -> pure ()
                case releasedEarly of
                    Nothing -> pure ()
                    Just () ->
                        expectationFailure
                            "the database was closed under the running query"
                case refused of
                    Left () ->
                        expectationFailure
                            "a waiting query was not refused when closing"
                    Right (Left (e :: SomeException)) ->
                        fromException e
                            `shouldBe` Just (ErrConnectionClosed dbStop)
                    Right (Right ()) ->
                        expectationFailure "a waiting query ran while closing"
                case second of
                    Left (e :: SomeException) ->
                        fromException e
                            `shouldBe` Just (ErrConnectionClosed dbStop)
                    Right () ->
                        expectationFailure
                            "a query ran after the database was closed"
                assertFilled "database release" releasedStop
                waitUntilAbsent registry widStop "stopped worker"
                runQuery (workerResource kept) (insertRow 7 >> readRows)
                    `shouldReturn` [7]
        body `finally` (tryPutMVar resume () >> drain registry)

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

data DummyCtx
    = DummyCtx
        (Tracer IO (WorkerLog WalletId Text))
        DummyResource
    deriving (Generic)

data DummyResource = DummyResource deriving (Generic)

instance HasWorkerCtx DummyResource DummyCtx where
    type WorkerCtx DummyCtx = DummyCtx
    type WorkerMsg DummyCtx = Text
    type WorkerKey DummyCtx = WalletId
    hoistResource _ _ = id

data DbCtx
    = DbCtx
        (Tracer IO (WorkerLog WalletId Text))
        SqliteContext
    deriving (Generic)

instance HasWorkerCtx SqliteContext DbCtx where
    type WorkerCtx DbCtx = DbCtx
    type WorkerMsg DbCtx = Text
    type WorkerKey DbCtx = WalletId
    hoistResource db _ (DbCtx tr _) = DbCtx tr db

-- | Register a worker whose resource is a 'SqliteContext' on a new file
-- holding a table @t@. Its 'main' throws once @die@ is filled, and
-- @released@ is filled after the database is closed.
registerDbWorker
    :: WorkerRegistry WalletId SqliteContext
    -> FilePath
    -> WalletId
    -> MVar ()
    -> MVar ()
    -> IO (Worker WalletId SqliteContext)
registerDbWorker registry fp wid die released =
    register registry ctx wid config >>= \case
        Nothing -> fail "expected worker to start"
        Just worker -> pure worker
  where
    ctx = DbCtx nullTracer noDatabase
    noDatabase = SqliteContext $ \_ -> throwIO $ userError "no database"
    config =
        MkWorker
            { workerBefore = \_ _ -> pure ()
            , workerMain = \_ _ -> do
                readMVar die
                throwIO $ userError "worker died"
            , workerAfter = \_ _ -> pure ()
            , workerAcquire = \use ->
                ( withSqliteContextFile
                    nullTracer
                    fp
                    noManualMigration
                    noAutoMigrations
                    (\db -> runQuery db createTable >> use db)
                    >>= either (throwIO . userError . show) pure
                )
                    `finally` void (tryPutMVar released ())
            }

createTable :: SqlPersistT IO ()
createTable = rawExecute "CREATE TABLE t (x INTEGER NOT NULL)" []

insertRow :: Int64 -> SqlPersistT IO ()
insertRow x = rawExecute "INSERT INTO t (x) VALUES (?)" [PersistInt64 x]

readRows :: SqlPersistT IO [Int64]
readRows = fmap unSingle <$> rawSql "SELECT x FROM t ORDER BY x" []

-- | Start a query in another thread, and return once it is blocked waiting
-- for its turn on the database.
startWaitingQuery
    :: SqliteContext -> IO (MVar (Either SomeException ()))
startWaitingQuery db = do
    result <- newEmptyMVar
    query <- async $ try (runQuery db (pure ())) >>= putMVar result
    waitUntilBlocked (asyncThreadId query)
    pure result

waitUntilBlocked :: ThreadId -> IO ()
waitUntilBlocked tid =
    threadStatus tid >>= \case
        ThreadBlocked BlockedOnSTM -> pure ()
        ThreadFinished -> fail "the waiting query finished instead of waiting"
        ThreadDied -> fail "the waiting query died instead of waiting"
        _ -> threadDelay 1_000 >> waitUntilBlocked tid

-- A reasonably 'long' delay to test asynchronous race conditions, in us
newtype Delay = Delay Int deriving (Show)

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
defaultWorkerTest =
    WorkerTest
        { _workerBefore = \_ _ -> pure ()
        , _workerMain = \_ _ -> pure ()
        , _workerAcquire = \cb -> cb DummyResource
        , _workerConcurrently = \_ -> pure ()
        , _workerAssertion = \_ -> fail "defaultWorkerTest"
        , _workerTimeout = 250 * 1000 -- 250ms
        }

delayedAcquire
    :: MVar ()
    -> Int
    -> (DummyResource -> IO ())
    -> IO ()
delayedAcquire released delayUs =
    bracket
        (pure DummyResource)
        (\_ -> threadDelay delayUs >> putMVar released ())

mustRegister
    :: WorkerRegistry WalletId DummyResource
    -> DummyCtx
    -> WalletId
    -> ((DummyResource -> IO ()) -> IO ())
    -> (DummyCtx -> WalletId -> IO ())
    -> ( Tracer IO (WorkerLog WalletId Text)
         -> Either SomeException ()
         -> IO ()
       )
    -> IO (Worker WalletId DummyResource)
mustRegister registry ctx wid acquire mainFn afterFn =
    register registry ctx wid config >>= \case
        Nothing -> fail "expected worker to start"
        Just worker -> pure worker
  where
    config =
        MkWorker
            { workerBefore = \_ _ -> pure ()
            , workerMain = mainFn
            , workerAfter = afterFn
            , workerAcquire = acquire
            }

registerDelayed
    :: WorkerRegistry WalletId DummyResource
    -> DummyCtx
    -> Int
    -> (WalletId, MVar (), MVar ())
    -> IO (Worker WalletId DummyResource)
registerDelayed registry ctx delayUs (wid, released, finalized) =
    mustRegister
        registry
        ctx
        wid
        (delayedAcquire released delayUs)
        (\_ _ -> forever $ threadDelay maxBound)
        (\_ _ -> threadDelay delayUs >> putMVar finalized ())

assertFilled :: String -> MVar () -> IO ()
assertFilled label mvar =
    race (threadDelay (2 * 1000 * 1000)) (takeMVar mvar) >>= \case
        Left _ -> fail $ "timed out waiting for " <> label
        Right _ -> pure ()

-- | Must already be full when drain returns. A post-drain wait would
-- hide a drain that only signals workers.
assertAlreadyFilled :: MVar () -> IO ()
assertAlreadyFilled mvar =
    tryTakeMVar mvar `shouldReturn` Just ()

assertAbsent
    :: WorkerRegistry WalletId DummyResource
    -> WalletId
    -> IO ()
assertAbsent registry wid = do
    found <- lookup registry wid
    isNothing found `shouldBe` True

waitUntilAbsent
    :: WorkerRegistry WalletId resource
    -> WalletId
    -> String
    -> IO ()
waitUntilAbsent registry wid label =
    race (threadDelay (2 * 1000 * 1000)) go >>= \case
        Left _ ->
            fail $ "timed out waiting for " <> label
        Right () -> pure ()
  where
    go = do
        found <- lookup registry wid
        if isNothing found
            then pure ()
            else threadDelay 1000 >> go

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
    let config =
            MkWorker
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
        , n' > 10 * 1000 -- 10ms
        ]
    arbitrary = do
        Positive n <- arbitrary
        pure $ Delay (n * 1000) -- n ~ [1, 100], so n * 1000 ~ [1ms, 100ms]
