{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Tests for closing the connection of a 'SqliteContext'.
module Cardano.DB.SqliteSpec
    ( spec
    ) where

import Cardano.DB.Sqlite
    ( CloseBounds (..)
    , DBLog (..)
    , ErrConnectionClosed (..)
    , ForeignKeysSetting (..)
    , SqliteContext (..)
    , beginShutdownAfter
    , defaultCloseBounds
    , newInMemorySqliteContext
    , newShutdownDeadline
    , noAutoMigrations
    , noManualMigration
    , shutdownCloseBounds
    , withSqliteContextFileUsing
    )
import Control.Monad
    ( void
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( Tracer
    , mkTracer
    , nullTracer
    )
import Data.Int
    ( Int64
    )
import Database.Persist.Sql
    ( PersistValue (..)
    , Single (..)
    , SqlPersistT
    , rawExecute
    , rawSql
    )
import GHC.Clock
    ( getMonotonicTime
    )
import GHC.Conc
    ( BlockReason (..)
    , ThreadStatus (..)
    , threadStatus
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
    , shouldContain
    , shouldNotContain
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )
import UnliftIO.Async
    ( async
    , asyncThreadId
    , poll
    , race
    , wait
    )
import UnliftIO.Concurrent
    ( ThreadId
    , threadDelay
    )
import UnliftIO.Exception
    ( SomeException
    , displayException
    , fromException
    , throwIO
    , try
    )
import UnliftIO.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import UnliftIO.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )
import Prelude

spec :: Spec
spec = do
    describe "withSqliteContextFile" $ do
        it
            "waits for a running query before closing the connection"
            fileCloseWaitsForRunningQuery
        it
            "rejects a query made after the connection is closed"
            fileQueryAfterCloseIsRejected
        it
            "closes the connection after the bound for a query that does not finish"
            fileCloseGivesUpOnStuckQuery
        it
            "waits for one deadline shared by all the connections closed during shutdown"
            shutdownDeadlineIsShared
        it
            "waits for the single bound when the shutdown deadline has not begun"
            unbegunShutdownDeadlineUsesBound
    describe "newInMemorySqliteContext" $ do
        it
            "waits for a running query before closing the connection"
            memoryCloseWaitsForRunningQuery
        it
            "rejects a query made after the connection is closed"
            memoryQueryAfterCloseIsRejected

{-------------------------------------------------------------------------------
                                  Tests
-------------------------------------------------------------------------------}

-- | A query that is running in another thread when the scope of
-- 'withSqliteContextFile' ends completes and commits, a query waiting for
-- its turn is refused, and the scope returns only after the running query,
-- with the connection closed after that query and not left open at the
-- bound.
fileCloseWaitsForRunningQuery :: IO ()
fileCloseWaitsForRunningQuery =
    withSystemTempDirectory "sqlite-context" $ \dir -> do
        let fp = dir </> "test.sqlite"
        logs <- newLogs
        running <- newPausedQuery
        waiting <- newEmptyMVar
        scope <- async $ withContext (logTracer logs) fp $ \ctx -> do
            runQuery ctx createTable
            startPausedQuery running ctx
            -- The scope ends once the waiting query is blocked.
            putMVar waiting =<< startWaitingQuery ctx
        refused <- takeMVar waiting >>= awaitResult
        closedEarly <- poll scope
        resumePausedQuery running >>= expectSuccess
        expectRefused fp refused
        void closedEarly `shouldBe` Nothing
        within 2_000_000 "the scope" (wait scope)
            >>= either expectationFailure pure
        msgs <- contextLogs <$> readLogs logs
        msgs `shouldContain` [MsgCloseWaitStarted fp]
        msgs `shouldContain` [MsgCloseSingleConnection fp]
        filter isTimedOut msgs `shouldBe` []
        withContext nullTracer fp (`runQuery` readRows)
            `shouldReturn` Right [1, 2]

-- | A query on a context whose scope has ended is not run, and fails with
-- 'ErrConnectionClosed'.
fileQueryAfterCloseIsRejected :: IO ()
fileQueryAfterCloseIsRejected =
    withSystemTempDirectory "sqlite-context" $ \dir -> do
        let fp = dir </> "test.sqlite"
        ctx <-
            withContext nullTracer fp (\ctx -> ctx <$ runQuery ctx createTable)
                >>= either fail pure
        runQuery ctx (insertRow 1)
            `shouldThrow` (== ErrConnectionClosed fp)

-- | When a query is still running at the bound, the scope returns without
-- closing the connection under it, traces 'MsgCloseWaitTimedOut', and
-- refuses later queries. The query finishes on its connection, which is
-- then closed by a background thread.
fileCloseGivesUpOnStuckQuery :: IO ()
fileCloseGivesUpOnStuckQuery =
    withSystemTempDirectory "sqlite-context" $ \dir -> do
        let fp = dir </> "test.sqlite"
            bound = 0.3
            bounds = defaultCloseBounds{closeBound = realToFrac bound}
        logs <- newLogs
        stuck <- newPausedQuery
        start <- getMonotonicTime
        scope <-
            within 5_000_000 "the scope"
                $ withContextUsing bounds (logTracer logs) fp
                $ \ctx -> do
                    runQuery ctx createTable
                    startPausedQuery stuck ctx
                    pure ctx
        elapsed <- subtract start <$> getMonotonicTime
        ctx <- either fail pure scope
        elapsed `shouldSatisfy` (>= bound)
        msgs <- contextLogs <$> readLogs logs
        msgs `shouldContain` [MsgCloseWaitStarted fp]
        filter isTimedOut msgs `shouldSatisfy` ((== 1) . length)
        msgs `shouldNotContain` [MsgCloseSingleConnection fp]
        runQuery ctx (insertRow 3)
            `shouldThrow` (== ErrConnectionClosed fp)
        resumePausedQuery stuck >>= expectSuccess
        eventually
            $ readLogs logs
                >>= \seen ->
                    contextLogs seen `shouldContain` [MsgCloseSingleConnection fp]

-- | The connections of all the contexts that close during shutdown wait for
-- one shared deadline, not each for their own bound or for a bound that
-- starts again at each close. The deadline is armed once, before the first
-- close; arming it again later does not move it. The first connection waits
-- until the deadline. The second starts closing after the deadline, so it
-- gives up after a single polling step.
shutdownDeadlineIsShared :: IO ()
shutdownDeadlineIsShared =
    withSystemTempDirectory "sqlite-context" $ \dir -> do
        let fpA = dir </> "a.sqlite"
            fpB = dir </> "b.sqlite"
            limit = 0.5
        deadline <- newShutdownDeadline
        let bounds =
                (shutdownCloseBounds deadline)
                    { closeBound = 10
                    }
        logs <- newLogs
        stuckA <- newPausedQuery
        stuckB <- newPausedQuery
        start <- getMonotonicTime
        result <-
            within 8_000_000 "the scopes"
                $ withContextUsing bounds (logTracer logs) fpA
                $ \ctxA -> do
                    runQuery ctxA createTable
                    startPausedQuery stuckA ctxA
                    inner <-
                        withContextUsing bounds (logTracer logs) fpB $ \ctxB -> do
                            runQuery ctxB createTable
                            startPausedQuery stuckB ctxB
                            beginShutdownAfter limit deadline
                    -- A later call does not move the deadline.
                    beginShutdownAfter 30 deadline
                    pure inner
        elapsed <- subtract start <$> getMonotonicTime
        either fail (either fail pure) result
        elapsed `shouldSatisfy` (>= realToFrac limit)
        elapsed `shouldSatisfy` (< 5)
        msgs <- readLogs logs
        let timedOut = [(fp, waited) | MsgCloseWaitTimedOut fp waited <- msgs]
        map fst timedOut `shouldBe` [fpB, fpA]
        case timedOut of
            [(_, waitedB), (_, waitedA)] -> do
                waitedB `shouldSatisfy` (>= realToFrac limit / 2)
                waitedA `shouldSatisfy` (< realToFrac limit / 2)
            _ -> expectationFailure "expected two timed out closes"
        resumePausedQuery stuckA >>= expectSuccess
        resumePausedQuery stuckB >>= expectSuccess

-- | A shutdown deadline that has not begun does not change how long a close
-- waits: it waits for 'closeBound', as when a worker stops by itself.
unbegunShutdownDeadlineUsesBound :: IO ()
unbegunShutdownDeadlineUsesBound =
    withSystemTempDirectory "sqlite-context" $ \dir -> do
        let fp = dir </> "test.sqlite"
            bound = 0.3
        deadline <- newShutdownDeadline
        let bounds =
                (shutdownCloseBounds deadline)
                    { closeBound = realToFrac bound
                    }
        logs <- newLogs
        stuck <- newPausedQuery
        start <- getMonotonicTime
        result <-
            within 5_000_000 "the scope"
                $ withContextUsing bounds (logTracer logs) fp
                $ \ctx -> do
                    runQuery ctx createTable
                    startPausedQuery stuck ctx
        elapsed <- subtract start <$> getMonotonicTime
        either fail pure result
        elapsed `shouldSatisfy` (>= bound)
        msgs <- contextLogs <$> readLogs logs
        filter isTimedOut msgs `shouldSatisfy` ((== 1) . length)
        resumePausedQuery stuck >>= expectSuccess

-- | The close action of 'newInMemorySqliteContext' waits for a running
-- query, and refuses a query waiting for its turn.
memoryCloseWaitsForRunningQuery :: IO ()
memoryCloseWaitsForRunningQuery = do
    logs <- newLogs
    (close, ctx) <- newMemoryContext (logTracer logs)
    running <- newPausedQuery
    startPausedQuery running ctx
    waiting <- startWaitingQuery ctx
    closing <- async close
    refused <- awaitResult waiting
    closedEarly <- poll closing
    resumePausedQuery running >>= expectSuccess
    expectRefused ":memory:" refused
    void closedEarly `shouldBe` Nothing
    within 2_000_000 "the close" (wait closing)
    msgs <- readLogs logs
    msgs `shouldContain` [MsgCloseWaitStarted ":memory:"]
    filter isTimedOut msgs `shouldBe` []

-- | After the close action of 'newInMemorySqliteContext', a query fails
-- with 'ErrConnectionClosed', and running the close action again does
-- nothing.
memoryQueryAfterCloseIsRejected :: IO ()
memoryQueryAfterCloseIsRejected = do
    (close, ctx) <- newMemoryContext nullTracer
    runQuery ctx (insertRow 1)
    close
    close
    runQuery ctx readRows
        `shouldThrow` (== ErrConnectionClosed ":memory:")

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

-- | A query that pauses inside its transaction, between two inserts.
data PausedQuery = PausedQuery
    { pausedInside :: MVar ()
    , pausedResume :: MVar ()
    , pausedResult :: MVar (Either SomeException ())
    }

newPausedQuery :: IO PausedQuery
newPausedQuery =
    PausedQuery <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar

-- | Start the query in another thread, and return once it has run its first
-- insert and is paused.
startPausedQuery :: PausedQuery -> SqliteContext -> IO ()
startPausedQuery PausedQuery{pausedInside, pausedResume, pausedResult} ctx =
    do
        void $ async $ do
            result <- try $ runQuery ctx $ do
                insertRow 1
                liftIO $ putMVar pausedInside () >> takeMVar pausedResume
                insertRow 2
            putMVar pausedResult result
        takeMVar pausedInside

-- | Let the query run its second insert and commit, and return its result.
resumePausedQuery :: PausedQuery -> IO (Either SomeException ())
resumePausedQuery PausedQuery{pausedResume, pausedResult} = do
    putMVar pausedResume ()
    takeMVar pausedResult

-- | Start a query in another thread, and return once it is blocked waiting
-- for its turn.
startWaitingQuery
    :: SqliteContext -> IO (MVar (Either SomeException ()))
startWaitingQuery ctx = do
    result <- newEmptyMVar
    query <- async $ try (runQuery ctx (pure ())) >>= putMVar result
    waitUntilBlocked (asyncThreadId query)
    pure result

waitUntilBlocked :: ThreadId -> IO ()
waitUntilBlocked tid =
    threadStatus tid >>= \case
        ThreadBlocked BlockedOnSTM -> pure ()
        ThreadFinished -> fail "the waiting query finished instead of waiting"
        ThreadDied -> fail "the waiting query died instead of waiting"
        _ -> threadDelay 1_000 >> waitUntilBlocked tid

-- | The result of a waiting query, or 'Nothing' if it has none after five
-- seconds.
awaitResult
    :: MVar (Either SomeException ())
    -> IO (Maybe (Either SomeException ()))
awaitResult result =
    either (const Nothing) Just
        <$> race (threadDelay 5_000_000) (takeMVar result)

expectSuccess :: Either SomeException () -> IO ()
expectSuccess = \case
    Left e ->
        expectationFailure
            $ "the running query failed: " <> displayException e
    Right () -> pure ()

expectRefused :: FilePath -> Maybe (Either SomeException ()) -> IO ()
expectRefused fp = \case
    Nothing ->
        expectationFailure "a waiting query was not refused when closing"
    Just (Left e) -> fromException e `shouldBe` Just (ErrConnectionClosed fp)
    Just (Right ()) ->
        expectationFailure "a waiting query ran while closing"

withContext
    :: Tracer IO DBLog
    -> FilePath
    -> (SqliteContext -> IO a)
    -> IO (Either String a)
withContext = withContextUsing defaultCloseBounds

withContextUsing
    :: CloseBounds
    -> Tracer IO DBLog
    -> FilePath
    -> (SqliteContext -> IO a)
    -> IO (Either String a)
withContextUsing bounds tr fp action =
    either (Left . show) Right
        <$> withSqliteContextFileUsing
            bounds
            tr
            fp
            noManualMigration
            noAutoMigrations
            action

newMemoryContext :: Tracer IO DBLog -> IO (IO (), SqliteContext)
newMemoryContext tr = do
    (close, ctx) <-
        newInMemorySqliteContext
            tr
            noManualMigration
            noAutoMigrations
            ForeignKeysEnabled
    runQuery ctx createTable
    pure (close, ctx)

createTable :: SqlPersistT IO ()
createTable = rawExecute "CREATE TABLE t (x INTEGER NOT NULL)" []

insertRow :: Int64 -> SqlPersistT IO ()
insertRow x = rawExecute "INSERT INTO t (x) VALUES (?)" [PersistInt64 x]

readRows :: SqlPersistT IO [Int64]
readRows = fmap unSingle <$> rawSql "SELECT x FROM t ORDER BY x" []

newtype Logs = Logs (IORef [DBLog])

newLogs :: IO Logs
newLogs = Logs <$> newIORef []

logTracer :: Logs -> Tracer IO DBLog
logTracer (Logs ref) =
    mkTracer $ \msg -> atomicModifyIORef' ref $ \xs -> (msg : xs, ())

readLogs :: Logs -> IO [DBLog]
readLogs (Logs ref) = reverse <$> readIORef ref

-- | The messages from the last time a connection was opened, which is the
-- connection of the context itself, after the connections of the migrations.
contextLogs :: [DBLog] -> [DBLog]
contextLogs = reverse . takeWhile (not . isOpen) . reverse
  where
    isOpen = \case
        MsgOpenSingleConnection{} -> True
        _ -> False

isTimedOut :: DBLog -> Bool
isTimedOut = \case
    MsgCloseWaitTimedOut{} -> True
    _ -> False

-- | Run an action, and fail if it takes longer than the given number of
-- microseconds.
within :: Int -> String -> IO a -> IO a
within us what action =
    race (threadDelay us) action >>= \case
        Left () -> fail $ what <> " did not return in time"
        Right a -> pure a

-- | Retry an assertion for up to two seconds.
eventually :: IO () -> IO ()
eventually assertion = go (200 :: Int)
  where
    go n =
        try assertion >>= \case
            Right () -> pure ()
            Left (e :: SomeException)
                | n <= 0 -> throwIO e
                | otherwise -> threadDelay 10_000 >> go (n - 1)
