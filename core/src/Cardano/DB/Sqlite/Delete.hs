{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- A function to wait until a suitable time to delete a SQLite database file,
-- and a function to delete a SQLite database file, which isn't as
-- straightforward as it sounds.

module Cardano.DB.Sqlite.Delete
    ( -- * Removing files with retry
      deleteSqliteDatabase
    , deleteSqliteDatabase'
    , deleteSqliteDatabaseRetryPolicy
    , DeleteSqliteDatabaseLog (..)
    -- * Ref-counting open databases
    , RefCount
    , newRefCount
    , withRef
    , waitForFree
    , waitForFree'
    , waitForFreeRetryPolicy
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, modifyMVar_, newMVar, readMVar )
import Control.Exception
    ( bracket_ )
import Control.Retry
    ( RetryPolicy
    , RetryStatus (..)
    , capDelay
    , fibonacciBackoff
    , limitRetries
    , limitRetriesByCumulativeDelay
    , retryPolicy
    , retrying
    )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )
import System.Directory
    ( removePathForcibly )

#if defined(mingw32_HOST_OS)
import Control.Retry
    ( logRetries, recovering )
import System.IO.Error
    ( isPermissionError )
#else
#endif

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                           Removing files with retry
-------------------------------------------------------------------------------}

-- | Remove a SQLite database file.
--
-- If <https://www.sqlite.org/tempfiles.html SQLite temporary files> are present
-- (@-wal@ and @-shm@), we remove them as well. Normally, they would be removed
-- when the SQLite connection is closed. But we attempt to remove them anyway,
-- in case cardano-wallet was unable to close the SQLite connection.
--
-- Additionally, on Windows, the deletion operations will be retried for a short
-- time if they fail.  The reason for this is that a FileDelete command just
-- marks a file for deletion. The file is really only removed when the last
-- handle to the file is closed. Unfortunately there are a lot of system
-- services that can have a file temporarily opened using a shared read-only
-- lock, such as the built in AV and search indexer.
--
-- We can't really guarantee that these are all off, so what we can do is
-- whenever after an rm the file still exists to try again and wait a bit.
--
-- See <https://github.com/haskell/directory/issues/96> for more information
-- about this issue.
deleteSqliteDatabase :: Tracer IO DeleteSqliteDatabaseLog -> FilePath -> IO ()
deleteSqliteDatabase tr =
    deleteSqliteDatabase' tr deleteSqliteDatabaseRetryPolicy

-- | A variant of 'deleteSqliteDatabase' where the caller can specify the
-- 'RetryPolicy'.
deleteSqliteDatabase'
    :: Tracer IO DeleteSqliteDatabaseLog
    -> RetryPolicy
    -> FilePath
    -> IO ()
deleteSqliteDatabase' tr pol db = mapM_ delete files
  where
    files = [ db, db <> "-wal", db <> "-shm" ]
    delete = handleErrors tr pol . removePathForcibly

handleErrors
    :: Tracer IO DeleteSqliteDatabaseLog
    -> RetryPolicy
    -> IO ()
    -> IO ()
#if defined(mingw32_HOST_OS)
handleErrors tr policy = recovering policy [check] . const
  where
    check = logRetries (pure . isPermissionError) logRetry
    logRetry True _ st = traceWith tr $ MsgRetryDelete $ rsIterNumber st
    logRetry False e _ = traceWith tr $ MsgGaveUpDelete $ show e
#else
handleErrors _ _ = id
#endif

-- | Retry policy where delay increases linearly from base with each retry.
-- (as <https://www.sqlite.org/src/info/89f1848d7f implemented by SQLite>)
linearBackoff
    :: Int
    -- ^ Base delay in microseconds
    -> RetryPolicy
linearBackoff base = retryPolicy $ \ RetryStatus { rsIterNumber = n } ->
  Just $! base * n

-- | Recommended retry policy for 'deleteSqliteDatabase'.
deleteSqliteDatabaseRetryPolicy :: RetryPolicy
deleteSqliteDatabaseRetryPolicy = linearBackoff 25000 <> limitRetries 10

-- | Log messages that may arise from 'deleteSqliteDatabase'.
data DeleteSqliteDatabaseLog
    = MsgRetryDelete Int
    | MsgGaveUpDelete String
    deriving (Generic, Show, Eq, ToJSON)

instance ToText DeleteSqliteDatabaseLog where
    toText msg = case msg of
        MsgRetryDelete retryNum ->
            "retry " <> T.pack (show retryNum) <> " for lock/sharing violation - probably due to antivirus software"
        MsgGaveUpDelete e ->
            "gave up on delete due to " <> T.pack e

instance HasPrivacyAnnotation DeleteSqliteDatabaseLog
instance HasSeverityAnnotation DeleteSqliteDatabaseLog where
    getSeverityAnnotation msg = case msg of
        MsgRetryDelete _ -> Warning
        MsgGaveUpDelete _ -> Error

{-------------------------------------------------------------------------------
                          Ref-counting open databases
-------------------------------------------------------------------------------}

-- | Mutable variable containing reference counts to IDs of type @ix@.
data RefCount ix = RefCount
    { _refCount :: MVar (Map ix Int) -- ^ number of references to each index
    , _takeLock :: MVar () -- ^ lock on incrementing references
    }

-- | Construct a 'RefCount' with zero references.
newRefCount :: Ord ix => IO (RefCount ix)
newRefCount = RefCount <$> newMVar mempty <*> newMVar ()

-- | Acquire a reference to the given identifier, perform the given action, then
-- release the reference. Multiple 'withRef' calls can take references at the
-- same time.
withRef :: Ord ix => RefCount ix -> ix -> IO a -> IO a
withRef (RefCount mvar lock) ix =
    bracket_ (modifyMVar_ lock $ const $ modify inc) (modify dec)
  where
    modify f = modifyMVar_ mvar (pure . f)
    inc = Map.insertWith (+) ix 1
    dec = Map.update (\n -> if n > 1 then Just (n - 1) else Nothing) ix

-- | Attempt to wait until all 'withRef' calls for the given identifier have
-- completed, then perform an action.
--
-- This will block for up to 2 minutes before running the action. The action is
-- passed the reference count, which should be @0@ under normal conditions.
--
-- No new references can be taken using 'withRef' while the action is running.
waitForFree
    :: Ord ix
    => Tracer IO (Maybe Int)
    -- ^ Logging of current number of references
    -> RefCount ix
    -- ^ Mutable variable containing reference counts
    -> ix
    -- ^ Identifier
    -> (Int -> IO a)
    -- ^ Action to run, passed number of references in use
    -> IO a
waitForFree tr = waitForFree' tr waitForFreeRetryPolicy

-- | A variant of 'waitForFree' where the caller can specify the 'RetryPolicy'.
waitForFree'
    :: Ord ix
    => Tracer IO (Maybe Int)
    -- ^ Logging of current number of references
    -> RetryPolicy
    -- ^ How and when to poll the 'RefCount'
    -> RefCount ix
    -- ^ Mutable variable containing reference counts
    -> ix
    -- ^ Identifier
    -> (Int -> IO a)
    -- ^ Action to run, passed number of references in use
    -> IO a
waitForFree' tr pol (RefCount mvar lock) ix action = modifyMVar lock $ const $ do
    res <- retrying pol (const $ pure . isJust) (const check)
    ((), ) <$> action (fromMaybe 0 res)
  where
    check = do
        refs <- Map.lookup ix <$> readMVar mvar
        traceWith tr refs
        pure refs

-- | Recommended retry schedule for polling the 'RefCount'. It will poll for up
-- to 2 minutes.
waitForFreeRetryPolicy :: RetryPolicy
waitForFreeRetryPolicy = fibonacciBackoff 50_000 & capDelay 5_000_000
    & limitRetriesByCumulativeDelay 120_000_000
