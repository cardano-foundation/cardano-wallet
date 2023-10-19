{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
module Cardano.Wallet.Deposit.IO.DB
    ( SqlM
    , SqlContext (..)
    , withSqliteFile

    , DBLog (..)
    ) where

import Prelude

import Cardano.BM.Extra
    ( bracketTracer )
import Cardano.DB.Sqlite
    ( DBLog (..), dbBackend, withDBHandle )
import Control.Concurrent.MVar
    ( newMVar, withMVar )
import Control.Tracer
    ( Tracer, contramap )
import Data.Time.Clock
    ( NominalDiffTime )

import qualified Database.Persist.Sql as Persistent

{-----------------------------------------------------------------------------
    Comment layout
------------------------------------------------------------------------------}
-- | Monad to run SQL queries in.
type SqlM = Persistent.SqlPersistT IO

-- | A facility to run 'SqlM' computations.
-- Importantly, computations are not run in parallel, but sequenced.
newtype SqlContext = SqlContext
    { runQuery :: forall a. SqlM a -> IO a
    }

-- | Open an .sqlite database file
-- and provide an 'SqlContext' for running 'SqlM' actions.
withSqliteFile
    :: Tracer IO DBLog
        -- ^ Logging
    -> FilePath
        -- ^ Database file
    -> (SqlContext -> IO a)
        -- ^ Action to run
    -> IO a
withSqliteFile tr fp action = do
    -- Lock ensures that database operations are sequenced.
    lock <- newMVar ()
    withDBHandle tr fp $ \dbHandle ->
        let
            -- Run a query on the open database,
            -- but retry on busy.
            runQuery :: SqlM a -> IO a
            runQuery cmd =
                observe
                    . retryOnBusy tr retryOnBusyTimeout
                    $ withMVar lock
                    $ const
                    $ Persistent.runSqlConn cmd
                    $ dbBackend dbHandle
        in
            action $ SqlContext{runQuery}
  where
    observe :: IO a -> IO a
    observe = bracketTracer (contramap MsgRun tr)

-- | Retry an action if the database yields an 'SQLITE_BUSY' error.
--
-- From <https://www.sqlite.org/rescode.html#busy>
--
--     The SQLITE_BUSY result code indicates that the database file could not be
--     written (or in some cases read) because of concurrent activity by some
--     other database connection, usually a database connection in a separate
--     process.
--
--     For example, if process A is in the middle of a large write transaction
--     and at the same time process B attempts to start a new write transaction,
--     process B will get back an SQLITE_BUSY result because SQLite only supports
--     one writer at a time. Process B will need to wait for process A to finish
--     its transaction before starting a new transaction. The sqlite3_busy_timeout()
--     and sqlite3_busy_handler() interfaces and the busy_timeout pragma are
--     available to process B to help it deal with SQLITE_BUSY errors.
retryOnBusy
    :: Tracer IO DBLog
    -- ^ Logging
    -> NominalDiffTime
    -- ^ Timeout
    -> IO a
    -- ^ Action to retry
    -> IO a
retryOnBusy _ _ action = action -- FIXME

-- | Default timeout for `retryOnBusy`
retryOnBusyTimeout :: NominalDiffTime
retryOnBusyTimeout = 60
