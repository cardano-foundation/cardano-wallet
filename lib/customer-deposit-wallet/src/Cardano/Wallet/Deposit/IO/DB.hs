{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
module Cardano.Wallet.Deposit.IO.DB
    ( SqlM
    , SqlContext (..)
    , withSqliteFile
    , withSqlContextInMemory

    , DBLog (..)
    ) where

import Prelude

import Cardano.BM.Extra
    ( bracketTracer
    )
import Cardano.DB.Sqlite
    ( DBLog (..)
    )
import Control.Concurrent.MVar
    ( newMVar
    , withMVar
    )
import Control.Monad.Trans.Reader
    ( ReaderT (..)
    )
import Control.Tracer
    ( Tracer
    , contramap
    , traceWith
    )

import qualified Database.SQLite.Simple as Sqlite

{-----------------------------------------------------------------------------
    SqlContext
------------------------------------------------------------------------------}
-- | Monad to run SQL queries in.
type SqlM = ReaderT Sqlite.Connection IO

-- | A facility to run 'SqlM' computations.
-- Importantly, computations are not run in parallel, but sequenced.
newtype SqlContext = SqlContext
    { runSqlM :: forall a. SqlM a -> IO a
    }

-- | Acquire and release an 'SqlContext' in memory.
withSqlContextInMemory
    :: Tracer IO DBLog
    -- ^ Logging
    -> (SqlContext -> IO a)
    -- ^ Action to run
    -> IO a
withSqlContextInMemory tr = withSqliteFile tr ":memory:"

-- | Use sqlite to open a database file
-- and provide an 'SqlContext' for running 'SqlM' actions.
withSqliteFile
    :: Tracer IO DBLog
    -- ^ Logging
    -> FilePath
    -- ^ Database file
    -> (SqlContext -> IO a)
    -- ^ Action to run
    -> IO a
withSqliteFile tr filepath action =
    Sqlite.withConnection filepath $ \connection0 -> do
        traceWith tr $ MsgOpenSingleConnection filepath
        -- The lock ensures that database operations are sequenced.
        lock <- newMVar connection0
        let runSqlM :: SqlM a -> IO a
            runSqlM cmd = withMVar lock (observe . runReaderT cmd)
        action SqlContext{runSqlM}
  where
    observe :: IO a -> IO a
    observe = bracketTracer (contramap MsgRun tr)
