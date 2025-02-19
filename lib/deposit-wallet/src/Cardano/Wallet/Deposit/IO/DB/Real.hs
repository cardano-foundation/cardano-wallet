{-# LANGUAGE Rank2Types #-}

module Cardano.Wallet.Deposit.IO.DB.Real
    ( Connection
    , withSqliteFile
    , withSqliteInMemory
    , SqlM
    , runSqlM
    , DBLog (..)
    ) where

import Prelude

import Control.Tracer
    ( Tracer
    , traceWith
    )
import Database.Table.SQLite.Simple
    ( Connection
    , SqlM
    , runSqlM
    , withConnection
    )

{-----------------------------------------------------------------------------
    SqlContext
------------------------------------------------------------------------------}

-- | Acquire and release an SQLite 'Connection' in memory.
withSqliteInMemory
    :: Tracer IO DBLog
    -- ^ Logging
    -> (Connection -> IO a)
    -- ^ Action to run
    -> IO a
withSqliteInMemory tr = withSqliteFile tr ":memory:"

-- | Acquire and release an SQLite 'Connection' from a file.
withSqliteFile
    :: Tracer IO DBLog
    -- ^ Logging
    -> FilePath
    -- ^ Database file
    -> (Connection -> IO a)
    -- ^ Action to run
    -> IO a
withSqliteFile tr filepath action =
    withConnection filepath $ \conn -> do
        traceWith tr $ MsgStartConnection filepath
        result <- action conn
        traceWith tr $ MsgDoneConnection filepath
        pure result

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}

data DBLog
    = MsgStartConnection FilePath
    | MsgDoneConnection FilePath
    deriving (Show, Eq)
