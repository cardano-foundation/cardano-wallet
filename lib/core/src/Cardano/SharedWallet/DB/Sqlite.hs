{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Redundant flip" -}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.SharedWallet.DB.Sqlite
    ( newDBLayer
    , withDBLayer
    , withDecoratedDBLayer
    , DBDecorator (..)
    , undecoratedDB
    , defaultFilePath
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBLog (..)
    , MigrationError
    , SqliteContext (..)
    , newInMemorySqliteContext
    , newSqliteContext
    , withConnectionPool
    )
import Cardano.SharedWallet.DB
    ( DBLayer (..) )
import Cardano.SharedWallet.DB.Log
    ( SharedWalletDbLog (..) )
import Cardano.SharedWallet.DB.Sqlite.TH
    ( migrateAll )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Function
    ( (&) )
import Database.Persist.Sqlite
    ( SqlPersistT )
import System.Directory
    ( removeFile )
import System.FilePath
    ( (</>) )
import UnliftIO.Exception
    ( bracket, catch, throwIO )


-- | Return the preferred @FilePath@ for the stake pool .sqlite file, given a
-- parent directory.
defaultFilePath
    :: FilePath
    -- ^ The directory in which the .sqlite file will be located.
    -> FilePath
defaultFilePath = (</> "shared-wallet.sqlite")

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: Tracer IO SharedWalletDbLog
    -- ^ Logging object.
    -> Maybe FilePath
    -- ^ Database file location, or 'Nothing' for in-memory database.
    -> (DBLayer IO k -> IO a)
    -- ^ Action to run.
    -> IO a
withDBLayer = withDecoratedDBLayer undecoratedDB

-- | A decorator for the database layer, useful for instrumenting or monitoring
--   calls to database operations.
newtype DBDecorator a k =
    DBDecorator { decorateDBLayer :: DBLayer a k -> DBLayer a k }

-- | The identity decorator.
--
-- Equivalent to an undecorated database.
--
undecoratedDB :: DBDecorator a k
undecoratedDB = DBDecorator id

-- | Runs an action with a connection to the SQLite database.
--
-- This function has the same behaviour as 'withDBLayer', but provides a way
-- to decorate the created 'DBLayer' object with a 'DBDecorator', useful for
-- instrumenting or monitoring calls to database operations.
--
withDecoratedDBLayer
    :: DBDecorator IO k
       -- ^ The database decorator.
    -> Tracer IO SharedWalletDbLog
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO k -> IO a)
       -- ^ Action to run.
    -> IO a
withDecoratedDBLayer dbDecorator tr mDatabaseDir action = do
    case mDatabaseDir of
        Nothing -> bracket
            (newInMemorySqliteContext tr' [] migrateAll)
            fst
            (action . decorateDBLayer dbDecorator . newDBLayer tr . snd)

        Just fp -> handlingPersistError tr fp $
            withConnectionPool tr' fp $ \wallet -> do
                ctx <- newSqliteContext tr' wallet [] migrateAll
                ctx & either
                    throwIO
                    (action . decorateDBLayer dbDecorator . newDBLayer tr)
  where
    tr' = contramap MsgGeneric tr

-- | Sets up a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
--
-- 'getDBLayer' will provide the actual 'DBLayer' implementation. The database
-- should be closed with 'destroyDBLayer'. If you use 'withDBLayer' then both of
-- these things will be handled for you.
newDBLayer
    :: Tracer IO SharedWalletDbLog
       -- ^ Logging object
    -> SqliteContext
        -- ^ A (thread-) safe wrapper for running db queries.
    -> DBLayer IO k
newDBLayer _tr SqliteContext{runQuery} =
    DBLayer {..}
      where
        initializeSharedState _walId _state _meta _gp = undefined

        removeSharedWallet _walId = undefined

        readSharedWalletState _walId = undefined

        readSharedWalletMetadata _walId = undefined

        addCosignerKey _walId _utctime _cosignerInfo = undefined

        listCosignerKeys _walId = undefined

        cleanDB = undefined

        atomically :: forall a. (SqlPersistT IO a -> IO a)
        atomically = runQuery


-- | 'Temporary', catches migration error from previous versions and if any,
-- _removes_ the database file completely before retrying to start the database.
--
-- This comes in handy to fix database schema in a non-backward compatible way
-- without altering too much the user experience. Indeed, the pools' database
-- can swiftly be re-synced from the chain, so instead of patching our mistakes
-- with ugly work-around we can, at least for now, reset it semi-manually when
-- needed to keep things tidy here.
handlingPersistError
    :: Tracer IO SharedWalletDbLog
       -- ^ Logging object
    -> FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO a
       -- ^ Action to retry
    -> IO a
handlingPersistError tr fp action =
    action `catch` \(_e :: MigrationError) -> do
        traceWith tr $ MsgGeneric MsgDatabaseReset
        removeFile fp
        action
