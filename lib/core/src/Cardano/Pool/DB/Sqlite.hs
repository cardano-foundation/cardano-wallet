{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Pool.DB.Sqlite
    ( newDBLayer
    , withDBLayer
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace )
import Cardano.DB.Sqlite
    ( SqliteContext (..)
    , destroyDBLayer
    , handleConstraint
    , startSqliteBackend
    , transformTrace
    )
import Cardano.Pool.DB
    ( DBLayer (..), ErrSlotAlreadyExists (..) )
import Cardano.Pool.DB.Sqlite.TH
import Cardano.Pool.DB.Sqlite.Types
    ()
import Control.Exception
    ( bracket )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word16, Word64 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , deleteWhere
    , insert_
    , selectList
    , (<=.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer logConfig trace fp action =
    bracket before after (action . snd)
  where
    before = newDBLayer logConfig trace fp
    after = destroyDBLayer . fst

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
    :: CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (SqliteContext, DBLayer IO)
newDBLayer logConfig trace fp = do
    let trace' = transformTrace trace
    ctx@SqliteContext{runQuery} <-
        startSqliteBackend logConfig migrateAll trace' fp
    return (ctx, DBLayer
        { putPoolProduction = \slot pool ->
            ExceptT $ runQuery $ handleConstraint (ErrSlotAlreadyExists slot) $
            insert_ (PoolProduction slot pool)

        , readPoolProduction = \epoch ->
            runQuery $ foldl (\m (PoolProduction s p) -> Map.alter (alter s) p m)
            Map.empty <$> selectPoolProduction epoch

        , rollbackTo = \slot ->
             runQuery $ deleteWhere [PoolProductionSlot >. slot]

        , cleanDB = runQuery $ deleteWhere ([] :: [Filter PoolProduction])

        })
  where
      alter slot = \case
          Nothing -> Just [slot]
          Just slots -> Just (slot:slots)

selectPoolProduction
    :: Word64
    -> SqlPersistT IO [PoolProduction]
selectPoolProduction epoch = do
    let from = W.SlotId epoch 0
    let to = W.SlotId epoch (maxBound :: Word16)
    fmap entityVal <$>
        selectList
            [PoolProductionSlot >=. from, PoolProductionSlot <=. to ]
            [Asc PoolProductionSlot]
