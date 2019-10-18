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
    ( DBLayer (..), ErrPointAlreadyExists (..) )
import Cardano.Pool.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), EpochNo (..), PoolId, SlotId (..) )
import Control.Exception
    ( bracket )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.List
    ( foldl' )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , deleteWhere
    , insertMany_
    , insert_
    , selectList
    , (==.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT )

import Cardano.Pool.DB.Sqlite.TH

import qualified Cardano.BM.Configuration.Model as CM
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
        { putPoolProduction = \point pool -> ExceptT $ runQuery $
            handleConstraint (ErrPointAlreadyExists point) $
                insert_ (mkPoolProduction pool point)

        , readPoolProduction = \epoch -> runQuery $ do
            production <- fmap fromPoolProduction <$> selectPoolProduction epoch

            let toMap :: Ord a => Map a [b] -> (a,b) -> Map a [b]
                toMap m (k, v) = Map.alter (alter v) k m
                  where
                    alter x = \case
                      Nothing -> Just [x]
                      Just xs -> Just (x:xs)

            pure (foldl' toMap Map.empty production)

        , putStakeDistribution = \epoch@(EpochNo ep) distribution -> runQuery $ do
            deleteWhere [StakeDistributionEpoch ==. ep]
            insertMany_ (mkStakeDistribution epoch distribution)

        , readStakeDistribution = \(EpochNo epoch) -> runQuery $ do
            fmap (fromStakeDistribution . entityVal) <$> selectList
                [ StakeDistributionEpoch ==. epoch ]
                []

        , rollbackTo = \point -> runQuery $ do
            let (EpochNo epoch) = epochNumber point
            deleteWhere [ PoolProductionSlot >. point ]
            deleteWhere [ StakeDistributionEpoch >. epoch ]


        , readCursor = \k -> runQuery $ do
            reverse . map (snd . fromPoolProduction . entityVal) <$> selectList
                []
                [Desc PoolProductionSlot, LimitTo k]

        , cleanDB = runQuery $
            deleteWhere ([] :: [Filter PoolProduction])
        })

{-------------------------------------------------------------------------------
                                   Queries
-------------------------------------------------------------------------------}

selectPoolProduction
    :: EpochNo
    -> SqlPersistT IO [PoolProduction]
selectPoolProduction epoch = fmap entityVal <$> selectList
    [PoolProductionSlot >=. SlotId epoch 0]
    [Asc PoolProductionSlot]

{-------------------------------------------------------------------------------
                              To / From SQLite
-------------------------------------------------------------------------------}

mkPoolProduction
    :: PoolId
    -> BlockHeader
    -> PoolProduction
mkPoolProduction pool block = PoolProduction
    { poolProductionPoolId = pool
    , poolProductionSlot = slotId block
    , poolProductionHeaderHash = BlockId (headerHash block)
    , poolProductionParentHash = BlockId (parentHeaderHash block)
    , poolProductionBlockHeight = getQuantity (blockHeight block)
    }

fromPoolProduction
    :: PoolProduction
    -> (PoolId, BlockHeader)
fromPoolProduction (PoolProduction pool slot headerH parentH height) =
    ( pool
    , BlockHeader
        { slotId = slot
        , blockHeight = Quantity height
        , headerHash = getBlockId headerH
        , parentHeaderHash = getBlockId parentH
        }
    )

mkStakeDistribution
    :: EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> [StakeDistribution]
mkStakeDistribution (EpochNo epoch) = map $ \(pool, (Quantity stake)) ->
    StakeDistribution
        { stakeDistributionPoolId = pool
        , stakeDistributionEpoch = epoch
        , stakeDistributionStake = stake
        }

fromStakeDistribution
    :: StakeDistribution
    -> (PoolId, Quantity "lovelace" Word64)
fromStakeDistribution distribution =
    ( stakeDistributionPoolId distribution
    , Quantity (stakeDistributionStake distribution)
    )
