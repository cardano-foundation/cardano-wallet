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
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Pool.DB.Sqlite
    ( newDBLayer
    , withDBLayer
    , defaultFilePath
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBLog (..)
    , MigrationError (..)
    , SqliteContext (..)
    , destroyDBLayer
    , handleConstraint
    , startSqliteBackend
    )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochNo (..)
    , PoolId
    , PoolRegistrationCertificate (..)
    , SlotId (..)
    )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.List
    ( foldl' )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , deleteWhere
    , insertMany_
    , insert_
    , selectFirst
    , selectList
    , (<.)
    , (==.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT )
import System.Directory
    ( removeFile )
import System.FilePath
    ( (</>) )
import System.Random
    ( newStdGen )

import Cardano.Pool.DB.Sqlite.TH

import qualified Data.Map.Strict as Map

-- | Return the preferred @FilePath@ for the stake pool .sqlite file, given a
-- parent directory.
defaultFilePath
    :: FilePath
    -- ^ The directory in which the .sqlite file will be located.
    -> FilePath
defaultFilePath = (</> "stake-pools.sqlite")

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: Tracer IO DBLog
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer trace fp action =
    bracket before after (action . snd)
  where
    before = newDBLayer trace fp
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
    :: Tracer IO DBLog
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (SqliteContext, DBLayer IO)
newDBLayer trace fp = do
    let io = startSqliteBackend (const $ pure ()) migrateAll trace fp
    ctx@SqliteContext{runQuery} <- handlingPersistError trace fp io
    return (ctx, DBLayer
        { putPoolProduction = \point pool -> ExceptT $
            handleConstraint (ErrPointAlreadyExists point) $
                insert_ (mkPoolProduction pool point)

        , readPoolProduction = \epoch -> do
            production <- fmap fromPoolProduction <$> selectPoolProduction epoch

            let toMap :: Ord a => Map a [b] -> (a,b) -> Map a [b]
                toMap m (k, v) = Map.alter (alter v) k m
                  where
                    alter x = \case
                      Nothing -> Just [x]
                      Just xs -> Just (x:xs)

            pure (foldl' toMap Map.empty production)

        , putStakeDistribution = \epoch@(EpochNo ep) distribution -> do
            deleteWhere [StakeDistributionEpoch ==. fromIntegral ep]
            insertMany_ (mkStakeDistribution epoch distribution)

        , readStakeDistribution = \(EpochNo epoch) -> do
            fmap (fromStakeDistribution . entityVal) <$> selectList
                [ StakeDistributionEpoch ==. fromIntegral epoch ]
                []

        , putPoolRegistration = \point PoolRegistrationCertificate
            { poolId
            , poolOwners
            , poolMargin
            , poolCost
            } -> do
            let poolMargin_ = fromIntegral $ fromEnum poolMargin
            let poolCost_ = getQuantity poolCost
            insert_ $ PoolRegistration poolId point poolMargin_ poolCost_
            insertMany_ $ uncurry (PoolOwner poolId) <$> zip poolOwners [0..]

        , readPoolRegistration = \poolId -> do
            selectFirst [ PoolRegistrationPoolId ==. poolId ] [] >>= \case
                Nothing -> pure Nothing
                Just meta -> do
                    let (PoolRegistration _ _ poolMargin_ poolCost_) = entityVal meta
                    let poolMargin = toEnum $ fromIntegral poolMargin_
                    let poolCost = Quantity poolCost_
                    poolOwners <- fmap (poolOwnerOwner . entityVal) <$> selectList
                        [ PoolOwnerPoolId ==. poolId ]
                        [ Asc PoolOwnerIndex ]
                    pure $ Just $ PoolRegistrationCertificate
                        { poolId, poolOwners, poolMargin, poolCost }

        , listRegisteredPools = do
            fmap (poolRegistrationPoolId . entityVal) <$> selectList [ ]
                [ Desc PoolRegistrationSlot ]

        , rollbackTo = \point -> do
            let (EpochNo epoch) = epochNumber point
            deleteWhere [ PoolProductionSlot >. point ]
            deleteWhere [ StakeDistributionEpoch >. fromIntegral epoch ]
            deleteWhere [ PoolRegistrationSlot >. point ]

        , readPoolProductionCursor = \k -> do
            reverse . map (snd . fromPoolProduction . entityVal) <$> selectList
                []
                [Desc PoolProductionSlot, LimitTo k]

        , readSystemSeed = do
            mseed <- selectFirst [] []
            case mseed of
                Nothing -> do
                    seed <- liftIO newStdGen
                    insert_ (ArbitrarySeed seed)
                    return seed
                Just seed ->
                    return $ seedSeed $ entityVal seed

        , cleanDB = do
            deleteWhere ([] :: [Filter PoolProduction])
            deleteWhere ([] :: [Filter PoolOwner])
            deleteWhere ([] :: [Filter PoolRegistration])
            deleteWhere ([] :: [Filter StakeDistribution])

        , atomically = runQuery
        })

-- | 'Temporary', catches migration error from previous versions and if any,
-- _removes_ the database file completely before retrying to start the database.
--
-- This comes in handy to fix database schema in a non-backward compatible way
-- without altering too much the user experience. Indeed, the pools' database
-- can swiftly be re-synced from the chain, so instead of patching our mistakes
-- with ugly work-around we can, at least for now, reset it semi-manually when
-- needed to keep things tidy here.
handlingPersistError
    :: Tracer IO DBLog
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (Either MigrationError ctx)
       -- ^ Action to set up the context.
    -> IO ctx
handlingPersistError trace fp action = action >>= \case
    Right ctx -> pure ctx
    Left _ -> do
        traceWith trace MsgDatabaseReset
        maybe (pure ()) removeFile fp
        action >>= either throwIO pure

{-------------------------------------------------------------------------------
                                   Queries
-------------------------------------------------------------------------------}

selectPoolProduction
    :: EpochNo
    -> SqlPersistT IO [PoolProduction]
selectPoolProduction epoch = fmap entityVal <$> selectList
    [ PoolProductionSlot >=. SlotId epoch 0
    , PoolProductionSlot <. SlotId (epoch + 1) 0 ]
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
        , stakeDistributionEpoch = fromIntegral epoch
        , stakeDistributionStake = stake
        }

fromStakeDistribution
    :: StakeDistribution
    -> (PoolId, Quantity "lovelace" Word64)
fromStakeDistribution distribution =
    ( stakeDistributionPoolId distribution
    , Quantity (stakeDistributionStake distribution)
    )
