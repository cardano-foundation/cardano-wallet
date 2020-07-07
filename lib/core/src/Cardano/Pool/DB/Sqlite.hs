{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
    ( DBField (..)
    , DBLog (..)
    , ManualMigration (..)
    , MigrationError (..)
    , SqliteContext (..)
    , destroyDBLayer
    , fieldName
    , handleConstraint
    , startSqliteBackend
    , tableName
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
    , PoolRetirementCertificate (..)
    , SlotId (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataHash
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Either
    ( rights )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List
    ( foldl' )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Ratio
    ( denominator, numerator, (%) )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import Data.Word
    ( Word64, Word8 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , Single (..)
    , deleteWhere
    , fromPersistValue
    , insertMany_
    , insert_
    , rawSql
    , repsert
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
import qualified Data.Text as T

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
withDBLayer trace fp action = do
    traceWith trace (MsgWillOpenDB fp)
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
    let io = startSqliteBackend
            (ManualMigration mempty)
            migrateAll
            trace
            fp
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

        , readTotalProduction = do
            production <- fmap entityVal <$>
                selectList ([] :: [Filter PoolProduction]) []

            let toMap m (PoolProduction{poolProductionPoolId}) =
                    Map.insertWith (+) poolProductionPoolId 1 m

            pure $ Map.map Quantity $ foldl' toMap Map.empty production

        , putStakeDistribution = \epoch@(EpochNo ep) distribution -> do
            deleteWhere [StakeDistributionEpoch ==. fromIntegral ep]
            insertMany_ (mkStakeDistribution epoch distribution)

        , readStakeDistribution = \(EpochNo epoch) -> do
            fmap (fromStakeDistribution . entityVal) <$> selectList
                [ StakeDistributionEpoch ==. fromIntegral epoch ]
                []

        , putPoolRegistration = \(slotId, slotInternalIndex) cert -> do
            let poolId = view #poolId cert
            deleteWhere [PoolOwnerPoolId ==. poolId, PoolOwnerSlot ==. slotId]
            let poolRegistrationKey = PoolRegistrationKey
                    poolId slotId slotInternalIndex
            let poolRegistrationRow = PoolRegistration
                    (poolId)
                    (slotId)
                    (slotInternalIndex)
                    (fromIntegral $ numerator
                        $ getPercentage $ poolMargin cert)
                    (fromIntegral $ denominator
                        $ getPercentage $ poolMargin cert)
                    (getQuantity $ poolCost cert)
                    (getQuantity $ poolPledge cert)
                    (fst <$> poolMetadata cert)
                    (snd <$> poolMetadata cert)
            _ <- repsert poolRegistrationKey poolRegistrationRow
            insertMany_ $
                zipWith
                    (PoolOwner poolId slotId slotInternalIndex)
                    (poolOwners cert)
                    [0..]

        , readPoolRegistration = \poolId -> do
            let filterBy = [ PoolRegistrationPoolId ==. poolId ]
            let orderBy =
                  [ Desc PoolRegistrationSlot
                  , Desc PoolRegistrationSlotInternalIndex
                  ]
            selectFirst filterBy orderBy >>= \case
                Nothing -> pure Nothing
                Just meta -> do
                    let PoolRegistration
                            _poolId
                            slotId
                            slotInternalIndex
                            marginNum
                            marginDen
                            poolCost_
                            poolPledge_
                            poolMetadataUrl
                            poolMetadataHash = entityVal meta
                    let poolMargin = unsafeMkPercentage $
                            toRational $ marginNum % marginDen
                    let poolCost = Quantity poolCost_
                    let poolPledge = Quantity poolPledge_
                    let poolMetadata =
                          (,) <$> poolMetadataUrl <*> poolMetadataHash
                    poolOwners <- fmap (poolOwnerOwner . entityVal) <$>
                        selectList
                            [ PoolOwnerPoolId
                                ==. poolId
                            , PoolOwnerSlot
                                ==. slotId
                            , PoolOwnerSlotInternalIndex
                                ==. slotInternalIndex
                            ]
                            [ Asc PoolOwnerIndex ]
                    let cert = PoolRegistrationCertificate
                            { poolId
                            , poolOwners
                            , poolMargin
                            , poolCost
                            , poolPledge
                            , poolMetadata
                            }
                    pure $ Just ((slotId, slotInternalIndex), cert)

        , putPoolRetirement = \(slotId, slotInternalIndex) cert -> do
            let PoolRetirementCertificate
                    { poolId
                    , retiredIn
                    } = cert
            let EpochNo retirementEpoch = retiredIn
            repsert (PoolRetirementKey poolId slotId slotInternalIndex) $
                PoolRetirement
                    poolId
                    slotId
                    slotInternalIndex
                    (fromIntegral retirementEpoch)

        , readPoolRetirement = \poolId -> do
            let filterBy = [ PoolRetirementPoolId ==. poolId ]
            let orderBy =
                    [ Desc PoolRetirementSlot
                    , Desc PoolRetirementSlotInternalIndex
                    ]
            selectFirst filterBy orderBy >>= \case
                Nothing -> pure Nothing
                Just meta -> do
                    let PoolRetirement
                            _poolId
                            slotId
                            slotInternalIndex
                            retirementEpoch = entityVal meta
                    let retiredIn = EpochNo (fromIntegral retirementEpoch)
                    let cert = PoolRetirementCertificate
                            { poolId
                            , retiredIn
                            }
                    pure $ Just ((slotId, slotInternalIndex), cert)

        , unfetchedPoolMetadataRefs = \limit -> do
            let nLimit = T.pack (show limit)
            let metadataHash  = fieldName (DBField PoolRegistrationMetadataHash)
            let metadataUrl   = fieldName (DBField PoolRegistrationMetadataUrl)
            let retryAfter    = fieldName (DBField PoolFetchAttemptsRetryAfter)
            let registrations = tableName (DBField PoolRegistrationMetadataHash)
            let fetchAttempts = tableName (DBField PoolFetchAttemptsMetadataHash)
            let metadata      = tableName (DBField PoolMetadataHash)
            let query = T.unwords
                    [ "SELECT"
                    , metadataUrl, ",", metadataHash
                    , "FROM", registrations
                    , "WHERE"
                    , metadataHash, "NOT", "IN" -- Successfully fetched metadata
                        , "("
                        , "SELECT", metadataHash
                        , "FROM", metadata
                        , ")"
                    , "AND"
                    , metadataHash, "NOT", "IN" -- Recently failed urls
                        , "("
                        , "SELECT", metadataHash
                        , "FROM", fetchAttempts
                        , "WHERE", retryAfter, ">=", "datetime('now')"
                        , ")"
                    , "LIMIT", nLimit
                    , ";"
                    ]

            let safeCast (Single a, Single b) = (,)
                    <$> fromPersistValue a
                    <*> fromPersistValue b

            rights . fmap safeCast <$> rawSql query []

        , putFetchAttempt = \(url, hash) -> do
            -- NOTE
            -- assuming SQLite has the same notion of "now" that the host system.
            now <- liftIO getCurrentTime
            let filters =
                    [ PoolFetchAttemptsMetadataHash ==. hash
                    , PoolFetchAttemptsMetadataUrl  ==. url
                    ]
            (fmap entityVal <$> selectFirst filters []) >>= \case
                Nothing -> do
                    let retryAfter = backoff now 0
                    repsert
                        (PoolMetadataFetchAttemptsKey hash url)
                        (PoolMetadataFetchAttempts hash url retryAfter 1)

                Just (PoolMetadataFetchAttempts _ _ _ retryCount) -> do
                    let retryAfter = backoff now retryCount
                    repsert
                        (PoolMetadataFetchAttemptsKey hash url)
                        (PoolMetadataFetchAttempts hash url retryAfter $ retryCount + 1)

        , putPoolMetadata = \hash metadata -> do
            let StakePoolMetadata{ticker,name,description,homepage} = metadata
            repsert
                (PoolMetadataKey hash)
                (PoolMetadata hash name ticker description homepage)
            deleteWhere [ PoolFetchAttemptsMetadataHash ==. hash ]

        , readPoolMetadata = do
            Map.fromList . map (fromPoolMeta . entityVal)
                <$> selectList [] []

        , listRegisteredPools = do
            fmap (poolRegistrationPoolId . entityVal) <$> selectList [ ]
                [ Desc PoolRegistrationSlot ]

        , rollbackTo = \point -> do
            let (EpochNo epoch) = epochNumber point
            deleteWhere [ PoolProductionSlot >. point ]
            deleteWhere [ StakeDistributionEpoch >. fromIntegral epoch ]
            deleteWhere [ PoolRegistrationSlot >. point ]
            -- TODO: remove dangling metadata no longer attached to a pool

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
            deleteWhere ([] :: [Filter PoolMetadata])
            deleteWhere ([] :: [Filter PoolMetadataFetchAttempts])

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

-- | Compute a new date from a base date, with an increasing delay.
--
-- > backoff t 0
-- t+3s
--
-- > backoff t 1
-- t+9s
--
-- > backoff t 2
-- t+27s
--
-- ...
--
-- > backoff t 9
-- t+16h
--
-- > backoff t 10
-- t+49h
backoff :: UTCTime -> Word8 -> UTCTime
backoff time iter = addUTCTime delay time
  where
    delay = fromIntegral @Integer $ foldr (*) 3 (replicate (fromIntegral iter) 3)

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

fromPoolMeta
    :: PoolMetadata
    -> (StakePoolMetadataHash, StakePoolMetadata)
fromPoolMeta meta = (poolMetadataHash meta,) $
    StakePoolMetadata
        { ticker = poolMetadataTicker meta
        , name = poolMetadataName meta
        , description = poolMetadataDescription meta
        , homepage = poolMetadataHomepage meta
        }
