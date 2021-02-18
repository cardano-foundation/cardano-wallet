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
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Pool.DB.Sqlite
    ( newDBLayer
    , withDBLayer
    , withDecoratedDBLayer
    , DBDecorator (..)
    , undecoratedDB
    , defaultFilePath
    , DatabaseView (..)
    , createViews
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBField (..)
    , DBLog (..)
    , ManualMigration (..)
    , MigrationError
    , SqliteContext (..)
    , fieldName
    , handleConstraint
    , newInMemorySqliteContext
    , newSqliteContext
    , tableName
    , withConnectionPool
    )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists (..), determinePoolLifeCycleStatus )
import Cardano.Pool.DB.Log
    ( ParseFailure (..), PoolDbLog (..) )
import Cardano.Pool.DB.Sqlite.TH hiding
    ( BlockHeader, blockHeight )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.Logging
    ( bracketTracer )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, firstSlotInEpoch, interpretQuery )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochNo (..)
    , PoolId (..)
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataHash
    , defaultSettings
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer (..), contramap, natTracer, traceWith )
import Data.Either
    ( partitionEithers, rights )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
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
import Data.String.Interpolate
    ( i )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import Data.Word
    ( Word64, Word8 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , PersistValue
    , RawSql
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
    , toPersistValue
    , update
    , (<.)
    , (=.)
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
import UnliftIO.Exception
    ( catch, throwIO )

import qualified Cardano.Pool.DB.Sqlite.TH as TH
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

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
    :: Tracer IO PoolDbLog
    -- ^ Logging object.
    -> Maybe FilePath
    -- ^ Database file location, or 'Nothing' for in-memory database.
    -> TimeInterpreter IO
    -- ^ The time interpreter object.
    -> (DBLayer IO -> IO a)
    -- ^ Action to run.
    -> IO a
withDBLayer = withDecoratedDBLayer undecoratedDB

-- | A decorator for the database layer, useful for instrumenting or monitoring
--   calls to database operations.
newtype DBDecorator a =
    DBDecorator { decorateDBLayer :: DBLayer a -> DBLayer a }

-- | The identity decorator.
--
-- Equivalent to an undecorated database.
--
undecoratedDB :: DBDecorator a
undecoratedDB = DBDecorator id

-- | Runs an action with a connection to the SQLite database.
--
-- This function has the same behaviour as 'withDBLayer', but provides a way
-- to decorate the created 'DBLayer' object with a 'DBDecorator', useful for
-- instrumenting or monitoring calls to database operations.
--
withDecoratedDBLayer
    :: DBDecorator IO
       -- ^ The database decorator.
    -> Tracer IO PoolDbLog
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> TimeInterpreter IO
       -- ^ The time interpreter object.
    -> (DBLayer IO -> IO a)
       -- ^ Action to run.
    -> IO a
withDecoratedDBLayer dbDecorator tr mDatabaseDir ti action = do
    case mDatabaseDir of
        Nothing -> do
            ctx <- newInMemorySqliteContext tr' createViews migrateAll
            action (decorateDBLayer dbDecorator $ newDBLayer tr ti ctx)

        Just fp -> handlingPersistError tr fp $
            withConnectionPool tr' fp $ \pool -> do
                ctx <- newSqliteContext tr' pool createViews migrateAll
                ctx & either
                    throwIO
                    (action . decorateDBLayer dbDecorator . newDBLayer tr ti)
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
    :: Tracer IO PoolDbLog
       -- ^ Logging object
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> SqliteContext
        -- ^ A (thread-) safe wrapper for running db queries.
    -> DBLayer IO
newDBLayer tr ti SqliteContext{runQuery} =
    DBLayer {..}
      where
        putPoolProduction point pool = ExceptT $
            handleConstraint (ErrPointAlreadyExists point) $
                insert_ (mkPoolProduction pool point)

        readPoolProduction epoch = do
            production <- fmap fromPoolProduction
                <$> selectPoolProduction ti epoch

            let toMap :: Ord a => Map a [b] -> (a,b) -> Map a [b]
                toMap m (k, v) = Map.alter (alter v) k m
                  where
                    alter x = \case
                      Nothing -> Just [x]
                      Just xs -> Just (x:xs)

            pure (foldl' toMap Map.empty production)

        readTotalProduction = Map.fromList <$> runRawQuery tr
            (RawQuery "readTotalProduction" query [] parseRow)
          where
            query = T.unwords
                [ "SELECT pool_id, count(pool_id) as block_count"
                , "FROM pool_production"
                , "GROUP BY pool_id;"
                ]
            parseRow
                ( Single fieldPoolId
                , Single fieldBlockCount
                ) = (,)
                    <$> fromPersistValue fieldPoolId
                    <*> (Quantity <$> fromPersistValue fieldBlockCount)

        putStakeDistribution epoch@(EpochNo ep) distribution = do
            deleteWhere [StakeDistributionEpoch ==. fromIntegral ep]
            insertMany_ (mkStakeDistribution epoch distribution)

        readStakeDistribution (EpochNo epoch) = do
            fmap (fromStakeDistribution . entityVal) <$> selectList
                [ StakeDistributionEpoch ==. fromIntegral epoch ]
                []

        readPoolLifeCycleStatus poolId =
            determinePoolLifeCycleStatus
                <$> readPoolRegistration poolId
                <*> readPoolRetirement poolId

        putPoolRegistration cpt cert = do
            let CertificatePublicationTime {slotNo, slotInternalIndex} = cpt
            let poolId = view #poolId cert
            deleteWhere
                [ PoolOwnerPoolId ==. poolId
                , PoolOwnerSlot ==. slotNo
                , PoolOwnerSlotInternalIndex ==. slotInternalIndex
                ]
            let poolRegistrationKey = PoolRegistrationKey
                    poolId slotNo slotInternalIndex
            let poolRegistrationRow = PoolRegistration
                    (poolId)
                    (slotNo)
                    (slotInternalIndex)
                    (fromIntegral $ numerator
                        $ getPercentage $ poolMargin cert)
                    (fromIntegral $ denominator
                        $ getPercentage $ poolMargin cert)
                    (W.unCoin $ poolCost cert)
                    (W.unCoin $ poolPledge cert)
                    (fst <$> poolMetadata cert)
                    (snd <$> poolMetadata cert)
            _ <- repsert poolRegistrationKey poolRegistrationRow
            insertMany_ $
                zipWith
                    (PoolOwner poolId slotNo slotInternalIndex)
                    (poolOwners cert)
                    [0..]

        putPoolRetirement cpt cert = do
            let CertificatePublicationTime {slotNo, slotInternalIndex} = cpt
            let PoolRetirementCertificate
                    poolId (EpochNo retirementEpoch) = cert
            repsert (PoolRetirementKey poolId slotNo slotInternalIndex) $
                PoolRetirement
                    poolId
                    slotNo
                    slotInternalIndex
                    (fromIntegral retirementEpoch)

        unfetchedPoolMetadataRefs limit = do
            let nLimit = T.pack (show limit)
            let poolId        = fieldName (DBField PoolRegistrationPoolId)
            let metadataHash  = fieldName (DBField PoolRegistrationMetadataHash)
            let metadataUrl   = fieldName (DBField PoolRegistrationMetadataUrl)
            let retryAfter    = fieldName (DBField PoolFetchAttemptsRetryAfter)
            let registrations = tableName (DBField PoolRegistrationMetadataHash)
            let fetchAttempts = tableName (DBField PoolFetchAttemptsMetadataHash)
            let metadata      = tableName (DBField PoolMetadataHash)
            let query = T.unwords
                    [ "SELECT"
                        , "a." <> poolId, ","
                        , "a." <> metadataUrl, ","
                        , "a." <> metadataHash
                    , "FROM", registrations, "AS a"
                    , "LEFT JOIN", fetchAttempts, "AS b"
                    , "ON"
                        , "a." <> metadataUrl,  "=", "b." <> metadataUrl, "AND"
                        , "a." <> metadataHash, "=", "b." <> metadataHash
                    , "WHERE"
                        -- Successfully fetched metadata
                        , "a." <> metadataHash, "NOT", "IN"
                        , "("
                        , "SELECT", metadataHash
                        , "FROM", metadata
                        , ")"
                    , "AND"
                        -- Discard recent failed attempts
                        , "("
                        , retryAfter, "<", "datetime('now')"
                        , "OR"
                        , retryAfter, "IS NULL"
                        , ")"
                    -- Important, since we have a limit, we order all results by
                    -- earlist "retry_after", so that we are sure that all
                    -- metadata gets _eventually_ processed.
                    --
                    -- Note that `NULL` is smaller than everything.
                    , "ORDER BY", retryAfter, "ASC"
                    , "LIMIT", nLimit
                    , ";"
                    ]

            let safeCast (Single a, Single b, Single c) = (,,)
                    <$> fromPersistValue a
                    <*> fromPersistValue b
                    <*> fromPersistValue c

            rights . fmap safeCast <$> rawSql query []

        putFetchAttempt (url, hash) = do
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

        putPoolMetadata hash metadata = do
            let StakePoolMetadata
                    {ticker, name, description, homepage} = metadata
            repsert
                (PoolMetadataKey hash)
                (PoolMetadata hash name ticker description homepage)
            deleteWhere [ PoolFetchAttemptsMetadataHash ==. hash ]

        removePoolMetadata = do
            deleteWhere ([] :: [Filter PoolMetadata])
            deleteWhere ([] :: [Filter PoolMetadataFetchAttempts])
            deleteWhere ([] :: [Filter PoolDelistment])

        readPoolMetadata = do
            Map.fromList . map (fromPoolMeta . entityVal)
                <$> selectList [] []

        listRegisteredPools = do
            fmap (poolRegistrationPoolId . entityVal) <$> selectList [ ]
                [ Desc PoolRegistrationSlot
                , Desc PoolRegistrationSlotInternalIndex
                ]

        listRetiredPools epochNo = runRawQuery tr $
            RawQuery "listRetiredPools" query parameters parseRow
          where
            query = T.unwords
                [ "SELECT *"
                , "FROM active_pool_retirements"
                , "WHERE retirement_epoch <= ?;"
                ]
            parameters = [ toPersistValue epochNo ]
            parseRow (Single poolId, Single retirementEpoch) =
                PoolRetirementCertificate
                    <$> fromPersistValue poolId
                    <*> fromPersistValue retirementEpoch

        listPoolLifeCycleData epochNo = runRawQuery tr $ RawQuery
            "listPoolLifeCycleData" query parameters parseRow
          where
            query = T.unwords
                [ "SELECT *"
                , "FROM active_pool_lifecycle_data"
                , "WHERE retirement_epoch IS NULL OR retirement_epoch > ?;"
                ]
            parameters = [ toPersistValue epochNo ]
            parseRow
                ( Single fieldPoolId
                , Single fieldRetirementEpoch
                , Single fieldOwners
                , Single fieldCost
                , Single fieldPledge
                , Single fieldMarginNumerator
                , Single fieldMarginDenominator
                , Single fieldMetadataHash
                , Single fieldMetadataUrl
                ) = do
                regCert <- parseRegistrationCertificate
                parseRetirementCertificate <&> maybe
                    (PoolRegistered regCert)
                    (PoolRegisteredAndRetired regCert)
              where
                parseRegistrationCertificate = PoolRegistrationCertificate
                    <$> fromPersistValue fieldPoolId
                    <*> fromPersistValue fieldOwners
                    <*> parseMargin
                    <*> (W.Coin <$> fromPersistValue fieldCost)
                    <*> (W.Coin <$> fromPersistValue fieldPledge)
                    <*> parseMetadata

                parseRetirementCertificate = do
                    poolId <- fromPersistValue fieldPoolId
                    mRetirementEpoch <- fromPersistValue fieldRetirementEpoch
                    pure $ PoolRetirementCertificate poolId <$> mRetirementEpoch

                parseMargin = mkMargin
                    <$> fromPersistValue @Word64 fieldMarginNumerator
                    <*> fromPersistValue @Word64 fieldMarginDenominator
                  where
                    mkMargin n d = unsafeMkPercentage $ toRational $ n % d

                parseMetadata = do
                    u <- fromPersistValue fieldMetadataUrl
                    h <- fromPersistValue fieldMetadataHash
                    pure $ (,) <$> u <*> h

        rollbackTo point = do
            -- TODO(ADP-356): What if the conversion blocks or fails?
            --
            -- Missing a rollback would be bad.
            EpochNo epoch <- liftIO $ interpretQuery ti (epochOf point)
            deleteWhere [ StakeDistributionEpoch >. fromIntegral epoch ]

            deleteWhere [ PoolProductionSlot >. point ]
            deleteWhere [ PoolRegistrationSlot >. point ]
            deleteWhere [ PoolRetirementSlot >. point ]
            deleteWhere [ BlockSlot >. point ]
            -- TODO: remove dangling metadata no longer attached to a pool

        putDelistedPools pools = do
            deleteWhere ([] :: [Filter PoolDelistment])
            insertMany_ $ fmap PoolDelistment pools

        readDelistedPools =
            fmap (delistedPoolId . entityVal) <$> selectList [] []

        removePools = mapM_ $ \pool -> do
            liftIO $ traceWith tr $ MsgRemovingPool pool
            deleteWhere [ PoolProductionPoolId ==. pool ]
            deleteWhere [ PoolOwnerPoolId ==. pool ]
            deleteWhere [ PoolRegistrationPoolId ==. pool ]
            deleteWhere [ PoolRetirementPoolId ==. pool ]
            deleteWhere [ StakeDistributionPoolId ==. pool ]

        removeRetiredPools epoch =
            bracketTracer traceOuter action
          where
            action = listRetiredPools epoch >>= \retirementCerts -> do
                traceInner retirementCerts
                removePools (view #poolId <$> retirementCerts)
                pure retirementCerts
            traceOuter = tr
                & natTracer liftIO
                & contramap (MsgRemovingRetiredPoolsForEpoch epoch)
            traceInner = liftIO
                . traceWith tr
                . MsgRemovingRetiredPools

        readPoolProductionCursor k = do
            reverse . map (snd . fromPoolProduction . entityVal) <$> selectList
                []
                [Desc PoolProductionSlot, LimitTo k]

        readSystemSeed = do
            mseed <- selectFirst [] []
            case mseed of
                Nothing -> do
                    seed <- liftIO newStdGen
                    insert_ (ArbitrarySeed seed)
                    return seed
                Just seed ->
                    return $ seedSeed $ entityVal seed

        readSettings = do
            l <- selectList
                []
                -- only ever read the first row
                [Asc SettingsId, LimitTo 1]
            case l of
                [] -> pure defaultSettings
                (x:_) -> pure . fromSettings . entityVal $ x

        putSettings =
            repsert
                -- only ever write the first row
                (SettingsKey 1)
            . toSettings

        readLastMetadataGC = do
            -- only ever read the first row
            result <- selectFirst
                []
                [Asc InternalStateId, LimitTo 1]
            pure $ (W.lastMetadataGC . fromInternalState . entityVal) =<< result

        putLastMetadataGC utc = do
            result <- selectFirst
                [ InternalStateId ==. (InternalStateKey 1) ]
                [ ]
            case result of
                Just _ -> update (InternalStateKey 1) [ LastGCMetadata =. Just utc ]
                Nothing -> insert_ (InternalState $ Just utc)

        cleanDB = do
            deleteWhere ([] :: [Filter PoolProduction])
            deleteWhere ([] :: [Filter PoolOwner])
            deleteWhere ([] :: [Filter PoolRegistration])
            deleteWhere ([] :: [Filter PoolRetirement])
            deleteWhere ([] :: [Filter PoolDelistment])
            deleteWhere ([] :: [Filter StakeDistribution])
            deleteWhere ([] :: [Filter PoolMetadata])
            deleteWhere ([] :: [Filter PoolMetadataFetchAttempts])
            deleteWhere ([] :: [Filter TH.BlockHeader])
            deleteWhere ([] :: [Filter Settings])
            deleteWhere ([] :: [Filter InternalState])

        atomically :: forall a. (SqlPersistT IO a -> IO a)
        atomically = runQuery

        readPoolRegistration poolId = do
            result <- selectFirst
                [ PoolRegistrationPoolId ==. poolId ]
                [ Desc PoolRegistrationSlot
                , Desc PoolRegistrationSlotInternalIndex
                ]
            forM result $ \meta -> do
                let PoolRegistration
                        _poolId
                        slotNo
                        slotInternalIndex
                        marginNum
                        marginDen
                        poolCost_
                        poolPledge_
                        poolMetadataUrl
                        poolMetadataHash = entityVal meta
                let poolMargin = unsafeMkPercentage $
                        toRational $ marginNum % marginDen
                let poolCost = W.Coin poolCost_
                let poolPledge = W.Coin poolPledge_
                let poolMetadata = (,) <$> poolMetadataUrl <*> poolMetadataHash
                poolOwners <- fmap (poolOwnerOwner . entityVal) <$>
                    selectList
                        [ PoolOwnerPoolId
                            ==. poolId
                        , PoolOwnerSlot
                            ==. slotNo
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
                let cpt = CertificatePublicationTime {slotNo, slotInternalIndex}
                pure (cpt, cert)

        readPoolRetirement poolId = do
            result <- selectFirst
                [ PoolRetirementPoolId ==. poolId ]
                [ Desc PoolRetirementSlot
                , Desc PoolRetirementSlotInternalIndex
                ]
            forM result $ \meta -> do
                let PoolRetirement
                        _poolId
                        slotNo
                        slotInternalIndex
                        retirementEpochNo = entityVal meta
                let retirementEpoch = EpochNo (fromIntegral retirementEpochNo)
                let cert = PoolRetirementCertificate {poolId, retirementEpoch}
                let cpt = CertificatePublicationTime {slotNo, slotInternalIndex}
                pure (cpt, cert)

        putHeader point =
            let record = mkBlockHeader point
                key = TH.blockHeight record
            in repsert (BlockHeaderKey key) record

        listHeaders k = do
            reverse . fmap (fromBlockHeaders . entityVal) <$> selectList [ ]
                [ Desc BlockHeight
                , LimitTo k
                ]

-- | Defines a raw SQL query, runnable with 'runRawQuery'.
--
data RawQuery a b = RawQuery
    { queryName :: Text
      -- ^ The name of the query.
    , queryDefinition :: Text
      -- ^ The SQL definition of the query.
    , queryParameters :: [PersistValue]
      -- ^ Parameters of the query.
    , queryParser :: a -> Either Text b
      -- ^ A parser for a row of the result.
    }

-- | Runs a raw SQL query, logging any parse failures that occur.
--
runRawQuery
    :: forall a b. RawSql a
    => Tracer IO PoolDbLog
    -> RawQuery a b
    -> SqlPersistT IO [b]
runRawQuery tr q = do
    (failures, results) <- partitionEithers . fmap (queryParser q) <$> rawSql
        (queryDefinition q)
        (queryParameters q)
    forM_ failures
        $ liftIO
        . traceWith tr
        . MsgParseFailure
        . ParseFailure (queryName q)
    pure results

createViews :: [ManualMigration]
createViews = ManualMigration <$>
    [ createView activePoolLifeCycleData
    , createView activePoolOwners
    , createView activePoolRegistrations
    , createView activePoolRetirements
    ]

-- | Represents a database view.
--
data DatabaseView = DatabaseView
    { databaseViewName :: Text
      -- ^ A name for the view.
    , databaseViewDefinition :: Text
      -- ^ A select query to generate the view.
    }

-- | Creates the specified database view, if it does not already exist.
--
createView :: DatabaseView -> Sqlite.Connection -> IO ()
createView (DatabaseView name definition) conn = do
    deleteQuery <- Sqlite.prepare conn deleteQueryString
    Sqlite.step deleteQuery *> Sqlite.finalize deleteQuery
    createQuery <- Sqlite.prepare conn createQueryString
    Sqlite.step createQuery *> Sqlite.finalize createQuery
  where
    deleteQueryString = T.unlines
        [ "DROP VIEW IF EXISTS"
        , name
        , ";"
        ]
    createQueryString = T.unlines
        [ "CREATE VIEW"
        , name
        , "AS"
        , definition
        ]

-- | Views active lifecycle data for every pool in the set of known pools.
--
-- This view has exactly ONE row for each known pool, where each row
-- corresponds to the most-recently-seen registration certificate,
-- retirement certificate, and set of owners for that pool.
--
-- This view does NOT exclude pools that have retired.
--
activePoolLifeCycleData :: DatabaseView
activePoolLifeCycleData = DatabaseView "active_pool_lifecycle_data" [i|
    SELECT
        active_pool_registrations.pool_id as pool_id,
        active_pool_retirements.retirement_epoch as retirement_epoch,
        active_pool_owners.pool_owners as pool_owners,
        cost,
        pledge,
        margin_numerator,
        margin_denominator,
        metadata_hash,
        metadata_url
    FROM
        active_pool_registrations
    LEFT JOIN
        active_pool_retirements
    ON active_pool_registrations.pool_id = active_pool_retirements.pool_id
    LEFT JOIN
        active_pool_owners
    ON active_pool_registrations.pool_id = active_pool_owners.pool_id;
|]

-- | Views the set of active owners for all pools.
--
-- This view has exactly ONE row for each known pool, where each row
-- corresponds to the most-recently-seen set of owners for that pool.
--
-- This view does NOT exclude pools that have retired.
--
activePoolOwners :: DatabaseView
activePoolOwners = DatabaseView "active_pool_owners" [i|
    SELECT pool_id, pool_owners FROM (
        SELECT row_number() OVER w AS r, *
        FROM (
            SELECT
                pool_id,
                slot,
                slot_internal_index,
                group_concat(pool_owner, ' ') as pool_owners
            FROM (
                SELECT * FROM pool_owner ORDER BY pool_owner_index
            )
            GROUP BY pool_id, slot, slot_internal_index
        )
        WINDOW w AS (ORDER BY pool_id, slot desc, slot_internal_index desc)
    )
    GROUP BY pool_id;
|]

-- | Views the set of pool registrations that are currently active.
--
-- This view has exactly ONE row for each known pool, where each row
-- corresponds to the most-recently-seen registration certificate for
-- that pool.
--
-- This view does NOT exclude pools that have retired.
--
activePoolRegistrations :: DatabaseView
activePoolRegistrations = DatabaseView "active_pool_registrations" [i|
    SELECT
        pool_id,
        cost,
        pledge,
        margin_numerator,
        margin_denominator,
        metadata_hash,
        metadata_url
    FROM (
        SELECT row_number() OVER w AS r, *
        FROM pool_registration
        WINDOW w AS (ORDER BY pool_id, slot desc, slot_internal_index desc)
    )
    GROUP BY pool_id;
|]

-- | Views the set of pool retirements that are currently active.
--
-- This view includes all pools for which there are published retirement
-- certificates that have not been revoked or superseded.
--
-- This view does NOT include:
--
--    - pools for which there are no published retirement certificates.
--
--    - pools that have had their most-recently-published retirement
--      certificates revoked by subsequent re-registration certificates.
--
activePoolRetirements :: DatabaseView
activePoolRetirements = DatabaseView "active_pool_retirements" [i|
    SELECT * FROM (
        SELECT
            pool_id,
            retirement_epoch
        FROM (
            SELECT row_number() OVER w AS r, *
            FROM (
                SELECT
                    pool_id, slot, slot_internal_index,
                    NULL as retirement_epoch
                    FROM pool_registration
                UNION
                SELECT
                    pool_id, slot, slot_internal_index,
                    epoch as retirement_epoch
                    FROM pool_retirement
            )
            WINDOW w AS (ORDER BY pool_id, slot desc, slot_internal_index desc)
        )
        GROUP BY pool_id
    )
    WHERE retirement_epoch IS NOT NULL;
|]

-- | 'Temporary', catches migration error from previous versions and if any,
-- _removes_ the database file completely before retrying to start the database.
--
-- This comes in handy to fix database schema in a non-backward compatible way
-- without altering too much the user experience. Indeed, the pools' database
-- can swiftly be re-synced from the chain, so instead of patching our mistakes
-- with ugly work-around we can, at least for now, reset it semi-manually when
-- needed to keep things tidy here.
handlingPersistError
    :: Tracer IO PoolDbLog
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
    :: TimeInterpreter IO
    -> EpochNo
    -> SqlPersistT IO [PoolProduction]
selectPoolProduction ti epoch = do
    (e, eplus1) <- liftIO $ interpretQuery ti
        ((,) <$> firstSlotInEpoch epoch <*> firstSlotInEpoch (epoch + 1))
    fmap entityVal <$> selectList
        [ PoolProductionSlot >=. e
        , PoolProductionSlot <. eplus1 ]
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
    , poolProductionSlot = view #slotNo block
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
        { slotNo = slot
        , blockHeight = Quantity height
        , headerHash = getBlockId headerH
        , parentHeaderHash = getBlockId parentH
        }
    )

mkBlockHeader
    :: BlockHeader
    -> TH.BlockHeader
mkBlockHeader block = TH.BlockHeader
    { blockSlot = view #slotNo block
    , blockHeaderHash = BlockId (headerHash block)
    , blockParentHash = BlockId (parentHeaderHash block)
    , TH.blockHeight = getQuantity (blockHeight block)
    }

fromBlockHeaders :: TH.BlockHeader -> BlockHeader
fromBlockHeaders h =
    BlockHeader blockSlot
        (Quantity blockHeight)
        (getBlockId blockHeaderHash)
        (getBlockId blockParentHash)
  where
    TH.BlockHeader
        { blockSlot
        , blockHeight
        , blockHeaderHash
        , blockParentHash
        } = h

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

fromSettings
    :: Settings
    -> W.Settings
fromSettings (Settings pms) = W.Settings pms

toSettings
    :: W.Settings
    -> Settings
toSettings (W.Settings pms) = Settings pms

fromInternalState
    :: InternalState
    -> W.InternalState
fromInternalState (InternalState utc) = W.InternalState utc
