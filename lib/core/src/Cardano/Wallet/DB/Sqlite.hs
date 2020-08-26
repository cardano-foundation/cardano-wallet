{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Cardano.Wallet.DB.Sqlite
    ( newDBLayer
    , newDBFactory
    , findDatabases
    , withDBLayer

    -- * Interfaces
    , PersistState (..)

    -- * Migration Support
    , DefaultFieldValues (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.DB.Sqlite
    ( DBField (..)
    , DBLog (..)
    , ManualMigration (..)
    , SqliteContext (..)
    , chunkSize
    , dbChunked
    , destroyDBLayer
    , fieldName
    , fieldType
    , handleConstraint
    , startSqliteBackend
    , tableName
    )
import Cardano.DB.Sqlite.Delete
    ( deleteSqliteDatabase, newRefCount, waitForFree, withRef )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrRemovePendingTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Sqlite.TH
    ( Checkpoint (..)
    , DelegationCertificate (..)
    , DelegationReward (..)
    , EntityField (..)
    , Key (..)
    , PrivateKey (..)
    , ProtocolParameters (..)
    , RndState (..)
    , RndStateAddress (..)
    , RndStatePendingAddress (..)
    , SeqState (..)
    , SeqStateAddress (..)
    , SeqStatePendingIx (..)
    , StakeKeyCertificate (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxWithdrawal (..)
    , UTxO (..)
    , Wallet (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), HDPassphrase (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, firstSlotInEpoch, startTime )
import Control.Concurrent.MVar
    ( modifyMVar, modifyMVar_, newMVar, readMVar )
import Control.Exception
    ( Exception, bracket, throwIO )
import Control.Monad
    ( forM, unless, void, when )
import Control.Monad.Extra
    ( concatMapM )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isRight )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List
    ( unzip3 )
import Data.List.Split
    ( chunksOf )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word16, Word32, Word64 )
import Database.Persist.Class
    ( toPersistValue )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , Update (..)
    , deleteCascadeWhere
    , deleteWhere
    , insertMany_
    , insert_
    , rawExecute
    , repsert
    , repsertMany
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (/<-.)
    , (<-.)
    , (<.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Database.Persist.Types
    ( PersistValue (PersistText), fromPersistValueText )
import Fmt
    ( pretty )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( doesFileExist, listDirectory )
import System.FilePath
    ( (</>) )

import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: forall s k a.
        ( PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> TimeInterpreter IO
    -> ((SqliteContext, DBLayer IO s k) -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer trace defaultFieldValues mDatabaseDir timeInterpreter =
    bracket before after
  where
    before = newDBLayer trace defaultFieldValues mDatabaseDir timeInterpreter
    after = destroyDBLayer . fst

-- | Instantiate a 'DBFactory' from a given directory
newDBFactory
    :: forall s k.
        ( PersistState s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> TimeInterpreter IO

    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> IO (DBFactory IO s k)
newDBFactory tr defaultFieldValues timeInterpreter = \case
    Nothing -> do
        -- NOTE
        -- For the in-memory database, we do actually preserve the database
        -- after the 'action' is done. This allows for calling 'withDatabase'
        -- several times within the same execution and get back the same
        -- database. The memory is only cleaned up when calling
        -- 'removeDatabase', to mimic the way the file database works!
        mvar <- newMVar mempty
        pure DBFactory
            { withDatabase = \wid action -> do
                db <- modifyMVar mvar $ \m -> case Map.lookup wid m of
                    Just (_, db) -> pure (m, db)
                    Nothing -> do
                        (ctx, db) <-
                            newDBLayer tr defaultFieldValues Nothing timeInterpreter
                        pure (Map.insert wid (ctx, db) m, db)
                action db
            , removeDatabase = \wid -> do
                traceWith tr $ MsgRemoving (pretty wid)
                modifyMVar_ mvar (pure . Map.delete wid)

            , listDatabases =
                Map.keys <$> readMVar mvar
            }

    Just databaseDir -> do
        refs <- newRefCount
        pure DBFactory
            { withDatabase = \wid action -> withRef refs wid $ withDBLayer
                tr
                defaultFieldValues
                (Just $ databaseFile wid)
                timeInterpreter
                (action . snd)
            , removeDatabase = \wid -> do
                let widp = pretty wid
                -- try to wait for all 'withDatabase' calls to finish before
                -- deleting database file.
                let trWait = contramap (MsgWaitingForDatabase widp) tr
                waitForFree trWait refs wid $ \inUse -> do
                    unless (inUse == 0) $
                        traceWith tr $ MsgRemovingInUse widp inUse
                    traceWith tr $ MsgRemoving widp
                    let trDel = contramap (MsgRemovingDatabaseFile widp) tr
                    deleteSqliteDatabase trDel (databaseFile wid)
            , listDatabases =
                findDatabases @k tr databaseDir
            }
      where
        databaseFilePrefix = keyTypeDescriptor $ Proxy @k
        databaseFile wid =
            databaseDir </>
            databaseFilePrefix <> "." <>
            T.unpack (toText wid) <> ".sqlite"

-- | Return all wallet databases that match the specified key type within the
--   specified directory.
findDatabases
    :: forall k. WalletKey k
    => Tracer IO DBLog
    -> FilePath
    -> IO [W.WalletId]
findDatabases tr dir = do
    files <- listDirectory dir
    fmap catMaybes $ forM files $ \file -> do
        isFile <- doesFileExist (dir </> file)
        case (isFile, T.splitOn "." $ T.pack file) of
            (True, prefix : basename : ["sqlite"]) | prefix == expectedPrefix ->
                case fromText basename of
                    Right wid -> do
                        traceWith tr $ MsgFoundDatabase (dir </> file) (toText wid)
                        return (Just wid)
                    _ -> do
                        traceWith tr $ MsgUnknownDBFile file
                        return Nothing
            _ -> return Nothing
  where
    expectedPrefix = T.pack $ keyTypeDescriptor $ Proxy @k

-- | A data-type for capturing column status. Used to be represented as a
-- 'Maybe Bool' which is somewhat confusing to interpret.
data SqlColumnStatus
    = TableMissing
    | ColumnMissing
    | ColumnPresent
    deriving Eq

-- | Executes any manual database migration steps that may be required on
--   startup.
--
migrateManually
    :: Tracer IO DBLog
    -> DefaultFieldValues
    -> ManualMigration
migrateManually tr defaultFieldValues =
    ManualMigration $ \conn -> do
        assignDefaultPassphraseScheme conn

        addActiveSlotCoefficientIfMissing conn

        addDesiredPoolNumberIfMissing conn

        addMinimumUTxOValueIfMissing conn

        addHardforkEpochIfMissing conn

        -- FIXME
        -- Temporary migration to fix Daedalus flight wallets. This should
        -- really be removed as soon as we have a fix for the cardano-sl:wallet
        -- currently in production.
        removeSoftRndAddresses conn

        removeOldTxParametersTable conn

        addAddressStateIfMissing conn
  where
    -- NOTE
    -- Wallets created before the 'PassphraseScheme' was introduced have no
    -- passphrase scheme set in the database. Yet, their passphrase is known
    -- to use the default / new scheme (i.e. PBKDF2) and, it is impossible
    -- to have a wallet with a scheme but no last update. Either they should
    -- have both, or they should have none.
    --
    --     Creation Method               | Scheme | Last Update
    --     ---                           | ---    | ---
    --     Byron, from mnemonic          | ✓      | ✓
    --     Byron, from xprv              | ✓      | ✓
    --     Shelley, from mnemonic        | ✓      | ✓
    --     Shelley, from account pub key | ø      | ø
    assignDefaultPassphraseScheme :: Sqlite.Connection -> IO ()
    assignDefaultPassphraseScheme conn = do
        isFieldPresent conn passphraseScheme >>= \case
            TableMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded passphraseScheme
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded passphraseScheme
                query <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName passphraseScheme
                    , "ADD COLUMN", fieldName passphraseScheme
                    , fieldType passphraseScheme, " NULL"
                    , ";"
                    ]
                Sqlite.step query *> Sqlite.finalize query
                assignDefaultPassphraseScheme conn -- loop to apply case below
            ColumnPresent  -> do
                value <- either (fail . show) (\x -> pure $ "\"" <> x <> "\"") $
                    fromPersistValueText (toPersistValue W.EncryptWithPBKDF2)
                traceWith tr $ MsgManualMigrationNeeded passphraseScheme value
                query <- Sqlite.prepare conn $ T.unwords
                    [ "UPDATE", tableName passphraseScheme
                    , "SET", fieldName passphraseScheme, "=", value
                    , "WHERE", fieldName passphraseScheme, "IS NULL"
                    , "AND", fieldName passphraseLastUpdatedAt, "IS NOT NULL"
                    , ";"
                    ]
                Sqlite.step query *> Sqlite.finalize query
      where
        passphraseScheme = DBField WalPassphraseScheme
        passphraseLastUpdatedAt = DBField WalPassphraseLastUpdatedAt

    -- | Remove any addresses that were wrongly generated in previous releases.
    -- See comment below in 'selectState' from 'RndState'.
    --
    -- Important: this _may_ remove USED addresses from the discovered set which
    -- is _okay-ish_ for two reasons:
    --
    --     1. Address will still be discovered in UTxOs and this won't affect
    --     users' balance. But the address won't show up when in the listing.
    --     This is a wanted behavior.
    --
    --     2. The discovered list of address is really used internally to avoid
    --     index clash when generating new change addresses. Since we'll
    --     generate addresses from a completely different part of the HD tree
    --     ANYWAY, there's no risk of clash.
    removeSoftRndAddresses :: Sqlite.Connection -> IO ()
    removeSoftRndAddresses conn = do
        isFieldPresent conn rndAccountIx >>= \case
            TableMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded rndAccountIx
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded rndAccountIx
            ColumnPresent -> do
                traceWith tr $ MsgManualMigrationNeeded rndAccountIx hardLowerBound
                stmt <- Sqlite.prepare conn $ T.unwords
                    [ "DELETE FROM", tableName rndAccountIx
                    , "WHERE", fieldName rndAccountIx, "<", hardLowerBound
                    , ";"
                    ]
                _ <- Sqlite.step stmt
                Sqlite.finalize stmt
      where
        hardLowerBound = toText $ fromEnum $ minBound @(Index 'Hardened _)
        rndAccountIx   = DBField RndStateAddressAccountIndex

    -- | Adds an 'active_slot_coeff' column to the 'checkpoint' table if
    --   it is missing.
    --
    addActiveSlotCoefficientIfMissing :: Sqlite.Connection -> IO ()
    addActiveSlotCoefficientIfMissing conn =
        addColumn_ conn True (DBField CheckpointActiveSlotCoeff) value
      where
        value = toText
            $ W.unActiveSlotCoefficient
            $ defaultActiveSlotCoefficient defaultFieldValues

    -- | Adds an 'desired_pool_number' column to the 'protocol_parameters'
    -- table if it is missing.
    --
    addDesiredPoolNumberIfMissing :: Sqlite.Connection -> IO ()
    addDesiredPoolNumberIfMissing conn = do
        addColumn_ conn True (DBField ProtocolParametersDesiredNumberOfPools) value
      where
        value = T.pack $ show $ defaultDesiredNumberOfPool defaultFieldValues

    -- | Adds an 'minimum_utxo_value' column to the 'protocol_parameters'
    -- table if it is missing.
    --
    addMinimumUTxOValueIfMissing :: Sqlite.Connection -> IO ()
    addMinimumUTxOValueIfMissing conn = do
        addColumn_ conn True (DBField ProtocolParametersMinimumUtxoValue) value
      where
        value = T.pack $ show $ W.getCoin $ defaultMinimumUTxOValue defaultFieldValues

    -- | Adds an 'hardfork_epoch' column to the 'protocol_parameters'
    -- table if it is missing.
    --
    addHardforkEpochIfMissing :: Sqlite.Connection -> IO ()
    addHardforkEpochIfMissing conn = do
        addColumn_ conn False (DBField ProtocolParametersHardforkEpoch) value
      where
        value = case defaultHardforkEpoch defaultFieldValues of
            Nothing -> "NULL"
            Just v -> T.pack $ show $ W.unEpochNo v

    -- | This table became @protocol_parameters@.
    removeOldTxParametersTable :: Sqlite.Connection -> IO ()
    removeOldTxParametersTable conn = do
        dropTable <- Sqlite.prepare conn "DROP TABLE IF EXISTS tx_parameters;"
        void $ Sqlite.stepConn conn dropTable
        Sqlite.finalize dropTable

    -- | In order to make listing addresses bearable for large wallet, we
    -- altered the discovery process to mark addresses as used as they are
    -- discovered. Existing databases don't have that pre-computed field.
    addAddressStateIfMissing :: Sqlite.Connection -> IO ()
    addAddressStateIfMissing conn = do
        _  <- addColumn conn False (DBField SeqStateAddressStatus) (toText W.Unused)
        st <- addColumn conn False (DBField RndStateAddressStatus) (toText W.Unused)
        when (st == ColumnMissing) $ do
            markAddressesAsUsed (DBField SeqStateAddressStatus)
            markAddressesAsUsed (DBField RndStateAddressStatus)
      where
        markAddressesAsUsed field = do
            query <- Sqlite.prepare conn $ T.unwords
                [ "UPDATE", tableName field
                , "SET status = '" <> toText W.Used <> "'"
                , "WHERE", tableName field <> ".address", "IN"
                , "(SELECT DISTINCT(address) FROM tx_out)"
                ]
            _ <- Sqlite.step query
            Sqlite.finalize query

    -- | Determines whether a field is present in its parent table.
    isFieldPresent :: Sqlite.Connection -> DBField -> IO SqlColumnStatus
    isFieldPresent conn field = do
        getCheckpointTableInfo <- Sqlite.prepare conn $ mconcat
            [ "SELECT sql FROM sqlite_master "
            , "WHERE type = 'table' "
            , "AND name = '" <> tableName field <> "';"
            ]
        row <- Sqlite.step getCheckpointTableInfo
            >> Sqlite.columns getCheckpointTableInfo
        Sqlite.finalize getCheckpointTableInfo
        pure $ case row of
            [PersistText t]
                | fieldName field `T.isInfixOf` t -> ColumnPresent
                | otherwise                       -> ColumnMissing
            _ -> TableMissing

    addColumn_
        :: Sqlite.Connection
        -> Bool
        -> DBField
        -> Text
        -> IO ()
    addColumn_ a b c =
        void . addColumn a b c

    -- | A migration for adding a non-existing column to a table. Factor out as
    -- it's a common use-case.
    addColumn
        :: Sqlite.Connection
        -> Bool
        -> DBField
        -> Text
        -> IO SqlColumnStatus
    addColumn conn notNull field value = do
        isFieldPresent conn field >>= \st -> st <$ case st of
            TableMissing ->
                traceWith tr $ MsgManualMigrationNotNeeded field
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNeeded field value
                query <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName field
                    , "ADD COLUMN", fieldName field
                    , fieldType field, if notNull then "NOT NULL" else ""
                    , "DEFAULT", value
                    , ";"
                    ]
                _ <- Sqlite.step query
                Sqlite.finalize query
            ColumnPresent ->
                traceWith tr $ MsgManualMigrationNotNeeded field

-- | A set of default field values that can be consulted when performing a
--   database migration.
--
data DefaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient :: W.ActiveSlotCoefficient
    , defaultDesiredNumberOfPool :: Word16
    , defaultMinimumUTxOValue :: W.Coin
    , defaultHardforkEpoch :: Maybe W.EpochNo
    }

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
    :: forall s k.
        ( PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database file, or Nothing for in-memory database
    -> TimeInterpreter IO
    -> IO (SqliteContext, DBLayer IO s k)
newDBLayer trace defaultFieldValues mDatabaseFile timeInterpreter = do
    ctx@SqliteContext{runQuery} <-
        either throwIO pure =<<
        startSqliteBackend
            (migrateManually trace defaultFieldValues)
            migrateAll
            trace
            mDatabaseFile
    return (ctx, DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { initializeWallet = \(PrimaryKey wid) cp meta txs pp -> ExceptT $ do
            res <- handleConstraint (ErrWalletAlreadyExists wid) $
                insert_ (mkWalletEntity wid meta)
            when (isRight res) $ do
                insertCheckpoint wid cp
                let (metas, txins, txouts, txws) = mkTxHistory wid txs
                putTxMetas metas
                putTxs txins txouts txws
                insert_ (mkProtocolParametersEntity wid pp)
            pure res

        , removeWallet = \(PrimaryKey wid) -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> do
                    deleteCascadeWhere [WalId ==. wid]
                    deleteLooseTransactions

        , listWallets =
            map (PrimaryKey . unWalletKey) <$> selectKeysList [] [Asc WalId]

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey wid) cp -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> insertCheckpoint wid cp

        , readCheckpoint = \(PrimaryKey wid) -> do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure Nothing
                Just cp -> do
                    utxo <- selectUTxO cp
                    s <- selectState (checkpointId cp)
                    pure (checkpointFromEntity @s cp utxo <$> s)

        , listCheckpoints = \(PrimaryKey wid) -> do
            map (blockHeaderFromEntity . entityVal) <$> selectList
                [ CheckpointWalletId ==. wid ]
                [ Asc CheckpointSlot ]

        , rollbackTo = \(PrimaryKey wid) requestedPoint -> ExceptT $ do
            findNearestPoint wid requestedPoint >>= \case
                Nothing -> selectLatestCheckpoint wid >>= \case
                    Nothing ->
                        pure $ Left $ ErrNoSuchWallet wid
                    Just _  ->
                        lift $ throwIO (ErrNoOlderCheckpoint wid requestedPoint)
                Just nearestPoint -> do
                    deleteCheckpoints wid
                        [ CheckpointSlot >. nearestPoint
                        ]
                    deleteDelegationCertificates wid
                        [ CertSlot >. nearestPoint
                        ]
                    updateTxMetas wid
                        [ TxMetaDirection ==. W.Outgoing
                        , TxMetaSlot >. nearestPoint
                        ]
                        [ TxMetaStatus =. W.Pending
                        , TxMetaSlot =. nearestPoint
                        ]
                    deleteTxMetas wid
                        [ TxMetaDirection ==. W.Incoming
                        , TxMetaSlot >. nearestPoint
                        ]
                    deleteStakeKeyCerts wid
                        [ StakeKeyCertSlot >. nearestPoint
                        ]
                    pure (Right nearestPoint)

        , prune = \(PrimaryKey wid) -> ExceptT $ do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just cp -> Right <$> do
                    pruneCheckpoints wid cp
                    deleteLooseTransactions

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \(PrimaryKey wid) meta -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    updateWhere [WalId ==. wid]
                        (mkWalletMetadataUpdate meta)
                    pure $ Right ()

        , readWalletMeta = \(PrimaryKey wid) -> do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure Nothing
                Just cp -> do
                    currentEpoch <- liftIO $
                        timeInterpreter (epochOf $ cp ^. #checkpointSlot)
                    readWalletDelegation timeInterpreter wid currentEpoch
                        >>= readWalletMetadata wid

        , putDelegationCertificate = \(PrimaryKey wid) cert sl -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> case cert of
                    W.CertDelegateNone _ -> do
                        repsert
                            (DelegationCertificateKey wid sl)
                            (DelegationCertificate wid sl Nothing)
                        pure <$> repsert
                            (StakeKeyCertificateKey wid sl)
                            (StakeKeyCertificate wid sl W.StakeKeyDeregistration)
                    W.CertDelegateFull _ pool ->
                        pure <$> repsert
                            (DelegationCertificateKey wid sl)
                            (DelegationCertificate wid sl (Just pool))
                    W.CertRegisterKey _ ->
                        pure <$> repsert
                            (StakeKeyCertificateKey wid sl)
                            (StakeKeyCertificate wid sl W.StakeKeyRegistration)

        , isStakeKeyRegistered = \(PrimaryKey wid) -> ExceptT $ do
              selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just{} -> do
                    val <- fmap entityVal <$> selectFirst
                        [StakeKeyCertWalletId ==. wid]
                        [Desc StakeKeyCertSlot]
                    return $ case val of
                        Nothing -> Right False
                        Just (StakeKeyCertificate _ _ status) ->
                            Right (status == W.StakeKeyRegistration)

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    let (metas, txins, txouts, txws) = mkTxHistory wid txs
                    putTxMetas metas
                    putTxs txins txouts txws
                    pure $ Right ()

        , readTxHistory = \(PrimaryKey wid) minWithdrawal order range status -> do
            selectTxHistory
                timeInterpreter wid minWithdrawal order $ catMaybes
                [ (TxMetaSlot >=.) <$> W.inclusiveLowerBound range
                , (TxMetaSlot <=.) <$> W.inclusiveUpperBound range
                , (TxMetaStatus ==.) <$> status
                ]

        , removePendingTx = \(PrimaryKey wid) tid -> ExceptT $ do
            let errNoSuchWallet =
                    Left $ ErrRemovePendingTxNoSuchWallet $ ErrNoSuchWallet wid
            let errNoMorePending =
                    Left $ ErrRemovePendingTxTransactionNoMorePending tid
            let errNoSuchTransaction =
                    Left $ ErrRemovePendingTxNoSuchTransaction tid
            selectWallet wid >>= \case
                Nothing -> pure errNoSuchWallet
                Just _  -> do
                    metas <- selectPendingTxs wid (TxId tid)
                    let isPending meta = txMetaStatus meta == W.Pending
                    case metas of
                        [] -> pure errNoSuchTransaction
                        txs | any isPending txs -> do
                            deletePendingTx wid (TxId tid)
                            pure $ Right ()
                        _ -> pure errNoMorePending

        , getTx = \(PrimaryKey wid) tid -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    metas <- selectTxHistory
                        timeInterpreter wid Nothing W.Descending
                            [ TxMetaTxId ==. (TxId tid) ]
                    case metas of
                        [] -> pure (Right Nothing)
                        meta:_ -> pure (Right $ Just meta)

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey wid) key -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> Right <$> do
                    deleteWhere [PrivateKeyWalletId ==. wid]
                    insert_ (mkPrivateKeyEntity wid key)

        , readPrivateKey = \(PrimaryKey wid) -> selectPrivateKey wid

        {-----------------------------------------------------------------------
                                 Blockchain Parameters
        -----------------------------------------------------------------------}

        , putProtocolParameters = \(PrimaryKey wid) pp -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> repsert
                    (ProtocolParametersKey wid)
                    (mkProtocolParametersEntity wid pp)

        , readProtocolParameters =
            \(PrimaryKey wid) -> selectProtocolParameters wid

        {-----------------------------------------------------------------------
                                 Delegation Rewards
        -----------------------------------------------------------------------}

        , putDelegationRewardBalance =
            \(PrimaryKey wid) (Quantity amt) -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> repsert
                    (DelegationRewardKey wid)
                    (DelegationReward wid amt)

        , readDelegationRewardBalance =
            \(PrimaryKey wid) ->
                maybe minBound (Quantity . rewardAccountBalance . entityVal) <$>
                selectFirst [RewardWalletId ==. wid] []

        {-----------------------------------------------------------------------
                                     ACID Execution
        -----------------------------------------------------------------------}

        , atomically = runQuery

        })

readWalletMetadata
    :: W.WalletId
    -> W.WalletDelegation
    -> SqlPersistT IO (Maybe W.WalletMetadata)
readWalletMetadata wid walDel =
     fmap (metadataFromEntity walDel . entityVal)
        <$> selectFirst [WalId ==. wid] []

readWalletDelegation
    :: TimeInterpreter IO
    -> W.WalletId
    -> W.EpochNo
    -> SqlPersistT IO W.WalletDelegation
readWalletDelegation ti wid epoch
    | epoch == 0 = pure $ W.WalletDelegation W.NotDelegating []
    | otherwise = do
        eMinus1 <- liftIO $ ti $ firstSlotInEpoch (epoch - 1)
        e <- liftIO $ ti $ firstSlotInEpoch epoch
        active <- maybe W.NotDelegating toWalletDelegationStatus
            <$> readDelegationCertificate wid
                [ CertSlot <. eMinus1
                ]

        next <- catMaybes <$> sequence
            [ fmap (W.WalletDelegationNext (epoch + 1) . toWalletDelegationStatus)
                <$> readDelegationCertificate wid
                    [ CertSlot >=. eMinus1
                    , CertSlot <. e
                    ]
            , fmap (W.WalletDelegationNext (epoch + 2) . toWalletDelegationStatus)
                <$> readDelegationCertificate wid
                    [ CertSlot >=. e
                    ]
            ]

        pure $ W.WalletDelegation active next

readDelegationCertificate
    :: W.WalletId
    -> [Filter DelegationCertificate]
    -> SqlPersistT IO (Maybe DelegationCertificate)
readDelegationCertificate wid filters = fmap entityVal
    <$> selectFirst ((CertWalletId ==. wid) : filters) [Desc CertSlot]

toWalletDelegationStatus
    :: DelegationCertificate
    -> W.WalletDelegationStatus
toWalletDelegationStatus = \case
    DelegationCertificate _ _ Nothing ->
        W.NotDelegating
    DelegationCertificate _ _ (Just pool) ->
        W.Delegating pool

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> Wallet
mkWalletEntity wid meta = Wallet
    { walId = wid
    , walName = meta ^. #name . coerce
    , walCreationTime = meta ^. #creationTime
    , walPassphraseLastUpdatedAt = W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walPassphraseScheme = W.passphraseScheme <$> meta ^. #passphraseInfo
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalName =. meta ^. #name . coerce
    , WalCreationTime =. meta ^. #creationTime
    , WalPassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , WalPassphraseScheme =.
        W.passphraseScheme <$> meta ^. #passphraseInfo
    ]

blockHeaderFromEntity :: Checkpoint -> W.BlockHeader
blockHeaderFromEntity cp = W.BlockHeader
    { slotNo = checkpointSlot cp
    , blockHeight = Quantity (checkpointBlockHeight cp)
    , headerHash = getBlockId (checkpointHeaderHash cp)
    , parentHeaderHash = getBlockId (checkpointParentHash cp)
    }

metadataFromEntity :: W.WalletDelegation -> Wallet -> W.WalletMetadata
metadataFromEntity walDelegation wal = W.WalletMetadata
    { name = W.WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo
        <$> walPassphraseLastUpdatedAt wal
        <*> walPassphraseScheme wal
    , delegation = walDelegation
    }

mkPrivateKeyEntity
    :: PersistPrivateKey (k 'RootK)
    => W.WalletId
    -> (k 'RootK XPrv, W.Hash "encryption")
    -> PrivateKey
mkPrivateKeyEntity wid kh = PrivateKey
    { privateKeyWalletId = wid
    , privateKeyRootKey = root
    , privateKeyHash = hash
    }
  where
    (root, hash) = serializeXPrv kh

privateKeyFromEntity
    :: PersistPrivateKey (k 'RootK)
    => PrivateKey
    -> (k 'RootK XPrv, W.Hash "encryption")
privateKeyFromEntity (PrivateKey _ k h) =
    unsafeDeserializeXPrv (k, h)

mkCheckpointEntity
    :: W.WalletId
    -> W.Wallet s
    -> (Checkpoint, [UTxO])
mkCheckpointEntity wid wal =
    (cp, utxo)
  where
    header = W.currentTip wal
    sl = header ^. #slotNo
    (Quantity bh) = header ^. #blockHeight
    gp = W.blockchainParameters wal
    cp = Checkpoint
        { checkpointWalletId = wid
        , checkpointSlot = sl
        , checkpointParentHash = BlockId (header ^. #parentHeaderHash)
        , checkpointHeaderHash = BlockId (header ^. #headerHash)
        , checkpointBlockHeight = bh
        , checkpointGenesisHash = BlockId (coerce (gp ^. #getGenesisBlockHash))
        , checkpointGenesisStart = coerce (gp ^. #getGenesisBlockDate)
        , checkpointSlotLength = coerceSlotLength $ gp ^. #getSlotLength
        , checkpointEpochLength = coerce (gp ^. #getEpochLength)
        , checkpointEpochStability = coerce (gp ^. #getEpochStability)
        , checkpointActiveSlotCoeff =
            W.unActiveSlotCoefficient (gp ^. #getActiveSlotCoefficient)
        , checkpointFeePolicyUnused = ""
        , checkpointTxMaxSizeUnused = 0
        }
    utxo =
        [ UTxO wid sl (TxId input) ix addr coin
        | (W.TxIn input ix, W.TxOut addr coin) <- utxoMap
        ]
    utxoMap = Map.assocs (W.getUTxO (W.utxo wal))

    coerceSlotLength :: W.SlotLength -> Word64
    coerceSlotLength (W.SlotLength x) = toEnum (fromEnum x)

-- note: TxIn records must already be sorted by order
-- and TxOut records must already by sorted by index.
checkpointFromEntity
    :: Checkpoint
    -> [UTxO]
    -> s
    -> W.Wallet s
checkpointFromEntity cp utxo s =
    W.unsafeInitWallet utxo' header s gp
  where
    (Checkpoint
        _walletId
        slot
        (BlockId headerHash)
        (BlockId parentHeaderHash)
        bh
        (BlockId genesisHash)
        genesisStart
        _feePolicyUnused
        slotLength
        epochLength
        _txMaxSizeUnused
        epochStability
        activeSlotCoeff
        ) = cp
    header = (W.BlockHeader slot (Quantity bh) headerHash parentHeaderHash)
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo
        ]
    gp = W.GenesisParameters
        { getGenesisBlockHash = coerce genesisHash
        , getGenesisBlockDate = W.StartTime genesisStart
        , getSlotLength = W.SlotLength (toEnum (fromEnum slotLength))
        , getEpochLength = W.EpochLength epochLength
        , getEpochStability = Quantity epochStability
        , getActiveSlotCoefficient = W.ActiveSlotCoefficient activeSlotCoeff
        }

mkTxHistory
    :: W.WalletId
    -> [(W.Tx, W.TxMeta)]
    -> ([TxMeta], [TxIn], [TxOut], [TxWithdrawal])
mkTxHistory wid txs = flatTxHistory
    [ ( mkTxMetaEntity wid txid meta
      , mkTxInputsOutputs (txid, tx)
      , mkTxWithdrawals (txid, tx)
      )
    | (tx, meta) <- txs
    , let txid = W.txId tx
    ]
  where
    -- | Make flat lists of entities from the result of 'mkTxHistory'.
    flatTxHistory
        :: [(TxMeta, ([TxIn], [TxOut]), [TxWithdrawal])]
        -> ([TxMeta], [TxIn], [TxOut], [TxWithdrawal])
    flatTxHistory entities =
        ( map (\(a,_,_) -> a) entities
        , concatMap (fst . (\(_,b,_) -> b)) entities
        , concatMap (snd . (\(_,b,_) -> b)) entities
        , concatMap (\(_,_,c) -> c) entities
        )

mkTxInputsOutputs
    :: (W.Hash "Tx", W.Tx)
    -> ([TxIn], [TxOut])
mkTxInputsOutputs tx =
    ( (dist mkTxIn . ordered W.resolvedInputs) tx
    , (dist mkTxOut . ordered W.outputs) tx )
  where
    mkTxIn tid (ix, (txIn, amt)) = TxIn
        { txInputTxId = TxId tid
        , txInputOrder = ix
        , txInputSourceTxId = TxId (W.inputId txIn)
        , txInputSourceIndex = W.inputIx txIn
        , txInputSourceAmount = amt
        }
    mkTxOut tid (ix, txOut) = TxOut
        { txOutputTxId = TxId tid
        , txOutputIndex = ix
        , txOutputAddress = W.address txOut
        , txOutputAmount = W.coin txOut
        }
    ordered f = fmap (zip [0..] . f)
    -- | Distribute `a` accross many `b`s using the given function.
    -- >>> dist TxOut (addr, [Coin 1, Coin 42, Coin 14])
    -- [TxOut addr (Coin 1), TxOut addr (Coin 42), TxOut addr (Coin 14)]
    dist :: (a -> b -> c) -> (a, [b]) -> [c]
    dist f (a, bs) = [f a b | b <- bs]

mkTxWithdrawals
    :: (W.Hash "Tx", W.Tx)
    -> [TxWithdrawal]
mkTxWithdrawals (txid, tx) =
    mkTxWithdrawal <$> Map.toList (tx ^. #withdrawals)
  where
    txWithdrawalTxId = TxId txid
    mkTxWithdrawal (txWithdrawalAccount, txWithdrawalAmount) =
        TxWithdrawal
            { txWithdrawalTxId
            , txWithdrawalAccount
            , txWithdrawalAmount
            }

mkTxMetaEntity :: W.WalletId -> W.Hash "Tx" -> W.TxMeta -> TxMeta
mkTxMetaEntity wid txid meta = TxMeta
    { txMetaTxId = TxId txid
    , txMetaWalletId = wid
    , txMetaStatus = meta ^. #status
    , txMetaDirection = meta ^. #direction
    , txMetaSlot = meta ^. #slotNo
    , txMetaBlockHeight = getQuantity (meta ^. #blockHeight)
    , txMetaAmount = getQuantity (meta ^. #amount)
    }

-- note: TxIn records must already be sorted by order
-- and TxOut records must already be sorted by index
txHistoryFromEntity
    :: Monad m
    => TimeInterpreter m
    -> W.BlockHeader
    -> [TxMeta]
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> [TxWithdrawal]
    -> m [W.TransactionInfo]
txHistoryFromEntity ti tip metas ins outs ws =
    mapM mkItem metas
  where
    startTime' = ti . startTime
    mkItem m = mkTxWith (txMetaTxId m) (mkTxMeta m)
    mkTxWith txid meta = do
        t <- startTime' (meta ^. #slotNo)
        return $ W.TransactionInfo
            { W.txInfoId =
                getTxId txid
            , W.txInfoInputs =
                map mkTxIn $ filter ((== txid) . txInputTxId . fst) ins
            , W.txInfoOutputs =
                map mkTxOut $ filter ((== txid) . txOutputTxId) outs
            , W.txInfoWithdrawals =
                Map.fromList $ map mkTxWithdrawal $ filter ((== txid) . txWithdrawalTxId) ws
            , W.txInfoMeta =
                meta
            , W.txInfoDepth =
                Quantity $ fromIntegral $ if tipH > txH then tipH - txH else 0
            , W.txInfoTime = t
            }
      where
        txH  = getQuantity (meta ^. #blockHeight)
        tipH = getQuantity (tip ^. #blockHeight)
    mkTxIn (tx, out) =
        ( W.TxIn
            { W.inputId = getTxId (txInputSourceTxId tx)
            , W.inputIx = txInputSourceIndex tx
            }
        , txInputSourceAmount tx
        , mkTxOut <$> out
        )
    mkTxOut tx = W.TxOut
        { W.address = txOutputAddress tx
        , W.coin = txOutputAmount tx
        }
    mkTxWithdrawal w =
        ( txWithdrawalAccount w
        , txWithdrawalAmount w
        )
    mkTxMeta m = W.TxMeta
        { W.status = txMetaStatus m
        , W.direction = txMetaDirection m
        , W.slotNo = txMetaSlot m
        , W.blockHeight = Quantity (txMetaBlockHeight m)
        , W.amount = Quantity (txMetaAmount m)
        }

mkProtocolParametersEntity
    :: W.WalletId
    -> W.ProtocolParameters
    -> ProtocolParameters
mkProtocolParametersEntity wid pp =
    ProtocolParameters wid fp (getQuantity mx) dl desiredPoolNum minUTxO epochNo
  where
    (W.ProtocolParameters
        (W.DecentralizationLevel dl)
        (W.TxParameters fp mx)
        desiredPoolNum
        minUTxO
        epochNo
        ) = pp

protocolParametersFromEntity
    :: ProtocolParameters
    -> W.ProtocolParameters
protocolParametersFromEntity (ProtocolParameters _ fp mx dl poolNum minUTxO epochNo) =
    W.ProtocolParameters
        (W.DecentralizationLevel dl)
        (W.TxParameters fp (Quantity mx))
        poolNum
        minUTxO
        epochNo

{-------------------------------------------------------------------------------
                                   DB Queries
-------------------------------------------------------------------------------}

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid =
    fmap entityVal <$> selectFirst [WalId ==. wid] []

insertCheckpoint
    :: forall s. (PersistState s)
    => W.WalletId
    -> W.Wallet s
    -> SqlPersistT IO ()
insertCheckpoint wid wallet = do
    let (cp, utxo) = mkCheckpointEntity wid wallet
    let sl = (W.currentTip wallet) ^. #slotNo
    deleteCheckpoints wid [CheckpointSlot ==. sl]
    insert_ cp
    dbChunked insertMany_ utxo
    insertState (wid, sl) (W.getState wallet)

-- | Delete one or all checkpoints associated with a wallet.
deleteCheckpoints
    :: W.WalletId
    -> [Filter Checkpoint]
    -> SqlPersistT IO ()
deleteCheckpoints wid filters = do
    deleteCascadeWhere ((CheckpointWalletId ==. wid) : filters)

-- | Prune checkpoints in the database to keep it tidy
pruneCheckpoints
    :: W.WalletId
    -> Checkpoint
    -> SqlPersistT IO ()
pruneCheckpoints wid cp = do
    let height = Quantity $ fromIntegral $ checkpointBlockHeight cp
    let epochStability = Quantity $ checkpointEpochStability cp
    let cps = sparseCheckpoints epochStability height
    deleteCheckpoints wid [ CheckpointBlockHeight /<-. cps ]

-- | Delete TxMeta values for a wallet.
deleteTxMetas
    :: W.WalletId
    -> [Filter TxMeta]
    -> SqlPersistT IO ()
deleteTxMetas wid filters =
    deleteWhere ((TxMetaWalletId ==. wid) : filters)


-- | Delete stake key certificates for a wallet.
deleteStakeKeyCerts
    :: W.WalletId
    -> [Filter StakeKeyCertificate]
    -> SqlPersistT IO ()
deleteStakeKeyCerts wid filters =
    deleteWhere ((StakeKeyCertWalletId ==. wid) : filters)

updateTxMetas
    :: W.WalletId
    -> [Filter TxMeta]
    -> [Update TxMeta]
    -> SqlPersistT IO ()
updateTxMetas wid filters =
    updateWhere ((TxMetaWalletId ==. wid) : filters)

-- | Add new TxMeta rows, overwriting existing ones.
putTxMetas :: [TxMeta] -> SqlPersistT IO ()
putTxMetas metas = dbChunked repsertMany
    [(TxMetaKey txMetaTxId txMetaWalletId, m) | m@TxMeta{..} <- metas]

-- | Insert multiple transactions, removing old instances first.
putTxs :: [TxIn] -> [TxOut] -> [TxWithdrawal] -> SqlPersistT IO ()
putTxs txins txouts txws = do
    dbChunked repsertMany
        [ (TxInKey txInputTxId txInputSourceTxId txInputSourceIndex, i)
        | i@TxIn{..} <- txins ]
    dbChunked repsertMany
        [ (TxOutKey txOutputTxId txOutputIndex, o)
        | o@TxOut{..} <- txouts ]
    dbChunked repsertMany
        [ (TxWithdrawalKey txWithdrawalTxId txWithdrawalAccount, w)
        | w@TxWithdrawal{..} <- txws ]

-- | Delete transactions that aren't referred to by TxMeta of any wallet.
deleteLooseTransactions :: SqlPersistT IO ()
deleteLooseTransactions = do
    deleteLoose "tx_in"
    deleteLoose "tx_out"
    deleteLoose "tx_withdrawal"
  where
    -- Deletes all TxIn/TxOuts/TxWithdrawal returned by the sub-select.
    -- The sub-select outer joins TxMeta with TxIn/TxOut/TxWithdrawal.
    -- All rows of the join table TxMeta as NULL are loose (unreferenced)
    -- transactions.
    deleteLoose t = flip rawExecute [] $
        "DELETE FROM "<> t <>" WHERE tx_id IN (" <>
            "SELECT "<> t <>".tx_id FROM "<> t <>" " <>
            "LEFT OUTER JOIN tx_meta ON tx_meta.tx_id = "<> t <>".tx_id " <>
            "WHERE (tx_meta.tx_id IS NULL))"

-- | Delete all delegation certificates matching the given filter
deleteDelegationCertificates
    :: W.WalletId
    -> [Filter DelegationCertificate]
    -> SqlPersistT IO ()
deleteDelegationCertificates wid filters = do
    deleteCascadeWhere ((CertWalletId ==. wid) : filters)

selectLatestCheckpoint
    :: W.WalletId
    -> SqlPersistT IO (Maybe Checkpoint)
selectLatestCheckpoint wid = fmap entityVal <$>
    selectFirst
        [ CheckpointWalletId ==. wid
        ] [ LimitTo 1, Desc CheckpointSlot ]

selectUTxO
    :: Checkpoint
    -> SqlPersistT IO [UTxO]
selectUTxO cp = fmap entityVal <$>
    selectList
        [ UtxoWalletId ==. checkpointWalletId cp
        , UtxoSlot ==. checkpointSlot cp
        ] []

-- This relies on available information from the database to reconstruct
-- coin selection information for __outgoing__ payments. We can't however guarantee
-- that we have such information for __incoming__ payments (we usually don't
-- have it).
--
-- To reliably provide this information for incoming payment, it should be looked
-- up when applying blocks from the global Ledger, but that is future work
--
-- See also: issue #573.
selectTxs
    :: [TxId]
    -> SqlPersistT IO ([(TxIn, Maybe TxOut)], [TxOut], [TxWithdrawal])
selectTxs = fmap concatUnzip . mapM select . chunksOf chunkSize
  where
    select txids = do
        inputs <- fmap entityVal <$> selectList
            [TxInputTxId <-. txids]
            [Asc TxInputTxId, Asc TxInputOrder]

        resolvedInputs <- toOutputMap . fmap entityVal <$>
            combineChunked inputs (\inputsChunk -> selectList
                [TxOutputTxId <-. (txInputSourceTxId <$> inputsChunk)]
                [Asc TxOutputTxId, Asc TxOutputIndex])

        outputs <- fmap entityVal <$> selectList
            [TxOutputTxId <-. txids]
            [Asc TxOutputTxId, Asc TxOutputIndex]

        withdrawals <- fmap entityVal <$> selectList
            [TxWithdrawalTxId <-. txids]
            []

        pure
            ( inputs `resolveWith` resolvedInputs
            , outputs
            , withdrawals
            )

    toOutputMap :: [TxOut] -> Map (TxId, Word32) TxOut
    toOutputMap = Map.fromList . fmap
        (\out -> let key = (txOutputTxId out, txOutputIndex out) in (key, out))

    resolveWith :: [TxIn] -> Map (TxId, Word32) TxOut -> [(TxIn, Maybe TxOut)]
    resolveWith inputs resolvedInputs =
        [ (i, Map.lookup key resolvedInputs)
        | i <- inputs
        , let key = (txInputSourceTxId i, txInputSourceIndex i)
        ]

    concatUnzip :: [([a], [b], [c])] -> ([a], [b], [c])
    concatUnzip = (\(a, b, c) -> (concat a, concat b, concat c)) . unzip3

    -- Split a query's input values into chunks, run multiple smaller queries,
    -- and then concatenate the results afterwards. Used to avoid "too many SQL
    -- variables" errors for "SELECT IN" queries.
    combineChunked :: [a] -> ([a] -> SqlPersistT IO [b]) -> SqlPersistT IO [b]
    combineChunked xs f = concatMapM f $ chunksOf chunkSize xs

selectTxHistory
    :: TimeInterpreter IO
    -> W.WalletId
    -> Maybe (Quantity "lovelace" Natural)
    -> W.SortOrder
    -> [Filter TxMeta]
    -> SqlPersistT IO [W.TransactionInfo]
selectTxHistory ti wid minWithdrawal order conditions = do
    selectLatestCheckpoint wid >>= \case
        Nothing -> pure []
        Just cp -> do
            minWithdrawalFilter <- case minWithdrawal of
                Nothing  -> pure []
                Just inf -> do
                    let coin = W.Coin $ fromIntegral $ getQuantity inf
                    wdrls <- fmap entityVal
                        <$> selectList [ TxWithdrawalAmount >=. coin ] []
                    pure [ TxMetaTxId <-. (txWithdrawalTxId <$> wdrls) ]

            metas <- fmap entityVal <$> selectList
                ( mconcat
                    [ [ TxMetaWalletId ==. wid ]
                    , minWithdrawalFilter
                    , conditions
                    ]
                ) sortOpt

            let txids = map txMetaTxId metas
            (ins, outs, ws) <- selectTxs txids

            let wal = checkpointFromEntity cp [] ()
            let tip = W.currentTip wal

            liftIO $ txHistoryFromEntity ti tip metas ins outs ws
  where
    -- Note: there are sorted indices on these columns.
    -- The secondary sort by TxId is to make the ordering stable
    -- so that testing with random data always works.
    sortOpt = case order of
        W.Ascending -> [Asc TxMetaSlot, Desc TxMetaTxId]
        W.Descending -> [Desc TxMetaSlot, Asc TxMetaTxId]

selectPendingTxs
    :: W.WalletId
    -> TxId
    -> SqlPersistT IO [TxMeta]
selectPendingTxs wid tid =
    fmap entityVal <$> selectList
        [TxMetaWalletId ==. wid, TxMetaTxId ==. tid] []

deletePendingTx
    :: W.WalletId
    -> TxId
    -> SqlPersistT IO ()
deletePendingTx wid tid = deleteWhere
    [TxMetaWalletId ==. wid, TxMetaTxId ==. tid, TxMetaStatus ==. W.Pending ]

selectPrivateKey
    :: (MonadIO m, PersistPrivateKey (k 'RootK))
    => W.WalletId
    -> SqlPersistT m (Maybe (k 'RootK XPrv, W.Hash "encryption"))
selectPrivateKey wid = do
    keys <- selectFirst [PrivateKeyWalletId ==. wid] []
    pure $ (privateKeyFromEntity . entityVal) <$> keys

selectProtocolParameters
    :: MonadIO m
    => W.WalletId
    -> SqlPersistT m (Maybe W.ProtocolParameters)
selectProtocolParameters wid = do
    pp <- selectFirst [ProtocolParametersWalletId ==. wid] []
    pure $ (protocolParametersFromEntity . entityVal) <$> pp

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint
    :: W.WalletId
    -> W.SlotNo
    -> SqlPersistT IO (Maybe W.SlotNo)
findNearestPoint wid sl =
    fmap (checkpointSlot . entityVal) <$> selectFirst
        [CheckpointWalletId ==. wid, CheckpointSlot <=. sl]
        [Desc CheckpointSlot]

-- | A fatal exception thrown when trying to rollback but, there's no checkpoint
-- to rollback to. The database maintain the invariant that there's always at
-- least one checkpoint (the first one made for genesis) present in the
-- database.
--
-- If we don't find any checkpoint, it means that this invariant has been
-- violated.
data ErrRollbackTo = ErrNoOlderCheckpoint W.WalletId W.SlotNo deriving (Show)
instance Exception ErrRollbackTo

{-------------------------------------------------------------------------------
                     DB queries for address discovery state
-------------------------------------------------------------------------------}

-- | Get a @(WalletId, SlotNo)@ pair from the checkpoint table, for use with
-- 'insertState' and 'selectState'.
checkpointId :: Checkpoint -> (W.WalletId, W.SlotNo)
checkpointId cp = (checkpointWalletId cp, checkpointSlot cp)

-- | Functions for saving/loading the wallet's address discovery state into
-- SQLite.
class PersistState s where
    -- | Store the state for a checkpoint.
    insertState :: (W.WalletId, W.SlotNo) -> s -> SqlPersistT IO ()
    -- | Load the state for a checkpoint.
    selectState :: (W.WalletId, W.SlotNo) -> SqlPersistT IO (Maybe s)

{-------------------------------------------------------------------------------
                          Sequential address discovery
-------------------------------------------------------------------------------}

instance
    ( Eq (k 'AccountK XPub)
    , PersistPublicKey (k 'AccountK)
    , PersistPublicKey (k 'AddressK)
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , PaymentAddress n k
    , SoftDerivation k
    ) => PersistState (Seq.SeqState n k) where
    insertState (wid, sl) st = do
        let (intPool, extPool) = (Seq.internalPool st, Seq.externalPool st)
        let (accountXPub, _) = W.invariant
                "Internal & External pool use different account public keys!"
                (Seq.accountPubKey intPool, Seq.accountPubKey extPool)
                (uncurry (==))
        let eGap = Seq.gap extPool
        let iGap = Seq.gap intPool
        repsert (SeqStateKey wid) $ SeqState
            { seqStateWalletId = wid
            , seqStateExternalGap = eGap
            , seqStateInternalGap = iGap
            , seqStateAccountXPub = serializeXPub accountXPub
            , seqStateRewardXPub = serializeXPub (Seq.rewardAccountKey st)
            }
        insertAddressPool @n wid sl intPool
        insertAddressPool @n wid sl extPool
        deleteWhere [SeqStatePendingWalletId ==. wid]
        dbChunked
            insertMany_
            (mkSeqStatePendingIxs wid $ Seq.pendingChangeIxs st)

    selectState (wid, sl) = runMaybeT $ do
        st <- MaybeT $ selectFirst [SeqStateWalletId ==. wid] []
        let SeqState _ eGap iGap accountBytes rewardBytes = entityVal st
        let accountXPub = unsafeDeserializeXPub accountBytes
        let rewardXPub = unsafeDeserializeXPub rewardBytes
        intPool <- lift $ selectAddressPool @n wid sl iGap accountXPub
        extPool <- lift $ selectAddressPool @n wid sl eGap accountXPub
        pendingChangeIxs <- lift $ selectSeqStatePendingIxs wid
        pure $ Seq.SeqState intPool extPool pendingChangeIxs rewardXPub

insertAddressPool
    :: forall n k c. (PaymentAddress n k, Typeable c)
    => W.WalletId
    -> W.SlotNo
    -> Seq.AddressPool c k
    -> SqlPersistT IO ()
insertAddressPool wid sl pool =
    void $ dbChunked insertMany_
        [ SeqStateAddress wid sl addr ix (Seq.accountingStyle @c) state
        | (ix, (addr, state))
        <- zip [0..] (Seq.addresses (liftPaymentAddress @n) pool)
        ]

selectAddressPool
    :: forall (n :: NetworkDiscriminant) k c.
        ( Typeable c
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k W.Address
        )
    => W.WalletId
    -> W.SlotNo
    -> Seq.AddressPoolGap
    -> k 'AccountK XPub
    -> SqlPersistT IO (Seq.AddressPool c k)
selectAddressPool wid sl gap xpub = do
    addrs <- fmap entityVal <$> selectList
        [ SeqStateAddressWalletId ==. wid
        , SeqStateAddressSlot ==. sl
        , SeqStateAddressAccountingStyle ==. Seq.accountingStyle @c
        ] [Asc SeqStateAddressIndex]
    pure $ addressPoolFromEntity addrs
  where
    addressPoolFromEntity
        :: [SeqStateAddress]
        -> Seq.AddressPool c k
    addressPoolFromEntity addrs
        = Seq.mkAddressPool @n @c @k xpub gap
        $ map (\x -> (seqStateAddressAddress x, seqStateAddressStatus x)) addrs

mkSeqStatePendingIxs :: W.WalletId -> Seq.PendingIxs -> [SeqStatePendingIx]
mkSeqStatePendingIxs wid =
    fmap (SeqStatePendingIx wid . W.getIndex) . Seq.pendingIxsToList

selectSeqStatePendingIxs :: W.WalletId -> SqlPersistT IO Seq.PendingIxs
selectSeqStatePendingIxs wid =
    Seq.pendingIxsFromList . fromRes <$> selectList
        [SeqStatePendingWalletId ==. wid]
        [Desc SeqStatePendingIxIndex]
  where
    fromRes = fmap (W.Index . seqStatePendingIxIndex . entityVal)

{-------------------------------------------------------------------------------
                          HD Random address discovery
-------------------------------------------------------------------------------}

-- Persisting 'RndState' requires that the wallet root key has already been
-- added to the database with 'putPrivateKey'. Unlike sequential AD, random
-- address discovery requires a root key to recognize addresses.
instance PersistState (Rnd.RndState t) where
    insertState (wid, sl) st = do
        let ix = W.getIndex (st ^. #accountIndex)
        let gen = st ^. #gen
        let pwd = st ^. #hdPassphrase
        repsert (RndStateKey wid) (RndState wid ix gen (HDPassphrase pwd))
        insertRndStateAddresses wid sl (Rnd.addresses st)
        insertRndStatePending wid (Rnd.pendingAddresses st)

    selectState (wid, sl) = runMaybeT $ do
        st <- MaybeT $ selectFirst
            [ RndStateWalletId ==. wid
            ] []
        let (RndState _ ix gen (HDPassphrase pwd)) = entityVal st
        addresses <- lift $ selectRndStateAddresses wid sl
        pendingAddresses <- lift $ selectRndStatePending wid
        pure $ Rnd.RndState
            { hdPassphrase = pwd
            , accountIndex =
                -- FIXME
                -- In the early days when Daedalus Flight was shipped, the
                -- wallet backend was generating addresses indexes across the
                -- whole domain which was causing a great deal of issues with
                -- the legacy cardano-sl:wallet ...
                --
                -- We later changed that to instead use "hardened indexes". Yet,
                -- for the few wallets which were already created, we revert
                -- this dynamically by replacing the index here.
                --
                -- This ugly hack could / should be removed eventually, in a few
                -- releases from 2020-04-06.
                if ix == 0
                then minBound
                else W.Index ix
            , addresses = addresses
            , pendingAddresses = pendingAddresses
            , gen = gen
            }

insertRndStateAddresses
    :: W.WalletId
    -> W.SlotNo
    -> Map Rnd.DerivationPath (W.Address, W.AddressState)
    -> SqlPersistT IO ()
insertRndStateAddresses wid sl addresses = do
    dbChunked insertMany_
        [ RndStateAddress wid sl accIx addrIx addr st
        | ((W.Index accIx, W.Index addrIx), (addr, st)) <- Map.assocs addresses
        ]

insertRndStatePending
    :: W.WalletId
    -> Map Rnd.DerivationPath W.Address
    -> SqlPersistT IO ()
insertRndStatePending wid addresses = do
    deleteWhere [RndStatePendingAddressWalletId ==. wid]
    dbChunked insertMany_
        [ RndStatePendingAddress wid accIx addrIx addr
        | ((W.Index accIx, W.Index addrIx), addr) <- Map.assocs addresses
        ]

selectRndStateAddresses
    :: W.WalletId
    -> W.SlotNo
    -> SqlPersistT IO (Map Rnd.DerivationPath (W.Address, W.AddressState))
selectRndStateAddresses wid sl = do
    addrs <- fmap entityVal <$> selectList
        [ RndStateAddressWalletId ==. wid
        , RndStateAddressSlot ==. sl
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStateAddress _ _ accIx addrIx addr st) =
        ((W.Index accIx, W.Index addrIx), (addr, st))

selectRndStatePending
    :: W.WalletId
    -> SqlPersistT IO (Map Rnd.DerivationPath W.Address)
selectRndStatePending wid = do
    addrs <- fmap entityVal <$> selectList
        [ RndStatePendingAddressWalletId ==. wid
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStatePendingAddress _ accIx addrIx addr) =
        ((W.Index accIx, W.Index addrIx), addr)
