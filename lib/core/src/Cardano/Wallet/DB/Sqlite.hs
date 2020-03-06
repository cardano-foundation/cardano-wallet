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

    -- * Logging
    , DatabasesStartupLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
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
    ( deleteSqliteDatabase )
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
    , EntityField (..)
    , Key (..)
    , PrivateKey (..)
    , RndState (..)
    , RndStateAddress (..)
    , RndStatePendingAddress (..)
    , SeqState (..)
    , SeqStateAddress (..)
    , SeqStatePendingIx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , UTxO (..)
    , Wallet (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), HDPassphrase (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , XPrv
    , XPub
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Control.Arrow
    ( (***) )
import Control.Concurrent.MVar
    ( modifyMVar, modifyMVar_, newMVar )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( Exception, bracket, throwIO )
import Control.Monad
    ( forM, void, when )
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
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word64 )
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
    ( PersistValue (PersistText) )
import Fmt
    ( pretty )
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
        ( IsOurs s W.Address
        , IsOurs s W.ChimericAccount
        , NFData s
        , Show s
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> ((SqliteContext, DBLayer IO s k) -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer trace defaultFieldValues mDatabaseDir =
    bracket before after
  where
    before = newDBLayer trace defaultFieldValues mDatabaseDir
    after = destroyDBLayer . fst

-- | Instantiate a 'DBFactory' from a given directory
newDBFactory
    :: forall s k.
        ( IsOurs s W.Address
        , IsOurs s W.ChimericAccount
        , NFData s
        , Show s
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> IO (DBFactory IO s k)
newDBFactory tr defaultFieldValues mDatabaseDir = do
    mvar <- newMVar mempty
    case mDatabaseDir of
        -- NOTE
        -- For the in-memory database, we do actually preserve the database
        -- after the 'action' is done. This allows for calling 'withDatabase'
        -- several times within the same execution and get back the same
        -- database. The memory is only cleaned up when calling
        -- 'removeDatabase', to mimic the way the file database works!
        Nothing -> pure DBFactory
            { withDatabase = \wid action -> do
                db <- modifyMVar mvar $ \m -> case Map.lookup wid m of
                    Just (_, db) -> pure (m, db)
                    Nothing -> do
                        (ctx, db) <-
                            newDBLayer tr defaultFieldValues Nothing
                        pure (Map.insert wid (ctx, db) m, db)
                action db
            , removeDatabase = \wid -> do
                traceWith tr $ MsgRemoving (pretty wid)
                modifyMVar_ mvar (pure . Map.delete wid)
            }
        Just databaseDir -> pure DBFactory
            { withDatabase = \wid action -> do
                withDBLayer
                    tr
                    defaultFieldValues
                    (Just $ databaseFile wid)
                    (action . snd)
            , removeDatabase = \wid -> do
                traceWith tr $ MsgRemoving (pretty wid)
                let tr' = contramap (MsgRemovingDatabaseFile (pretty wid)) tr
                deleteSqliteDatabase tr' (databaseFile wid)
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
    => Tracer IO DatabasesStartupLog
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
                        traceWith tr $ MsgFoundDatabase (dir </> file) wid
                        return (Just wid)
                    _ -> do
                        traceWith tr $ MsgUnknownFile file
                        return Nothing
            _ -> return Nothing
  where
    expectedPrefix = T.pack $ keyTypeDescriptor $ Proxy @k

-- | Executes any manual database migration steps that may be required on
--   startup.
--
migrateManually
    :: Tracer IO DBLog
    -> DefaultFieldValues
    -> ManualMigration
migrateManually tr defaultFieldValues =
    ManualMigration
        addActiveSlotCoefficientIfMissing
  where
    activeSlotCoeff = DBField CheckpointActiveSlotCoeff

    -- | Adds an 'active_slot_coeff' column to the 'checkpoint' table if
    --   it is missing.
    --
    addActiveSlotCoefficientIfMissing :: Sqlite.Connection -> IO ()
    addActiveSlotCoefficientIfMissing conn = do
        isFieldPresent conn activeSlotCoeff >>= \case
            Nothing ->
                -- NOTE
                -- The host table doesn't even exist. Typically, when the db is
                -- first created.
                traceWith tr $ MsgManualMigrationNotNeeded activeSlotCoeff
            Just True ->
                traceWith tr $ MsgManualMigrationNotNeeded activeSlotCoeff
            Just False -> do
                traceWith tr $ MsgManualMigrationNeeded activeSlotCoeff value
                addColumn <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName activeSlotCoeff
                    , "ADD COLUMN", fieldName activeSlotCoeff
                    , fieldType activeSlotCoeff, "NOT NULL", "DEFAULT", value
                    , ";"
                    ]
                _ <- Sqlite.step addColumn
                Sqlite.finalize addColumn
      where
        value = toText
            $ W.unActiveSlotCoefficient
            $ defaultActiveSlotCoefficient defaultFieldValues

    -- | Determines whether a field is present in its parent table.
    --
    -- Returns 'Nothing' if the parent table doesn't exist. Just Bool otherwise.
    isFieldPresent :: Sqlite.Connection -> DBField -> IO (Maybe Bool)
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
                | fieldName field `T.isInfixOf` t -> Just True
                | otherwise                       -> Just False
            _ -> Nothing

-- | A set of default field values that can be consulted when performing a
--   database migration.
--
newtype DefaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient :: W.ActiveSlotCoefficient }

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
        ( IsOurs s W.Address
        , IsOurs s W.ChimericAccount
        , NFData s
        , Show s
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database file, or Nothing for in-memory database
    -> IO (SqliteContext, DBLayer IO s k)
newDBLayer trace defaultFieldValues mDatabaseFile = do
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

        { initializeWallet = \(PrimaryKey wid) cp meta txs -> ExceptT $ do
            res <- handleConstraint (ErrWalletAlreadyExists wid) $
                insert_ (mkWalletEntity wid meta)
            when (isRight res) $ do
                insertCheckpoint wid cp
                let (metas, txins, txouts) = mkTxHistory wid txs
                putTxMetas metas
                putTxs txins txouts
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

        , rollbackTo = \(PrimaryKey wid) point -> ExceptT $ do
            findNearestPoint wid point >>= \case
                Nothing -> selectLatestCheckpoint wid >>= \case
                    Nothing -> pure $ Left $ ErrNoSuchWallet wid
                    Just _  -> lift $ throwIO (ErrNoOlderCheckpoint wid point)
                Just nearestPoint -> do
                    deleteCheckpoints wid
                        [ CheckpointSlot >. point
                        ]
                    deleteDelegationCertificates wid
                        [ CertSlot >. point
                        ]
                    updateTxMetas wid
                        [ TxMetaDirection ==. W.Outgoing
                        , TxMetaSlot >. point
                        ]
                        [ TxMetaStatus =. W.Pending
                        , TxMetaSlot =. nearestPoint
                        ]
                    deleteTxMetas wid
                        [ TxMetaDirection ==. W.Incoming
                        , TxMetaSlot >. point
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
                    let (W.SlotId currentEpoch _) = checkpointSlot cp
                    readWalletDelegation wid currentEpoch >>= readWalletMetadata wid

        , putDelegationCertificate = \(PrimaryKey wid) cert sl -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> pure <$> repsert
                    (DelegationCertificateKey wid sl)
                    (DelegationCertificate wid sl (W.dlgCertPoolId cert))

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    let (metas, txins, txouts) = mkTxHistory wid txs
                    putTxMetas metas
                    putTxs txins txouts
                    pure $ Right ()

        , readTxHistory = \(PrimaryKey wid) order range status -> do
            selectTxHistory wid order $ catMaybes
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
                    let isPending (TxMeta _ _ st _ _ _ _) = st == W.Pending
                    case metas of
                        [] -> pure errNoSuchTransaction
                        txs | any isPending txs -> do
                            deletePendingTx wid (TxId tid)
                            pure $ Right ()
                        _ -> pure errNoMorePending

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
    :: W.WalletId
    -> W.EpochNo
    -> SqlPersistT IO W.WalletDelegation
readWalletDelegation wid epoch
    | epoch == 0 = pure $ W.WalletDelegation W.NotDelegating []
    | otherwise  = do
        active <- maybe W.NotDelegating toWalletDelegationStatus
            <$> readDelegationCertificate wid
                [ CertSlot <. W.SlotId (epoch - 1) 0
                ]

        next <- catMaybes <$> sequence
            [ fmap (W.WalletDelegationNext (epoch + 1) . toWalletDelegationStatus)
                <$> readDelegationCertificate wid
                    [ CertSlot >=. W.SlotId (epoch - 1) 0
                    , CertSlot <. W.SlotId epoch 0
                    ]
            , fmap (W.WalletDelegationNext (epoch + 2) . toWalletDelegationStatus)
                <$> readDelegationCertificate wid
                    [ CertSlot >=. W.SlotId epoch 0
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
    , walPassphraseLastUpdatedAt =
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalName =. meta ^. #name . coerce
    , WalCreationTime =. meta ^. #creationTime
    , WalPassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    ]

blockHeaderFromEntity :: Checkpoint -> W.BlockHeader
blockHeaderFromEntity cp = W.BlockHeader
    { slotId = checkpointSlot cp
    , blockHeight = Quantity (checkpointBlockHeight cp)
    , headerHash = getBlockId (checkpointHeaderHash cp)
    , parentHeaderHash = getBlockId (checkpointParentHash cp)
    }

metadataFromEntity :: W.WalletDelegation -> Wallet -> W.WalletMetadata
metadataFromEntity walDelegation wal = W.WalletMetadata
    { name = W.WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo <$>
        walPassphraseLastUpdatedAt wal
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
    sl = header ^. #slotId
    (Quantity bh) = header ^. #blockHeight
    bp = W.blockchainParameters wal
    cp = Checkpoint
        { checkpointWalletId = wid
        , checkpointSlot = sl
        , checkpointParentHash = BlockId (header ^. #parentHeaderHash)
        , checkpointHeaderHash = BlockId (header ^. #headerHash)
        , checkpointBlockHeight = bh
        , checkpointGenesisHash = BlockId (coerce (bp ^. #getGenesisBlockHash))
        , checkpointGenesisStart = coerce (bp ^. #getGenesisBlockDate)
        , checkpointFeePolicy = bp ^. #getFeePolicy
        , checkpointSlotLength = coerceSlotLength $ bp ^. #getSlotLength
        , checkpointEpochLength = coerce (bp ^. #getEpochLength)
        , checkpointTxMaxSize = coerce (bp ^. #getTxMaxSize)
        , checkpointEpochStability = coerce (bp ^. #getEpochStability)
        , checkpointActiveSlotCoeff =
            W.unActiveSlotCoefficient (bp ^. #getActiveSlotCoefficient)
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
    :: (IsOurs s W.Address, IsOurs s W.ChimericAccount, NFData s, Show s)
    => Checkpoint
    -> [UTxO]
    -> s
    -> W.Wallet s
checkpointFromEntity cp utxo s =
    W.unsafeInitWallet utxo' header s bp
  where
    (Checkpoint
        _walletId
        slot
        (BlockId headerHash)
        (BlockId parentHeaderHash)
        bh
        (BlockId genesisHash)
        genesisStart
        feePolicy
        slotLength
        epochLength
        txMaxSize
        epochStability
        activeSlotCoeff
        ) = cp
    header = (W.BlockHeader slot (Quantity bh) headerHash parentHeaderHash)
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo
        ]
    bp = W.BlockchainParameters
        { getGenesisBlockHash = coerce genesisHash
        , getGenesisBlockDate = W.StartTime genesisStart
        , getFeePolicy = feePolicy
        , getSlotLength = W.SlotLength (toEnum (fromEnum slotLength))
        , getEpochLength = W.EpochLength epochLength
        , getTxMaxSize = Quantity txMaxSize
        , getEpochStability = Quantity epochStability
        , getActiveSlotCoefficient = W.ActiveSlotCoefficient activeSlotCoeff
        }

mkTxHistory
    :: W.WalletId
    -> [(W.Tx, W.TxMeta)]
    -> ([TxMeta], [TxIn], [TxOut])
mkTxHistory wid txs = flatTxHistory
    [ (mkTxMetaEntity wid txid meta, mkTxInputsOutputs (txid, tx))
    | (tx, meta) <- txs
    , let txid = W.txId tx
    ]
  where
    -- | Make flat lists of entities from the result of 'mkTxHistory'.
    flatTxHistory
        :: [(TxMeta, ([TxIn], [TxOut]))] -> ([TxMeta], [TxIn], [TxOut])
    flatTxHistory entities =
        ( map fst entities
        , concatMap (fst . snd) entities
        , concatMap (snd . snd) entities
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

mkTxMetaEntity :: W.WalletId -> W.Hash "Tx" -> W.TxMeta -> TxMeta
mkTxMetaEntity wid txid meta = TxMeta
    { txMetaTxId = TxId txid
    , txMetaWalletId = wid
    , txMetaStatus = meta ^. #status
    , txMetaDirection = meta ^. #direction
    , txMetaSlot = meta ^. #slotId
    , txMetaBlockHeight = getQuantity (meta ^. #blockHeight)
    , txMetaAmount = getQuantity (meta ^. #amount)
    }

-- note: TxIn records must already be sorted by order
-- and TxOut records must already be sorted by index
txHistoryFromEntity
    :: [TxMeta]
    -> [TxIn]
    -> [TxOut]
    -> [(W.Tx, W.TxMeta)]
txHistoryFromEntity metas ins outs = map mkItem metas
  where
    mkItem m = (mkTxWith (txMetaTxId m), mkTxMeta m)
    mkTxWith txid = W.Tx
        (getTxId txid)
        (map mkTxIn $ filter ((== txid) . txInputTxId) ins)
        (map mkTxOut $ filter ((== txid) . txOutputTxId) outs)
    mkTxIn tx =
        ( W.TxIn
            { W.inputId = getTxId (txInputSourceTxId tx)
            , W.inputIx = txInputSourceIndex tx
            }
        , txInputSourceAmount tx
        )
    mkTxOut tx = W.TxOut
        { W.address = txOutputAddress tx
        , W.coin = txOutputAmount tx
        }
    mkTxMeta m = W.TxMeta
        { W.status = txMetaStatus m
        , W.direction = txMetaDirection m
        , W.slotId = txMetaSlot m
        , W.blockHeight = Quantity (txMetaBlockHeight m)
        , W.amount = Quantity (txMetaAmount m)
        }

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
    let sl = (W.currentTip wallet) ^. #slotId
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
putTxs :: [TxIn] -> [TxOut] -> SqlPersistT IO ()
putTxs txins txouts = do
    dbChunked repsertMany
        [ (TxInKey txInputTxId txInputSourceTxId txInputSourceIndex, i)
        | i@TxIn{..} <- txins ]
    dbChunked repsertMany
        [ (TxOutKey txOutputTxId txOutputIndex, o)
        | o@TxOut{..} <- txouts ]

-- | Delete transactions that aren't referred to by TxMeta of any wallet.
deleteLooseTransactions :: SqlPersistT IO ()
deleteLooseTransactions = do
    deleteLoose "tx_in"
    deleteLoose "tx_out"
  where
    -- Deletes all TxIn/TxOuts returned by the sub-select.
    -- The sub-select outer joins TxMeta with TxIn/TxOut.
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

selectTxs
    :: [TxId]
    -> SqlPersistT IO ([TxIn], [TxOut])
selectTxs = fmap concatUnzip . mapM select . chunksOf chunkSize
  where
    select txids = do
        ins <- fmap entityVal <$> selectList [TxInputTxId <-. txids]
            [Asc TxInputTxId, Asc TxInputOrder]
        outs <- fmap entityVal <$> selectList [TxOutputTxId <-. txids]
            [Asc TxOutputTxId, Asc TxOutputIndex]
        pure (ins, outs)

    concatUnzip :: [([a], [b])] -> ([a], [b])
    concatUnzip = (concat *** concat) . unzip

selectTxHistory
    :: W.WalletId
    -> W.SortOrder
    -> [Filter TxMeta]
    -> SqlPersistT IO [(W.Tx, W.TxMeta)]
selectTxHistory wid order conditions = do
    metas <- fmap entityVal <$> selectList
        ((TxMetaWalletId ==. wid) : conditions) sortOpt
    let txids = map txMetaTxId metas
    (ins, outs) <- selectTxs txids
    return $ txHistoryFromEntity metas ins outs
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

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint
    :: W.WalletId
    -> W.SlotId
    -> SqlPersistT IO (Maybe W.SlotId)
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
data ErrRollbackTo = ErrNoOlderCheckpoint W.WalletId W.SlotId deriving (Show)
instance Exception ErrRollbackTo

{-------------------------------------------------------------------------------
                     DB queries for address discovery state
-------------------------------------------------------------------------------}

-- | Get a @(WalletId, SlotId)@ pair from the checkpoint table, for use with
-- 'insertState' and 'selectState'.
checkpointId :: Checkpoint -> (W.WalletId, W.SlotId)
checkpointId cp = (checkpointWalletId cp, checkpointSlot cp)

-- | Functions for saving/loading the wallet's address discovery state into
-- SQLite.
class PersistState s where
    -- | Store the state for a checkpoint.
    insertState :: (W.WalletId, W.SlotId) -> s -> SqlPersistT IO ()
    -- | Load the state for a checkpoint.
    selectState :: (W.WalletId, W.SlotId) -> SqlPersistT IO (Maybe s)

{-------------------------------------------------------------------------------
                          Sequential address discovery
-------------------------------------------------------------------------------}

instance
    ( Eq (k 'AccountK XPub)
    , PersistPublicKey (k 'AccountK)
    , PersistPublicKey (k 'AddressK)
    , MkKeyFingerprint k (k 'AddressK XPub)
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
        intPool <- lift $ selectAddressPool wid sl iGap accountXPub
        extPool <- lift $ selectAddressPool wid sl eGap accountXPub
        pendingChangeIxs <- lift $ selectSeqStatePendingIxs wid
        pure $ Seq.SeqState intPool extPool pendingChangeIxs rewardXPub

insertAddressPool
    :: forall n k c. (PaymentAddress n k, Typeable c)
    => W.WalletId
    -> W.SlotId
    -> Seq.AddressPool c k
    -> SqlPersistT IO ()
insertAddressPool wid sl pool =
    void $ dbChunked insertMany_
        [ SeqStateAddress wid sl addr ix (Seq.accountingStyle @c)
        | (ix, addr) <- zip [0..] (Seq.addresses (liftPaymentAddress @n) pool)
        ]

selectAddressPool
    :: forall k c.
        ( Typeable c
        , SoftDerivation k
        , MkKeyFingerprint k (k 'AddressK XPub)
        , MkKeyFingerprint k W.Address
        )
    => W.WalletId
    -> W.SlotId
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
    addressPoolFromEntity addrs =
        Seq.mkAddressPool @c @k xpub gap (map seqStateAddressAddress addrs)

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

-- | Type alias for the index -> address map so that lines do not exceed 80
-- characters in width.
type RndStateAddresses = Map
    (W.Index 'W.WholeDomain 'W.AccountK, W.Index 'W.WholeDomain 'W.AddressK)
    W.Address

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
            , accountIndex = W.Index ix
            , addresses = addresses
            , pendingAddresses = pendingAddresses
            , gen = gen
            }

insertRndStateAddresses
    :: W.WalletId
    -> W.SlotId
    -> RndStateAddresses
    -> SqlPersistT IO ()
insertRndStateAddresses wid sl addresses = do
    dbChunked insertMany_
        [ RndStateAddress wid sl accIx addrIx addr
        | ((W.Index accIx, W.Index addrIx), addr) <- Map.assocs addresses
        ]

insertRndStatePending
    :: W.WalletId
    -> RndStateAddresses
    -> SqlPersistT IO ()
insertRndStatePending wid addresses = do
    deleteWhere [RndStatePendingAddressWalletId ==. wid]
    dbChunked insertMany_
        [ RndStatePendingAddress wid accIx addrIx addr
        | ((W.Index accIx, W.Index addrIx), addr) <- Map.assocs addresses
        ]

selectRndStateAddresses
    :: W.WalletId
    -> W.SlotId
    -> SqlPersistT IO RndStateAddresses
selectRndStateAddresses wid sl = do
    addrs <- fmap entityVal <$> selectList
        [ RndStateAddressWalletId ==. wid
        , RndStateAddressSlot ==. sl
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStateAddress _ _ accIx addrIx addr) =
        ((W.Index accIx, W.Index addrIx), addr)

selectRndStatePending
    :: W.WalletId
    -> SqlPersistT IO RndStateAddresses
selectRndStatePending wid = do
    addrs <- fmap entityVal <$> selectList
        [ RndStatePendingAddressWalletId ==. wid
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStatePendingAddress _ accIx addrIx addr) =
        ((W.Index accIx, W.Index addrIx), addr)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages arising from accessing the wallet repository.
data DatabasesStartupLog
    = MsgFoundDatabase FilePath W.WalletId
    | MsgUnknownFile FilePath
    deriving (Show, Eq)

instance ToText DatabasesStartupLog where
    toText = \case
        MsgFoundDatabase _file wid ->
            "Found existing wallet: " <> toText wid
        MsgUnknownFile file -> mconcat
            [ "Found something other than a database file in "
            , "the database folder: ", T.pack file
            ]

instance DefinePrivacyAnnotation DatabasesStartupLog
instance DefineSeverity DatabasesStartupLog where
    defineSeverity = \case
        MsgFoundDatabase _ _ -> Info
        MsgUnknownFile _ -> Notice
