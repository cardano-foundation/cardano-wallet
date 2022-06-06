{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant ^." -}
{- HLINT ignore "Use fst" -}
{- HLINT ignore "Use snd" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Layer
    ( -- * Directory of single-file wallet databases
      newDBFactory
    , findDatabases
    , DBFactoryLog (..)

    -- * Internal implementation
    , withDBLayer
    , withDBLayerInMemory
    , WalletDBLog (..)
    , CacheBehavior (..)

    -- * Unbracketed internal implementation
    , newDBLayerWith
    , newDBLayerInMemory

    -- * Interfaces
    , PersistAddressBook (..)

    -- * Migration Support
    , DefaultFieldValues (..)

    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.DB.Sqlite
    ( DBLog (..)
    , SqliteContext (..)
    , handleConstraint
    , newInMemorySqliteContext
    , newSqliteContext
    , withConnectionPool
    )
import Cardano.DB.Sqlite.Delete
    ( DeleteSqliteDatabaseLog
    , deleteSqliteDatabase
    , newRefCount
    , waitForFree
    , withRef
    )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Checkpoints.Model
    ( DeltaCheckpoints (..), fromWallet, getBlockHeight, getSlot )
import Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..), migrateManually )
import Cardano.Wallet.DB.Sqlite.Schema
    ( DelegationCertificate (..)
    , DelegationReward (..)
    , EntityField (..)
    , Key (..)
    , LocalTxSubmission (..)
    , StakeKeyCertificate (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Sqlite.WrapSTM
    ()
import Cardano.Wallet.DB.Unstored
    ( ErrInitializeGenesisAbsent (ErrInitializeGenesisAbsent)
    , ErrRollbackTo (ErrNoOlderCheckpoint)
    , deleteDelegationCertificates
    , deleteLooseTransactions
    , deletePendingOrExpiredTx
    , deleteStakeKeyCerts
    , deleteTxMetas
    , listPendingLocalTxSubmissionQuery
    , localTxSubmissionFromEntity
    , mkPrivateKeyEntity
    , mkWalletEntity
    , mkWalletMetadataUpdate
    , pruneLocalTxSubmission
    , readWalletDelegation
    , readWalletMetadata
    , selectGenesisParameters
    , selectPrivateKey
    , selectTxHistory
    , selectTxMeta
    , selectWallet
    , updatePendingTxForExpiryQuery
    , updateTxHistory
    , updateTxMetas
    )
import Cardano.Wallet.DB.Wallets.State
    ( DeltaMap (..)
    , DeltaWalletState1 (..)
    , findNearestPoint
    , fromGenesis
    , getLatest
    )
import Cardano.Wallet.DB.Wallets.Store
    ( PersistAddressBook (..), blockHeaderFromEntity, mkStoreWallets )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PersistPrivateKey (..), WalletKey (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, interpretQuery )
import Control.Monad
    ( forM, unless, void, when, (<=<) )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.DBVar
    ( loadDBVar, modifyDBMaybe, readDBVar, updateDBVar )
import Data.Either
    ( isRight )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( catMaybes )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import qualified Data.Set as Set
import Data.Text
    ( Text )
import qualified Data.Text as T
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( Entity (..)
    , SelectOpt (..)
    , deleteWhere
    , insert_
    , repsert
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , upsert
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Fmt
    ( pretty, (+|), (|+) )
import GHC.Generics
    ( Generic )
import System.Directory
    ( doesFileExist, listDirectory )
import System.FilePath
    ( (</>) )
import UnliftIO.Exception
    ( bracket, throwIO )
import UnliftIO.MVar
    ( modifyMVar, modifyMVar_, newMVar, readMVar, withMVar )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Tx as W
{-------------------------------------------------------------------------------
                               Database "factory"
             (a directory containing one database file per wallet)
-------------------------------------------------------------------------------}

-- | Instantiate a 'DBFactory' from a given directory, or in-memory for testing.
newDBFactory
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO DBFactoryLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> IO (DBFactory IO s k)
newDBFactory tr defaultFieldValues ti = \case
    Nothing -> do
        -- NOTE1
        -- For the in-memory database, we do actually preserve the database
        -- after the 'action' is done. This allows for calling 'withDatabase'
        -- several times within the same execution and get back the same
        -- database. The memory is only cleaned up when calling
        -- 'removeDatabase', to mimic the way the file database works!
        --
        -- NOTE2
        -- The in-memory withDatabase will leak memory unless removeDatabase is
        -- called after using the database. In practice, this is only a problem
        -- for testing.
        mvar <- newMVar mempty
        pure DBFactory
            { withDatabase = \wid action -> do
                db <- modifyMVar mvar $ \m -> case Map.lookup wid m of
                    Just db -> pure (m, db)
                    Nothing -> do
                        let tr' = contramap (MsgWalletDB "") tr
                        (_cleanup, db) <- newDBLayerInMemory tr' ti
                        pure (Map.insert wid db m, db)
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
                (contramap (MsgWalletDB (databaseFile wid)) tr)
                defaultFieldValues
                (databaseFile wid)
                ti
                action
            , removeDatabase = \wid -> do
                let widp = pretty wid
                -- try to wait for all 'withDatabase' calls to finish before
                -- deleting database file.
                let trWait = contramap (MsgWaitingForDatabase widp) tr
                -- TODO: rather than refcounting, why not keep retrying the
                -- delete until there are no file busy errors?
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
    => Tracer IO DBFactoryLog
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

data DBFactoryLog
    = MsgFoundDatabase FilePath Text
    | MsgUnknownDBFile FilePath
    | MsgRemoving Text
    | MsgRemovingInUse Text Int
    | MsgRemovingDatabaseFile Text DeleteSqliteDatabaseLog
    | MsgWaitingForDatabase Text (Maybe Int)
    | MsgWalletDB FilePath WalletDBLog
    deriving (Generic, Show, Eq)

instance HasPrivacyAnnotation DBFactoryLog
instance HasSeverityAnnotation DBFactoryLog where
    getSeverityAnnotation ev = case ev of
        MsgFoundDatabase _ _ -> Info
        MsgUnknownDBFile _ -> Notice
        MsgRemoving _ -> Info
        MsgRemovingInUse _ _ -> Notice
        MsgRemovingDatabaseFile _ msg -> getSeverityAnnotation msg
        MsgWaitingForDatabase _ _ -> Info
        MsgWalletDB _ msg -> getSeverityAnnotation msg

instance ToText DBFactoryLog where
    toText = \case
        MsgFoundDatabase _file wid ->
            "Found existing wallet: " <> wid
        MsgUnknownDBFile file -> mconcat
            [ "Found something other than a database file in "
            , "the database folder: ", T.pack file
            ]
        MsgRemoving wid ->
            "Removing wallet's database. Wallet id was " <> wid
        MsgRemovingDatabaseFile wid msg ->
            "Removing " <> wid <> ": " <> toText msg
        MsgWaitingForDatabase wid Nothing ->
            "Database "+|wid|+" is ready to be deleted"
        MsgWaitingForDatabase wid (Just count) ->
            "Waiting for "+|count|+" withDatabase "+|wid|+" call(s) to finish"
        MsgRemovingInUse wid count ->
            "Timed out waiting for "+|count|+" withDatabase "+|wid|+" call(s) to finish. " <>
            "Attempting to remove the database anyway."
        MsgWalletDB _file msg -> toText msg

{-------------------------------------------------------------------------------
                                 Database layer
-------------------------------------------------------------------------------}

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: forall s k a.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO WalletDBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> FilePath
       -- ^ Path to database file
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> (DBLayer IO s k -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer tr defaultFieldValues dbFile ti action = do
    let trDB = contramap MsgDB tr
    let manualMigrations = migrateManually trDB (Proxy @k) defaultFieldValues
    let autoMigrations   = migrateAll
    withConnectionPool trDB dbFile $ \pool -> do
        res <- newSqliteContext trDB pool manualMigrations autoMigrations
        either throwIO (action <=< newDBLayerWith CacheLatestCheckpoint tr ti) res

newtype WalletDBLog
    = MsgDB DBLog
    deriving (Generic, Show, Eq)

instance HasPrivacyAnnotation WalletDBLog
instance HasSeverityAnnotation WalletDBLog where
    getSeverityAnnotation = \case
        MsgDB msg -> getSeverityAnnotation msg

instance ToText WalletDBLog where
    toText = \case
        MsgDB msg -> toText msg

-- | Runs an IO action with a new 'DBLayer' backed by a sqlite in-memory
-- database.
withDBLayerInMemory
    :: forall s k a.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO WalletDBLog
       -- ^ Logging object
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> (DBLayer IO s k -> IO a)
    -> IO a
withDBLayerInMemory tr ti action = bracket (newDBLayerInMemory tr ti) fst (action . snd)

-- | Creates a 'DBLayer' backed by a sqlite in-memory database.
--
-- Returns a cleanup function which you should always use exactly once when
-- finished with the 'DBLayer'.
newDBLayerInMemory
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO WalletDBLog
       -- ^ Logging object
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> IO (IO (), DBLayer IO s k)
newDBLayerInMemory tr ti = do
    let tr' = contramap MsgDB tr
    (destroy, ctx) <- newInMemorySqliteContext tr' [] migrateAll
    db <- newDBLayer tr ti ctx
    pure (destroy, db)

-- | What to do with regards to caching. This is useful to disable caching in
-- database benchmarks.
data CacheBehavior
    = NoCache
    | CacheLatestCheckpoint
    deriving (Eq, Show)

-- | Sets up a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
--
-- 'newDBLayer' will provide the actual 'DBLayer' implementation. It requires an
-- 'SqliteContext' which can be obtained from a database connection pool. This
-- is better initialized with 'withDBLayer'.
newDBLayer
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO WalletDBLog
       -- ^ Logging
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> SqliteContext
       -- ^ A (thread-)safe wrapper for query execution.
    -> IO (DBLayer IO s k)
newDBLayer = newDBLayerWith @s @k CacheLatestCheckpoint

{- HLINT ignore newDBLayerWith "Redundant <$>" -}
-- | Like 'newDBLayer', but allows to explicitly specify the caching behavior.
newDBLayerWith
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => CacheBehavior
       -- ^ Option to disable caching.
    -> Tracer IO WalletDBLog
       -- ^ Logging
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> SqliteContext
       -- ^ A (thread-)safe wrapper for query execution.
    -> IO (DBLayer IO s k)
newDBLayerWith _cacheBehavior _tr ti SqliteContext{runQuery} = do
    -- FIXME LATER during ADP-1043:
    --   Remove the 'NoCache' behavior, we cannot get it back.
    --   This will affect read benchmarks, they will need to benchmark
    --   'loadDBVar' instead.

    -- FIXME LATER during ADP-1043:
    --   Handle the case where loading the database fails.
    walletsDB_ <- runQuery $ loadDBVar mkStoreWallets

    -- NOTE
    -- The cache will not work properly unless 'atomically' is protected by a
    -- mutex (queryLock), which means no concurrent queries.
    queryLock <- newMVar () -- fixme: ADP-586

    -- Insert genesis checkpoint into the DBVar.
    -- Throws an internal error if the checkpoint is not actually at genesis.
    let insertGenesis wid cp _txs =
            case fromGenesis cp of -- txs of
                Nothing -> throwIO $ ErrInitializeGenesisAbsent wid header
                Just wallet ->
                    updateDBVar walletsDB_ $ Insert wid wallet
          where
            header = cp ^. #currentTip

    -- Retrieve the latest checkpoint from the DBVar
    let readCheckpoint_
            :: W.WalletId
            -> SqlPersistT IO (Maybe (W.Wallet s))
        readCheckpoint_ wid =
            fmap getLatest . Map.lookup wid <$> readDBVar walletsDB_

    let pruneCheckpoints
            :: W.WalletId
            -> Quantity "block" Word32 -> W.BlockHeader
            -> SqlPersistT IO ()
        pruneCheckpoints wid epochStability tip = do
            let heights = Set.fromList $ sparseCheckpoints
                    (defaultSparseCheckpointsConfig epochStability)
                    (tip ^. #blockHeight)
            modifyDBMaybe walletsDB_ $ \ws ->
                case Map.lookup wid ws of
                    Nothing  -> (Nothing, ())
                    Just wal ->
                        let willKeep cp
                                = getBlockHeight cp `Set.member` heights
                            slots = Map.filter willKeep
                                        $ wal ^. (#checkpoints . #checkpoints)
                            delta = Adjust wid
                                [ UpdateCheckpoints
                                    $ RestrictTo
                                    $ Map.keys slots
                                ]
                        in  (Just delta, ())

    -- Delete the a wallet from the checkpoint DBVar
    let deleteCheckpoints :: W.WalletId -> SqlPersistT IO ()
        deleteCheckpoints wid = updateDBVar walletsDB_ $ Delete wid

    return DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { initializeWallet = \wid cp meta txs gp -> ExceptT $ do
            res <- handleConstraint (ErrWalletAlreadyExists wid) $
                insert_ (mkWalletEntity wid meta gp)
            when (isRight res) $ do
                insertGenesis wid cp txs
                updateTxHistory wid txs
            pure res

        , removeWallet = \wid -> ExceptT $
            selectWallet wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just _  -> Right <$> do
            deleteWhere [WalId ==. wid]
            deleteLooseTransactions
            deleteCheckpoints wid

        , listWallets = map unWalletKey <$> selectKeysList [] [Asc WalId]

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        , walletsDB = walletsDB_

        , putCheckpoint = \wid cp -> ExceptT $
            modifyDBMaybe walletsDB_ $ \ws ->
        case Map.lookup wid ws of
            Nothing -> (Nothing, Left $ ErrNoSuchWallet wid)
            Just _  ->
                let (prologue, wcp) = fromWallet cp
                    slot = getSlot wcp
                    delta = Just $ Adjust wid
                        [ UpdateCheckpoints $ PutCheckpoint slot wcp
                        , ReplacePrologue prologue
                        ]
                in  (delta, Right ())

        , readCheckpoint = readCheckpoint_

        , listCheckpoints = \wid -> do
            let toChainPoint = W.chainPointFromBlockHeader
            map (toChainPoint . blockHeaderFromEntity . entityVal) <$> selectList
                [ CheckpointWalletId ==. wid ]
                [ Asc CheckpointSlot ]

        , rollbackTo = \wid requestedPoint -> ExceptT $ do
            iomNearestCheckpoint <- modifyDBMaybe walletsDB_ $ \ws ->
                case Map.lookup wid ws of
                    Nothing  -> (Nothing, pure Nothing)
                    Just wal -> case findNearestPoint wal requestedPoint of
                        Nothing ->
                            ( Nothing
                            , throwIO $ ErrNoOlderCheckpoint wid requestedPoint
                            )
                        Just nearestPoint ->
                            ( Just $ Adjust wid
                                [ UpdateCheckpoints $ RollbackTo nearestPoint ]
                            , pure $ Map.lookup nearestPoint $
                                wal ^. #checkpoints ^. #checkpoints
                            )
            mNearestCheckpoint <- liftIO iomNearestCheckpoint

            case mNearestCheckpoint of
                Nothing  -> pure $ Left $ ErrNoSuchWallet wid
                Just wcp -> do
                    let nearestPoint = wcp ^. (#currentTip . #slotNo)
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
                    pure $ Right
                        $ W.chainPointFromBlockHeader
                        $ view #currentTip wcp

        , prune = \wid epochStability -> ExceptT $
            readCheckpoint_ wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just cp -> Right <$> do
            let tip = cp ^. #currentTip
            pruneCheckpoints wid epochStability tip
            pruneLocalTxSubmission wid epochStability tip
            deleteLooseTransactions

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \wid meta -> ExceptT $
            selectWallet wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just _ -> do
            updateWhere [WalId ==. wid]
                (mkWalletMetadataUpdate meta)
            pure $ Right ()

        , readWalletMeta = \wid ->
            readCheckpoint_ wid >>= \case
        Nothing -> pure Nothing
        Just cp -> do
            currentEpoch <- liftIO $
                interpretQuery ti (epochOf $ cp ^. #currentTip . #slotNo)
            readWalletDelegation ti wid currentEpoch
                >>= readWalletMetadata wid

        , putDelegationCertificate = \wid cert sl -> ExceptT $
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

        , isStakeKeyRegistered = \wid -> ExceptT $
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
        , putTxHistory = \wid txs -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    updateTxHistory wid txs
                    pure $ Right ()

        , readTxHistory = \wid minWithdrawal order range status ->
            readCheckpoint_ wid >>= \case
        Nothing -> pure []
        Just cp -> selectTxHistory cp
            ti wid minWithdrawal order $ catMaybes
            [ (TxMetaSlot >=.) <$> W.inclusiveLowerBound range
            , (TxMetaSlot <=.) <$> W.inclusiveUpperBound range
            , (TxMetaStatus ==.) <$> status
            ]

        , putLocalTxSubmission = \wid txid tx sl -> ExceptT $ do
            let errNoSuchWallet = ErrPutLocalTxSubmissionNoSuchWallet $
                    ErrNoSuchWallet wid
            let errNoSuchTx = ErrPutLocalTxSubmissionNoSuchTransaction $
                    ErrNoSuchTransaction wid txid

            selectWallet wid >>= \case
                Nothing -> pure $ Left errNoSuchWallet
                Just _ -> handleConstraint errNoSuchTx $ do
                    let record = LocalTxSubmission (TxId txid) wid sl tx
                    void $ upsert record [ LocalTxSubmissionLastSlot =. sl ]

        , readLocalTxSubmissionPending =
            fmap (map localTxSubmissionFromEntity)
            . listPendingLocalTxSubmissionQuery

        , updatePendingTxForExpiry = \wid tip -> ExceptT $
            selectWallet wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just _ -> Right <$> updatePendingTxForExpiryQuery wid tip

        , removePendingOrExpiredTx = \wid tid -> ExceptT $ do
            let errNoSuchWallet =
                    Left $ ErrRemoveTxNoSuchWallet $ ErrNoSuchWallet wid
            let errNoMorePending =
                    Left $ ErrRemoveTxAlreadyInLedger tid
            let errNoSuchTransaction =
                    Left $ ErrRemoveTxNoSuchTransaction $
                    ErrNoSuchTransaction wid tid
            selectWallet wid >>= \case
                Nothing -> pure errNoSuchWallet
                Just _  -> selectTxMeta wid tid >>= \case
                    Nothing -> pure errNoSuchTransaction
                    Just _ -> do
                        count <- deletePendingOrExpiredTx wid tid
                        pure $ if count == 0
                            then errNoMorePending
                            else Right ()

        , getTx = \wid tid -> ExceptT $
            readCheckpoint_ wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just cp -> do
            metas <- selectTxHistory cp
                ti wid Nothing W.Descending
                    [ TxMetaTxId ==. TxId tid ]
            case metas of
                [] -> pure (Right Nothing)
                meta:_ -> pure (Right $ Just meta)

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \wid key -> ExceptT $
            selectWallet wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just _ -> Right <$> do
            deleteWhere [PrivateKeyWalletId ==. wid]
            insert_ (mkPrivateKeyEntity wid key)

        , readPrivateKey = selectPrivateKey

        {-----------------------------------------------------------------------
                                 Blockchain Parameters
        -----------------------------------------------------------------------}

        , readGenesisParameters = selectGenesisParameters

        {-----------------------------------------------------------------------
                                 Delegation Rewards
        -----------------------------------------------------------------------}

        , putDelegationRewardBalance =
            \wid amt -> ExceptT $
            selectWallet wid >>= \case
        Nothing -> pure $ Left $ ErrNoSuchWallet wid
        Just _  -> Right <$> repsert
            (DelegationRewardKey wid)
            (DelegationReward wid (Coin.unsafeToWord64 amt))

        , readDelegationRewardBalance =
            \wid ->
                Coin.fromWord64 . maybe 0 (rewardAccountBalance . entityVal) <$>
                selectFirst [RewardWalletId ==. wid] []

        {-----------------------------------------------------------------------
                                     ACID Execution
        -----------------------------------------------------------------------}

        , atomically = withMVar queryLock . const . runQuery
        }
