{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

    -- * Open a database
    , withDBOpenFromFile
    , newDBOpenInMemory
    , retrieveWalletId
    , WalletDBLog (..)
    , DefaultFieldValues (..)

    -- * Database for a specific 'WalletId'
    , withDBLayer
    , withDBLayerInMemory
    , newDBLayerInMemory
    , withDBLayerFromDBOpen

    -- * Interfaces
    , PersistAddressBook (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.DB.Sqlite
    ( DBLog (..)
    , ForeignKeysSetting (ForeignKeysEnabled)
    , SqliteContext (..)
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
import Cardano.Slotting.Slot
    ( WithOrigin (..) )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.DB
    ( DBCheckpoints (..)
    , DBDelegation (..)
    , DBFactory (..)
    , DBLayer
    , DBLayerCollection (..)
    , DBOpen (..)
    , DBPrivateKey (..)
    , DBTxHistory (..)
    , DBWalletMeta (..)
    , DBWallets (..)
    , ErrWalletAlreadyInitialized (ErrWalletAlreadyInitialized)
    , ErrWalletNotInitialized (..)
    , mkDBLayerFromParts
    )
import Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..), migrateManually )
import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..)
    , DelegationCertificate (..)
    , DelegationReward (..)
    , EntityField (..)
    , Key (..)
    , PrivateKey (..)
    , StakeKeyCertificate (..)
    , TxMeta (..)
    , Wallet (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), TxId (..) )
import Cardano.Wallet.DB.Store.Checkpoints
    ( PersistAddressBook (..), blockHeaderFromEntity, mkStoreWallets )
import Cardano.Wallet.DB.Store.Meta.Model
    ( mkTxMetaFromEntity )
import Cardano.Wallet.DB.Store.QueryStore
    ( QueryStore (..) )
import Cardano.Wallet.DB.Store.Submissions.Layer
    ( pruneByFinality, rollBackSubmissions )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( submissionMetaFromTxMeta )
import Cardano.Wallet.DB.Store.Transactions.Decoration
    ( TxInDecorator
    , decorateTxInsForReadTxFromLookupTxOut
    , decorateTxInsForRelationFromLookupTxOut
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation (..), txCBORPrism )
import Cardano.Wallet.DB.Store.Transactions.TransactionInfo
    ( mkTransactionInfoFromReadTx, mkTransactionInfoFromRelation )
import Cardano.Wallet.DB.Store.Wallets.Layer
    ( QueryStoreTxWalletsHistory
    , QueryTxWalletsHistory (..)
    , newQueryStoreTxWalletsHistory
    )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( DeltaTxWalletsHistory (..) )
import Cardano.Wallet.DB.WalletState
    ( DeltaMap (..)
    , DeltaWalletState1 (..)
    , findNearestPoint
    , fromGenesis
    , fromWallet
    , getBlockHeight
    , getLatest
    , getSlot
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PersistPrivateKey (..), WalletKey (..) )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, firstSlotInEpoch, hoistTimeInterpreter, interpretQuery )
import Cardano.Wallet.Read.Eras
    ( EraValue )
import Cardano.Wallet.Read.Tx.CBOR
    ( parseTxFromCBOR )
import Control.DeepSeq
    ( force )
import Control.Exception
    ( evaluate, throw )
import Control.Monad
    ( forM, unless, (>=>) )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Coerce
    ( coerce )
import Data.DBVar
    ( loadDBVar, modifyDBMaybe, readDBVar, updateDBVar )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, maybeToList )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Store
    ( Store (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , Update (..)
    , deleteWhere
    , insert_
    , repsert
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (<.)
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
    ( Exception, bracket, throwIO )
import UnliftIO.MVar
    ( modifyMVar, modifyMVar_, newMVar, readMVar, withMVar )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Passphrase as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.Generics.Internal.VL as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

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
                        (_cleanup, db) <- newDBLayerInMemory tr' ti wid
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
                (Just defaultFieldValues)
                (databaseFile wid)
                ti
                wid
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
    DBOpen
-------------------------------------------------------------------------------}
-- | Open an SQLite database file and run an action on it.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created.
withDBOpenFromFile
    :: forall s k a
     . WalletKey k
    => Tracer IO WalletDBLog
       -- ^ Logging object
    -> Maybe DefaultFieldValues
       -- ^ Default database field values, used during manual migration.
       -- Use 'Nothing' to skip manual migrations.
    -> FilePath
       -- ^ Path to database file
    -> (DBOpen (SqlPersistT IO) IO s k -> IO a)
       -- ^ Action to run.
    -> IO a
withDBOpenFromFile tr defaultFieldValues dbFile action = do
    let trDB = contramap MsgDB tr
    let manualMigrations =
            maybe [] (migrateManually trDB (Proxy @k)) defaultFieldValues
    let autoMigrations   = migrateAll
    withConnectionPool trDB dbFile $ \pool -> do
        res <- newSqliteContext trDB pool manualMigrations autoMigrations
        case res of
            Left err -> throwIO err
            Right sqliteContext -> action =<< newQueryLock sqliteContext

-- | Open an SQLite database in-memory.
--
-- Returns a cleanup function which you should always use exactly once when
-- finished with the 'DBOpen'.
newDBOpenInMemory
    :: forall s k
     . Tracer IO WalletDBLog
       -- ^ Logging object
    -> IO (IO (), DBOpen (SqlPersistT IO) IO s k)
newDBOpenInMemory tr = do
    let tr' = contramap MsgDB tr
    (destroy, sqliteContext) <-
        newInMemorySqliteContext tr' [] migrateAll ForeignKeysEnabled
    db <- newQueryLock sqliteContext
    pure (destroy, db)

newQueryLock
    :: SqliteContext
    -> IO (DBOpen (SqlPersistT IO) IO s k)
newQueryLock SqliteContext{runQuery} = do
    -- NOTE
    -- The cache will not work properly unless 'atomically' is protected by a
    -- mutex (queryLock), which means no concurrent queries.
    queryLock <- newMVar () -- fixme: ADP-586
    pure $ DBOpen
        { atomically = withMVar queryLock . const . runQuery }

-- | Retrieve the wallet id from the database if it's initialized.
retrieveWalletId :: DBOpen (SqlPersistT IO) IO s k -> IO (Maybe W.WalletId)
retrieveWalletId DBOpen{atomically} =
    atomically
        $ fmap (walId . entityVal)
            <$> selectFirst [] []

{-------------------------------------------------------------------------------
    DBLayer
-------------------------------------------------------------------------------}

withDBLayerFromDBOpen
    :: forall k s a
     . (PersistAddressBook s, PersistPrivateKey (k 'RootK))
    => TimeInterpreter IO
    -- ^ Time interpreter for slot to time conversions
    -> W.WalletId
    -- ^ Wallet ID of the database
    -> (DBLayer IO s k -> IO a)
    -- ^ Action to run.
    -> DBOpen (SqlPersistT IO) IO s k
    -- ^ Already opened database.
    -> IO a
withDBLayerFromDBOpen ti wid action dbopen =
    newDBLayerFromDBOpen ti wid dbopen >>= action

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
    -> Maybe DefaultFieldValues
       -- ^ Default database field values, used during manual migration.
       -- Use 'Nothing' to skip manual migrations.
    -> FilePath
       -- ^ Path to database file
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions.
    -> W.WalletId
         -- ^ Wallet ID of the database.
    -> (DBLayer IO s k -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer tr defaultFieldValues dbFile ti wid action =
    withDBOpenFromFile tr defaultFieldValues dbFile
        $ newDBLayerFromDBOpen ti wid >=> action

-- | Runs an IO action with a new 'DBLayer' backed by a sqlite in-memory
-- database.
withDBLayerInMemory
    :: forall s k a.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => Tracer IO WalletDBLog
       -- ^ Logging object.
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> W.WalletId
       -- ^ Wallet ID of the database.
    -> (DBLayer IO s k -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayerInMemory tr ti wid action = bracket
    (newDBLayerInMemory tr ti wid) fst (action . snd)

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
       -- ^ Logging object.
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions.
    -> W.WalletId
       -- ^ Wallet ID of the database.
    -> IO (IO (), DBLayer IO s k)
newDBLayerInMemory tr ti wid = do
    (destroy, dbopen) <- newDBOpenInMemory tr
    db <- newDBLayerFromDBOpen ti wid dbopen
    pure (destroy, db)

-- | From a 'DBOpen', create a database which can store the state
-- of one wallet with a specific 'WalletId'.
newDBLayerFromDBOpen
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> W.WalletId
       -- ^ Wallet ID of the database.
    -> DBOpen (SqlPersistT IO) IO s k
       -- ^ A (thread-)safe wrapper for query execution.
    -> IO (DBLayer IO s k)
newDBLayerFromDBOpen ti wid_ DBOpen{atomically=runQuery} = mdo

    -- FIXME LATER during ADP-1043:
    --   Handle the case where loading the database fails.
    walletsDB <- runQuery $ loadDBVar mkStoreWallets
    let transactionsQS = newQueryStoreTxWalletsHistory

    -- Insert genesis checkpoint into the DBVar.
    -- Throws an internal error if the checkpoint is not actually at genesis.
    let insertCheckpointGenesis wid cp =
            case fromGenesis cp of
                Nothing -> throwIO $ ErrInitializeGenesisAbsent wid header
                Just wallet ->
                    updateDBVar walletsDB $ Insert wid wallet
          where
            header = cp ^. #currentTip

    -- Retrieve the latest checkpoint from the DBVar
    let readCheckpoint
            ::  SqlPersistT IO (Maybe (W.Wallet s))
        readCheckpoint =
            fmap (getLatest . snd). Map.lookupMin <$> readDBVar walletsDB

    let pruneCheckpoints
            :: W.WalletId
            -> Quantity "block" Word32 -> W.BlockHeader
            -> SqlPersistT IO ()
        pruneCheckpoints wid epochStability tip = do
            let heights = Set.fromList $ sparseCheckpoints
                    (defaultSparseCheckpointsConfig epochStability)
                    (tip ^. #blockHeight)
            modifyDBMaybe walletsDB $ \ws ->
                case Map.lookup wid ws of
                    Nothing  -> (Nothing, ())
                    Just wal ->
                        let willKeep cp = getBlockHeight cp `Set.member` heights
                            slots = Map.filter willKeep (wal ^. #checkpoints . #checkpoints)
                            delta = Adjust wid
                                [ UpdateCheckpoints [ RestrictTo $ Map.keys slots ] ]
                        in  (Just delta, ())

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}
    let
      dbWallets = DBWallets
        { initializeWallet_ = \cp meta txs gp -> do
            res <- lift $ runExceptT $ getWalletId_ dbWallets
            case res of
                Left ErrWalletNotInitialized -> lift $ do
                    insert_ $ mkWalletEntity wid_ meta gp
                    insertCheckpointGenesis wid_ cp
                    updateS (store transactionsQS) Nothing $
                                ExpandTxWalletsHistory wid_ txs
                Right _ -> throwE ErrWalletAlreadyInitialized

        , readGenesisParameters_ = selectGenesisParameters

        , getWalletId_ = do
            ws <- lift $ map unWalletKey <$> selectKeysList [] [Asc WalId]
            case ws of
                [w] -> pure w
                _ -> throwE ErrWalletNotInitialized

        , hasWallet_ = fmap isJust . selectWallet
        }

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
    let
      dbCheckpoints = DBCheckpoints
        { walletsDB_ = walletsDB

        , putCheckpoint_ = \cp -> ExceptT $ do
            modifyDBMaybe walletsDB $ \ws ->
                if null ws
                    then (Nothing, Left ErrWalletNotInitialized)
                    else
                        let (prologue, wcp) = fromWallet cp
                            slot = getSlot wcp
                            delta = Just $ Adjust wid_
                                [ UpdateCheckpoints [ PutCheckpoint slot wcp ]
                                , ReplacePrologue prologue
                                ]
                        in  (delta, Right ())

        , readCheckpoint_ = readCheckpoint

        , listCheckpoints_ = do
            let toChainPoint = W.chainPointFromBlockHeader
            map (toChainPoint . blockHeaderFromEntity . entityVal) <$> selectList
                [ CheckpointWalletId ==. wid_ ]
                [ Asc CheckpointSlot ]
        }

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}
    let
      lookupTx = queryS transactionsQS . GetByTxId
      lookupTxOut = queryS transactionsQS . GetTxOut
      dbTxHistory = DBTxHistory
        { putTxHistory_ = \wid ->
            updateS (store transactionsQS) Nothing
                . ExpandTxWalletsHistory wid

        , readTxHistory_ = \range tip mlimit order -> do
            txs <- queryS transactionsQS $ SomeMetas range mlimit order
            forM txs $
                selectTransactionInfo ti tip lookupTx lookupTxOut

        , getTx_ = \txid tip -> do
            txm <- queryS transactionsQS $ OneMeta $ TxId txid
            forM txm $
                selectTransactionInfo ti tip lookupTx lookupTxOut

        , mkDecorator_ = mkDecorator transactionsQS
        }

    let rollbackTo_ wid requestedPoint = do
            mNearestCheckpoint <-
                ExceptT $ modifyDBMaybe walletsDB $ \ws ->
                    case Map.lookup wid ws of
                        Nothing  ->
                            ( Nothing
                            , pure Nothing
                            )
                        Just wal -> case findNearestPoint wal requestedPoint of
                            Nothing ->
                                ( Nothing
                                , throw
                                    $ ErrNoOlderCheckpoint wid requestedPoint
                                )
                            Just nearestPoint ->
                                ( Just $ Adjust wid
                                    [ UpdateCheckpoints
                                        [ RollbackTo nearestPoint ]
                                    , UpdateSubmissions
                                        [ rollBackSubmissions $
                                             case nearestPoint of
                                                { At s -> s; Origin -> 0 }
                                        ]
                                    ]
                                , pure $
                                    Map.lookup nearestPoint
                                        (wal ^. #checkpoints . #checkpoints)
                                )

            case mNearestCheckpoint of
                Nothing  -> ExceptT $ pure $ Left ErrWalletNotInitialized
                Just wcp -> lift $ do
                    let nearestPoint = wcp ^. #currentTip . #slotNo
                    deleteDelegationCertificates wid
                        [ CertSlot >. nearestPoint
                        ]
                    deleteStakeKeyCerts wid
                        [ StakeKeyCertSlot >. nearestPoint
                        ]
                    updateS (store transactionsQS) Nothing $
                        RollbackTxWalletsHistory nearestPoint

                    pure
                        $ W.chainPointFromBlockHeader
                        $ view #currentTip wcp

    let prune_ wid epochStability finalitySlot = do
            ExceptT $ do
                readCheckpoint >>= \case
                    Nothing -> pure $ Left ErrWalletNotInitialized
                    Just cp -> Right <$> do
                        let tip = cp ^. #currentTip
                        pruneCheckpoints wid epochStability tip
            lift $ updateDBVar walletsDB $ Adjust wid
                [ UpdateSubmissions [pruneByFinality finalitySlot]
                ]

        {-----------------------------------------------------------------------
                                    Wallet Delegation
        -----------------------------------------------------------------------}
    let dbDelegation = mkDBDelegation ti
        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}
    let
      dbWalletMeta = DBWalletMeta
        { putWalletMeta_ = \wid meta -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left ErrWalletNotInitialized
                Just _ -> do
                    updateWhere [WalId ==. wid]
                        (mkWalletMetadataUpdate meta)
                    pure $ Right ()

        , readWalletMeta_ = readWalletMetadata
        }

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}
    let dbPrivateKey = mkDBPrivateKey

        {-----------------------------------------------------------------------
                                     ACID Execution
        -----------------------------------------------------------------------}

    let atomically_ = runQuery

    pure $ mkDBLayerFromParts ti wid_ DBLayerCollection{..}

mkDecorator
    :: QueryStoreTxWalletsHistory
    -> TxInDecorator (EraValue Read.Tx) (SqlPersistT IO)
mkDecorator transactionsQS =
    decorateTxInsForReadTxFromLookupTxOut lookupTxOut
  where
    lookupTxOut = queryS transactionsQS . GetTxOut

readWalletMetadata
    :: W.WalletId
    -> SqlPersistT IO (Maybe W.WalletMetadata)
readWalletMetadata wid =
     fmap (metadataFromEntity . entityVal)
        <$> selectFirst [WalId ==. wid] []

{-----------------------------------------------------------------------
                            Wallet Delegation
-----------------------------------------------------------------------}

mkDBDelegation ::
    TimeInterpreter IO -> W.WalletId -> DBDelegation (SqlPersistT IO)
mkDBDelegation ti wid =
    DBDelegation
        { isStakeKeyRegistered_
        , putDelegationCertificate_
        , putDelegationRewardBalance_
        , readDelegationRewardBalance_
        , readDelegation_
        }
  where
    isStakeKeyRegistered_ :: SqlPersistT IO Bool
    isStakeKeyRegistered_ = do
        val <- fmap entityVal <$>
            selectFirst [StakeKeyCertWalletId ==. wid] [Desc StakeKeyCertSlot]
        pure $
            case val of
                Nothing -> False
                Just (StakeKeyCertificate _ _ status) ->
                    status == W.StakeKeyRegistration

    putDelegationCertificate_ ::
        W.DelegationCertificate -> W.SlotNo -> SqlPersistT IO ()
    putDelegationCertificate_ cert sl =
        case cert of
            W.CertDelegateNone _ -> do
                repsert
                    (DelegationCertificateKey wid sl)
                    (DelegationCertificate wid sl Nothing)
                repsert
                    (StakeKeyCertificateKey wid sl)
                    (StakeKeyCertificate wid sl W.StakeKeyDeregistration)
            W.CertDelegateFull _ pool ->
                repsert
                    (DelegationCertificateKey wid sl)
                    (DelegationCertificate wid sl (Just pool))
            W.CertRegisterKey _ ->
                repsert
                    (StakeKeyCertificateKey wid sl)
                    (StakeKeyCertificate wid sl W.StakeKeyRegistration)

    putDelegationRewardBalance_ :: Coin.Coin -> SqlPersistT IO ()
    putDelegationRewardBalance_ amount =
        repsert
            (DelegationRewardKey wid)
            (DelegationReward wid (Coin.unsafeToWord64 amount))

    readDelegationRewardBalance_ :: SqlPersistT IO Coin.Coin
    readDelegationRewardBalance_ =
        Coin.fromWord64 . maybe 0 (rewardAccountBalance . entityVal) <$>
            selectFirst [RewardWalletId ==. wid] []

    readDelegation_ :: W.EpochNo -> SqlPersistT IO W.WalletDelegation
    readDelegation_ epoch = case epoch of
        0 -> do
            currEpochStartSlot <-
                liftIO $ interpretQuery ti $ firstSlotInEpoch epoch
            let nextDelegations =
                    readDelegationStatus [CertSlot >=. currEpochStartSlot]
                    <&> maybeToList . (<&> W.WalletDelegationNext (epoch + 2))
            W.WalletDelegation W.NotDelegating <$> nextDelegations
        _ -> do
            (prevEpochStartSlot, currEpochStartSlot) <-
                liftIO $ interpretQuery ti $
                    (,) <$> firstSlotInEpoch (epoch - 1)
                        <*> firstSlotInEpoch epoch
            let currentDelegation =
                    readDelegationStatus [CertSlot <. prevEpochStartSlot]
                        <&> fromMaybe W.NotDelegating
            let nextDelegations = catMaybes <$> sequence
                    [ readDelegationStatus
                        [ CertSlot >=. prevEpochStartSlot
                        , CertSlot <. currEpochStartSlot
                        ] <&> (<&> W.WalletDelegationNext (epoch + 1))
                    , readDelegationStatus
                        [CertSlot >=. currEpochStartSlot]
                        <&> (<&> W.WalletDelegationNext (epoch + 2))
                    ]
            W.WalletDelegation <$> currentDelegation <*> nextDelegations
      where
        readDelegationStatus =
            (fmap . fmap) toWalletDelegationStatus
                . readDelegationCertificate wid

readDelegationCertificate
    :: W.WalletId
    -> [Filter DelegationCertificate]
    -> SqlPersistT IO (Maybe DelegationCertificate)
readDelegationCertificate wid filters = fmap entityVal
    <$> selectFirst ((CertWalletId ==. wid) : filters) [Desc CertSlot]

{-------------------------------------------------------------------------------
    Conversion between types
        from the `persistent` database (Cardano.Wallet.DB.Sqlite.Schema)
        and from the wallet core ( Cardano.Wallet.Primitive.Types.*)
-------------------------------------------------------------------------------}

toWalletDelegationStatus
    :: DelegationCertificate
    -> W.WalletDelegationStatus
toWalletDelegationStatus = \case
    DelegationCertificate _ _ Nothing ->
        W.NotDelegating
    DelegationCertificate _ _ (Just pool) ->
        W.Delegating pool

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> W.GenesisParameters -> Wallet
mkWalletEntity wid meta gp = Wallet
    { walId = wid
    , walName = meta ^. #name . coerce
    , walCreationTime = meta ^. #creationTime
    , walPassphraseLastUpdatedAt = W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walPassphraseScheme = W.passphraseScheme <$> meta ^. #passphraseInfo
    , walGenesisHash = BlockId (coerce (gp ^. #getGenesisBlockHash))
    , walGenesisStart = coerce (gp ^. #getGenesisBlockDate)
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

metadataFromEntity :: Wallet -> W.WalletMetadata
metadataFromEntity wal = W.WalletMetadata
    { name = W.WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo
        <$> walPassphraseLastUpdatedAt wal
        <*> walPassphraseScheme wal
    }

genesisParametersFromEntity
    :: Wallet
    -> W.GenesisParameters
genesisParametersFromEntity (Wallet _ _ _ _ _ hash startTime) =
    W.GenesisParameters
        { W.getGenesisBlockHash = coerce (getBlockId hash)
        , W.getGenesisBlockDate = W.StartTime startTime
        }

{-----------------------------------------------------------------------
                    Private Key store
-----------------------------------------------------------------------}
mkDBPrivateKey
    :: forall k. PersistPrivateKey (k 'RootK)
    => W.WalletId
    -> DBPrivateKey (SqlPersistT IO) k
mkDBPrivateKey wid = DBPrivateKey
    { putPrivateKey_ = \key -> do
        deleteWhere [PrivateKeyWalletId ==. wid]
        insert_ (mkPrivateKeyEntity wid key)
    , readPrivateKey_ = selectPrivateKey wid
    }

mkPrivateKeyEntity
    :: PersistPrivateKey (k 'RootK)
    => W.WalletId
    -> (k 'RootK XPrv, W.PassphraseHash)
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
    -> (k 'RootK XPrv, PassphraseHash)
privateKeyFromEntity (PrivateKey _ k h) =
    unsafeDeserializeXPrv (k, h)

{-------------------------------------------------------------------------------
    SQLite database operations
-------------------------------------------------------------------------------}

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid =
    fmap entityVal <$> selectFirst [WalId ==. wid] []

-- | Delete stake key certificates for a wallet.
deleteStakeKeyCerts
    :: W.WalletId
    -> [Filter StakeKeyCertificate]
    -> SqlPersistT IO ()
deleteStakeKeyCerts wid filters =
    deleteWhere ((StakeKeyCertWalletId ==. wid) : filters)

-- | Delete all delegation certificates matching the given filter
deleteDelegationCertificates
    :: W.WalletId
    -> [Filter DelegationCertificate]
    -> SqlPersistT IO ()
deleteDelegationCertificates wid filters = do
    deleteWhere ((CertWalletId ==. wid) : filters)

-- | For a given 'TxMeta', read all necessary data to construct
-- the corresponding 'W.TransactionInfo'.
--
-- Assumption: The 'TxMeta' has a result when applying the given
-- lookup function.
--
-- Note: Transaction inputs are references to the outputs of
-- previous transactions. Given any input, the Ada quantity and
-- assets associated with it are unknown until we look them up.
-- Here, 'selectTransactionInfo' will try to look up those outputs that
-- are in the transaction history of the wallet,
-- but the function will not attempt to look up all possible outputs.
-- This approach typically provides enough information
-- for /outgoing/ payments, but less so for /ingoing/ payments.
-- This function will simply decode the cbor, when present.

selectTransactionInfo
    :: MonadIO m
    => TimeInterpreter IO
    -> W.BlockHeader
    -> (TxId -> m (Maybe (Either TxRelation CBOR)))
    -> ((TxId, Word32) -> m (Maybe W.TxOut))
    -> TxMeta
    -> m W.TransactionInfo
selectTransactionInfo ti tip lookupTx lookupTxOut meta = do
    let
        err = error $ "Transaction not found: " <> show meta
        wmeta = mkTxMetaFromEntity meta
    transaction <- fromMaybe err <$> lookupTx (txMetaTxId meta)
    result <- case transaction of
        Left relation -> do
            decoration <-
                decorateTxInsForRelationFromLookupTxOut lookupTxOut relation
            mkTransactionInfoFromRelation
                (hoistTimeInterpreter liftIO ti)
                tip
                relation
                decoration
                meta
        Right cbor -> do
            case L.match txCBORPrism cbor of
                Right (_, txCBOR) -> case parseTxFromCBOR txCBOR of
                    Right cbor' -> do
                        decoration <-
                            decorateTxInsForReadTxFromLookupTxOut
                                lookupTxOut
                                cbor'
                        mkTransactionInfoFromReadTx
                            (hoistTimeInterpreter liftIO ti)
                            tip
                            cbor'
                            decoration
                            (submissionMetaFromTxMeta wmeta (error "no resub"))
                            (wmeta ^. #status)
                    Left _ -> error "failed to parse cbor"
                Left _ -> error "failed to extract cbor for era"
    liftIO . evaluate $ force result

selectPrivateKey
    :: (MonadIO m, PersistPrivateKey (k 'RootK))
    => W.WalletId
    -> SqlPersistT m (Maybe (k 'RootK XPrv, PassphraseHash))
selectPrivateKey wid = do
    keys <- selectFirst [PrivateKeyWalletId ==. wid] []
    pure $ (privateKeyFromEntity . entityVal) <$> keys

selectGenesisParameters
    :: MonadIO m
    => W.WalletId
    -> SqlPersistT m (Maybe W.GenesisParameters)
selectGenesisParameters wid = do
    gp <- selectFirst [WalId ==. wid] []
    pure $ (genesisParametersFromEntity . entityVal) <$> gp

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}
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

{-------------------------------------------------------------------------------
    Internal errors
-------------------------------------------------------------------------------}
-- | A fatal exception thrown when trying to rollback but, there's no checkpoint
-- to rollback to. The database maintain the invariant that there's always at
-- least one checkpoint (the first one made for genesis) present in the
-- database.
--
-- If we don't find any checkpoint, it means that this invariant has been
-- violated.
data ErrRollbackTo = ErrNoOlderCheckpoint W.WalletId W.Slot deriving (Show)
instance Exception ErrRollbackTo

-- | Can't initialize a wallet because the given 'BlockHeader' is not genesis.
data ErrInitializeGenesisAbsent
    = ErrInitializeGenesisAbsent W.WalletId W.BlockHeader deriving (Eq, Show)

instance Exception ErrInitializeGenesisAbsent
