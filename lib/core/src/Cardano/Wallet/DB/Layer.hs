{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
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
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchTransaction (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    )
import Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..), migrateManually )
import Cardano.Wallet.DB.Sqlite.Schema
    ( DelegationCertificate (..)
    , DelegationReward (..)
    , EntityField (..)
    , Key (..)
    , LocalTxSubmission (..)
    , PrivateKey (..)
    , StakeKeyCertificate (..)
    , TxMeta (..)
    , TxWithdrawal (txWithdrawalAmount)
    , Wallet (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), TxId (..) )
import Cardano.Wallet.DB.Store.Checkpoints
    ( PersistAddressBook (..), blockHeaderFromEntity, mkStoreWallets )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..)
    , ManipulateTxMetaHistory (..)
    , TxMetaHistory (..)
    )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( TxLocalSubmissionHistory (..) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxHistoryF (..), decorateWithTxOuts, withdrawals )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaWalletsMetaWithSubmissions (..)
    , TxWalletsHistory
    , mkTransactionInfo
    )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( DeltaTxWalletsHistory (..), mkStoreTxWalletsHistory )
import Cardano.Wallet.DB.WalletState
    ( DeltaMap (..)
    , DeltaWalletState1 (..)
    , ErrNoSuchWallet (..)
    , findNearestPoint
    , fromGenesis
    , fromWallet
    , getBlockHeight
    , getLatest
    , getSlot
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PersistPrivateKey (..), WalletKey (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, firstSlotInEpoch, interpretQuery )
import Cardano.Wallet.Primitive.Types.Tx
    ( TransactionInfo (..), TxMeta (..) )
import Control.Exception
    ( throw )
import Control.Monad
    ( forM, guard, unless, void, when, (<=<) )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Coerce
    ( coerce )
import Data.DBVar
    ( loadDBVar, modifyDBMaybe, readDBVar, updateDBVar )
import Data.Either
    ( isRight )
import Data.Foldable
    ( toList )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( sortOn )
import Data.Maybe
    ( catMaybes, fromMaybe, listToMaybe, maybeToList )
import Data.Ord
    ( Down (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Word
    ( Word32 )
import Database.Persist.Class
    ( toPersistValue )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , Single (..)
    , Update (..)
    , deleteWhere
    , insert_
    , rawExecute
    , rawSql
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

import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import qualified Cardano.Wallet.DB.Store.Submissions.Model as TxSubmissions
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Passphrase as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model (TxHistoryWithCBOR(TxHistoryWithCBOR))

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
    (destroy, ctx) <-
        newInMemorySqliteContext tr' [] migrateAll ForeignKeysEnabled
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
    transactionsDBVar <- runQuery $ loadDBVar mkStoreTxWalletsHistory

    -- NOTE
    -- The cache will not work properly unless 'atomically' is protected by a
    -- mutex (queryLock), which means no concurrent queries.
    queryLock <- newMVar () -- fixme: ADP-586

    -- Insert genesis checkpoint into the DBVar.
    -- Throws an internal error if the checkpoint is not actually at genesis.
    let insertCheckpointGenesis wid cp =
            case fromGenesis cp of
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
                        let willKeep cp = getBlockHeight cp `Set.member` heights
                            slots = Map.filter willKeep (wal ^. #checkpoints . #checkpoints)
                            delta = Adjust wid
                                [ UpdateCheckpoints [ RestrictTo $ Map.keys slots ] ]
                        in  (Just delta, ())

    -- Delete the a wallet from the checkpoint DBVar
    let deleteCheckpoints :: W.WalletId -> SqlPersistT IO ()
        deleteCheckpoints wid = updateDBVar walletsDB_ $ Delete wid

    return DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { initializeWallet = \wid cp meta txs gp -> do
            ExceptT $ do
                res <- handleConstraint (ErrWalletAlreadyExists wid) $
                    insert_ (mkWalletEntity wid meta gp)
                when (isRight res) $ do
                    insertCheckpointGenesis wid cp
                    void $ modifyDBMaybe transactionsDBVar $ \(_txsOld, _ws) ->
                        let delta = Just $ ExpandTxWalletsHistory wid txs
                        in  (delta, Right ())
                pure res
        , removeWallet = \wid -> do
            ExceptT $ do
                selectWallet wid >>= \case
                    Nothing -> pure $ Left $ ErrNoSuchWallet wid
                    Just _  -> Right <$> do
                        deleteWhere [WalId ==. wid]
                        deleteCheckpoints wid
            ExceptT $ modifyDBMaybe transactionsDBVar $ \_ ->
                        let
                            delta = Just $ RemoveWallet wid
                        in  (delta, Right ())
            ExceptT $ modifyDBMaybe transactionsDBVar $ \_ ->
                        let
                            delta = Just GarbageCollectTxWalletsHistory
                        in  (delta, Right ())
        , listWallets = map unWalletKey <$> selectKeysList [] [Asc WalId]

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        , walletsDB = walletsDB_

        , putCheckpoint = \wid cp -> ExceptT $ do
            modifyDBMaybe walletsDB_ $ \ws ->
                case Map.lookup wid ws of
                    Nothing -> (Nothing, Left $ ErrNoSuchWallet wid)
                    Just _  ->
                        let (prologue, wcp) = fromWallet cp
                            slot = getSlot wcp
                            delta = Just $ Adjust wid
                                [ UpdateCheckpoints [ PutCheckpoint slot wcp ]
                                , ReplacePrologue prologue
                                ]
                        in  (delta, Right ())

        , readCheckpoint = readCheckpoint_

        , listCheckpoints = \wid -> do
            let toChainPoint = W.chainPointFromBlockHeader
            map (toChainPoint . blockHeaderFromEntity . entityVal) <$> selectList
                [ CheckpointWalletId ==. wid ]
                [ Asc CheckpointSlot ]

        , rollbackTo = \wid requestedPoint -> do
            mNearestCheckpoint <-  ExceptT $ do
                modifyDBMaybe walletsDB_ $ \ws ->
                    case Map.lookup wid ws of
                        Nothing  -> (Nothing, pure Nothing)
                        Just wal -> case findNearestPoint wal requestedPoint of
                            Nothing ->
                                ( Nothing
                                , throw $ ErrNoOlderCheckpoint wid requestedPoint
                                )
                            Just nearestPoint ->
                                ( Just $ Adjust wid
                                    [ UpdateCheckpoints [ RollbackTo nearestPoint ] ]
                                , pure $ Map.lookup nearestPoint $
                                    wal ^. #checkpoints . #checkpoints
                                )

            case mNearestCheckpoint of
                Nothing  -> ExceptT $ pure $ Left $ ErrNoSuchWallet wid
                Just wcp -> do
                    let nearestPoint = wcp ^. #currentTip . #slotNo
                    lift $ deleteDelegationCertificates wid
                        [ CertSlot >. nearestPoint
                        ]
                    lift $ deleteStakeKeyCerts wid
                        [ StakeKeyCertSlot >. nearestPoint
                        ]
                    ExceptT $ modifyDBMaybe transactionsDBVar $ \_ ->
                        let
                            delta = Just
                                $ ChangeTxMetaWalletsHistory wid
                                $ ChangeMeta
                                $ Manipulate
                                $ RollBackTxMetaHistory nearestPoint
                        in  (delta, Right ())
                    pure
                        $ W.chainPointFromBlockHeader
                        $ view #currentTip wcp

        , prune = \wid epochStability -> do
            ExceptT $ do
                readCheckpoint_ wid >>= \case
                    Nothing -> pure $ Left $ ErrNoSuchWallet wid
                    Just cp -> Right <$> do
                        let tip = cp ^. #currentTip
                        pruneCheckpoints wid epochStability tip
                        pruneLocalTxSubmission wid epochStability tip
            lift $ modifyDBMaybe transactionsDBVar $ \_ ->
                (Just GarbageCollectTxWalletsHistory, ())

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \wid meta -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    updateWhere [WalId ==. wid]
                        (mkWalletMetadataUpdate meta)
                    pure $ Right ()

        , readWalletMeta = \wid -> do
            readCheckpoint_ wid >>= \case
                Nothing -> pure Nothing
                Just cp -> do
                    currentEpoch <- liftIO $
                        interpretQuery ti (epochOf $ cp ^. #currentTip . #slotNo)
                    readWalletDelegation ti wid currentEpoch
                        >>= readWalletMetadata wid

        , putDelegationCertificate = \wid cert sl -> ExceptT $ do
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

        , isStakeKeyRegistered = \wid -> ExceptT $ do
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
                Just _ -> modifyDBMaybe transactionsDBVar $ \_ ->
                    let
                        delta = Just $ ExpandTxWalletsHistory wid txs
                    in  (delta, Right ())

        , readTxHistory = \wid minWithdrawal order range status -> do
            readCheckpoint_ wid >>= \case
                Nothing -> pure []
                Just cp -> do
                    txHistory <- readDBVar transactionsDBVar
                    let filtering DB.TxMeta{..} = and $ catMaybes
                            [ (txMetaSlot >=) <$> W.inclusiveLowerBound range
                            , (txMetaSlot <=) <$> W.inclusiveUpperBound range
                            , (txMetaStatus ==) <$> status
                            ]
                    lift $ selectTxHistory cp ti wid minWithdrawal
                        order filtering txHistory

        , putLocalTxSubmission = \wid txid tx sl -> do
            let errNoSuchWallet = ErrPutLocalTxSubmissionNoSuchWallet $
                    ErrNoSuchWallet wid
            let errNoSuchTx = ErrPutLocalTxSubmissionNoSuchTransaction $
                    ErrNoSuchTransaction wid txid
            ExceptT $ modifyDBMaybe transactionsDBVar
                    $ \(_txsOld, ws) -> do
                case Map.lookup wid ws of
                        Nothing -> (Nothing, Left errNoSuchWallet)
                        Just (TxMetaHistory metas, _)  -> case
                            Map.lookup (TxId txid) metas of
                                Nothing -> (Nothing, Left errNoSuchTx)
                                Just _ ->
                                    let
                                        delta = Just
                                            $ ChangeTxMetaWalletsHistory wid
                                            $ ChangeSubmissions
                                            $ TxSubmissions.Expand
                                            $ TxLocalSubmissionHistory
                                            $ Map.fromList [
                                                ( TxId txid
                                                , LocalTxSubmission (TxId txid)
                                                    wid sl tx
                                                )
                                            ]
                                    in  (delta, Right ())


        , readLocalTxSubmissionPending =
            fmap (map localTxSubmissionFromEntity)
            . listPendingLocalTxSubmissionQuery

        , updatePendingTxForExpiry = \wid tip -> ExceptT $
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> modifyDBMaybe transactionsDBVar $ \_ ->
                    let
                        delta = Just
                            $ ChangeTxMetaWalletsHistory wid
                            $ ChangeMeta
                            $ Manipulate
                            $ AgeTxMetaHistory tip
                    in  (delta, Right ())

        , removePendingOrExpiredTx = \wid txId ->
            let noTx =
                    (   Nothing
                        , Left
                            $ ErrRemoveTxNoSuchTransaction
                            $ ErrNoSuchTransaction wid txId
                    )
            in ExceptT $ selectWallet wid >>= \case
                Nothing -> pure $ Left
                    $ ErrRemoveTxNoSuchWallet
                    $ ErrNoSuchWallet wid
                Just _ -> modifyDBMaybe transactionsDBVar
                    $ \(_ , ws) -> fromMaybe noTx $ do
                        (TxMetaHistory metas, _) <- Map.lookup wid ws
                        DB.TxMeta{..} <- Map.lookup (TxId txId) metas
                        pure $
                            if txMetaStatus == W.InLedger
                            then (Nothing
                                , Left $ ErrRemoveTxAlreadyInLedger txId)
                            else
                                let delta = Just
                                        $ ChangeTxMetaWalletsHistory wid
                                        $ ChangeMeta
                                        $ Manipulate
                                        $ PruneTxMetaHistory $ TxId txId
                                in  (delta, Right ())

        , getTx = \wid tid -> ExceptT $ do
            readCheckpoint_ wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just cp -> do
                    txHistory <- readDBVar transactionsDBVar
                    metas <- lift $ selectTxHistory cp
                        ti wid Nothing W.Descending
                            (\meta -> txMetaTxId meta == TxId tid )
                            txHistory
                    pure $ Right $ listToMaybe metas

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \wid key -> ExceptT $ do
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
            \wid amt -> ExceptT $ do
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
        (eMinus1, e) <- liftIO $ interpretQuery ti $
            (,) <$> firstSlotInEpoch (epoch - 1) <*> firstSlotInEpoch epoch
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


genesisParametersFromEntity
    :: Wallet
    -> W.GenesisParameters
genesisParametersFromEntity (Wallet _ _ _ _ _ hash startTime) =
    W.GenesisParameters
        { W.getGenesisBlockHash = coerce (getBlockId hash)
        , W.getGenesisBlockDate = W.StartTime startTime
        }

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

-- This relies on available information from the database to reconstruct coin
-- selection information for __outgoing__ payments. We can't however guarantee
-- that we have such information for __incoming__ payments (we usually don't
-- have it).
--
-- To reliably provide this information for incoming payments, it should be
-- looked up when applying blocks from the global ledger, but that is future
-- work.
--

-- See also: issue #573.
selectTxHistory
    :: Monad m
    => W.Wallet s
    -> TimeInterpreter m
    -> W.WalletId
    -> Maybe W.Coin
    -> W.SortOrder
    -> (DB.TxMeta -> Bool)
    -> TxWalletsHistory
    -> m [W.TransactionInfo]
selectTxHistory cp ti wid minWithdrawal order whichMeta
    (TxHistoryWithCBOR txHistory _, wmetas) = do
    tinfos <- mapM (uncurry $ mkTransactionInfo ti (W.currentTip cp)) $ do
        (TxMetaHistory metas, _) <- maybeToList $ Map.lookup wid wmetas
        meta <- toList metas
        guard $  whichMeta meta
        transaction <- maybeToList $ Map.lookup (txMetaTxId meta) txs
        guard $ maybe
            True
            (\coin -> any (>= coin)
                $ txWithdrawalAmount <$>  withdrawals transaction)
            minWithdrawal
        pure (transaction, meta)
    pure $ sortTx tinfos
    where
        sortTx = case order of
            W.Ascending -> sortOn
                $ (,) <$> slotNo . txInfoMeta <*> Down . txInfoId
            W.Descending -> sortOn
                $ (,) <$> (Down . slotNo . txInfoMeta) <*> txInfoId
        TxHistoryF txs = decorateWithTxOuts txHistory


-- | Returns the initial submission slot and submission record for all pending
-- transactions in the wallet.
listPendingLocalTxSubmissionQuery
    :: W.WalletId
    -> SqlPersistT IO [(W.SlotNo, LocalTxSubmission)]
listPendingLocalTxSubmissionQuery wid = fmap unRaw <$> rawSql query params
  where
    -- fixme: sort results
    query =
        "SELECT tx_meta.slot,?? " <>
        "FROM tx_meta INNER JOIN local_tx_submission " <>
        "ON tx_meta.wallet_id=local_tx_submission.wallet_id " <>
        "    AND tx_meta.tx_id=local_tx_submission.tx_id " <>
        "WHERE tx_meta.wallet_id=? AND tx_meta.status=? " <>
        "ORDER BY local_tx_submission.wallet_id, local_tx_submission.tx_id"
    params = [toPersistValue wid, toPersistValue W.Pending]
    unRaw (Single sl, Entity _ tx) = (sl, tx)

localTxSubmissionFromEntity
    :: (W.SlotNo, LocalTxSubmission)
    -> W.LocalTxSubmissionStatus W.SealedTx
localTxSubmissionFromEntity (sl0, LocalTxSubmission (TxId txid) _ sl tx) =
    W.LocalTxSubmissionStatus txid tx sl0 sl

-- | Remove transactions from the local submission pool once they can no longer
-- be rolled back.
pruneLocalTxSubmission
    :: W.WalletId
    -> Quantity "block" Word32
    -> W.BlockHeader
    -> SqlPersistT IO ()
pruneLocalTxSubmission wid (Quantity epochStability) tip =
    rawExecute query params
  where
    query =
        "DELETE FROM local_tx_submission " <>
        "WHERE wallet_id=? AND tx_id IN " <>
        "( SELECT tx_id FROM tx_meta WHERE tx_meta.block_height < ? )"
    params = [toPersistValue wid, toPersistValue stableHeight]
    stableHeight = getQuantity (tip ^. #blockHeight) - epochStability

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
