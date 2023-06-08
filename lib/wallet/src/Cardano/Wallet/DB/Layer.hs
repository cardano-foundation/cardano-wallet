{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBFresh which uses Persistent and SQLite.

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
    , withDBFresh
    , withDBFreshInMemory
    , newDBFreshInMemory
    , withDBFreshFromDBOpen

    -- * Interfaces
    , PersistAddressBook (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.DB.Sqlite
    ( DBLog (..)
    , ForeignKeysSetting (ForeignKeysEnabled)
    , SqliteContext (..)
    , newInMemorySqliteContext
    , withSqliteContextFile
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
import Cardano.Wallet.Address.Keys.WalletKey
    ( keyTypeDescriptor )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..) )
import Cardano.Wallet.DB
    ( DBCheckpoints (..)
    , DBDelegation (..)
    , DBFactory (..)
    , DBFresh (..)
    , DBLayer (..)
    , DBLayerCollection (..)
    , DBLayerParams (..)
    , DBOpen (..)
    , DBTxHistory (..)
    , ErrNotGenesisBlockHeader (ErrNotGenesisBlockHeader)
    , ErrWalletAlreadyInitialized (ErrWalletAlreadyInitialized)
    , ErrWalletNotInitialized (..)
    , mkDBLayerFromParts
    , transactionsStore
    )
import Cardano.Wallet.DB.Sqlite.MigrationNew
    ( runNewStyleMigrations )
import Cardano.Wallet.DB.Sqlite.MigrationOld
    ( DefaultFieldValues (..), migrateManually )
import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..)
    , DelegationCertificate (..)
    , DelegationReward (..)
    , EntityField (..)
    , Key (..)
    , StakeKeyCertificate (..)
    , TxMeta (..)
    , Wallet (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), TxId (..) )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..), blockHeaderFromEntity )
import Cardano.Wallet.DB.Store.Info.Store
    ( WalletInfo (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( mkTxMetaFromEntity )
import Cardano.Wallet.DB.Store.Submissions.Layer
    ( rollBackSubmissions )
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
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..) )
import Cardano.Wallet.DB.Store.WalletState.Store
    ( mkStoreWallet )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState
    , DeltaWalletState1 (..)
    , findNearestPoint
    , fromGenesis
    , getLatest
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS, WalletFlavorS, keyOfWallet )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, firstSlotInEpoch, hoistTimeInterpreter, interpretQuery )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue )
import Cardano.Wallet.Read.Tx.CBOR
    ( parseTxFromCBOR )
import Control.DeepSeq
    ( force )
import Control.Exception
    ( evaluate, throw )
import Control.Monad
    ( forM, unless )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans
    ( lift )
import Control.Monad.Trans.Except
    ( mapExceptT, throwE )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Bifunctor
    ( second )
import Data.Coerce
    ( coerce )
import Data.DBVar
    ( DBVar, initDBVar, loadDBVar, readDBVar )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( catMaybes, fromMaybe, maybeToList )
import Data.Store
    ( Store (..), UpdateStore )
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
    , deleteWhere
    , repsert
    , selectFirst
    , selectKeysList
    , selectList
    , (<.)
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
    ( modifyMVar, modifyMVar_, newMVar, readMVar )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TransactionInfo as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.Delta.Update as Delta
import qualified Data.Generics.Internal.VL as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                               Database "factory"
             (a directory containing one database file per wallet)
-------------------------------------------------------------------------------}

-- | Instantiate a 'DBFactory' from a given directory, or in-memory for testing.
newDBFactory
    :: forall s
     . PersistAddressBook s
    => WalletFlavorS s
    -> Tracer IO DBFactoryLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> TimeInterpreter IO
       -- ^ Time interpreter for slot to time conversions
    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> IO (DBFactory IO s )
newDBFactory wf tr defaultFieldValues ti = \case
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
                        (_cleanup, db) <- newDBFreshInMemory wf tr' ti wid
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
            { withDatabase = \wid action -> withRef refs wid $ withDBFresh wf
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
                findDatabases key tr databaseDir
            }
      where
        key = keyOfWallet wf
        databaseFilePrefix = keyTypeDescriptor key
        databaseFile wid =
            databaseDir </>
            databaseFilePrefix <> "." <>
            T.unpack (toText wid) <> ".sqlite"

-- | Return all wallet databases that match the specified key type within the
--   specified directory.
findDatabases
    :: KeyFlavorS k
    -> Tracer IO DBFactoryLog
    -> FilePath
    -> IO [W.WalletId]
findDatabases key tr dir = do
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
    expectedPrefix = T.pack $ keyTypeDescriptor key

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
    :: WalletFlavorS s
    -> Tracer IO WalletDBLog
       -- ^ Logging object
    -> Maybe DefaultFieldValues
       -- ^ Default database field values, used during manual migration.
       -- Use 'Nothing' to skip manual migrations.
    -> FilePath
       -- ^ Path to database file
    -> (DBOpen (SqlPersistT IO) IO s -> IO a)
       -- ^ Action to run.
    -> IO a
withDBOpenFromFile walletF tr defaultFieldValues dbFile action = do
    let trDB = contramap MsgDB tr
    let manualMigrations =
            maybe [] (migrateManually trDB $ keyOfWallet walletF)
                defaultFieldValues
    let autoMigrations   = migrateAll
    res <- withSqliteContextFile trDB dbFile
            manualMigrations autoMigrations runNewStyleMigrations
        $ \ctx -> action $ DBOpen { atomically = runQuery ctx}

    either throwIO pure res

-- | Open an SQLite database in-memory.
--
-- Returns a cleanup function which you should always use exactly once when
-- finished with the 'DBOpen'.
newDBOpenInMemory
    :: forall s
     . Tracer IO WalletDBLog
       -- ^ Logging object
    -> IO (IO (), DBOpen (SqlPersistT IO) IO s)
newDBOpenInMemory tr = do
    let tr' = contramap MsgDB tr
    (destroy, sqliteContext) <-
        newInMemorySqliteContext tr' [] migrateAll ForeignKeysEnabled
    pure (destroy, DBOpen { atomically = runQuery sqliteContext})

-- | Retrieve the wallet id from the database if it's initialized.
retrieveWalletId :: DBOpen (SqlPersistT IO) IO s -> IO (Maybe W.WalletId)
retrieveWalletId DBOpen{atomically} =
    atomically
        $ fmap (walId . entityVal)
            <$> selectFirst [] []

{-------------------------------------------------------------------------------
    DBFresh
-------------------------------------------------------------------------------}

withDBFreshFromDBOpen
    :: forall s a
     . PersistAddressBook s
    => WalletFlavorS s
    -- ^ Wallet flavor
    -> TimeInterpreter IO
    -- ^ Time interpreter for slot to time conversions
    -> W.WalletId
    -- ^ Wallet ID of the database
    -> (DBFresh IO s -> IO a)
    -- ^ Action to run.
    -> DBOpen (SqlPersistT IO) IO s
    -- ^ Already opened database.
    -> IO a
withDBFreshFromDBOpen wf ti wid action = action . newDBFreshFromDBOpen wf ti wid

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBFresh
    :: forall s a
     . PersistAddressBook s
    => WalletFlavorS s
    -- ^ Wallet flavor
    -> Tracer IO WalletDBLog
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
    -> (DBFresh IO s -> IO a)
       -- ^ Action to run.
    -> IO a
withDBFresh wf tr defaultFieldValues dbFile ti wid action =
    withDBOpenFromFile wf tr defaultFieldValues dbFile
        $  action . newDBFreshFromDBOpen wf ti wid

-- | Runs an IO action with a new 'DBFresh' backed by a sqlite in-memory
-- database.
withDBFreshInMemory
    :: forall s a
     . PersistAddressBook s
    => WalletFlavorS s
    -- ^ Wallet flavor
    -> Tracer IO WalletDBLog
    -- ^ Logging object.
    -> TimeInterpreter IO
    -- ^ Time interpreter for slot to time conversions
    -> W.WalletId
    -- ^ Wallet ID of the database.
    -> (DBFresh IO s -> IO a)
    -- ^ Action to run.
    -> IO a
withDBFreshInMemory wf tr ti wid action = bracket
    (newDBFreshInMemory wf tr ti wid) fst (action . snd)

-- | Creates a 'DBFresh' backed by a sqlite in-memory database.
--
-- Returns a cleanup function which you should always use exactly once when
-- finished with the 'DBFresh'.
newDBFreshInMemory
    :: forall s
     . PersistAddressBook s
    => WalletFlavorS s
    -- ^ Wallet flavor
    -> Tracer IO WalletDBLog
    -- ^ Logging object.
    -> TimeInterpreter IO
    -- ^ Time interpreter for slot to time conversions.
    -> W.WalletId
    -- ^ Wallet ID of the database.
    -> IO (IO (), DBFresh IO s)
newDBFreshInMemory wf tr ti wid = do
    second (newDBFreshFromDBOpen wf ti wid) <$> newDBOpenInMemory tr

-- | From a 'DBOpen', create a database which can store the state
-- of one wallet with a specific 'WalletId'.
newDBFreshFromDBOpen
    :: forall s
     . PersistAddressBook s
    => WalletFlavorS s
    -- ^ Wallet flavor
    -> TimeInterpreter IO
    -- ^ Time interpreter for slot to time conversions
    -> W.WalletId
    -- ^ Wallet ID of the database.
    -> DBOpen (SqlPersistT IO) IO s
    -- ^ A (thread-)safe wrapper for query execution.
    -> DBFresh IO s
newDBFreshFromDBOpen wF ti wid_ DBOpen{atomically=atomically_} =
    mkDBFreshFromParts ti wid_
        getWalletId_ (mkStoreWallet wF wid_)
            dbLayerCollection atomically_
  where
    transactionsQS = newQueryStoreTxWalletsHistory

    getWalletId_ = do
        ws <- map unWalletKey <$> selectKeysList [] [Asc WalId]
        pure $ case ws of
            [_] -> True
            _ -> False

    dbLayerCollection walletState = DBLayerCollection{..}
      where
        transactionsStore_ = transactionsQS

        readCheckpoint
            ::  SqlPersistT IO (W.Wallet s)
        readCheckpoint = getLatest <$> readDBVar walletState

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        dbCheckpoints = DBCheckpoints
            { walletsDB_ = walletState

            , readCheckpoint_ = readCheckpoint

            , listCheckpoints_ = do
                let toChainPoint = W.chainPointFromBlockHeader
                map (toChainPoint . blockHeaderFromEntity . entityVal) <$> selectList
                    [ CheckpointWalletId ==. wid_ ]
                    [ Asc CheckpointSlot ]
            , readGenesisParameters_ = selectGenesisParameters wid_
            }

        rollbackTo_ requestedPoint = do
            nearestCheckpoint
                <- Delta.onDBVar walletState
                $ Delta.updateWithResult
                $ \wal ->
                case findNearestPoint wal requestedPoint of
                    Nothing -> throw $ ErrNoOlderCheckpoint wid_ requestedPoint
                    Just nearestPoint ->
                        (  [ UpdateCheckpoints
                                [ RollbackTo nearestPoint ]
                            , UpdateSubmissions
                                $ rollBackSubmissions $
                                    case nearestPoint of
                                        { At s -> s; Origin -> 0 }
                            ]
                        ,   case Map.lookup nearestPoint
                                    (wal ^. #checkpoints . #checkpoints) of
                                Nothing -> error "rollbackTo_: \
                                    \nearest point not found, impossible!"
                                Just p -> p
                        )
            let currentTip = nearestCheckpoint ^. #currentTip
                nearestPoint = currentTip ^. #slotNo
            deleteDelegationCertificates wid_
                [ CertSlot >. nearestPoint
                ]
            deleteStakeKeyCerts wid_
                [ StakeKeyCertSlot >. nearestPoint
                ]
            updateS transactionsQS Nothing $
                RollbackTxWalletsHistory nearestPoint
            pure $ W.chainPointFromBlockHeader currentTip

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}
    lookupTx = queryS transactionsQS . GetByTxId
    lookupTxOut = queryS transactionsQS . GetTxOut

    dbTxHistory = DBTxHistory
        { putTxHistory_ = updateS transactionsQS Nothing
                . ExpandTxWalletsHistory wid_
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

    dbDelegation = mkDBDelegation ti wid_


mkDBFreshFromParts
    :: forall stm m s
     . ( PersistAddressBook s
       , stm ~ SqlPersistT IO
       , MonadIO m
       )
    => TimeInterpreter IO
    -> W.WalletId
    -> stm Bool
    -> UpdateStore stm (DeltaWalletState s)
    -> (DBVar stm (DeltaWalletState s) -> DBLayerCollection stm m s)
    -> (forall a. stm a -> m a)
    -> DBFresh m s
mkDBFreshFromParts
    ti
    wid_
    getWalletId_
    store
    parts
    atomically_ =
        DBFresh
            { bootDBLayer = \params -> do
                let cp = dBLayerParamsState params
                case fromGenesis cp
                    $ WalletInfo
                        wid_
                        (dBLayerParamsMetadata params)
                        (dBLayerParamsGenesisParameters params) of
                    Nothing ->
                        throwIO
                            $ ErrNotGenesisBlockHeader
                            $ cp ^. #currentTip
                    Just wallet -> do
                        present <- lift . atomically_ $ getWalletId_
                        if present
                            then throwE ErrWalletAlreadyInitialized
                            else pure ()
                        lift $ do
                            r@DBLayer{transactionsStore, atomically}
                                <- atomically_ $ db <$> initDBVar store wallet
                            atomically $ updateS transactionsStore Nothing
                                $ ExpandTxWalletsHistory wid_
                                $ dBLayerParamsHistory params
                            pure r
            , loadDBLayer = mapExceptT atomically_ $ do
                present <- lift getWalletId_
                if present
                    then do
                        walletState <- lift $ loadDBVar store
                        pure $ db walletState
                    else throwE ErrWalletNotInitialized
            }
      where
        db = mkDBLayerFromParts ti wid_ . parts

mkDecorator
    :: QueryStoreTxWalletsHistory
    -> TxInDecorator (EraValue Read.Tx) (SqlPersistT IO)
mkDecorator transactionsQS =
    decorateTxInsForReadTxFromLookupTxOut lookupTxOut
  where
    lookupTxOut = queryS transactionsQS . GetTxOut

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
