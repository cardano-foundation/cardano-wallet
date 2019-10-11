{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Sqlite
    ( SqliteContext
    , newDBLayer
    , destroyDBLayer
    , mkDBFactory
    , findDatabases
    , withDBLayer
    , unsafeRunQuery

    -- * Interfaces
    , PersistState (..)
    , PersistTx (..)
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Observer.Monadic
    ( bracketObserveIO )
import Cardano.BM.Trace
    ( Trace, appendName, logDebug, logInfo, logNotice, traceNamedItem )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Sqlite.TH
    ( Checkpoint (..)
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
    ( AddressPoolXPub (..), BlockId (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PersistKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( DefineTx (..) )
import Control.Arrow
    ( (***) )
import Control.Concurrent.MVar
    ( newMVar, withMVar )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( bracket )
import Control.Monad
    ( forM, mapM_, void, when )
import Control.Monad.Catch
    ( MonadCatch (..), handleJust )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Logger
    ( LogLevel (..) )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Control.Tracer
    ( contramap )
import Data.Aeson
    ( ToJSON )
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
    ( catMaybes, fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText, toText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word64 )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , LogFunc
    , SelectOpt (..)
    , Update (..)
    , close'
    , deleteCascadeWhere
    , deleteWhere
    , insertMany_
    , insert_
    , rawExecute
    , repsert
    , repsertMany
    , runMigrationSilent
    , runSqlConn
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (/<-.)
    , (<-.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistT, mkSqliteConnectionInfo, wrapConnectionInfo )
import Database.Sqlite
    ( Error (ErrorConstraint), SqliteException (SqliteException) )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import System.Directory
    ( doesFileExist, listDirectory, removePathForcibly )
import System.FilePath
    ( (</>) )
import System.Log.FastLogger
    ( fromLogStr )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Sqlite as Sqlite

{-------------------------------------------------------------------------------
                            Sqlite connection set up
-------------------------------------------------------------------------------}

-- | Context for the SQLite 'DBLayer'.
data SqliteContext = SqliteContext
    { getSqlBackend :: SqlBackend
    -- ^ A handle to the Persistent SQL backend.
    , runQuery :: forall a. SqlPersistT IO a -> IO a
    -- ^ 'safely' run a query with logging and lock-protection
    , dbFile :: Maybe FilePath
    -- ^ The actual database file, if any. If none, runs in-memory
    , trace :: Trace IO DBLog
    -- ^ A 'Trace' for logging
    }

-- | Run a raw query from the outside using an instantiate DB layer. This is
-- completely unsafe because it breaks the abstraction boundary and can have
-- disastrous results on the database consistency.
unsafeRunQuery :: SqliteContext -> SqlPersistT IO a -> IO a
unsafeRunQuery = runQuery

queryLogFunc :: Trace IO DBLog -> LogFunc
queryLogFunc trace _loc _source level str = dbLog trace (MsgQuery msg sev)
  where
    -- Filter out parameters which appear after the statement semicolon.
    -- They will contain sensitive material that we don't want in the log.
    stmt = B8.takeWhile (/= ';') $ fromLogStr str
    msg = T.decodeUtf8 stmt
    sev = case level of
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Warning

-- | Run an action, and convert any Sqlite constraints exception into the given
-- error result. No other exceptions are handled.
handleConstraint :: MonadCatch m => e -> m a -> m (Either e a)
handleConstraint e = handleJust select handler . fmap Right
  where
      select (SqliteException ErrorConstraint _ _) = Just ()
      select _ = Nothing
      handler = const . pure  . Left $ e

-- | Finalize database statements and close the database connection.
destroyDBLayer :: SqliteContext -> IO ()
destroyDBLayer (SqliteContext {getSqlBackend, trace, dbFile}) = do
    logDebug trace (MsgClosing dbFile)
    close' getSqlBackend

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: forall s t k a.
        ( IsOurs s
        , NFData s
        , Show s
        , PersistState s
        , PersistTx t
        , PersistKey k
        )
    => CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO s t k -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer logConfig trace fp action =
    bracket before after (action . snd)
  where
    before = newDBLayer logConfig trace fp
    after = destroyDBLayer . fst

-- | Instantiate a 'DBFactory' from a given directory
mkDBFactory
    :: forall s t k.
        ( IsOurs s
        , NFData s
        , Show s
        , PersistState s
        , PersistTx t
        , PersistKey k
        )
    => CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> DBFactory IO s t k
mkDBFactory cfg tr databaseDir = case databaseDir of
    Nothing -> DBFactory
        { withDatabase = \_ ->
            withDBLayer cfg tracerDB Nothing
        , removeDatabase = \_ ->
            pure ()
        }
    Just folder -> DBFactory
        { withDatabase = \wid ->
            withDBLayer cfg tracerDB (Just $ filepath wid)
        , removeDatabase = \wid -> do
            let files =
                    [ filepath wid
                    , filepath wid <> "-wal"
                    , filepath wid <> "-shm"
                    ]
            mapM_ removePathForcibly files
        }
      where
        filepath wid = folder </> T.unpack (toText wid) <> ".sqlite"
  where
    tracerDB = appendName "database" tr

-- | Lookup file-system for existing wallet databases
findDatabases
    :: Trace IO Text
    -> FilePath
    -> IO [W.WalletId]
findDatabases tr dir = do
    files <- listDirectory dir
    fmap catMaybes $ forM files $ \file -> do
        isFile <- doesFileExist (dir </> file)
        let (basename:rest) = T.splitOn "." $ T.pack file
        case (isFile, fromText basename, rest) of
            (True, Right wid, ["sqlite"]) -> do
                logInfo tr $ "Found existing wallet: " <> basename
                return (Just wid)
            (True, Right _, _) -> do
                return Nothing
            _ -> do
                logNotice tr $ mconcat
                    [ "Found something else than a database file in the "
                    , "database folder: ", T.pack file
                    ]
                return Nothing

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
    :: forall s t k. (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (SqliteContext, DBLayer IO s t k)
newDBLayer logConfig trace fp = do
    lock <- newMVar ()
    let trace' = transformTrace trace
    ctx@SqliteContext{runQuery} <- startSqliteBackend logConfig trace' fp
    return (ctx, DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \(PrimaryKey wid) cp meta txs ->
              ExceptT $ runQuery $ do
                  res <- handleConstraint (ErrWalletAlreadyExists wid) $
                      insert_ (mkWalletEntity wid meta)
                  when (isRight res) $ do
                      insertCheckpoint wid cp
                      let (metas, txins, txouts) = mkTxHistory @t wid txs
                      putTxMetas metas
                      putTxs txins txouts
                  pure res

        , removeWallet = \(PrimaryKey wid) ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> Right <$> do
                      deleteCascadeWhere [WalId ==. wid]
                      deleteLooseTransactions
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , listWallets = runQuery $
              map (PrimaryKey . unWalletKey) <$>
              selectKeysList [] [Asc WalId]

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey wid) cp ->
              ExceptT $ runQuery $ selectWallet wid >>= \case
                  Just _ -> Right <$> do
                      insertCheckpoint wid cp
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readCheckpoint = \(PrimaryKey wid) -> runQuery $
              selectLatestCheckpoint wid >>= \case
                  Just cp -> do
                      utxo <- selectUTxO cp
                      s <- selectState (checkpointId cp)
                      pure (checkpointFromEntity @s @t cp utxo <$> s)
                  Nothing -> pure Nothing

        , rollbackTo = \(PrimaryKey wid) point -> ExceptT $ runQuery $ do
            findNearestPoint wid point >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just nearestPoint -> Right <$> do
                    deleteCheckpoints wid
                        [ CheckpointSlot >. point
                        ]
                    updateTxMetas wid
                        [ TxMetaDirection ==. W.Outgoing
                        , TxMetaSlotId >. point
                        ]
                        [ TxMetaStatus =. W.Pending
                        , TxMetaSlotId =. nearestPoint
                        ]
                    deleteTxMetas wid
                        [ TxMetaDirection ==. W.Incoming
                        , TxMetaSlotId >. point
                        ]

        , prune = \(PrimaryKey wid) -> ExceptT $ runQuery $
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just cp -> Right <$> do
                    pruneCheckpoints wid cp
                    deleteLooseTransactions

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \(PrimaryKey wid) meta ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> do
                      updateWhere [WalId ==. wid]
                          (mkWalletMetadataUpdate meta)
                      pure $ Right ()
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readWalletMeta = \(PrimaryKey wid) ->
              runQuery $
              fmap (metadataFromEntity . entityVal) <$>
              selectFirst [WalId ==. wid] []

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> do
                      let (metas, txins, txouts) = mkTxHistory @t wid txs
                      putTxMetas metas
                      putTxs txins txouts
                      pure $ Right ()
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readTxHistory = \(PrimaryKey wid) order range status -> runQuery $
              selectTxHistory @t wid order $ catMaybes
                [ (TxMetaSlotId >=.) <$> W.inclusiveLowerBound range
                , (TxMetaSlotId <=.) <$> W.inclusiveUpperBound range
                , (TxMetaStatus ==.) <$> status
                ]

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey wid) key ->
                ExceptT $ runQuery $
                selectWallet wid >>= \case
                    Just _ -> Right <$> do
                        deleteWhere [PrivateKeyWalletId ==. wid]
                        insert_ (mkPrivateKeyEntity wid key)
                    Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readPrivateKey = \(PrimaryKey wid) ->
              runQuery $ selectPrivateKey wid

        {-----------------------------------------------------------------------
                                       Lock
        -----------------------------------------------------------------------}

        , withLock = \action ->
              ExceptT $ withMVar lock $ \() -> runExceptT action
        })

{-------------------------------------------------------------------------------
                           Internal / Database Setup
-------------------------------------------------------------------------------}

-- | Opens the SQLite database connection, sets up query logging and timing,
-- runs schema migrations if necessary.
startSqliteBackend
    :: CM.Configuration
    -> Trace IO DBLog
    -> Maybe FilePath
    -> IO SqliteContext
startSqliteBackend logConfig trace fp = do
    let traceQuery = appendName "query" trace
    backend <- createSqliteBackend trace fp (queryLogFunc traceQuery)
    lock <- newMVar ()
    let observe :: IO a -> IO a
        observe = bracketObserveIO logConfig traceQuery Debug "query"
    let runQuery :: SqlPersistT IO a -> IO a
        runQuery cmd = withMVar lock $ const $ observe $ runSqlConn cmd backend
    migrations <- runQuery $ runMigrationSilent migrateAll
    dbLog trace $ MsgMigrations (length migrations)
    pure $ SqliteContext backend runQuery fp trace

createSqliteBackend
    :: Trace IO DBLog
    -> Maybe FilePath
    -> LogFunc
    -> IO SqlBackend
createSqliteBackend trace fp logFunc = do
    let connStr = sqliteConnStr fp
    dbLog trace $ MsgConnStr connStr
    conn <- Sqlite.open connStr
    wrapConnectionInfo (mkSqliteConnectionInfo connStr) conn logFunc

sqliteConnStr :: Maybe FilePath -> Text
sqliteConnStr = maybe ":memory:" T.pack

{-------------------------------------------------------------------------------
           Conversion between Persistent table types and wallet types
-------------------------------------------------------------------------------}
delegationToPoolId :: W.WalletDelegation W.PoolId -> Maybe W.PoolId
delegationToPoolId W.NotDelegating = Nothing
delegationToPoolId (W.Delegating pool) = Just pool

delegationFromPoolId :: Maybe W.PoolId -> W.WalletDelegation W.PoolId
delegationFromPoolId Nothing = W.NotDelegating
delegationFromPoolId (Just pool) = W.Delegating pool

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> Wallet
mkWalletEntity wid meta = Wallet
    { walId = wid
    , walName = meta ^. #name . coerce
    , walCreationTime = meta ^. #creationTime
    , walPassphraseLastUpdatedAt =
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walStatus = meta ^. #status
    , walDelegation = delegationToPoolId $ meta ^. #delegation
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalName =. meta ^. #name . coerce
    , WalCreationTime =. meta ^. #creationTime
    , WalPassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , WalStatus =. meta ^. #status
    , WalDelegation =. delegationToPoolId (meta ^. #delegation)
    ]

metadataFromEntity :: Wallet -> W.WalletMetadata
metadataFromEntity wal = W.WalletMetadata
    { name = W.WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo <$>
        walPassphraseLastUpdatedAt wal
    , status = walStatus wal
    , delegation = delegationFromPoolId (walDelegation wal)
    }

mkPrivateKeyEntity
    :: PersistKey k
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
    :: PersistKey k
    => PrivateKey
    -> Either String (k 'RootK XPrv, W.Hash "encryption")
privateKeyFromEntity (PrivateKey _ k h) = deserializeXPrv (k, h)

mkCheckpointEntity
    :: forall s t. ()
    => W.WalletId
    -> W.Wallet s t
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
        , checkpointBlockHeight = fromIntegral bh
        , checkpointGenesisHash = BlockId (coerce (bp ^. #getGenesisBlockHash))
        , checkpointGenesisStart = coerce (bp ^. #getGenesisBlockDate)
        , checkpointFeePolicy = bp ^. #getFeePolicy
        , checkpointSlotLength = coerceSlotLength $ bp ^. #getSlotLength
        , checkpointEpochLength = coerce (bp ^. #getEpochLength)
        , checkpointTxMaxSize = coerce (bp ^. #getTxMaxSize)
        , checkpointEpochStability = coerce (bp ^. #getEpochStability)
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
    :: forall s t. (IsOurs s, NFData s, Show s, DefineTx t)
    => Checkpoint
    -> [UTxO]
    -> s
    -> W.Wallet s t
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
        ) = cp
    header = (W.BlockHeader slot blockHeight' headerHash parentHeaderHash)
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo
        ]
    blockHeight' = Quantity . toEnum . fromEnum $ bh
    bp = W.BlockchainParameters
        { getGenesisBlockHash = coerce genesisHash
        , getGenesisBlockDate = W.StartTime genesisStart
        , getFeePolicy = feePolicy
        , getSlotLength = W.SlotLength (toEnum (fromEnum slotLength))
        , getEpochLength = W.EpochLength epochLength
        , getTxMaxSize = Quantity txMaxSize
        , getEpochStability = Quantity epochStability
        }

mkTxHistory
    :: forall t. PersistTx t
    => W.WalletId
    -> [(W.Tx t, W.TxMeta)]
    -> ([TxMeta], [TxIn], [TxOut])
mkTxHistory wid txs = flatTxHistory
    [ (mkTxMetaEntity wid txid meta, mkTxInputsOutputs @t (txid, tx))
    | (tx, meta) <- txs
    , let txid = txId @t tx
    ]
  where
    -- | Make flat lists of entities from the result of 'mkTxHistory'.
    flatTxHistory :: [(TxMeta, ([TxIn], [TxOut]))] -> ([TxMeta], [TxIn], [TxOut])
    flatTxHistory entities =
        ( map fst entities
        , concatMap (fst . snd) entities
        , concatMap (snd. snd) entities
        )

mkTxInputsOutputs
    :: forall t. PersistTx t
    => (W.Hash "Tx", W.Tx t)
    -> ([TxIn], [TxOut])
mkTxInputsOutputs tx =
    ( (dist mkTxIn . ordered (resolvedInputs @t)) tx
    , (dist mkTxOut . ordered (W.outputs @t)) tx )
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
    , txMetaSlotId = meta ^. #slotId
    , txMetaAmount = getAmount (meta ^. #amount)
    }
    where getAmount (Quantity n) = n

-- note: TxIn records must already be sorted by order
-- and TxOut records must already be sorted by index
txHistoryFromEntity
    :: forall t. PersistTx t
    => [TxMeta]
    -> [TxIn]
    -> [TxOut]
    -> [(W.Tx t, W.TxMeta)]
txHistoryFromEntity metas ins outs = map mkItem metas
  where
    mkItem m = (mkTxWith (txMetaTxId m), mkTxMeta m)
    mkTxWith txid = mkTx @t
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
        , W.slotId = txMetaSlotId m
        , W.amount = Quantity (txMetaAmount m)
        }

{-------------------------------------------------------------------------------
                                   DB Queries
-------------------------------------------------------------------------------}

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid =
    fmap entityVal <$> selectFirst [WalId ==. wid] []

insertCheckpoint
    :: forall s t. (PersistState s)
    => W.WalletId
    -> W.Wallet s t
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
    :: forall t. PersistTx t
    => W.WalletId
    -> W.SortOrder
    -> [Filter TxMeta]
    -> SqlPersistT IO [(W.Tx t, W.TxMeta)]
selectTxHistory wid order conditions = do
    metas <- fmap entityVal <$> selectList
        ((TxMetaWalletId ==. wid) : conditions) sortOpt
    let txids = map txMetaTxId metas
    (ins, outs) <- selectTxs txids
    return $ txHistoryFromEntity @t metas ins outs
  where
    -- Note: there are sorted indices on these columns.
    -- The secondary sort by TxId is to make the ordering stable
    -- so that testing with random data always works.
    sortOpt = case order of
        W.Ascending -> [Asc TxMetaSlotId, Desc TxMetaTxId]
        W.Descending -> [Desc TxMetaSlotId, Asc TxMetaTxId]

selectPrivateKey
    :: (MonadIO m, PersistKey k)
    => W.WalletId
    -> SqlPersistT m (Maybe (k 'RootK XPrv, W.Hash "encryption"))
selectPrivateKey wid =
    let keys = selectFirst [PrivateKeyWalletId ==. wid] []
        toMaybe = either (const Nothing) Just
    in (>>= toMaybe . privateKeyFromEntity . entityVal) <$> keys

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint
    :: W.WalletId
    -> W.SlotId
    -> SqlPersistT IO (Maybe W.SlotId)
findNearestPoint wid sl =
    fmap (checkpointSlot . entityVal) <$> selectFirst
        [CheckpointWalletId ==. wid, CheckpointSlot <=. sl]
        [Desc CheckpointSlot]

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

class DefineTx t => PersistTx t where
    resolvedInputs :: Tx t -> [(W.TxIn, Maybe W.Coin)]
    -- | Extract transaction resolved inputs. This slightly breaks the
    -- abstraction boundary of 'DefineTx' which doesn't really force any
    -- structure on the resolved inputs.
    -- However, here in the DB-Layer, supporting arbitrary shape for the inputs
    -- be much more complex and require quite a lot of work. So, we kinda force
    -- the format here and only here, and leave the rest of the code with an
    -- opaque 'ResolvedTxIn' type.

    mkTx :: W.Hash "Tx" -> [(W.TxIn, Maybe W.Coin)] -> [W.TxOut] -> Tx t
    -- | Re-construct a transaction from a set resolved inputs and
    -- some outputs. Returns 'Nothing' if the transaction couldn't be
    -- constructed.

{-------------------------------------------------------------------------------
                          Sequential address discovery
-------------------------------------------------------------------------------}

instance W.KeyToAddress t Seq.SeqKey => PersistState (Seq.SeqState t) where
    insertState (wid, sl) st = do
        let (intPool, extPool) = (Seq.internalPool st, Seq.externalPool st)
        let (xpub, _) = W.invariant
                "Internal & External pool use different account public keys!"
                (Seq.accountPubKey intPool, Seq.accountPubKey extPool)
                (uncurry (==))
        let eGap = Seq.gap extPool
        let iGap = Seq.gap intPool
        repsert (SeqStateKey wid) (SeqState wid eGap iGap (AddressPoolXPub xpub))
        insertAddressPool wid sl intPool
        insertAddressPool wid sl extPool
        deleteWhere [SeqStatePendingWalletId ==. wid]
        dbChunked insertMany_ (mkSeqStatePendingIxs wid $ Seq.pendingChangeIxs st)

    selectState (wid, sl) = runMaybeT $ do
        st <- MaybeT $ selectFirst [SeqStateWalletId ==. wid] []
        let SeqState _ eGap iGap xPub = entityVal st
        intPool <- lift $ selectAddressPool wid sl iGap xPub
        extPool <- lift $ selectAddressPool wid sl eGap xPub
        pendingChangeIxs <- lift $ selectSeqStatePendingIxs wid
        pure $ Seq.SeqState intPool extPool pendingChangeIxs

insertAddressPool
    :: forall t c. (Typeable c)
    => W.WalletId
    -> W.SlotId
    -> Seq.AddressPool t c
    -> SqlPersistT IO ()
insertAddressPool wid sl pool =
    void $ dbChunked insertMany_
        [ SeqStateAddress wid sl addr ix (Seq.changeChain @c)
        | (ix, addr) <- zip [0..] (Seq.addresses pool)
        ]

selectAddressPool
    :: forall t c. (W.KeyToAddress t Seq.SeqKey, Typeable c)
    => W.WalletId
    -> W.SlotId
    -> Seq.AddressPoolGap
    -> AddressPoolXPub
    -> SqlPersistT IO (Seq.AddressPool t c)
selectAddressPool wid sl gap (AddressPoolXPub xpub) = do
    addrs <- fmap entityVal <$> selectList
        [ SeqStateAddressWalletId ==. wid
        , SeqStateAddressSlot ==. sl
        , SeqStateAddressChangeChain ==. Seq.changeChain @c
        ] [Asc SeqStateAddressIndex]
    pure $ addressPoolFromEntity addrs
  where
    addressPoolFromEntity
        :: [SeqStateAddress]
        -> Seq.AddressPool t c
    addressPoolFromEntity addrs =
        Seq.mkAddressPool @t @c xpub gap (map seqStateAddressAddress addrs)

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
    (W.Index 'W.Hardened 'W.AccountK, W.Index 'W.Hardened 'W.AddressK)
    W.Address

-- Persisting 'RndState' requires that the wallet root key has already been
-- added to the database with 'putPrivateKey'. Unlike sequential AD, random
-- address discovery requires a root key to recognize addresses.
instance PersistState (Rnd.RndState t) where
    insertState (wid, sl) st = do
        let ix = W.getIndex (st ^. #accountIndex)
        let gen = st ^. #gen
        repsert (RndStateKey wid) (RndState wid ix gen)
        insertRndStateAddresses wid sl (Rnd.addresses st)
        insertRndStatePending wid (Rnd.pendingAddresses st)

    selectState (wid, sl) = runMaybeT $ do
        st <- MaybeT $ selectFirst
            [ RndStateWalletId ==. wid
            ] []
        let (RndState _ ix gen) = entityVal st
        addresses <- lift $ selectRndStateAddresses wid sl
        pendingAddresses <- lift $ selectRndStatePending wid
        rndKey <- lift $ selectRndStateKey wid
        pure $ Rnd.RndState
            { rndKey = rndKey
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

-- | Gets the wallet root key to put in RndState. If there is none yet, just
-- return a placeholder.
selectRndStateKey :: W.WalletId -> SqlPersistT IO (Rnd.RndKey 'RootK XPrv)
selectRndStateKey wid = maybe Rnd.nullKey fst <$> selectPrivateKey wid

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data DBLog
    = MsgMigrations Int
    | MsgQuery Text Severity
    | MsgConnStr Text
    | MsgClosing (Maybe FilePath)
    deriving (Generic, Show, Eq, ToJSON)

transformTrace :: Trace IO Text -> Trace IO DBLog
transformTrace = contramap (fmap dbLogText)

dbLog :: MonadIO m => Trace m DBLog -> DBLog -> m ()
dbLog logTrace msg = traceNamedItem logTrace Public (dbLogLevel msg) msg

dbLogLevel :: DBLog -> Severity
dbLogLevel (MsgMigrations 0) = Debug
dbLogLevel (MsgMigrations _) = Notice
dbLogLevel (MsgQuery _ sev) = sev
dbLogLevel (MsgConnStr _) = Debug
dbLogLevel (MsgClosing _) = Debug

dbLogText :: DBLog -> Text
dbLogText (MsgMigrations 0) = "No database migrations were necessary."
dbLogText (MsgMigrations n) = fmt $ ""+||n||+" migrations were applied to the database."
dbLogText (MsgQuery stmt _) = stmt
dbLogText (MsgConnStr connStr) = "Using connection string: " <> connStr
dbLogText (MsgClosing fp) = "Closing database ("+|fromMaybe "in-memory" fp|+")"

{-------------------------------------------------------------------------------
                               Extra DB Helpers
-------------------------------------------------------------------------------}

-- | Convert a single DB "updateMany" (or similar) query into multiple
-- updateMany queries with smaller lists of values.
--
-- This is to prevent too many variables appearing in the SQL statement.
-- SQLITE_MAX_VARIABLE_NUMBER is 999 by default, and we will get a
-- "too many SQL variables" exception if that is exceeded.
--
-- We choose a conservative value 'chunkSize' << 999 because there can be
-- multiple variables per row updated.
dbChunked :: ([a] -> SqlPersistT IO b) -> [a] -> SqlPersistT IO ()
dbChunked = chunkedM chunkSize

-- | Given an action which takes a list of items, and a list of items, run that
-- action multiple times with the input list cut into chunks.
chunkedM
    :: Monad m
    => Int -- ^ Chunk size
    -> ([a] -> m b) -- ^ Action to run on values
    -> [a] -- ^ The values
    -> m ()
chunkedM n f = mapM_ f . chunksOf n

-- | Size of chunks when inserting, updating or deleting many rows at once. We
-- only act on `chunkSize` values at a time. See also 'dbChunked'.
chunkSize :: Int
chunkSize = 100
