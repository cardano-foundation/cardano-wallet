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

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Sqlite
    ( SqliteContext
    , newDBLayer
    , destroyDBLayer
    , withDBLayer
    , unsafeRunQuery

    -- * Interfaces
    , PersistState (..)
    , PersistTx (..)
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..), PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Observer.Monadic
    ( bracketObserveIO )
import Cardano.BM.Trace
    ( Trace, appendName, logDebug, traceNamedItem )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.Sqlite.TH
    ( AddressPool (..)
    , AddressPoolId
    , AddressPoolIndex (..)
    , Checkpoint (..)
    , EntityField (..)
    , Key (..)
    , PrivateKey (..)
    , SeqState (..)
    , SeqStateExternalPool (..)
    , SeqStateId
    , SeqStateInternalPool (..)
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
    ( Depth (..), deserializeXPrv, serializeXPrv )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( DefineTx (..) )
import Control.Concurrent.MVar
    ( newMVar, withMVar )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( bracket )
import Control.Monad
    ( mapM_, when )
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
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import Database.Persist.Class
    ( DeleteCascade, PersistField, PersistRecordBackend )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , LogFunc
    , SelectOpt (..)
    , Update (..)
    , close'
    , deleteCascadeWhere
    , deleteWhere
    , insert
    , insertMany_
    , insert_
    , rawExecute
    , repsertMany
    , runMigrationSilent
    , runSqlConn
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (<-.)
    , (=.)
    , (==.)
    )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistT, mkSqliteConnectionInfo, wrapConnectionInfo )
import Database.Sqlite
    ( Error (ErrorConstraint), SqliteException (SqliteException) )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import System.Log.FastLogger
    ( fromLogStr )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.AddressDiscovery as W
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Sqlite as Sqlite

----------------------------------------------------------------------------
-- Sqlite connection set up

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

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: forall s t a. (IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO s t -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer logConfig trace fp action = bracket before after between
  where
    before = newDBLayer logConfig trace fp
    after = destroyDBLayer . fst
    between = action . snd

-- | Finalize database statements and close the database connection.
destroyDBLayer :: SqliteContext -> IO ()
destroyDBLayer (SqliteContext {getSqlBackend, trace, dbFile}) = do
    logDebug trace (MsgClosing dbFile)
    close' getSqlBackend

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
    :: forall s t. (IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (SqliteContext, DBLayer IO s t)
newDBLayer logConfig trace fp = do
    lock <- newMVar ()
    let trace' = transformTrace trace
    ctx@SqliteContext{runQuery} <- startSqliteBackend logConfig trace' fp
    return (ctx, DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \(PrimaryKey wid) cp meta ->
              ExceptT $ runQuery $ do
                  res <- handleConstraint (ErrWalletAlreadyExists wid) $
                      insert_ (mkWalletEntity wid meta)
                  when (isRight res) $
                      insertCheckpoint wid cp
                  pure res

        , removeWallet = \(PrimaryKey wid) ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> Right <$> do
                      deleteCheckpoints @s wid
                      deleteTxMetas wid
                      deleteLooseTransactions
                      deleteWhere [PrivateKeyTableWalletId ==. wid]
                      deleteCascadeWhere [WalTableId ==. wid]
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , listWallets = runQuery $
              map (PrimaryKey . unWalletKey) <$>
              selectKeysList [] [Asc WalTableId]

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey wid) cp ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> Right <$> do
                      deleteCheckpoints @s wid -- clear out all checkpoints
                      deleteLooseTransactions -- clear unused transaction data
                      insertCheckpoint wid cp -- add this checkpoint
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readCheckpoint = \(PrimaryKey wid) ->
              runQuery $
              selectLatestCheckpoint wid >>= \case
                  Just cp -> do
                      utxo <- selectUTxO cp
                      txs <- selectTxHistory @t wid [TxMetaTableStatus ==. W.Pending]
                      s <- selectState (checkpointId cp)
                      pure (checkpointFromEntity @s @t cp utxo txs <$> s)
                  Nothing -> pure Nothing

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \(PrimaryKey wid) meta ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> do
                      updateWhere [WalTableId ==. wid]
                          (mkWalletMetadataUpdate meta)
                      pure $ Right ()
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readWalletMeta = \(PrimaryKey wid) ->
              runQuery $
              fmap (metadataFromEntity . entityVal) <$>
              selectFirst [WalTableId ==. wid] []

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs ->
              ExceptT $ runQuery $
              selectWallet wid >>= \case
                  Just _ -> do
                      let (metas, txins, txouts) = mkTxHistory @t wid $ W.invariant
                              ("putTxHistory has been called with pending txs: "
                                <> show txs)
                              txs
                              (not . any (W.isPending . snd))
                      putTxMetas metas
                      putTxs txins txouts
                      pure $ Right ()
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readTxHistory = \(PrimaryKey wid) ->
              runQuery $
              selectTxHistory @t wid []

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey wid) key ->
                ExceptT $ runQuery $
                selectWallet wid >>= \case
                    Just _ -> Right <$> do
                        deleteWhere [PrivateKeyTableWalletId ==. wid]
                        insert_ (mkPrivateKeyEntity wid key)
                    Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readPrivateKey = \(PrimaryKey wid) ->
              runQuery $
              let keys = selectFirst [PrivateKeyTableWalletId ==. wid] []
                  toMaybe = either (const Nothing) Just
              in (>>= toMaybe . privateKeyFromEntity . entityVal) <$> keys

        {-----------------------------------------------------------------------
                                       Lock
        -----------------------------------------------------------------------}

        , withLock = \action ->
              ExceptT $ withMVar lock $ \() -> runExceptT action
        })

----------------------------------------------------------------------------
-- Internal / Database Setup

-- | Opens the SQLite database connection, sets up query logging and timing,
-- runs schema migrations if necessary.
startSqliteBackend
    :: CM.Configuration
    -> Trace IO DBLog
    -> Maybe FilePath
    -> IO SqliteContext
startSqliteBackend logConfig trace fp = do
    traceQuery <- appendName "query" trace
    backend <- createSqliteBackend trace fp (queryLogFunc traceQuery)
    lock <- newMVar ()
    let observe :: IO a -> IO a
        observe = bracketObserveIO logConfig traceQuery Debug "query"
    let runQuery :: SqlPersistT IO a -> IO a
        runQuery cmd = withMVar lock $ const $ observe $ runSqlConn cmd backend
    migrations <- runQuery $ runMigrationSilent migrateAll
    dbLog trace $ MsgMigrations (length migrations)
    runQuery addIndexes
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

----------------------------------------------------------------------------
-- SQLite database setup

addIndexes :: SqlPersistT IO ()
addIndexes = mapM_ (`rawExecute` [])
    [ createIndex "tx_meta_wallet_id" "tx_meta (wallet_id)"
    -- Covers filtering and sorting (either direction) of tx history
    , createIndex "tx_meta_slot" "tx_meta (slot ASC)"
    , createIndex "tx_in_tx_id" "tx_in (tx_id)"
    , createIndex "tx_out_tx_id" "tx_out (tx_id)"
    ]
  where
    createIndex name on = "CREATE INDEX IF NOT EXISTS " <> name <> " ON " <> on

----------------------------------------------------------------------------
-- Conversion between Persistent table types and wallet types

delegationToText :: W.WalletDelegation W.PoolId -> Maybe Text
delegationToText W.NotDelegating = Nothing
delegationToText (W.Delegating pool) = Just (W.getPoolId pool)

delegationFromText :: Maybe Text -> W.WalletDelegation W.PoolId
delegationFromText Nothing = W.NotDelegating
delegationFromText (Just pool) = W.Delegating (W.PoolId pool)

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> Wallet
mkWalletEntity wid meta = Wallet
    { walTableId = wid
    , walTableName = meta ^. #name . coerce
    , walTableCreationTime = meta ^. #creationTime
    , walTablePassphraseLastUpdatedAt =
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walTableStatus = meta ^. #status
    , walTableDelegation = delegationToText $ meta ^. #delegation
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalTableName =. meta ^. #name . coerce
    , WalTableCreationTime =. meta ^. #creationTime
    , WalTablePassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , WalTableStatus =. meta ^. #status
    , WalTableDelegation =. delegationToText (meta ^. #delegation)
    ]

metadataFromEntity :: Wallet -> W.WalletMetadata
metadataFromEntity wal = W.WalletMetadata
    { name = W.WalletName (walTableName wal)
    , creationTime = walTableCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo <$>
        walTablePassphraseLastUpdatedAt wal
    , status = walTableStatus wal
    , delegation = delegationFromText (walTableDelegation wal)
    }

mkPrivateKeyEntity
    :: W.WalletId
    -> (W.Key 'RootK XPrv, W.Hash "encryption")
    -> PrivateKey
mkPrivateKeyEntity wid kh = PrivateKey
    { privateKeyTableWalletId = wid
    , privateKeyTableRootKey = root
    , privateKeyTableHash = hash
    }
  where
    (root, hash) = serializeXPrv kh

privateKeyFromEntity
    :: PrivateKey
    -> Either String (W.Key 'RootK XPrv, W.Hash "encryption")
privateKeyFromEntity (PrivateKey _ k h) = deserializeXPrv (k, h)

mkCheckpointEntity
    :: forall s t. PersistTx t
    => W.WalletId
    -> W.Wallet s t
    -> (Checkpoint, [UTxO], [TxIn], [TxOut], [TxMeta])
mkCheckpointEntity wid wal =
    (cp, utxo, ins, outs, metas)
  where
    pending =
        [ (W.txId @t tx, (tx, meta))
        | (tx, meta) <- Set.toList (W.getPending wal)
        ]
    (metas, ins, outs) = mkTxHistory @t wid (Map.fromList pending)
    header = (W.currentTip wal)
    sl = header ^. #slotId
    parent = header ^. #prevBlockHash
    cp = Checkpoint
        { checkpointTableWalletId = wid
        , checkpointTableSlot = sl
        , checkpointTableParent = BlockId parent
        }
    utxo = [ UTxO wid sl (TxId input) ix addr coin
           | (W.TxIn input ix, W.TxOut addr coin) <- utxoMap ]
    utxoMap = Map.assocs (W.getUTxO (W.utxo wal))

-- note: TxIn records must already be sorted by order
-- and TxOut records must already by sorted by index.
checkpointFromEntity
    :: forall s t. (IsOurs s, NFData s, Show s, DefineTx t)
    => Checkpoint
    -> [UTxO]
    -> [(W.Hash "Tx", (W.Tx t, W.TxMeta))]
    -> s
    -> W.Wallet s t
checkpointFromEntity (Checkpoint _ slot (BlockId parentHeaderHash)) utxo txs =
    W.unsafeInitWallet utxo' pending (W.BlockHeader slot parentHeaderHash)
  where
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo
        ]
    pending = Set.fromList $ map snd txs

mkTxHistory
    :: forall t. PersistTx t
    => W.WalletId
    -> Map (W.Hash "Tx") (W.Tx t, W.TxMeta)
    -> ([TxMeta], [TxIn], [TxOut])
mkTxHistory wid txs = (map (uncurry (mkTxMetaEntity wid)) metas, ins, outs)
  where
    pairs = Map.toList txs
    metas = fmap snd <$> pairs
    hist = fmap fst <$> pairs
    (ins, outs) = mkTxInputsOutputs @t hist

mkTxInputsOutputs
    :: forall t. PersistTx t
    => [(W.Hash "Tx", W.Tx t)]
    -> ([TxIn], [TxOut])
mkTxInputsOutputs txs =
    ( concatMap (dist mkTxIn . ordered (resolvedInputs @t)) txs
    , concatMap (dist mkTxOut . ordered (W.outputs @t)) txs )
  where
    mkTxIn tid (ix, (txIn, amt)) = TxIn
        { txInputTableTxId = TxId tid
        , txInputTableOrder = ix
        , txInputTableSourceTxId = TxId (W.inputId txIn)
        , txInputTableSourceIndex = W.inputIx txIn
        , txInputTableSourceAmount = amt
        }
    mkTxOut tid (ix, txOut) = TxOut
        { txOutputTableTxId = TxId tid
        , txOutputTableIndex = ix
        , txOutputTableAddress = W.address txOut
        , txOutputTableAmount = W.coin txOut
        }
    ordered f = fmap (zip [0..] . f)
    -- | Distribute `a` accross many `b`s using the given function.
    -- >>> dist TxOut (addr, [Coin 1, Coin 42, Coin 14])
    -- [TxOut addr (Coin 1), TxOut addr (Coin 42), TxOut addr (Coin 14)]
    dist :: (a -> b -> c) -> (a, [b]) -> [c]
    dist f (a, bs) = [f a b | b <- bs]

mkTxMetaEntity :: W.WalletId -> W.Hash "Tx" -> W.TxMeta -> TxMeta
mkTxMetaEntity wid txid meta = TxMeta
    { txMetaTableTxId = TxId txid
    , txMetaTableWalletId = wid
    , txMetaTableStatus = meta ^. #status
    , txMetaTableDirection = meta ^. #direction
    , txMetaTableSlotId = meta ^. #slotId
    , txMetaTableAmount = getAmount (meta ^. #amount)
    }
    where getAmount (Quantity n) = n

-- note: TxIn records must already be sorted by order
-- and TxOut records must already be sorted by index
txHistoryFromEntity
    :: forall t. PersistTx t
    => [TxMeta]
    -> [TxIn]
    -> [TxOut]
    -> [(W.Hash "Tx", (W.Tx t, W.TxMeta))]
txHistoryFromEntity metas ins outs = map mkItem metas
  where
    mkItem m = ( getTxId (txMetaTableTxId m)
               , (mkTxWith (txMetaTableTxId m), mkTxMeta m) )
    mkTxWith txid = mkTx @t
        (map mkTxIn $ filter ((== txid) . txInputTableTxId) ins)
        (map mkTxOut $ filter ((== txid) . txOutputTableTxId) outs)
    mkTxIn tx =
        ( W.TxIn
            { W.inputId = getTxId (txInputTableSourceTxId tx)
            , W.inputIx = txInputTableSourceIndex tx
            }
        , txInputTableSourceAmount tx
        )
    mkTxOut tx = W.TxOut
        { W.address = txOutputTableAddress tx
        , W.coin = txOutputTableAmount tx
        }
    mkTxMeta m = W.TxMeta
        { W.status = txMetaTableStatus m
        , W.direction = txMetaTableDirection m
        , W.slotId = txMetaTableSlotId m
        , W.amount = Quantity (txMetaTableAmount m)
        }

----------------------------------------------------------------------------
-- DB Queries

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid = fmap entityVal <$> selectFirst [WalTableId ==. wid] []

insertCheckpoint
    :: (PersistState s, PersistTx t)
    => W.WalletId
    -> W.Wallet s t
    -> SqlPersistT IO ()
insertCheckpoint wid cp = do
    let (cp', utxo, ins, outs, metas) = mkCheckpointEntity wid cp
    insert_ cp'
    putTxMetas metas
    putTxs ins outs
    dbChunked insertMany_ utxo
    insertState (wid, (W.currentTip cp) ^. #slotId) (W.getState cp)

-- | Delete all checkpoints associated with a wallet.
deleteCheckpoints
    :: forall s. PersistState s
    => W.WalletId
    -> SqlPersistT IO ()
deleteCheckpoints wid = do
    deleteWhere [UtxoTableWalletId ==. wid]
    deleteWhere [CheckpointTableWalletId ==. wid]
    deleteState @s wid -- clear state

-- | Delete TxMeta values for a wallet.
deleteTxMetas
    :: W.WalletId
    -> SqlPersistT IO ()
deleteTxMetas wid = deleteWhere [ TxMetaTableWalletId ==. wid ]

-- | Add new TxMeta rows, overwriting existing ones.
putTxMetas :: [TxMeta] -> SqlPersistT IO ()
putTxMetas metas = dbChunked repsertMany
    [(TxMetaKey txMetaTableTxId txMetaTableWalletId, m) | m@TxMeta{..} <- metas]

-- | Insert multiple transactions, removing old instances first.
putTxs :: [TxIn] -> [TxOut] -> SqlPersistT IO ()
putTxs txins txouts = do
    dbChunked repsertMany
        [ (TxInKey txInputTableTxId txInputTableSourceTxId txInputTableSourceIndex, i)
        | i@TxIn{..} <- txins ]
    dbChunked repsertMany
        [ (TxOutKey txOutputTableTxId txOutputTableIndex, o)
        | o@TxOut{..} <- txouts ]

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
-- only act on `chunkSize` values at a time. See also 'dbChunked' and
-- 'deleteMany'.
chunkSize :: Int
chunkSize = 100

-- | Remove many entities from the database in chunks of fixed size.
--
-- This is to prevent too many variables appearing in the SQL statement.
-- SQLITE_MAX_VARIABLE_NUMBER is 999 by default, and we will get a
-- "too many SQL variables" exception if that is exceeded.
--
-- We choose a conservative value 'chunkSize' << 999 because there can be
-- multiple variables per row updated.
deleteMany
    :: forall typ record.
       ( PersistField typ
       , DeleteCascade record SqlBackend
       , PersistRecordBackend record SqlBackend )
    => [Filter record]
    -> EntityField record typ
    -> [typ]
    -> SqlPersistT IO ()
deleteMany filters entity types
    -- SQLite max limit is at 999 variables. We may have other variables so,
    -- we arbitrarily pick 500 which is way below 999. This should prevent the
    -- infamous: too many SQL variables
    | length types < chunkSize =
        deleteCascadeWhere ((entity <-. types):filters)
    | otherwise = do
        deleteCascadeWhere ((entity <-. take chunkSize types):filters)
        deleteMany filters entity (drop chunkSize types)

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
        [ CheckpointTableWalletId ==. wid
        ] [ LimitTo 1, Desc CheckpointTableSlot ]

selectUTxO
    :: Checkpoint
    -> SqlPersistT IO [UTxO]
selectUTxO (Checkpoint wid sl _parent) = fmap entityVal <$>
    selectList
        [ UtxoTableWalletId ==. wid
        , UtxoTableCheckpointSlot ==. sl
        ] []

selectTxs
    :: [TxId]
    -> SqlPersistT IO ([TxIn], [TxOut])
selectTxs txids = do
    ins <- fmap entityVal <$> selectList [TxInputTableTxId <-. txids]
        [Asc TxInputTableTxId, Asc TxInputTableOrder]
    outs <- fmap entityVal <$> selectList [TxOutputTableTxId <-. txids]
        [Asc TxOutputTableTxId, Asc TxOutputTableIndex]
    pure (ins, outs)

selectTxHistory
    :: forall t. PersistTx t
    => W.WalletId
    -> [Filter TxMeta]
    -> SqlPersistT IO [(W.Hash "Tx", (W.Tx t, W.TxMeta))]
selectTxHistory wid conditions = do
    let opt = [Desc TxMetaTableSlotId] -- note: there is an index on this column
    metas <- fmap entityVal <$> selectList
        ((TxMetaTableWalletId ==. wid) : conditions) opt
    let txids = map txMetaTableTxId metas
    (ins, outs) <- selectTxs txids
    pure $ txHistoryFromEntity @t metas ins outs

---------------------------------------------------------------------------
-- DB queries for address discovery state

-- | Get a @(WalletId, SlotId)@ pair from the checkpoint table, for use with
-- 'insertState' and 'selectState'.
checkpointId :: Checkpoint -> (W.WalletId, W.SlotId)
checkpointId cp = (checkpointTableWalletId cp, checkpointTableSlot cp)

-- | Functions for saving/loading the wallet's address discovery state into
-- SQLite.
class PersistState s where
    -- | Store the state for a checkpoint.
    insertState :: (W.WalletId, W.SlotId) -> s -> SqlPersistT IO ()
    -- | Load the state for a checkpoint.
    selectState :: (W.WalletId, W.SlotId) -> SqlPersistT IO (Maybe s)
    -- | Remove the state for all checkpoints of a wallet.
    deleteState :: W.WalletId -> SqlPersistT IO ()

class DefineTx t => PersistTx t where
    resolvedInputs :: Tx t -> [(W.TxIn, Maybe W.Coin)]
    -- | Extract transaction resolved inputs. This slightly breaks the
    -- abstraction boundary of 'DefineTx' which doesn't really force any
    -- structure on the resolved inputs.
    -- However, here in the DB-Layer, supporting arbitrary shape for the inputs
    -- be much more complex and require quite a lot of work. So, we kinda force
    -- the format here and only here, and leave the rest of the code with an
    -- opaque 'ResolvedTxIn' type.

    mkTx :: [(W.TxIn, Maybe W.Coin)] -> [W.TxOut] -> Tx t
    -- | Re-construct a transaction from a set resolved inputs and
    -- some outputs. Returns 'Nothing' if the transaction couldn't be
    -- constructed.

instance W.KeyToAddress t => PersistState (W.SeqState t) where
    insertState (wid, sl) st = do
        ssid <- insert (SeqState wid sl)
        intApId <- insertAddressPool $ W.internalPool st
        extApId <- insertAddressPool $ W.externalPool st
        insert_ $ SeqStateInternalPool ssid intApId
        insert_ $ SeqStateExternalPool ssid extApId
        insertMany_ $ mkSeqStatePendingIxs ssid $ W.pendingChangeIxs st

    selectState (wid, sl) = runMaybeT $ do
        ssid <- MaybeT $ fmap entityKey <$>
            selectFirst [ SeqStateTableWalletId ==. wid
                        , SeqStateTableCheckpointSlot ==. sl ] []
        intApId <- MaybeT $
            fmap (seqStateInternalPoolAddressPool . entityVal) <$>
            selectFirst [ SeqStateInternalPoolSeqStateId ==. ssid ] []
        extApId <- MaybeT $
            fmap (seqStateExternalPoolAddressPool . entityVal) <$>
            selectFirst [ SeqStateExternalPoolSeqStateId ==. ssid ] []
        internalPool <- MaybeT $ selectAddressPool intApId
        externalPool <- MaybeT $ selectAddressPool extApId
        pendingChangeIxs <- lift $ selectSeqStatePendingIxs ssid
        pure $ W.SeqState internalPool externalPool pendingChangeIxs

    deleteState wid = do
        ssid <- fmap entityKey <$> selectList [ SeqStateTableWalletId ==. wid ] []
        intApId <- fmap (seqStateInternalPoolAddressPool . entityVal) <$>
            selectList [ SeqStateInternalPoolSeqStateId <-. ssid ] []
        extApId <- fmap (seqStateExternalPoolAddressPool . entityVal) <$>
            selectList [ SeqStateExternalPoolSeqStateId <-. ssid ] []
        deleteMany [] AddressPoolId intApId
        deleteMany [] AddressPoolId extApId
        deleteCascadeWhere [SeqStateTableWalletId ==. wid]

insertAddressPool
    :: W.AddressPool t c
    -> SqlPersistT IO AddressPoolId
insertAddressPool ap = do
    let ap' = AddressPool (AddressPoolXPub $ W.accountPubKey ap) (W.gap ap)
    apid <- insert ap'
    let ixs = [ AddressPoolIndex apid a i | (i, a) <- zip [0..] (W.addresses ap) ]
    dbChunked insertMany_ ixs
    pure apid

mkSeqStatePendingIxs :: SeqStateId -> W.PendingIxs -> [SeqStatePendingIx]
mkSeqStatePendingIxs ssid =
    fmap (SeqStatePendingIx ssid . W.getIndex) . W.pendingIxsToList

selectAddressPool
    :: forall t chain. (W.KeyToAddress t, Typeable chain)
    => AddressPoolId
    -> SqlPersistT IO (Maybe (W.AddressPool t chain))
selectAddressPool apid = do
    ix <- fmap entityVal <$> selectList [IndexAddressPool ==. apid]
        [Asc IndexNumber]
    ap <- fmap entityVal <$> selectFirst [AddressPoolId ==. apid] []
    pure $ addressPoolFromEntity ix <$> ap
  where
    addressPoolFromEntity
        :: [AddressPoolIndex]
        -> AddressPool
        -> W.AddressPool t chain
    addressPoolFromEntity ixs (AddressPool (AddressPoolXPub pubKey) gap) =
        W.mkAddressPool @t @chain pubKey gap (map indexAddress ixs)

selectSeqStatePendingIxs :: SeqStateId -> SqlPersistT IO W.PendingIxs
selectSeqStatePendingIxs ssid =
    W.pendingIxsFromList . fromRes <$> selectList
        [SeqStatePendingIxSeqStateId ==. ssid]
        [Desc SeqStatePendingIxIndex]
  where
    fromRes = fmap (W.Index . seqStatePendingIxIndex . entityVal)

----------------------------------------------------------------------------
-- Logging

data DBLog
    = MsgMigrations Int
    | MsgQuery Text Severity
    | MsgConnStr Text
    | MsgClosing (Maybe FilePath)
    deriving (Generic, Show, Eq, ToJSON)

transformTrace :: Trace IO Text -> Trace IO DBLog
transformTrace = contramap (converter dbLogText)

-- bit of boilerplate here, a helper function could be useful.
converter :: (a -> b) -> LogObject a -> LogObject b
converter f (LogObject nm me loc) = LogObject nm me (conv loc)
  where
    conv (LogMessage msg) = LogMessage (f msg)
    conv (LogError a) = LogError a
    conv (LogValue a n) = LogValue a n
    conv (ObserveOpen st) = ObserveOpen st
    conv (ObserveDiff st) = ObserveDiff st
    conv (ObserveClose st) = ObserveClose st
    conv (AggregatedMessage ag) = AggregatedMessage ag
    conv (MonitoringEffect act) = MonitoringEffect act
    conv (Command v) = Command v
    conv KillPill = KillPill

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
