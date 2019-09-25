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
import Cardano.Wallet.Primitive.Model
    ( currentTip )
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
    ( mapM_, void, when )
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
import qualified Data.Set as Set
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

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: forall s t k a. (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => CM.Configuration
       -- ^ Logging configuration
    -> Trace IO Text
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO s t k -> IO a)
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
                      deleteCheckpoints wid []
                      deleteState @s wid
                      deleteUTxOs wid []
                      deleteTxMetas wid []
                      deleteLooseTransactions
                      deleteWhere [PrivateKeyWalletId ==. wid]
                      deleteCascadeWhere [WalId ==. wid]
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
                      purgeCheckpoints wid cp
                      deleteLooseTransactions
                      insertCheckpoint wid cp
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readCheckpoint = \(PrimaryKey wid) -> runQuery $
              selectLatestCheckpoint wid >>= \case
                  Just cp -> do
                      utxo <- selectUTxO cp
                      -- 'checkpointFromEntity' will create a 'Set' from the
                      -- pending txs so the order is not important.
                      let order = W.Descending
                      txs <- selectTxHistory @t wid order
                          [ TxMetaStatus ==. W.Pending
                          ]
                      s <- selectState (checkpointId cp)
                      pure (checkpointFromEntity @s @t cp utxo txs <$> s)
                  Nothing -> pure Nothing

        , rollbackTo = \(PrimaryKey wid) point -> ExceptT $ runQuery $ do
            findNearestPoint wid point >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just nearestPoint -> Right <$> do
                    deleteCheckpoints wid
                        [ CheckpointSlot >. point
                        ]
                    deleteUTxOs wid
                        [ UtxoSlot >. point
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
                    rollbackState @s wid point

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
                      let entities = mkTxHistory @t wid $ W.invariant
                              ("putTxHistory has been called with pending txs: "
                                <> show txs)
                              txs
                              (not . any (W.isPending . snd))
                      let (metas, txins, txouts) = flatTxHistory entities
                      putTxMetas metas
                      putTxs txins txouts
                      pure $ Right ()
                  Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readTxHistory = \(PrimaryKey wid) order range -> runQuery $
              selectTxHistory @t wid order $ catMaybes
                [ (TxMetaSlotId >=.) <$> W.inclusiveLowerBound range
                , (TxMetaSlotId <=.) <$> W.inclusiveUpperBound range
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
    runQuery addIndices
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
                             SQLite database setup
-------------------------------------------------------------------------------}

addIndices :: SqlPersistT IO ()
addIndices = mapM_ (`rawExecute` [])
    [ createIndex "tx_meta_wallet_id" "tx_meta (wallet_id)"
    -- Covers filtering and sorting (either direction) of tx history
    , createIndex "tx_meta_slot" "tx_meta (slot ASC)"
    , createIndex "tx_meta_tx_id" "tx_meta (tx_id ASC)"
    -- Indices for filtering TxIn/TxOut by TxId
    , createIndex "tx_in_tx_id" "tx_in (tx_id)"
    , createIndex "tx_out_tx_id" "tx_out (tx_id)"
    ]
  where
    createIndex name on = "CREATE INDEX IF NOT EXISTS " <> name <> " ON " <> on

{-------------------------------------------------------------------------------
           Conversion between Persistent table types and wallet types
-------------------------------------------------------------------------------}

delegationToText :: W.WalletDelegation W.PoolId -> Maybe Text
delegationToText W.NotDelegating = Nothing
delegationToText (W.Delegating pool) = Just (W.getPoolId pool)

delegationFromText :: Maybe Text -> W.WalletDelegation W.PoolId
delegationFromText Nothing = W.NotDelegating
delegationFromText (Just pool) = W.Delegating (W.PoolId pool)

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> Wallet
mkWalletEntity wid meta = Wallet
    { walId = wid
    , walName = meta ^. #name . coerce
    , walCreationTime = meta ^. #creationTime
    , walPassphraseLastUpdatedAt =
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walStatus = meta ^. #status
    , walDelegation = delegationToText $ meta ^. #delegation
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalName =. meta ^. #name . coerce
    , WalCreationTime =. meta ^. #creationTime
    , WalPassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , WalStatus =. meta ^. #status
    , WalDelegation =. delegationToText (meta ^. #delegation)
    ]

metadataFromEntity :: Wallet -> W.WalletMetadata
metadataFromEntity wal = W.WalletMetadata
    { name = W.WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo <$>
        walPassphraseLastUpdatedAt wal
    , status = walStatus wal
    , delegation = delegationFromText (walDelegation wal)
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
    (metas, ins, outs) = flatTxHistory $
        mkTxHistory @t wid (Map.fromList pending)
    header = (W.currentTip wal)
    sl = header ^. #slotId
    parent = header ^. #prevBlockHash
    (Quantity bh) = header ^. #blockHeight
    bp = W.blockchainParameters wal
    cp = Checkpoint
        { checkpointWalletId = wid
        , checkpointSlot = sl
        , checkpointBlockHeight = fromIntegral bh
        , checkpointParent = BlockId parent
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
    -> [(W.Hash "Tx", (W.Tx t, W.TxMeta))]
    -> s
    -> W.Wallet s t
checkpointFromEntity cp utxo txs s =
    W.unsafeInitWallet utxo' pending header s bp
  where
    (Checkpoint
        _walletId
        slot
        (BlockId parentHeaderHash)
        bh
        genesisStart
        feePolicy
        slotLength
        epochLength
        txMaxSize
        epochStability
        ) = cp
    header = (W.BlockHeader slot blockHeight' parentHeaderHash)
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo
        ]
    pending = Set.fromList $ map snd txs
    blockHeight' = Quantity . toEnum . fromEnum $ bh
    bp = W.BlockchainParameters
        { getGenesisBlockDate = W.StartTime genesisStart
        , getFeePolicy = feePolicy
        , getSlotLength = W.SlotLength (toEnum (fromEnum slotLength))
        , getEpochLength = W.EpochLength epochLength
        , getTxMaxSize = Quantity txMaxSize
        , getEpochStability = Quantity epochStability
        }

mkTxHistory
    :: forall t. PersistTx t
    => W.WalletId
    -> Map (W.Hash "Tx") (W.Tx t, W.TxMeta)
    -> [(TxMeta, ([TxIn], [TxOut]))]
mkTxHistory wid txs =
    [ (mkTxMetaEntity wid txid meta, mkTxInputsOutputs @t (txid, tx))
    | (txid, (tx, meta)) <- Map.toList txs ]

-- | Make flat lists of entities from the result of 'mkTxHistory'.
flatTxHistory :: [(TxMeta, ([TxIn], [TxOut]))] -> ([TxMeta], [TxIn], [TxOut])
flatTxHistory entities =
    ( map fst entities
    , concatMap (fst . snd) entities
    , concatMap (snd. snd) entities )

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
    -> [(W.Hash "Tx", (W.Tx t, W.TxMeta))]
txHistoryFromEntity metas ins outs = map mkItem metas
  where
    mkItem m =
        ( getTxId (txMetaTxId m)
        , (mkTxWith (txMetaTxId m), mkTxMeta m)
        )
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
    :: forall s t. (PersistState s, PersistTx t)
    => W.WalletId
    -> W.Wallet s t
    -> SqlPersistT IO ()
insertCheckpoint wid cp = do
    let (cp', utxo, ins, outs, metas) = mkCheckpointEntity wid cp
    let sl = (W.currentTip cp) ^. #slotId
    repsert (CheckpointKey wid sl) cp'
    deleteTxMetas wid [TxMetaSlotId ==. sl, TxMetaStatus ==. W.Pending]
    putTxMetas metas
    putTxs ins outs
    deleteWhere [UtxoWalletId ==. wid, UtxoSlot ==. sl]
    dbChunked insertMany_ utxo
    insertState (wid, sl) (W.getState cp)

-- | Delete one or all checkpoints associated with a wallet.
deleteCheckpoints
    :: W.WalletId
    -> [Filter Checkpoint]
    -> SqlPersistT IO ()
deleteCheckpoints wid filters = do
    deleteWhere ((CheckpointWalletId ==. wid) : filters)

-- | Delete UTxO values for a wallet
deleteUTxOs
    :: W.WalletId
    -> [Filter UTxO]
    -> SqlPersistT IO ()
deleteUTxOs wid filters =
    deleteWhere ((UtxoWalletId ==. wid) : filters)

-- | Clean up checkpoints which are greater than `k` blocks old for we know we
-- can't rollback that far.
purgeCheckpoints
    :: W.WalletId
    -> W.Wallet s t
    -> SqlPersistT IO ()
purgeCheckpoints wid cp = do
    let epochStability = W.blockchainParameters cp ^. #getEpochStability
    let tipHeight = W.blockHeight . currentTip $ cp
    let minHeight = word64 tipHeight - word64 epochStability
    mCp <- selectFirst
        [ CheckpointWalletId ==. wid
        , CheckpointBlockHeight <=. minHeight
        ] [Desc CheckpointBlockHeight]
    case mCp of
        Nothing -> return ()
        Just minCp -> do
            deleteWhere
                [ UtxoWalletId ==. wid
                , UtxoSlot <=. checkpointSlot (entityVal minCp)
                ]
            deleteWhere
                [ CheckpointWalletId ==. wid
                , CheckpointBlockHeight <=. minHeight
                ]
  where
    word64 :: Integral a => Quantity "block" a -> Word64
    word64 (Quantity x) = fromIntegral x

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
    -> SqlPersistT IO [(W.Hash "Tx", (W.Tx t, W.TxMeta))]
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
    -- | Drop the state and everything related to it
    deleteState :: W.WalletId -> SqlPersistT IO ()
    -- | Rollback the state to a given slot
    rollbackState :: W.WalletId -> W.SlotId -> SqlPersistT IO ()

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
        deleteWhere [SeqStateAddressWalletId ==. wid, SeqStateAddressSlot ==. sl]
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

    deleteState wid = do
        deleteWhere [SeqStateAddressWalletId ==. wid]
        deleteWhere [SeqStatePendingWalletId ==. wid]
        deleteWhere [SeqStateWalletId ==. wid]

    rollbackState wid slot = do
        deleteWhere
            [ SeqStateAddressWalletId ==. wid
            , SeqStateAddressSlot >. slot
            ]

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

    deleteState wid = do
        deleteWhere [ RndStateAddressWalletId ==. wid ]
        deleteWhere [ RndStatePendingAddressWalletId ==. wid ]
        deleteWhere [ RndStateWalletId ==. wid ]

    rollbackState wid slot = do
        deleteWhere
            [ RndStateAddressWalletId ==. wid
            , RndStateAddressSlot >. slot
            ]

insertRndStateAddresses
    :: W.WalletId
    -> W.SlotId
    -> RndStateAddresses
    -> SqlPersistT IO ()
insertRndStateAddresses wid sl addresses = do
    deleteWhere [RndStateAddressWalletId ==. wid, RndStateAddressSlot ==. sl]
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
