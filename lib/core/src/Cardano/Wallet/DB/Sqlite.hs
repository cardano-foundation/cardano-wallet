{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Sqlite
    ( newDBLayer
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( AddressPoolXPub (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), deserializeXPrv, serializeXPrv )
import Conduit
    ( runResourceT )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( MonadCatch (..), handleJust )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Logger
    ( LogLevel (..), runNoLoggingT )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Data.Bifunctor
    ( bimap )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import Database.Persist.Sql
    ( Entity (..)
    , LogFunc
    , SelectOpt (..)
    , Update (..)
    , deleteCascadeWhere
    , deleteWhere
    , insert
    , insertMany_
    , insert_
    , putMany
    , runMigration
    , runSqlConn
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (/<-.)
    , (<-.)
    , (=.)
    , (==.)
    )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistM, SqlPersistT, wrapConnection )
import Database.Sqlite
    ( Error (ErrorConstraint), SqliteException (SqliteException) )
import System.IO
    ( stderr )
import System.Log.FastLogger
    ( fromLogStr )

import Cardano.Wallet.DB.Sqlite.TH

import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.AddressDiscovery as W
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

----------------------------------------------------------------------------
-- Sqlite connection set up

enableForeignKeys :: Sqlite.Connection -> IO ()
enableForeignKeys conn = stmt >>= void . Sqlite.step
    where stmt = Sqlite.prepare conn "PRAGMA foreign_keys = ON;"

createSqliteBackend :: Maybe FilePath -> LogFunc -> IO SqlBackend
createSqliteBackend fp logFunc = do
  conn <- Sqlite.open (sqliteConnStr fp)
  enableForeignKeys conn
  wrapConnection conn logFunc

sqliteConnStr :: Maybe FilePath -> Text
sqliteConnStr = maybe ":memory:" T.pack

dbLogs :: [LogLevel] -> LogFunc
dbLogs levels _ _ level str =
    if level `elem` levels then
        B8.hPutStrLn stderr (fromLogStr str)
    else
        pure ()

-- | Run a query without error handling. There will be exceptions thrown if it
-- fails.
runQuery :: SqlBackend -> SqlPersistM a -> IO a
runQuery conn = runResourceT . runNoLoggingT . flip runSqlConn conn

-- | Run an action, and convert any Sqlite constraints exception into the given
-- error result. No other exceptions are handled.
handleConstraint :: MonadCatch m => e -> m a -> m (Either e a)
handleConstraint e = handleJust select handler . fmap Right
  where
    select (SqliteException ErrorConstraint _ _) = Just ()
    select _ = Nothing
    handler = const . pure  . Left $ e

----------------------------------------------------------------------------
-- Database layer methods

-- | Sets up a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
newDBLayer
    :: forall s t. (W.IsOurs s, NFData s, Show s, PersistState s, W.TxId t)
    => Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (DBLayer IO s t)
newDBLayer fp = do
    conn <- createSqliteBackend fp (dbLogs [LevelError])
    runQuery conn $ runMigration migrateAll
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \(PrimaryKey wid) cp meta ->
            ExceptT $ runQuery conn $
            handleConstraint (ErrWalletAlreadyExists wid) $ do
                insert_ (mkWalletEntity wid meta)
                insertCheckpoint wid cp

        , removeWallet = \(PrimaryKey wid) ->
            ExceptT $ runQuery conn $
            selectWallet wid >>= \case
                Just _ -> Right <$> do
                    -- fixme: deleteCascade is not working with persistent-sqlite.
                    -- Therefore we need to delete related entities as well.
                    deleteCheckpoints @s wid
                    deleteLooseTransactions wid
                    deleteCascadeWhere [WalTableId ==. wid]
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , listWallets = runQuery conn $
            map (PrimaryKey . unWalletKey) <$> selectKeysList [] []

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey wid) cp ->
            ExceptT $ runQuery conn $ Right <$> do
                deleteCheckpoints @s wid -- clear out all checkpoints
                deleteLooseTransactions wid -- clear transactions
                insertCheckpoint wid cp -- add this checkpoint

        , readCheckpoint = \(PrimaryKey wid) ->
            runQuery conn $
            selectLatestCheckpoint wid >>= \case
                Just cp -> do
                    utxo <- selectUTxO cp
                    pendings <- selectPending cp
                    (ins, outs) <- selectTxs pendings
                    s <- selectState (checkpointId cp)
                    pure (checkpointFromEntity cp utxo ins outs <$> s)
                Nothing -> pure Nothing

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \(PrimaryKey wid) meta ->
            ExceptT $ runQuery conn $
            selectWallet wid >>= \case
                Just _ -> do
                    updateWhere [WalTableId ==. wid]
                        (mkWalletMetadataUpdate meta)
                    pure $ Right ()
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readWalletMeta = \(PrimaryKey wid) ->
            runQuery conn $
            fmap (metadataFromEntity . entityVal) <$>
            selectFirst [WalTableId ==. wid] []

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs ->
            ExceptT $ runQuery conn $
            selectWallet wid >>= \case
                Just _ -> do
                    let (metas, txins, txouts) = mkTxHistory wid txs
                    putMany metas
                    putMany txins
                    putMany txouts
                    pure $ Right ()
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readTxHistory = \(PrimaryKey wid) -> runQuery conn $
            selectTxHistory wid

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey wid) key ->
            ExceptT $ runQuery conn $
            selectWallet wid >>= \case
                Just _ -> Right <$> do
                    deleteWhere [PrivateKeyTableWalletId ==. wid]
                    insert_ (mkPrivateKeyEntity wid key)
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readPrivateKey = \(PrimaryKey wid) ->
            runQuery conn $ let
                keys = selectFirst [PrivateKeyTableWalletId ==. wid] []
                toMaybe = either (const Nothing) Just
            in (>>= toMaybe . privateKeyFromEntity . entityVal) <$> keys

        {-----------------------------------------------------------------------
                                       Lock
        -----------------------------------------------------------------------}

        , withLock = \_action -> error "withLock to be implemented"

        }

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
    , walTablePassphraseLastUpdatedAt =
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walTableStatus = meta ^. #status
    , walTableDelegation = delegationToText $ meta ^. #delegation
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalTableName =. meta ^. #name . coerce
    , WalTablePassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , WalTableStatus =. meta ^. #status
    , WalTableDelegation =. delegationToText (meta ^. #delegation)
    ]

metadataFromEntity :: Wallet -> W.WalletMetadata
metadataFromEntity wal = W.WalletMetadata
    { name = W.WalletName (walTableName wal)
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
    :: forall s t. W.TxId t
    => W.WalletId
    -> W.Wallet s t
    -> (Checkpoint, [UTxO], [PendingTx], [TxIn], [TxOut])
mkCheckpointEntity wid wal =
    ( cp, utxo, map (pendingTx . fst) pending
    , concatMap (dist pendingTxIn . fmap W.inputs) pending
    , concatMap (dist pendingTxOut . fmap (zip [0..] . W.outputs)) pending )
  where
    pending = [(TxId (W.txId @t tx), tx) | tx <- Set.toList (W.getPending wal)]
    sl = W.currentTip wal
    cp = Checkpoint
        { checkpointTableWalletId = wid
        , checkpointTableSlot = sl
        }
    pendingTx tid = PendingTx
        { pendingTxTableWalletId = wid
        , pendingTxTableCheckpointSlot = sl
        , pendingTxTableId2 = tid
        }
    pendingTxIn tid txIn = TxIn
        { txInputTableTxId = tid
        , txInputTableSourceTxId = TxId (W.inputId txIn)
        , txInputTableSourceIndex = W.inputIx txIn
        }
    pendingTxOut tid (ix, txOut) = TxOut
        { txOutputTableTxId = tid
        , txOutputTableIndex = ix
        , txOutputTableAddress = W.address txOut
        , txOutputTableAmount = W.coin txOut
        }
    utxo = [ UTxO wid sl (TxId input) ix addr coin
           | (W.TxIn input ix, W.TxOut addr coin) <- utxoMap ]
    utxoMap = Map.assocs (W.getUTxO (W.totalUTxO wal))

-- inputs and outputs must be sorted by txid, then ix
checkpointFromEntity
    :: forall s t. (W.IsOurs s, NFData s, Show s, W.TxId t)
    => Checkpoint
    -> [UTxO]
    -> [TxIn]
    -> [TxOut]
    -> s
    -> W.Wallet s t
checkpointFromEntity (Checkpoint _ tip) utxo ins outs =
    W.Wallet utxo' pending tip
  where
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo ]
    ins' = [(txid, W.TxIn src ix) | TxIn txid (TxId src) ix <- ins]
    outs' = [ (txid, W.TxOut addr amt)
            | TxOut txid _ix addr amt <- outs ]
    txids = Set.fromList $ map fst ins' ++ map fst outs'
    pending = flip Set.map txids $ \txid -> W.Tx
        { W.inputs = lookupTx txid ins'
        , W.outputs = lookupTx txid outs'
        }
    lookupTx txid = map snd . filter ((== txid) . fst)

mkTxHistory
    :: W.WalletId
    -> Map.Map (W.Hash "Tx") (W.Tx, W.TxMeta)
    -> ([TxMeta], [TxIn], [TxOut])
mkTxHistory wid txs =
    ( map (uncurry (mkTxMetaEntity wid)) metas
    , concatMap (dist mkTxIn . fmap W.inputs) hist
    , concatMap (dist mkTxOut . fmap (zip [0..] . W.outputs)) hist )
  where
    pairs = Map.toList txs
    metas = fmap snd <$> pairs
    hist = bimap TxId fst <$> pairs
    mkTxIn tid txIn = TxIn
        { txInputTableTxId = tid
        , txInputTableSourceTxId = TxId (W.inputId txIn)
        , txInputTableSourceIndex = W.inputIx txIn
        }
    mkTxOut tid (ix, txOut) = TxOut
        { txOutputTableTxId = tid
        , txOutputTableIndex = ix
        , txOutputTableAddress = W.address txOut
        , txOutputTableAmount = W.coin txOut
        }

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

-- note: TxOut records must already be sorted by index
txHistoryFromEntity
    :: [TxMeta]
    -> [TxIn]
    -> [TxOut]
    -> Map.Map (W.Hash "Tx") (W.Tx, W.TxMeta)
txHistoryFromEntity metas ins outs = Map.fromList
    [ (getTxId (txMetaTableTxId m), (mkTx (txMetaTableTxId m), mkTxMeta m))
    | m <- metas ]
  where
    mkTx txid = W.Tx
        { W.inputs = map mkTxIn $ filter ((== txid) . txInputTableTxId) ins
        , W.outputs = map mkTxOut $ filter ((== txid) . txOutputTableTxId) outs
        }
    mkTxIn tx = W.TxIn
        { W.inputId = getTxId (txInputTableSourceTxId tx)
        , W.inputIx = txInputTableSourceIndex tx
        }
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
    :: (PersistState s, W.TxId t)
    => W.WalletId
    -> W.Wallet s t
    -> SqlPersistM ()
insertCheckpoint wid cp = do
    let (cp', utxo, pendings, ins, outs) = mkCheckpointEntity wid cp
    insert_ cp'
    insertMany_ ins
    insertMany_ outs
    insertMany_ pendings
    insertMany_ utxo
    insertState (wid, W.currentTip cp) (W.getState cp)

-- | Delete all checkpoints associated with a wallet.
deleteCheckpoints
    :: forall s. PersistState s
    => W.WalletId
    -> SqlPersistM ()
deleteCheckpoints wid = do
    deleteWhere [UtxoTableWalletId ==. wid]
    deleteWhere [PendingTxTableWalletId ==. wid]
    deleteWhere [CheckpointTableWalletId ==. wid]
    deleteState @s wid -- clear state

-- | Delete transactions that belong to a wallet and aren't referred to by
-- either Pending or TxMeta.
deleteLooseTransactions
    :: W.WalletId
    -> SqlPersistM ()
deleteLooseTransactions wid = do
    pendingTxId <- fmap (pendingTxTableId2 . entityVal) <$>
        selectList [PendingTxTableWalletId ==. wid] []
    metaTxId <- fmap (txMetaTableTxId . entityVal) <$>
        selectList [TxMetaTableWalletId ==. wid] []
    deleteWhere [ TxInputTableTxId /<-. pendingTxId
                , TxInputTableTxId /<-. metaTxId ]
    deleteWhere [ TxOutputTableTxId /<-. pendingTxId
                , TxOutputTableTxId /<-. metaTxId ]

selectLatestCheckpoint
    :: W.WalletId
    -> SqlPersistM (Maybe Checkpoint)
selectLatestCheckpoint wid = fmap entityVal <$>
    selectFirst [CheckpointTableWalletId ==. wid]
    [LimitTo 1, Desc CheckpointTableSlot]

selectUTxO
    :: Checkpoint
    -> SqlPersistM [UTxO]
selectUTxO (Checkpoint wid sl) = fmap entityVal <$>
    selectList [UtxoTableWalletId ==. wid, UtxoTableCheckpointSlot ==. sl] []

selectPending
    :: Checkpoint
    -> SqlPersistM [TxId]
selectPending (Checkpoint wid sl) = fmap (pendingTxTableId2 . entityVal) <$>
    selectList [ PendingTxTableWalletId ==. wid
               , PendingTxTableCheckpointSlot ==. sl ] []

selectTxs
    :: [TxId]
    -> SqlPersistM ([TxIn], [TxOut])
selectTxs txids = do
    ins <- fmap entityVal <$> selectList [TxInputTableTxId <-. txids]
        [Asc TxInputTableTxId, Asc TxInputTableSourceIndex]
    outs <- fmap entityVal <$> selectList [TxOutputTableTxId <-. txids]
        [Asc TxOutputTableTxId, Asc TxOutputTableIndex]
    pure (ins, outs)

selectTxHistory
    :: W.WalletId
    -> SqlPersistM (Map.Map (W.Hash "Tx") (W.Tx, W.TxMeta))
selectTxHistory wid = do
    metas <- fmap entityVal <$> selectList [TxMetaTableWalletId ==. wid] []
    let txids = map txMetaTableTxId metas
    ins <- fmap entityVal <$> selectList [TxInputTableTxId <-. txids]
        [Asc TxInputTableTxId, Asc TxInputTableSourceIndex]
    outs <- fmap entityVal <$> selectList [TxOutputTableTxId <-. txids]
        [Asc TxOutputTableTxId, Asc TxOutputTableIndex]
    pure $ txHistoryFromEntity metas ins outs

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
    insertState :: (W.WalletId, W.SlotId) -> s -> SqlPersistM ()
    -- | Load the state for a checkpoint.
    selectState :: (W.WalletId, W.SlotId) -> SqlPersistM (Maybe s)
    -- | Remove the state for all checkpoints of a wallet.
    deleteState :: W.WalletId -> SqlPersistM ()

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
        -- fixme: cascading delete not working with persistent-sqlite
        deleteCascadeWhere [SeqStateTableWalletId ==. wid]

insertAddressPool
    :: W.AddressPool t c
    -> SqlPersistM AddressPoolId
insertAddressPool ap = do
    let ap' = AddressPool (AddressPoolXPub $ W.accountPubKey ap) (W.gap ap)
    apid <- insert ap'
    insertMany_ [ AddressPoolIndex apid a (W.getIndex i)
                | (a, i) <- Map.toList (W.indexedAddresses ap) ]
    pure apid

mkSeqStatePendingIxs :: SeqStateId -> W.PendingIxs -> [SeqStatePendingIx]
mkSeqStatePendingIxs ssid (W.PendingIxs ixs) =
    [SeqStatePendingIx ssid i (W.getIndex ix) | (i, ix) <- zip [0..] ixs]

selectAddressPool
    :: forall t chain. (W.KeyToAddress t, Typeable chain)
    => AddressPoolId
    -> SqlPersistM (Maybe (W.AddressPool t chain))
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
        ap { W.indexedAddresses = addrs }
      where
        ap = W.mkAddressPool @t @chain pubKey gap []
        addrs = Map.fromList
            [(addr, W.Index ix) | AddressPoolIndex _ addr ix <- ixs]

selectSeqStatePendingIxs :: SeqStateId -> SqlPersistM W.PendingIxs
selectSeqStatePendingIxs ssid =
    fromRes <$> selectList
        [SeqStatePendingIxSeqStateId ==. ssid]
        [Asc SeqStatePendingIxPos]
  where
    fromRes = W.PendingIxs . fmap (W.Index . seqStatePendingIxIndex . entityVal)

----------------------------------------------------------------------------
-- Utilities

-- | Distribute `a` accross many `b`s using the given function.
-- >>> dist TxOut (addr, [Coin 1, Coin 42, Coin 14])
-- [TxOut addr (Coin 1), TxOut addr (Coin 42), TxOut addr (Coin 14)]
dist :: (a -> b -> c) -> (a, [b]) -> [c]
dist f (a, bs) = [f a b | b <- bs]
