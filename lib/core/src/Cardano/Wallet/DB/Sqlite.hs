{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Sqlite
    ( newDBLayer
    ) where

import Prelude


import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.SqliteTypes
    ( TxId, sqlSettings' )
import Conduit
    ( runResourceT )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( MonadCatch (..), handleJust )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Logger
    ( runNoLoggingT )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( LogFunc
    , Update (..)
    , deleteCascadeWhere
    , entityVal
    , insert_
    , runMigration
    , runSqlConn
    , selectFirst
    , selectKeysList
    , updateWhere
    , (=.)
    , (==.)
    )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistM, SqlPersistT, wrapConnection )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import Database.Sqlite
    ( Error (ErrorConstraint), SqliteException (SqliteException) )
import GHC.Generics
    ( Generic (..) )
import Numeric.Natural
    ( Natural )
import System.IO
    ( stderr )
import System.Log.FastLogger
    ( fromLogStr )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- Wallet IDs, address discovery state, and metadata.
Wallet
    walTableId                 W.WalletId     sql=wallet_id
    walTableName               Text           sql=name
    walTablePassphraseLastUpdatedAt  UTCTime Maybe  sql=passphrase_last_updated_at
    walTableStatus             W.WalletState  sql=status
    walTableDelegation         Text Maybe     sql=delegation

    Primary walTableId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                                  sql=private_key
    privateKeyTableWalletId  W.WalletId     sql=wallet_id
    privateKeyTableRootKey   B8.ByteString  sql=root
    privateKeyTableHash      B8.ByteString  sql=hash

    Primary privateKeyTableWalletId
    Foreign Wallet fk_wallet_private_key privateKeyTableWalletId

    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metadata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
TxMeta
    txMetaTableTxId       TxId         sql=tx_id
    txMetaTableWalletId   W.WalletId   sql=wallet_id
    txMetaTableStatus     W.TxStatus   sql=status
    txMetaTableDirection  W.Direction  sql=direction
    txMetaTableSlotId     W.SlotId     sql=slot
    txMetaTableAmount     Natural      sql=amount

    Primary txMetaTableTxId txMetaTableWalletId
    Foreign Wallet fk_wallet_tx_meta txMetaTableWalletId
    deriving Show Generic

-- A transaction input associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txInputTableTxId is referred to by TxMeta and PendingTx
TxIn
    txInputTableTxId         TxId        sql=tx_id
    txInputTableSourceTxId   TxId        sql=source_id
    txInputTableSourceIndex  Word32      sql=source_index

    Primary txInputTableTxId txInputTableSourceTxId txInputTableSourceIndex
    deriving Show Generic

-- A transaction output associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTableTxId is referred to by TxMeta and PendingTx
TxOut
    txOutputTableTxId     TxId        sql=tx_id
    txOutputTableIndex    Word32      sql=index
    txOutputTableAddress  W.Address   sql=address
    txOutputTableAmount   W.Coin      sql=amount

    Primary txOutputTableTxId txOutputTableIndex
    deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot).
-- Checkpoint data such as UTxO will refer to this table.
Checkpoint
    checkpointTableWalletId    W.WalletId  sql=wallet_id
    checkpointTableSlot        W.SlotId    sql=slot

    Primary checkpointTableWalletId checkpointTableSlot
    Foreign Wallet fk_wallet_checkpoint checkpointTableWalletId

    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                     sql=utxo

    -- The wallet checkpoint (wallet_id, slot)
    utxoTableWalletId        W.WalletId  sql=wallet_id
    utxoTableCheckpointSlot  W.SlotId    sql=slot

    -- TxIn
    utxoTableInputId         TxId        sql=input_tx_id
    utxoTableInputIndex      Word32      sql=input_index

    -- TxOut
    utxoTableOutputAddress   W.Address   sql=output_address
    utxoTableOutputCoin      W.Coin      sql=output_coin

    Primary
        utxoTableWalletId
        utxoTableCheckpointSlot
        utxoTableInputId
        utxoTableInputIndex
        utxoTableOutputAddress
        utxoTableOutputCoin

    Foreign Checkpoint fk_checkpoint_utxo utxoTableWalletId utxoTableCheckpointSlot
    deriving Show Generic
|]


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

logStderr :: LogFunc
logStderr _ _ _ str = B8.hPutStrLn stderr (fromLogStr str)

-- | Run a query without error handling. There will be exceptions thrown if it
-- fails.
runQuery
    :: SqlBackend
    -> SqlPersistM a
    -> IO a
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
    :: Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (DBLayer IO s t)
newDBLayer fp = do
    conn <- createSqliteBackend fp logStderr
    runQuery conn $ runMigration migrateAll
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \(PrimaryKey wid) _cp meta ->
            ExceptT $ runQuery conn $
            handleConstraint (ErrWalletAlreadyExists wid) $
            insert_ (mkWalletEntity wid meta)

        , removeWallet = \(PrimaryKey wid) ->
            ExceptT $ runQuery conn $
            selectWallet wid >>= \case
                Just _ -> Right <$> deleteCascadeWhere [WalTableId ==. wid]
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , listWallets = runQuery conn $
            map (PrimaryKey . unWalletKey) <$> selectKeysList [] []

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey _wid) _cp -> error "unimplemented"

        , readCheckpoint = \(PrimaryKey _wid) -> error "unimplemented"

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

        , putTxHistory = \(PrimaryKey _wid) _txs -> error "unimplemented"

        , readTxHistory = \(PrimaryKey _wid) -> error "unimplemented"

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey _wid) _key -> error "unimplemented"

        , readPrivateKey = \(PrimaryKey _wid) -> error "unimplemented"

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

----------------------------------------------------------------------------
-- DB Queries

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid = fmap entityVal <$> selectFirst [WalTableId ==. wid] []
