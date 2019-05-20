{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

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
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), deserializeXPrv, serializeXPrv )
import Conduit
    ( runResourceT )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( MonadCatch (..), handleJust )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Logger
    ( LogLevel (..), runNoLoggingT )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
import Database.Persist.Sql
    ( LogFunc
    , Update (..)
    , deleteCascadeWhere
    , deleteWhere
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
import Database.Sqlite
    ( Error (ErrorConstraint), SqliteException (SqliteException) )
import System.IO
    ( stderr )
import System.Log.FastLogger
    ( fromLogStr )

import Cardano.Wallet.DB.Sqlite.TH

import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
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
    :: Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (DBLayer IO s t)
newDBLayer fp = do
    conn <- createSqliteBackend fp (dbLogs [LevelError])
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

----------------------------------------------------------------------------
-- DB Queries

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid = fmap entityVal <$> selectFirst [WalTableId ==. wid] []
