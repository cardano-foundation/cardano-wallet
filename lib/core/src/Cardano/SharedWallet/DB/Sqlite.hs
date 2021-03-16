{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Redundant flip" -}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.SharedWallet.DB.Sqlite
    ( newDBLayer
    , withDBLayer
    , defaultFilePath
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBLog (..)
    , MigrationError
    , SqliteContext (..)
    , handleConstraint
    , newInMemorySqliteContext
    , newSqliteContext
    , withConnectionPool
    )
import Cardano.SharedWallet.DB
    ( DBLayer (..)
    , ErrNoSuchSharedWallet (..)
    , ErrSharedWalletAlreadyExists (..)
    )
import Cardano.SharedWallet.DB.Log
    ( SharedWalletDbLog (..) )
import Cardano.SharedWallet.DB.Sqlite.TH
    ( EntityField (..), SharedWallet (..), migrateAll )
import Cardano.SharedWallet.SharedState
    ( SharedWalletInfo (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Index (..), PersistPublicKey (..), getIndex )
import Control.Monad.IO.Class
    ( MonadIO (..), liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Database.Persist.Sql
    ( Entity (..), deleteWhere, insert_, selectFirst, (==.) )
import Database.Persist.Sqlite
    ( SqlPersistT )
import System.Directory
    ( removeFile )
import System.FilePath
    ( (</>) )
import UnliftIO.Exception
    ( bracket, catch, throwIO )

import qualified Cardano.Wallet.Primitive.Types as W


-- | Return the preferred @FilePath@ for the stake pool .sqlite file, given a
-- parent directory.
defaultFilePath
    :: FilePath
    -- ^ The directory in which the .sqlite file will be located.
    -> FilePath
defaultFilePath = (</> "shared-wallet.sqlite")

-- | Runs an action with a connection to the SQLite database.
--
-- This function has the same behaviour as 'withDBLayer', but provides a way
-- to decorate the created 'DBLayer' object with a 'DBDecorator', useful for
-- instrumenting or monitoring calls to database operations.
--
withDBLayer
    :: PersistPublicKey (k 'AccountK)
    => Tracer IO SharedWalletDbLog
       -- ^ Logging object
    -> Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> (DBLayer IO k -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer tr mDatabaseDir action = do
    case mDatabaseDir of
        Nothing -> bracket
            (newInMemorySqliteContext tr' [] migrateAll)
            fst
            (action . newDBLayer tr . snd)

        Just fp -> handlingPersistError tr fp $
            withConnectionPool tr' fp $ \wallet -> do
                ctx <- newSqliteContext tr' wallet [] migrateAll
                ctx & either
                    throwIO
                    (action . newDBLayer tr)
  where
    tr' = contramap MsgGeneric tr

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
    :: PersistPublicKey (k 'AccountK)
    => Tracer IO SharedWalletDbLog
       -- ^ Logging object
    -> SqliteContext
        -- ^ A (thread-) safe wrapper for running db queries.
    -> DBLayer IO k
newDBLayer tr SqliteContext{runQuery} =
    DBLayer {..}
      where
        initializeSharedState wid state meta gp = ExceptT $ do
            res <- handleConstraint (ErrSharedWalletAlreadyExists wid) $
                insert_ (mkSharedWalletEntity wid state meta gp)
            liftIO $ traceWith tr $ MsgCreatingSharedWallet wid
            pure res

        removeSharedWallet wid = ExceptT $ do
            selectSharedWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchSharedWallet wid
                Just _  -> Right <$> do
                    liftIO $ traceWith tr $ MsgRemovingSharedWallet wid
                    deleteWhere [SharedWalletWalletId ==. wid]

        readSharedWalletState wid = do
            selectSharedWallet wid >>= \case
                Nothing -> pure Nothing
                Just _  -> selectSharedWalletState wid

        readSharedWalletMetadata wid = do
            selectSharedWallet wid >>= \case
                Nothing -> pure Nothing
                Just _  -> selectSharedWalletMetadata wid

        addCosignerKey _walId _utctime _cosignerInfo = undefined

        listCosignerKeys _walId = undefined

        cleanDB = undefined

        atomically :: forall a. (SqlPersistT IO a -> IO a)
        atomically = runQuery


-- | 'Temporary', catches migration error from previous versions and if any,
-- _removes_ the database file completely before retrying to start the database.
--
-- This comes in handy to fix database schema in a non-backward compatible way
-- without altering too much the user experience. Indeed, the pools' database
-- can swiftly be re-synced from the chain, so instead of patching our mistakes
-- with ugly work-around we can, at least for now, reset it semi-manually when
-- needed to keep things tidy here.
handlingPersistError
    :: Tracer IO SharedWalletDbLog
       -- ^ Logging object
    -> FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO a
       -- ^ Action to retry
    -> IO a
handlingPersistError tr fp action =
    action `catch` \(_e :: MigrationError) -> do
        traceWith tr $ MsgGeneric MsgDatabaseReset
        removeFile fp
        action

mkSharedWalletEntity
    :: PersistPublicKey (k 'AccountK)
    => W.WalletId
    -> SharedWalletInfo k
    -> W.WalletMetadata
    -> W.GenesisParameters
    -> SharedWallet
mkSharedWalletEntity wid state meta gp = SharedWallet
    { sharedWalletWalletId = wid
    , sharedWalletCreationTime = meta ^. #creationTime
    , sharedWalletUpdateTime = Nothing
    , sharedWalletName = meta ^. #name . coerce
    , sharedWalletAccountXPub = serializeXPub (state ^. #walletAccountKey)
    , sharedWalletAccountIndex = getIndex (state ^. #accountIx)
    , sharedWalletScriptGap = state ^. #poolGap
    , sharedWalletPaymentScript = state ^. #paymentScript
    , sharedWalletDelegationScript = state ^. #delegationScript
    , sharedWalletState = state ^. #walletState
    , sharedWalletPassphraseLastUpdatedAt = W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , sharedWalletPassphraseScheme = W.passphraseScheme <$> meta ^. #passphraseInfo
    , sharedWalletGenesisHash = BlockId (coerce (gp ^. #getGenesisBlockHash))
    , sharedWalletGenesisStart = coerce (gp ^. #getGenesisBlockDate)
    }

selectSharedWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe SharedWallet)
selectSharedWallet wid =
    fmap entityVal <$> selectFirst [SharedWalletWalletId ==. wid] []

selectSharedWalletMetadata
    :: W.WalletId
    -> SqlPersistT IO (Maybe W.WalletMetadata)
selectSharedWalletMetadata wid = do
    let del = W.WalletDelegation W.NotDelegating []
    fmap (metadataFromEntity del . entityVal)
        <$> selectFirst [SharedWalletWalletId ==. wid] []

metadataFromEntity :: W.WalletDelegation -> SharedWallet -> W.WalletMetadata
metadataFromEntity walDelegation wal = W.WalletMetadata
    { name = W.WalletName (sharedWalletName wal)
    , creationTime = sharedWalletCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo
        <$> sharedWalletPassphraseLastUpdatedAt wal
        <*> sharedWalletPassphraseScheme wal
    , delegation = walDelegation
    }

selectSharedWalletState
    :: PersistPublicKey (k 'AccountK)
    => W.WalletId
    -> SqlPersistT IO (Maybe (SharedWalletInfo k))
selectSharedWalletState wid = do
    fmap (stateFromEntity . entityVal)
        <$> selectFirst [SharedWalletWalletId ==. wid] []

stateFromEntity
    :: PersistPublicKey (k 'AccountK)
    => SharedWallet -> SharedWalletInfo k
stateFromEntity wal = SharedWalletInfo
    { walletState = sharedWalletState wal
    , walletAccountKey = unsafeDeserializeXPub (sharedWalletAccountXPub wal)
    , accountIx = Index (sharedWalletAccountIndex wal)
    , paymentScript = sharedWalletPaymentScript wal
    , delegationScript = sharedWalletDelegationScript wal
    , poolGap = sharedWalletScriptGap wal
    }
