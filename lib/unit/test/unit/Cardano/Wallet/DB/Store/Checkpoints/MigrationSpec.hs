{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Checkpoints.MigrationSpec where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext (..)
    , noAutoMigrations
    , noManualMigration
    , withSqliteContextFile
    )
import Cardano.Wallet.Address.Book
    ( Prologue (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState
    )
import Cardano.Wallet.DB.Layer
    ( readWalletId
    )
import Cardano.Wallet.DB.LayerSpec
    ( withinCopiedFile
    )
import Cardano.Wallet.DB.Migration
    ( runMigrations
    )
import Cardano.Wallet.DB.Sqlite.Migration.New
    ( newMigrationInterface
    )
import Cardano.Wallet.DB.Store.Checkpoints.Migration
    ( migratePrologue
    )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Proxy
    ( Proxy (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )

spec :: Spec
spec =
    describe "migratePrologue :: Migration _ 3 4" $ do

        it "'migrate' db sequential table" $
            testCanLoadAfterMigration
            (Proxy :: Proxy (SeqState 'Mainnet ShelleyKey))
            "api-bench/she.1ceb45b37a94c7022837b5ca14045f11a5927c65.sqlite"

        it "'migrate' db shared table" $
            testCanLoadAfterMigration
            (Proxy :: Proxy (SharedState 'Mainnet SharedKey))
            "api-bench/sha.a1d5337305630db051fac6da5f8038abf4067068.sqlite"

        it "do not 'migrate' db byron table" $
            testCanLoadAfterMigration
            (Proxy :: Proxy (RndState 'Mainnet))
            "api-bench/rnd.423b423718660431ebfe9c761cd72e64ee5065ac.sqlite"

-- | Test that the 'Store' can load the database after migration.
testCanLoadAfterMigration
    :: forall s. PersistAddressBook s
    => Proxy s -> FilePath -> IO ()
testCanLoadAfterMigration _ dbName = do
    Just (_ :: Prologue s) <-
        withCopiedAndMigrated dbName $ \ctx -> runQuery ctx $ do
            Just wid <- readWalletId
            loadPrologue wid
    pure ()

withCopiedAndMigrated :: FilePath -> (SqliteContext -> IO a) -> IO a
withCopiedAndMigrated file action =
    withCopiedFile file $ \path -> do
        runMigrations
            (newMigrationInterface nullTracer)
            path
            migratePrologue
        Right a <- withSqliteContextFile
            nullTracer
            path
            noManualMigration
            noAutoMigrations
            action
        pure a

withCopiedFile :: FilePath -> (FilePath -> IO a) -> IO a
withCopiedFile orig action =
    snd <$> withinCopiedFile orig (\path _ -> action path)
