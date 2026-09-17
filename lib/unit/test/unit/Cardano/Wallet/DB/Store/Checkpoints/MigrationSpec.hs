{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Checkpoints.MigrationSpec where

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
    , migrateSingleAddressMode
    )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    )
import Control.Monad
    ( void
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Proxy
    ( Proxy (..)
    )
import Database.Persist.Types
    ( PersistValue (..)
    )
import System.Directory
    ( removeFile
    )
import System.IO
    ( hClose
    )
import System.IO.Temp
    ( withSystemTempFile
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Prelude

import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

spec :: Spec
spec = do
    describe "migratePrologue :: Migration _ 3 4" $ do
        it "'migrate' db sequential table"
            $ testCanLoadAfterMigration
                (Proxy :: Proxy (SeqState 'Mainnet ShelleyKey))
                "api-bench/she.1ceb45b37a94c7022837b5ca14045f11a5927c65.sqlite"

        it "'migrate' db shared table"
            $ testCanLoadAfterMigration
                (Proxy :: Proxy (SharedState 'Mainnet SharedKey))
                "api-bench/sha.a1d5337305630db051fac6da5f8038abf4067068.sqlite"

        it "do not 'migrate' db byron table"
            $ testCanLoadAfterMigration
                (Proxy :: Proxy (RndState 'Mainnet))
                "api-bench/rnd.423b423718660431ebfe9c761cd72e64ee5065ac.sqlite"


    describe "migrateSingleAddressMode :: Migration _ 6 7" $ do
        it "enables Shelley account 0 once and preserves controls" $
            withSystemTempFile "single-address-mode.sqlite" $ \path handle -> do
                hClose handle
                removeFile path
                conn <- Sqlite.open $ T.pack path
                execute conn
                    "CREATE TABLE database_schema_version \
                    \(name TEXT PRIMARY KEY, version INTEGER NOT NULL)"
                execute conn
                    "INSERT INTO database_schema_version VALUES ('schema', 6)"
                execute conn
                    "CREATE TABLE seq_state \
                    \(derivation_prefix TEXT NOT NULL, change_addr_mode TEXT NOT NULL)"
                mapM_ (execute conn)
                    [ "INSERT INTO seq_state VALUES \
                      \('2147485500/2147485463/2147483648', 'single')"
                    , "INSERT INTO seq_state VALUES \
                      \('2147485500/2147485463/2147483648', 'increasing')"
                    , "INSERT INTO seq_state VALUES ('byron', 'increasing')"
                    , "INSERT INTO seq_state VALUES \
                      \('2147485500/2147485463/2147483648', 'corrupt')"
                    ]
                Sqlite.close conn
                runMigrations
                    (newMigrationInterface nullTracer)
                    path
                    migrateSingleAddressMode
                conn' <- Sqlite.open $ T.pack path
                rows <- query conn'
                    "SELECT derivation_prefix, change_addr_mode FROM seq_state ORDER BY rowid"
                rows `shouldBe`
                    [ [PersistText shelleyPrefix, PersistText "single_receiving"]
                    , [PersistText shelleyPrefix, PersistText "single_receiving"]
                    , [PersistText "byron", PersistText "increasing"]
                    , [PersistText shelleyPrefix, PersistText "corrupt"]
                    ]
                execute conn'
                    "UPDATE seq_state SET change_addr_mode = 'increasing' WHERE rowid = 1"
                Sqlite.close conn'
                runMigrations
                    (newMigrationInterface nullTracer)
                    path
                    migrateSingleAddressMode
                conn'' <- Sqlite.open $ T.pack path
                [[PersistText persistedOff]] <-
                    query conn'' "SELECT change_addr_mode FROM seq_state WHERE rowid = 1"
                persistedOff `shouldBe` "increasing"
                Sqlite.close conn''
          where
            shelleyPrefix = "2147485500/2147485463/2147483648"
-- | Test that the 'Store' can load the database after migration.
testCanLoadAfterMigration
    :: forall s
     . PersistAddressBook s
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
        Right a <-
            withSqliteContextFile
                nullTracer
                path
                noManualMigration
                noAutoMigrations
                action
        pure a

withCopiedFile :: FilePath -> (FilePath -> IO a) -> IO a
withCopiedFile orig action =
    snd <$> withinCopiedFile orig (\path _ -> action path)

query :: Sqlite.Connection -> T.Text -> IO [[PersistValue]]
query conn sql = do
    statement <- Sqlite.prepare conn sql
    let collect rows = Sqlite.step statement >>= \case
            Sqlite.Row -> Sqlite.columns statement >>= \row -> collect (row : rows)
            Sqlite.Done -> pure $ reverse rows
    rows <- collect []
    Sqlite.finalize statement
    pure rows

execute :: Sqlite.Connection -> T.Text -> IO ()
execute conn sql = do
    statement <- Sqlite.prepare conn sql
    void $ Sqlite.step statement
    Sqlite.finalize statement
