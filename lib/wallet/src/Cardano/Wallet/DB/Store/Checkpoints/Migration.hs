{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Checkpoints.Migration
    ( migratePrologue
    , migrateSingleAddressMode
    ) where

import Cardano.DB.Sqlite
    ( ReadDBHandle
    , dbConn
    )
import Cardano.DB.Sqlite.Migration.Old
    ( DBField (..)
    , fieldName
    , fieldType
    , tableName
    )
import Cardano.Wallet.DB.Migration
    ( Migration
    , mkMigration
    )
import Control.Exception
    ( onException
    )
import Cardano.Wallet.DB.Sqlite.Migration.Old
    ( SqlColumnStatus (..)
    , isFieldPresent
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..)
    )
import Control.Monad
    ( void
    )
import Control.Monad.Trans.Reader
    ( ReaderT (..)
    )
import Data.Text
    ( Text
    )
import Prelude

import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

migratePrologue
    :: Migration (ReadDBHandle IO) 3 4
migratePrologue = mkMigration $ ReaderT $ \db -> void $ do
    let conn = dbConn db
    addColumnIfMissing
        conn
        True
        (DBField SeqStateChangeAddrMode)
        defaultVal
    addColumnIfMissing
        conn
        True
        (DBField SharedStateChangeAddrMode)
        defaultVal
  where
    defaultVal = "increasing"

migrateSingleAddressMode :: Migration (ReadDBHandle IO) 6 7
migrateSingleAddressMode = mkMigration $ ReaderT $ \db -> do
    let conn = dbConn db
    execute conn "BEGIN IMMEDIATE"
    let migration = do
            isFieldPresent conn (DBField SeqStateChangeAddrMode) >>= \case
                TableMissing -> pure ()
                ColumnMissing ->
                    fail "Expected seq_state.change_addr_mode to exist."
                ColumnPresent -> execute conn
                    "UPDATE seq_state \
                    \SET change_addr_mode = 'single_receiving' \
                    \WHERE derivation_prefix = '2147485500/2147485463/2147483648' \
                    \AND change_addr_mode IN ('single', 'increasing')"
            execute conn
                "UPDATE database_schema_version SET version = 7 WHERE name = 'schema'"
            execute conn "COMMIT"
    migration `onException` execute conn "ROLLBACK"

headerFail :: Text
headerFail = "Database migration from version 3 to version 4 failed:"

addColumnIfMissing
    :: Sqlite.Connection
    -> Bool
    -> DBField
    -> Text
    -> IO ()
addColumnIfMissing conn notNull field value = do
    isFieldPresent conn field >>= \case
        TableMissing ->
            fail
                $ T.unpack
                $ T.unwords
                    [ headerFail
                    , "Expected TABLE"
                    , tableName field
                    , "to exist."
                    ]
        ColumnMissing -> do
            query <-
                Sqlite.prepare conn
                    $ T.unwords
                        [ "ALTER TABLE"
                        , tableName field
                        , "ADD COLUMN"
                        , fieldName field
                        , fieldType field
                        , if notNull then "NOT NULL" else ""
                        , "DEFAULT"
                        , value
                        , ";"
                        ]
            _ <- Sqlite.step query
            Sqlite.finalize query
        ColumnPresent ->
            fail
                $ T.unpack
                $ T.unwords
                    [ headerFail
                    , "Expected COLUMN"
                    , fieldName field
                    , "in TABLE"
                    , tableName field
                    , "to not exist."
                    ]

execute :: Sqlite.Connection -> Text -> IO ()
execute conn sql = do
    statement <- Sqlite.prepare conn sql
    void $ Sqlite.step statement
    Sqlite.finalize statement
