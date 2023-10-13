{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Checkpoints.Migration
    ( migratePrologue
    ) where

import Prelude

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

import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

migratePrologue
    :: Migration (ReadDBHandle IO) 3 4
migratePrologue = mkMigration $ ReaderT $ \db -> void $ do
    let conn = dbConn db
    addColumnIfMissing conn True (DBField SeqStateChangeAddrMode) defaultVal
    addColumnIfMissing conn True (DBField SharedStateChangeAddrMode) defaultVal
  where
    defaultVal = "increasing"

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
        TableMissing -> fail $ T.unpack $ T.unwords
            [ headerFail
            , "Expected TABLE", tableName field
            , "to exist."
            ]
        ColumnMissing -> do
            query <- Sqlite.prepare conn $ T.unwords
                [ "ALTER TABLE", tableName field
                , "ADD COLUMN", fieldName field
                , fieldType field, if notNull then "NOT NULL" else ""
                , "DEFAULT", value
                , ";"
                ]
            _ <- Sqlite.step query
            Sqlite.finalize query
        ColumnPresent -> fail $ T.unpack $ T.unwords
            [ headerFail
            , "Expected COLUMN", fieldName field
            , "in TABLE", tableName field
            , "to not exist."
            ]
