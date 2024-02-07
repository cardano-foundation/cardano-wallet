{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Delegations.Migrations.V5.Migration
    ( migrateDelegations
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
import Cardano.Wallet.DB.Store.Delegations.Schema
    ( EntityField (..)
    )
import Control.Monad
    ( void
    )
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Data.Text
    ( Text
    )

import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

migrateDelegations :: Migration (ReadDBHandle IO) 4 5
migrateDelegations = mkMigration $ ReaderT $ \db -> void $ do
    let conn = dbConn db
    addColumnIfMissing conn (DBField DelegationVote)

headerFail :: Text
headerFail = "Database migration from version 4 to version 5 failed:"

addColumnIfMissing
    :: Sqlite.Connection
    -> DBField
    -> IO ()
addColumnIfMissing conn field = do
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
                , fieldType field , "NULL"
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
