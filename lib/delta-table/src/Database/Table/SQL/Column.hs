{-# LANGUAGE FlexibleInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

SQL column types.
-}
module Database.Table.SQL.Column
    (
    -- * SQL column types
      SqlType
    , escapeSqlType

    -- * Haskell types to SQL column types
    , IsColumn (..)
    ) where

import Prelude

import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )

import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite

{-------------------------------------------------------------------------------
    Types for database columns
-------------------------------------------------------------------------------}
-- | SQL column types, including constraints.
-- Example values:
--
-- > INTEGER  PRIMARY KEY NOT NULL
-- > TEXT  NOT NULL
newtype SqlType = SqlType Text
    deriving (Eq,Ord,Show)

escapeSqlType :: SqlType -> Text
escapeSqlType (SqlType x) = x

-- | Class that maps a Haskell type to a column type of an SQL database.
-- Includes marshalling (via the `sqlite-simple` package).
class (Sqlite.ToField a, Sqlite.FromField a) => IsColumn a where
    getSqlType :: proxy a -> SqlType

instance IsColumn Int where
    getSqlType _ = SqlType "INTEGER NOT NULL"

instance IsColumn (Maybe Int) where
    getSqlType _ = SqlType "INTEGER"

instance IsColumn Text where
    getSqlType _ = SqlType "TEXT NOT NULL"

instance IsColumn (Maybe Text) where
    getSqlType _ = SqlType "TEXT"

instance IsColumn String where
    getSqlType _ = SqlType "TEXT NOT NULL"

instance IsColumn (Maybe String) where
    getSqlType _ = SqlType "TEXT"

instance IsColumn ByteString where
    getSqlType _ = SqlType "BLOB NOT NULL"

instance IsColumn (Maybe ByteString) where
    getSqlType _ = SqlType "BLOB"
