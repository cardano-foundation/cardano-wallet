{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

SQL statements for typed database tables.
-}
module Database.Table.SQL.Stmt
    (
    -- * SQL statements
    Stmt
    , renderStmt

    -- * Specific statements
    , createTable
    , selectAll
    , insertOne
    , deleteAll
    ) where

import Prelude

import Data.Text
    ( Text
    )
import Database.Table
    ( IsTable (getTableName)
    , getColNames
    , getTableName
    )
import Database.Table.SQL.Column
    ( SqlType
    , escapeSqlType
    )
import Database.Table.SQL.Table
    ( IsTableSql
    , getColumnTypes
    )

import qualified Data.Text as T

{-------------------------------------------------------------------------------
    SQL Statements
-------------------------------------------------------------------------------}
type TableName = Text
type ColumnName = Text

-- | A subset of SQL statements.
data Stmt where
    CreateTable :: TableName -> [(ColumnName, SqlType)] -> Stmt
    Select :: [ColumnName] -> TableName -> Where -> Stmt
    Insert :: TableName -> [ColumnName] -> Stmt
    Delete :: TableName -> Where -> Stmt

-- | An SQL @WHERE@ clause.
data Where where
    All :: Where

{-------------------------------------------------------------------------------
    Rendering
-------------------------------------------------------------------------------}
-- | Render an statement as SQL source code.
renderStmt :: Stmt -> Text
renderStmt (CreateTable table cols) =
    "CREATE TABLE IF NOT EXISTS "
        <> renderName table
        <> " " <> renderTuple (map renderCol cols)
        <> ";"
  where
    renderCol (col, typ)= renderName col <> " " <> escapeSqlType typ
renderStmt (Insert table cols) =
    "INSERT INTO "
        <> renderName table
        <> " " <> renderTuple (map renderName cols)
        <> " VALUES " <> renderTuple ("?" <$ cols)
        <> ";"
renderStmt (Select cols table All) =
    "SELECT " <> T.intercalate "," (map renderName cols)
        <> " FROM " <> renderName table
        <> ";"
renderStmt (Delete table All) =
    "DELETE FROM " <> renderName table

-- | Escape a column or table name.
renderName :: Text -> Text
renderName s = "\"" <> s <> "\""

-- | Render a tuple
renderTuple :: [Text] -> Text
renderTuple xs = "(" <> T.intercalate ", " xs <> ")"

{-------------------------------------------------------------------------------
    SQL queries
-------------------------------------------------------------------------------}

-- | Create a database table.
createTable :: IsTableSql t => proxy t -> Stmt
createTable proxy =
    CreateTable
        (getTableName proxy)
        (zip (getColNames proxy) (getColumnTypes proxy))

-- | Select all rows from the table.
selectAll :: IsTableSql t => proxy t -> Stmt
selectAll proxy =
    Select (getColNames proxy) (getTableName proxy) All

-- | Insert one row into the corresponding table.
insertOne :: IsTableSql t => proxy t -> Stmt
insertOne proxy =
    Insert (getTableName proxy) (getColNames proxy)

-- | Delete all rows from a database table
deleteAll :: IsTableSql t => proxy t -> Stmt
deleteAll proxy =
    Delete (getTableName proxy) All
