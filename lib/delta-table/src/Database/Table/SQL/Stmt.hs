{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , selectWhere
    , insertOne
    , Update
    , (=.)
    , updateWhere
    , deleteAll
    , deleteWhere
    ) where

import Prelude

import Data.Text
    ( Text
    )
import Data.Traversable
    ( for
    )
import Database.Table
    ( Col
    , IsColumnName
    , IsTable (getTableName)
    , getColNames
    , getColumnName
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
import qualified Database.SQLite.Simple.ToField as Sqlite
import qualified Database.Table.SQL.Expr as Expr
import qualified Database.Table.SQL.Var as Var

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
    UpdateWhere :: TableName -> [Update] -> Where -> Stmt
    Delete :: TableName -> Where -> Stmt

-- | An SQL @WHERE@ clause.
data Where where
    All :: Where
    Where :: Expr.Expr Bool -> Where

{-------------------------------------------------------------------------------
    Rendering
-------------------------------------------------------------------------------}
-- | Render an statement as SQL source code.
renderStmt :: Stmt -> Var.Lets Text
renderStmt (CreateTable table cols) = Expr.text
    $ "CREATE TABLE IF NOT EXISTS "
        <> renderName table
        <> " " <> renderTuple (map renderCol cols)
        <> ";"
  where
    renderCol (col, typ)= renderName col <> " " <> escapeSqlType typ
renderStmt (Insert table cols) = Expr.text
    $ "INSERT INTO "
        <> renderName table
        <> " " <> renderTuple (map renderName cols)
        <> " VALUES " <> renderTuple ("?" <$ cols)
        <> ";"
renderStmt (Select cols table All) = Expr.text
    $ "SELECT " <> T.intercalate "," (map renderName cols)
        <> " FROM " <> renderName table
        <> ";"
renderStmt (Select cols table (Where expr)) =
    Expr.text ("SELECT " <> T.intercalate "," (map renderName cols))
        <> Expr.text (" FROM " <> renderName table)
        <> Expr.text " WHERE " <> Expr.renderExpr expr
        <> Expr.text ";"
renderStmt (Delete table All) = Expr.text
    $ "DELETE FROM " <> renderName table
renderStmt (Delete table (Where expr)) =
    Expr.text ("DELETE FROM " <> renderName table)
        <> Expr.text " WHERE " <> Expr.renderExpr expr
        <> Expr.text ";"
renderStmt (UpdateWhere table updates whereClause) =
    Expr.text ("UPDATE " <> renderName table)
        <> Expr.text " SET "
            <> (T.intercalate ", " <$> for updates renderUpdate)
        <> renderWhereClause whereClause
        <> Expr.text ";"
  where
    renderWhereClause All =
        mempty
    renderWhereClause (Where expr) =
        Expr.text " WHERE " <> Expr.renderExpr expr

renderUpdate :: Update -> Var.Lets Text
renderUpdate (Update{lhs,rhs}) =
    Expr.text (renderName lhs)
        <> Expr.text " = "
        <> (Var.bindValue rhs >>= Expr.var)

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

-- | Select those rows from the table that satisfy a condition.
selectWhere
    :: IsTableSql t
    => Expr.Expr Bool -> proxy t -> Stmt
selectWhere expr proxy =
    Select (getColNames proxy) (getTableName proxy) (Where expr)

-- | Insert one row into the corresponding table.
insertOne :: IsTableSql t => proxy t -> Stmt
insertOne proxy =
    Insert (getTableName proxy) (getColNames proxy)

-- | A column update.
data Update = Update
    { lhs :: ColumnName
    , rhs :: Var.Value
    }

-- | Set the value of a given column.
(=.)
    :: forall n a. (IsColumnName n, Sqlite.ToField a)
    => Col n a -> a -> Update
(=.) col a = Update
    { lhs = getColumnName col
    , rhs = Sqlite.toField a
    }

-- | Update those rows in a database table that satisfy a condition.
updateWhere
    :: IsTableSql t
    => Expr.Expr Bool -> [Update] -> proxy t -> Stmt
updateWhere expr updates proxy =
    UpdateWhere (getTableName proxy) updates (Where expr)

-- | Delete all rows from a database table
deleteAll :: IsTableSql t => proxy t -> Stmt
deleteAll proxy =
    Delete (getTableName proxy) All

-- | Delete those rows from a database table that satisfy a condition.
deleteWhere
    :: IsTableSql t
    => Expr.Expr Bool -> proxy t -> Stmt
deleteWhere expr proxy =
    Delete (getTableName proxy) (Where expr)
