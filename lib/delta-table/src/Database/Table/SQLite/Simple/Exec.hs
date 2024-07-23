{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Execute SQL statements for typed database tables.
-}
module Database.Table.SQLite.Simple.Exec
    (
    -- * SQL statements
      createTable
    , selectAll
    , selectWhere
    , insertOne
    , insertMany
    , deleteAll
    , deleteWhere
    , updateWhere
    , Update
    , (=.)
    ) where

import Prelude

import Data.Foldable
    ( for_
    )
import Database.Table
    ( Row
    )
import Database.Table.SQL.Stmt
    ( Update
    , (=.)
    )
import Database.Table.SQL.Table
    ( IsTableSql
    )
import Database.Table.SQLite.Simple.Monad
    ( SqlM
    , rawSqlite
    )

import qualified Data.Map.Strict as Map
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.Table.SQL.Expr as Expr
import qualified Database.Table.SQL.Stmt as Stmt
import qualified Database.Table.SQL.Var as Var

{-------------------------------------------------------------------------------
    Helpers
-------------------------------------------------------------------------------}
renderStmt :: Stmt.Stmt -> (Sqlite.Query, [Sqlite.NamedParam])
renderStmt stmt =
    ( Sqlite.Query (Var.val rendered)
    , toNamedParams (Var.bindings rendered)
    )
  where
    rendered = Var.render $ Stmt.renderStmt stmt

toNamedParams :: Var.Bindings -> [Sqlite.NamedParam]
toNamedParams bindings = do
    (k,v) <- Map.toList bindings
    [Var.renderVarName k Sqlite.:= v]

-- | Query that does not contain any variable bindings.
query_ :: Sqlite.FromRow row => Stmt.Stmt -> SqlM [row]
query_ stmt =
    rawSqlite $ \conn -> Sqlite.query_ conn (fst $ renderStmt stmt)

-- | Query with named variables.
queryNamed :: Sqlite.FromRow row => Stmt.Stmt -> SqlM [row]
queryNamed stmt =
    rawSqlite $ \conn ->
        let (query, bindings) = renderStmt stmt
        in  Sqlite.queryNamed conn query bindings

-- | Execution that does not contain any variable bindings.
execute_ :: Stmt.Stmt -> SqlM ()
execute_ stmt =
    rawSqlite $ \conn -> Sqlite.execute_ conn (fst $ renderStmt stmt)

-- | FIXME: Execution with '?' parameters
executeOne :: Sqlite.ToRow row => Stmt.Stmt -> row -> SqlM ()
executeOne stmt row =
    rawSqlite $ \conn -> Sqlite.execute conn (fst $ renderStmt stmt) row

-- | Execution with named variables.
executeNamed :: Stmt.Stmt -> SqlM ()
executeNamed stmt =
    rawSqlite $ \conn ->
        let (query, bindings) = renderStmt stmt
        in  Sqlite.executeNamed conn query bindings

{-------------------------------------------------------------------------------
    SQL statements
-------------------------------------------------------------------------------}
createTable :: IsTableSql t => proxy t -> SqlM ()
createTable = execute_ . Stmt.createTable

selectAll :: IsTableSql t => proxy t -> SqlM [Row t]
selectAll = query_ . Stmt.selectAll

selectWhere :: IsTableSql t => Expr.Expr Bool -> proxy t -> SqlM [Row t]
selectWhere expr = queryNamed . Stmt.selectWhere expr

insertOne :: IsTableSql t => Row t -> proxy t -> SqlM ()
insertOne row proxy = executeOne (Stmt.insertOne proxy) row

-- | Insert many rows into a database table.
--
-- As an optimization, we use a single prepared SQL statement.
insertMany :: IsTableSql t => [Row t] -> proxy t -> SqlM ()
insertMany rows proxy =
    rawSqlite $ \conn -> do
        -- The 'query' string contains '?' which will be substituted
        -- for the values in the row, but no named params.
        let (query, _noBindings) = renderStmt $ Stmt.insertOne proxy
        Sqlite.withStatement conn query $ \stmt ->
            for_ rows $ \row ->
                -- From <https://www.sqlite.org/cintro.html>:
                --   Each call to sqlite3_bind() overrides
                --   prior bindings on the same parameter.
                Sqlite.withBind stmt row (Sqlite.nextRow stmt)
                    :: IO (Maybe [Bool]) -- dummy type, we expect 'Nothing'

updateWhere
    :: IsTableSql t
    => Expr.Expr Bool -> [Stmt.Update] -> proxy t -> SqlM ()
updateWhere expr updates = executeNamed . Stmt.updateWhere expr updates

deleteAll :: IsTableSql t => proxy t -> SqlM ()
deleteAll = execute_ . Stmt.deleteAll

deleteWhere :: IsTableSql t => Expr.Expr Bool -> proxy t -> SqlM ()
deleteWhere expr = executeNamed . Stmt.deleteWhere expr
