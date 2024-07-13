{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Execute SQL statements for typed database tables.
-}
module Database.Table.SQLite.Simple.Exec
    (
    -- * SQL monad
    SqlM
    , runSqlM

    -- * SQL statements
    , createTable
    , selectAll
    , insertOne
    , insertMany
    , deleteAll
    ) where

import Prelude

import Control.Monad.Trans.Reader
    ( ReaderT (..)
    )
import Data.Foldable
    ( for_
    )
import Database.Table
    ( Row
    )
import Database.Table.SQL.Table
    ( IsTableSql
    )

import qualified Database.SQLite.Simple as Sqlite
import qualified Database.Table.SQL.Stmt as Stmt

{-------------------------------------------------------------------------------
    SQL monad
-------------------------------------------------------------------------------}
-- | Monad to run SQL queries in.
--
-- This monad includes effects such as
--
-- * mutable state
-- * exceptions
-- * concurrency
--
-- This type makes no attempt at handling these, you have to do that yourself.
-- For example, in order to handle exception, consider using
-- 'Sqlite.withTransaction'.
--
-- FIXME: No, we do have handle these types of things.
type SqlM = ReaderT Sqlite.Connection IO

-- | Run a computation from the 'SqlM' monad.
runSqlM :: SqlM a -> Sqlite.Connection -> IO a
runSqlM = runReaderT

{-------------------------------------------------------------------------------
    Helpers
-------------------------------------------------------------------------------}
mkQuery :: Stmt.Stmt -> Sqlite.Query
mkQuery = Sqlite.Query . Stmt.renderStmt

query_ :: Sqlite.FromRow row => Stmt.Stmt -> SqlM [row]
query_ stmt =
    ReaderT $ \conn -> Sqlite.query_ conn (mkQuery stmt)

executeOne :: Sqlite.ToRow row => Stmt.Stmt -> row -> SqlM ()
executeOne stmt row =
    ReaderT $ \conn -> Sqlite.execute conn (mkQuery stmt) row

execute_ :: Stmt.Stmt -> SqlM ()
execute_ stmt =
    ReaderT $ \conn -> Sqlite.execute_ conn (mkQuery stmt)

{-------------------------------------------------------------------------------
    SQL statements
-------------------------------------------------------------------------------}
createTable :: IsTableSql t => proxy t -> SqlM ()
createTable = execute_ . Stmt.createTable

selectAll :: IsTableSql t => proxy t -> SqlM [Row t]
selectAll = query_ . Stmt.selectAll

insertOne :: IsTableSql t => Row t -> proxy t -> SqlM ()
insertOne row proxy = executeOne (Stmt.insertOne proxy) row

insertMany :: IsTableSql t => [Row t] -> proxy t -> SqlM ()
insertMany rows proxy = for_ rows (`insertOne` proxy)

deleteAll :: IsTableSql t => proxy t -> SqlM ()
deleteAll = execute_ . Stmt.deleteAll
