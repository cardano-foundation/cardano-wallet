{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- 'Table' types that can be mapped to SQL tables.
module Database.Table.SQL.Table
    ( -- * SQL tables
      IsTableSql
    , getColumnTypes
    , HasColumns
    ) where

import Data.Foldable
    ( toList
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Sequence
    ( Seq
    , empty
    , (|>)
    )
import Database.Table
    ( Col
    , IsTable
    , Row
    , Table
    , (:.)
    )
import Database.Table.SQL.Column
    ( IsColumn (getSqlType)
    , SqlType
    )
import Prelude

import qualified Database.SQLite.Simple as Sqlite

{-------------------------------------------------------------------------------
    Types for database tables
-------------------------------------------------------------------------------}

-- | Constaint for types that represent tables with columns that can be
-- mapped to SQL tables.
--
-- Note: Usage sites of this constraint synonym must
-- use the @FlexibleContexts@ extension.
--
-- Typical instances of this constraint constructed from "Database.Table",
-- where the column types can be mapped from Haskell to SQL and vice-versa.
-- Example:
--
-- @
-- type ExampleTable =
--     Table "person"
--         :. Col "name" Text
--         :. Col "birthyear" Int
-- @
type IsTableSql t =
    ( IsTable t
    , HasColumns t
    , Sqlite.ToRow (Row t)
    , Sqlite.FromRow (Row t)
    )

getColumnTypes :: IsTableSql t => proxy t -> [SqlType]
getColumnTypes = toList . getColumnTypesD

-- | Class for types that correspond to a list of SQL types.
class HasColumns t where
    getColumnTypesD :: proxy t -> Seq SqlType

instance HasColumns (Table name) where
    getColumnTypesD _ = empty

instance (HasColumns t, IsColumn a) => HasColumns (t :. Col name a) where
    getColumnTypesD _ =
        getColumnTypesD (Proxy :: Proxy t)
            |> getSqlType (Proxy :: Proxy a)
