{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

'Table' represents the schema for a database table.

-}
module Database.Table.Schema
    (
    -- * Table types
    IsTable (..)
    , getColNames
    , ExampleTable
    , Table (..)
    , Col (..)
    , (:.) (..)

    , IsColumnName
    , getColumnName

    -- * Rows
    , Row
    , Only (..)
    , exampleRow
    ) where

import Prelude

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
import Data.Text
    ( Text
    )
import Data.Tuple.Only
    ( Only (..)
    )
import GHC.TypeLits
    ( KnownSymbol
    , Symbol
    , symbolVal
    )

import qualified Data.Text as T

{-------------------------------------------------------------------------------
    Class
-------------------------------------------------------------------------------}
-- | Class of named tables with named columns.
--
-- The data contained in the table is essentially a list of rows
-- with the given column names.
class IsTable t where
    getTableName :: proxy t -> Text
    getColNamesSeq :: proxy t -> Seq Text

getColNames :: IsTable t => proxy t -> [Text]
getColNames = toList . getColNamesSeq

{-------------------------------------------------------------------------------
    Type
-------------------------------------------------------------------------------}
-- | Infix notation for a pair of types.
data a :. b = a :. b
    deriving (Eq,Ord,Show,Read)
infixl 3 :.

-- | Named database column.
data Col (name :: Symbol) a = Col
    deriving (Eq,Ord,Show)

-- | Constraint synonym for 'getColName'.
type IsColumnName (name :: Symbol) = KnownSymbol name

-- | Get the name of a column from its type.
getColumnName :: forall name a. IsColumnName name => Col name a -> Text
getColumnName _ = T.pack $ symbolVal (Proxy :: Proxy name)

-- | Named database table.
data Table (name :: Symbol) = Table
    deriving (Eq,Ord,Show)

instance KnownSymbol name => IsTable (Table name) where
    getTableName _ = T.pack $ symbolVal (Proxy :: Proxy name)
    getColNamesSeq  _ = empty

instance (IsTable t, KnownSymbol name) => IsTable (t :. Col name a) where
    getTableName _ = getTableName (Proxy :: Proxy t)
    getColNamesSeq  _ =
        getColNamesSeq (Proxy :: Proxy t)
        |> T.pack (symbolVal (Proxy :: Proxy name))

-- | Example 'Table' type.
type ExampleTable =
    Table "person"
        :. Col "name" Text
        :. Col "birthyear" Int

-- | > exampleRow = ("Ada Lovelace", 1815)
exampleRow :: Row ExampleTable
exampleRow = ("Ada Lovelace", 1815)

{-------------------------------------------------------------------------------
    Columns
-------------------------------------------------------------------------------}
-- | Type family
-- that maps a table schema @t@ (which ideally satisfies @IsTable t@)
-- to a type representing rows of that table.
type family Row t

type instance Row (Table n0 :. Col n1 a1) =
    Only a1

type instance Row (Table n0 :. Col n1 a1 :. Col n2 a2) =
    (a1, a2)

type instance Row (Table n0 :. Col n1 a1 :. Col n2 a2 :. Col n3 a3) =
    (a1, a2, a3)

type instance
    Row (Table n0
            :. Col n1 a1
            :. Col n2 a2
            :. Col n3 a3
            :. Col n4 a4
        ) =
        (a1, a2, a3, a4)

type instance
    Row (Table n0
            :. Col n1 a1
            :. Col n2 a2
            :. Col n3 a3
            :. Col n4 a4
            :. Col n5 a5
        ) =
        (a1, a2, a3, a4, a5)

type instance
    Row (Table n0
            :. Col n1 a1
            :. Col n2 a2
            :. Col n3 a3
            :. Col n4 a4
            :. Col n5 a5
            :. Col n6 a6
        ) =
        (a1, a2, a3, a4, a5, a6)
