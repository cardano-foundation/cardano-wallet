-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- The 'Table' data type represents a database table
-- in a relational database.
-- This type
--
-- * tracks table and column names on the type level, and
-- * tracks Haskell types for table columns.
--
-- However, this type is /not/ yet specific to
-- SQL or a particular database backend.
module Database.Table
    ( module Database.Table.Schema
    ) where

import Database.Table.Schema
