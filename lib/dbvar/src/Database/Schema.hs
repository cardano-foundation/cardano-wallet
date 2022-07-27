{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Schema (
    -- * Synopsis
    -- | Typed database tables and rows.

    -- * Database columns
      IsCol (..), SqlColType (..)
    , toColType
    , Primary (..)

    -- * Database rows and tables
    , Table (..), Col (..), (:.) (..)
    , IsRow (..)

    -- * SQL Queries
    , Query, callSql, runSql
    , createTable, selectAll, insertOne, repsertOne, updateOne, deleteAll, deleteOne

    -- * Testing
    , testPerson
    ) where

import Prelude

import Control.Monad.IO.Class
    ( MonadIO )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Database.Persist
    ( PersistField (..), PersistValue )
import Database.Persist.Sql
    ( PersistFieldSql (..), RawSql (..), SqlPersistT, SqlType (..) )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )

import qualified Data.Text as T
import qualified Database.Persist.Sql as Persist

{-------------------------------------------------------------------------------
    Types for database columns
-------------------------------------------------------------------------------}
-- | Primary key.
newtype Primary = Primary { getPrimary :: Int }
    deriving (Eq,Ord,Show)

-- | SQL column types, including constraints.
-- Values of type 'SqlColType' represent SQL types such as
--
-- > INTEGER  PRIMARY KEY NOT NULL
-- > TEXT  NOT NULL
newtype SqlColType = SqlColType Text
    deriving (Eq,Ord,Show)

-- | Helper for converting 'SqlType' into an SQL column type with constraints.
toColType :: Persist.SqlType -> SqlColType
toColType SqlString = SqlColType "TEXT"
toColType SqlInt32  = SqlColType "INTEGER"
toColType SqlInt64  = SqlColType "INTEGER"
toColType x = error $ "toColType: case not implemented: " <> show x

escapeSqlType :: SqlColType -> Text
escapeSqlType (SqlColType x) = x

-- | Class of columns that can be stored in database tables.
class PersistField a => IsCol a where
    getSqlType :: Proxy a -> SqlColType

instance {-# OVERLAPPABLE #-} (PersistField a, PersistFieldSql a) => IsCol a where
    getSqlType = toColType . sqlType

instance PersistField Primary where
    toPersistValue = toPersistValue . getPrimary
    fromPersistValue = fmap Primary . fromPersistValue

instance IsCol Primary where
    getSqlType _ = SqlColType "INTEGER NOT NULL PRIMARY KEY"

instance IsCol Int where
    getSqlType _ = SqlColType "INTEGER NOT NULL"

instance IsCol (Maybe Int) where
    getSqlType _ = SqlColType "INTEGER"

{-------------------------------------------------------------------------------
    Types for database tables and rows
-------------------------------------------------------------------------------}
-- | Infix notation for a pair of types.
data a :. b = a :. b
    deriving (Eq,Ord,Show,Read)
infixl 3 :.

-- | Named database column.
newtype Col (name :: Symbol) a = Col a
    deriving (Eq,Ord,Show)

-- | Named database table.
data Table (name :: Symbol) = Table
    deriving (Eq,Ord,Show)

-- | Class of row types that can be stored in database tables.
-- Instances of this class are essentially lists of columns.
-- Example:
--
-- > type PersonRow = Table "person" :. Col "name" Text :. Col "age" Int
class IsRow row where
    getTableName :: Proxy row -> Text
    getColNames :: Proxy row -> [Text]
    getSqlTypes :: Proxy row -> [SqlColType]

    toSqlValues   :: row -> [PersistValue]
    fromSqlValues :: [PersistValue] -> Either Text row

instance KnownSymbol name => IsRow (Table name) where
    getTableName _ = T.pack $ symbolVal (Proxy :: Proxy name)
    getColNames  _ = []
    getSqlTypes  _ = []

    toSqlValues  _ = []
    fromSqlValues [] = Right Table
    fromSqlValues _  = Left "Table should contain zero rows"

-- FIXME: O(n^2) when getting the values!
instance (IsRow row, KnownSymbol name, IsCol a)
    => IsRow (row :. Col name a)
  where
    getTableName _ = getTableName (Proxy :: Proxy row)
    getColNames  _ = getColNames (Proxy :: Proxy row)
        ++ [T.pack $ symbolVal (Proxy :: Proxy name)]
    getSqlTypes  _ = getSqlTypes (Proxy :: Proxy row)
        ++ [getSqlType (Proxy :: Proxy a)]

    toSqlValues (cs :. Col a) = toSqlValues cs ++ [toPersistValue a]

    fromSqlValues xs = case xs of
        [] -> Left $ "Expected column " <> colname
        _  -> case fromSqlValues (init xs) of
            Left e   -> Left e
            Right cs -> case fromPersistValue (last xs) of
                Left e  -> Left $ "Column " <> colname <> ": " <> e
                Right c -> Right $ cs :. Col c
      where
        colname = T.pack $ symbolVal (Proxy :: Proxy name)

{-------------------------------------------------------------------------------
    Types test
-------------------------------------------------------------------------------}
type PersonRow = Table "person"
    :. Col "name" Text :. Col "birth" Int :. Col "id" Primary

testPerson :: PersonRow
testPerson = Table :. Col "Ada Lovelace" :. Col 1815 :. Col (Primary 0)

{-------------------------------------------------------------------------------
    Connect with Persistent
-------------------------------------------------------------------------------}
newtype Wrap a = Wrap { unWrap :: a }

instance IsRow row => RawSql (Wrap row) where
    rawSqlCols _ _ = (length n, [])
      where n = getColNames (Proxy :: Proxy row)
    rawSqlColCountReason _ = T.unpack $
        "Table " <> getTableName proxy <> " has columns "
        <> mkTuple (getColNames proxy)
      where proxy = Proxy :: Proxy row
    rawSqlProcessRow = fmap Wrap . fromSqlValues

-- | Run an SQL query and return a list of rows as result.
callSql :: (MonadIO m, IsRow row)
    => Query row
    -> SqlPersistT m [row]
callSql Query{stmt,params} = map unWrap <$> Persist.rawSql stmt params

-- | Execute an SQL query, but do not return any results
runSql :: MonadIO m => Query () -> SqlPersistT m ()
runSql Query{stmt,params} = Persist.rawExecute stmt params

{-------------------------------------------------------------------------------
    SQL queries
-------------------------------------------------------------------------------}
-- | An SQL query that returns a list of values of type @row@.
data Query row = Query
    { stmt :: Text
    -- ^ SQL statement containing placeholders \"?\" which are
    -- replaced by the parameters
    , params :: [PersistValue]
    -- ^ Parameters to insert into the SQL statement.
    } deriving (Eq, Show)

-- | Escape a column or table name.
--
-- FIXME: Use a newtype for more type safety.
-- 'Query' used to be this newtype, but that has changed
-- due to the 'params' field.
escape :: Text -> Text
escape s = "\"" <> s <> "\""

-- | Helper for making a syntactically correct SQL tuple.
mkTuple :: [Text] -> Text
mkTuple xs = "(" <> T.intercalate ", " xs <> ")"

-- | Create a database table that can store the given rows.
createTable :: IsRow row => Proxy row -> Query ()
createTable proxy = Query
    { stmt =
        "CREATE TABLE IF NOT EXISTS " <> table
        <> " " <> mkTuple cols <> ";"
    , params = []
    }
  where
    table = escape $ getTableName proxy
    cols  = zipWith (\name typ -> escape name <> " " <> escapeSqlType typ)
        (getColNames proxy) (getSqlTypes proxy)

-- | Select all rows from the table.
selectAll :: forall row. IsRow row => Query row
selectAll = Query
    { stmt = "SELECT " <> T.intercalate "," cols <> " FROM " <> table <> ";"
    , params = []
    }
  where
    proxy = Proxy :: Proxy row
    table = escape $ getTableName proxy
    cols  = map escape $ getColNames proxy

-- | Insert a single row into the corresponding table.
insertOne :: forall row. IsRow row => row -> Query ()
insertOne row = Query
    { stmt =
        "INSERT INTO " <> table <> " " <> mkTuple cols
        <> " VALUES " <> mkTuple ("?" <$ cols) <> ";"
    , params = toSqlValues row
    }
  where
    proxy = Proxy :: Proxy row
    table = escape $ getTableName proxy
    cols  = map escape $ getColNames proxy

-- | Replace or insert a single row with a primary key into a database.
--
-- FIXME: It would be nicer if the "id" column was the first column
-- instead of the last column in the table.
repsertOne :: forall row. IsRow row
    => (row :. Col "id" Primary) -> Query ()
repsertOne row = Query
    { stmt =
        "INSERT OR REPLACE INTO " <> table <> " " <> mkTuple cols
        <> " VALUES " <> mkTuple ("?" <$ cols) <> ";"
    , params = toSqlValues row
    }
  where
    proxy = Proxy :: Proxy (row :. Col "id" Primary)
    table = escape $ getTableName proxy
    cols  = map escape $ getColNames proxy

-- | Update one row with a given \"id\" column in a database table.
updateOne :: forall row. IsRow row
    => (row :. Col "id" Primary) -> Query ()
updateOne row= Query
    { stmt = "UPDATE " <> table <> " SET " <> sets <> " WHERE \"id\"=?;"
    , params = toSqlValues row
    }
  where
    proxy = Proxy :: Proxy row
    table = escape $ getTableName proxy
    cols  = map escape $ getColNames proxy
    sets  = T.intercalate ", " [col <> "=?" | col <- cols]

-- | Delete one row with a given \"id\" column in a database table.
deleteOne :: forall row. IsRow row => Proxy row -> Col "id" Primary -> Query ()
deleteOne proxy (Col key) = Query
    { stmt = "DELETE FROM " <> table <> " WHERE \"id\"=?;"
    , params = [Persist.toPersistValue key]
    }
  where
    table = escape $ getTableName proxy

-- | Delete all rows in a database table
deleteAll :: forall row. IsRow row => Proxy row -> Query ()
deleteAll proxy = Query
    { stmt = "DELETE FROM " <> table
    , params = []
    }
  where
    table = escape $ getTableName proxy
