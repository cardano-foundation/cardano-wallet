{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Old-style manual migrations.
--
-- Depends on "Database.Sqlite" from @persistent-sqlite@.
module Cardano.DB.Sqlite.Migration.Old
    ( -- * Old-style Manual Migration
      ManualMigration (..)
    , noManualMigration
    , foldMigrations

    , MigrationError (..)
    , MatchMigrationError (..)

    -- * Migration helpers
    , DBField (..)
    , tableName
    , fieldName
    , fieldType
    ) where

import Prelude

import Control.Exception
    ( Exception
    )
import Data.Aeson
    ( ToJSON (..)
    )
import Data.List
    ( isInfixOf
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Database.Persist.EntityDef
    ( getEntityDBName
    )
import Database.Persist.Sql
    ( EntityField
    , PersistEntity (..)
    , PersistException
    , SqlType (..)
    , fieldDB
    , fieldSqlType
    , unEntityNameDB
    , unFieldNameDB
    )
import Database.Sqlite
    ( Error (ErrorConstraint)
    , SqliteException (SqliteException)
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}

-- | Encapsulates an old-style manual migration action
--   (or sequence of actions) to be
--   performed immediately after an SQL connection is initiated.
newtype ManualMigration = ManualMigration
    {executeManualMigration :: Sqlite.Connection -> IO ()}

noManualMigration :: ManualMigration
noManualMigration = ManualMigration $ const $ pure ()

foldMigrations :: [Sqlite.Connection -> IO ()] -> ManualMigration
foldMigrations ms = ManualMigration $ \conn -> mapM_ ($ conn) ms

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}

data DBField where
    DBField
        :: forall record typ
         . (PersistEntity record)
        => EntityField record typ
        -> DBField

tableName :: DBField -> Text
tableName (DBField (_ :: EntityField record typ)) =
    unEntityNameDB $ getEntityDBName $ entityDef (Proxy :: Proxy record)

fieldName :: DBField -> Text
fieldName (DBField field) =
    unFieldNameDB $ fieldDB $ persistFieldDef field

fieldType :: DBField -> Text
fieldType (DBField field) =
    showSqlType $ fieldSqlType $ persistFieldDef field

showSqlType :: SqlType -> Text
showSqlType = \case
    SqlString -> "VARCHAR"
    SqlInt32 -> "INTEGER"
    SqlInt64 -> "INTEGER"
    SqlReal -> "REAL"
    SqlDay -> "DATE"
    SqlTime -> "TIME"
    SqlDayTime -> "TIMESTAMP"
    SqlBlob -> "BLOB"
    SqlBool -> "BOOLEAN"
    SqlOther t -> t
    SqlNumeric precision scale ->
        T.concat
            [ "NUMERIC("
            , T.pack (show precision)
            , ","
            , T.pack (show scale)
            , ")"
            ]

instance Show DBField where
    show field = T.unpack (tableName field <> "." <> fieldName field)

instance Eq DBField where
    field0 == field1 = show field0 == show field1

instance ToJSON DBField where
    toJSON = Aeson.String . T.pack . show

{-----------------------------------------------------------------------------
    Errors
------------------------------------------------------------------------------}

-- | Error type for when migrations go wrong after opening a database.
newtype MigrationError = MigrationError
    {getMigrationErrorMessage :: Text}
    deriving (Show, Eq, Generic, ToJSON)

instance Exception MigrationError

class Exception e => MatchMigrationError e where
    -- | Exception predicate for migration errors.
    matchMigrationError :: e -> Maybe MigrationError

instance MatchMigrationError PersistException where
    matchMigrationError e
        | mark `isInfixOf` msg = Just $ MigrationError $ T.pack msg
        | otherwise = Nothing
      where
        msg = show e
        mark = "Database migration: manual intervention required."

instance MatchMigrationError SqliteException where
    matchMigrationError (SqliteException ErrorConstraint _ msg) =
        Just $ MigrationError msg
    matchMigrationError _ =
        Nothing
