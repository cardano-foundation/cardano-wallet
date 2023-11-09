{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Helper functions for enabling / disabling foreign keys
-- on an SQLite database.
--
-- Depends on "Database.Sqlite" from @persistent-sqlite@.
module Cardano.DB.Sqlite.ForeignKeys
    ( ForeignKeysSetting (..)
    , withForeignKeysDisabled
    ) where

import Prelude

import Control.Monad
    ( when
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Aeson
    ( ToJSON (..)
    )
import GHC.Generics
    ( Generic
    )
import UnliftIO.Exception
    ( bracket_
    )

import qualified Database.Persist.Sql as Persist
import qualified Database.Sqlite as Sqlite

{-------------------------------------------------------------------------------
    Foreign key settings
-------------------------------------------------------------------------------}

-- | Run the given task in a context where foreign key constraints are
--   /temporarily disabled/, before re-enabling them.
withForeignKeysDisabled
    :: Tracer IO ForeignKeysSetting
    -> Sqlite.Connection
    -> IO a
    -> IO a
withForeignKeysDisabled t c =
    bracket_
        (updateForeignKeysSetting t c ForeignKeysDisabled)
        (updateForeignKeysSetting t c ForeignKeysEnabled)

-- | Specifies whether or not foreign key constraints are enabled, equivalent
--   to the Sqlite 'foreign_keys' setting.
--
-- When foreign key constraints are /enabled/, the database will enforce
-- referential integrity, and cascading deletes are enabled.
--
-- When foreign keys constraints are /disabled/, the database will not enforce
-- referential integrity, and cascading deletes are disabled.
--
-- See the following resource for more information:
-- https://www.sqlite.org/foreignkeys.html#fk_enable
data ForeignKeysSetting
    = -- | Foreign key constraints are /enabled/.
      ForeignKeysEnabled
    | -- | Foreign key constraints are /disabled/.
      ForeignKeysDisabled
    deriving (Eq, Generic, ToJSON, Show)

-- | Read the current value of the Sqlite 'foreign_keys' setting.
readForeignKeysSetting :: Sqlite.Connection -> IO ForeignKeysSetting
readForeignKeysSetting connection = do
    query <- Sqlite.prepare connection "PRAGMA foreign_keys"
    state <- Sqlite.step query >> Sqlite.columns query
    Sqlite.finalize query
    case state of
        [Persist.PersistInt64 0] -> pure ForeignKeysDisabled
        [Persist.PersistInt64 1] -> pure ForeignKeysEnabled
        unexpectedValue ->
            error
                $ mconcat
                    [ "Unexpected result when querying the current value of "
                    , "the Sqlite 'foreign_keys' setting: "
                    , show unexpectedValue
                    , "."
                    ]

-- | Update the current value of the Sqlite 'foreign_keys' setting.
updateForeignKeysSetting
    :: Tracer IO ForeignKeysSetting
    -> Sqlite.Connection
    -> ForeignKeysSetting
    -> IO ()
updateForeignKeysSetting trace connection desiredValue = do
    traceWith trace desiredValue
    query <-
        Sqlite.prepare connection
            $ "PRAGMA foreign_keys = " <> valueToWrite <> ";"
    _ <- Sqlite.step query
    Sqlite.finalize query
    finalValue <- readForeignKeysSetting connection
    when (desiredValue /= finalValue)
        $ error
        $ mconcat
            [ "Unexpected error when updating the value of the Sqlite "
            , "'foreign_keys' setting. Attempted to write the value "
            , show desiredValue
            , " but retrieved the final value "
            , show finalValue
            , "."
            ]
  where
    valueToWrite = case desiredValue of
        ForeignKeysEnabled -> "ON"
        ForeignKeysDisabled -> "OFF"
