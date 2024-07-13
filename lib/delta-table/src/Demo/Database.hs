{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Demonstration for 'Table' type.

-}
module Demo.Database where

import Prelude

import Data.Foldable
    ( for_
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Database.Table
    ( Col (..)
    , Row
    , Table
    , (:.)
    )

import qualified Database.SQLite.Simple as Sqlite
import qualified Database.Table.SQLite.Simple as Sql

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
type TablePerson =
    Table "person"
        :. Col "name" Text
        :. Col "birthyear" Int

tablePerson :: Proxy TablePerson
tablePerson = Proxy

colName :: Col "name" Text
colName = Col

colBirthYear :: Col "birthyear" Int
colBirthYear = Col

action :: Sql.SqlM [Row TablePerson]
action = do
    Sql.createTable tablePerson
    Sql.insertOne ("Neko", 1603) tablePerson
    Sql.deleteAll tablePerson
    Sql.insertOne ("Babbage", 1791) tablePerson
    Sql.insertOne ("William", 1805) tablePerson
    Sql.insertOne ("Ada", 1815) tablePerson
    Sql.selectWhere
        (colName Sql./=. "William" Sql.&&. colBirthYear Sql.<. 1800)
        tablePerson

main :: IO ()
main = do
    rows <- Sqlite.withConnection ":memory:" $ Sql.runSqlM action
    for_ rows print
