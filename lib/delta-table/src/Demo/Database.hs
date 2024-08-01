{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Demonstration for 'Table' type.

-}
module Demo.Database where

import Prelude

import Control.Exception
    ( SomeException
    , catch
    )
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
    Sql.deleteWhere (colName Sql.==. "Neko") tablePerson
    Sql.insertOne ("Babbage", 1791) tablePerson
    Sql.insertOne ("William", 1805) tablePerson
    Sql.insertOne ("Bada", 1815) tablePerson
    Sql.updateWhere
        (colName Sql.==. "Bada")
        [colName Sql.=. "Ada"]
        tablePerson
    Sql.selectWhere
        (colName Sql./=. "William" Sql.&&. colBirthYear Sql.>. 1800)
        tablePerson

main :: IO ()
main = do
    rows <- Sql.withConnection ":memory:" $ Sql.runSqlM action
    for_ rows print

testExceptions :: IO ()
testExceptions = do
    Sql.withConnection ":memory:" $ \conn -> do
        _ <- Sql.runSqlM action conn
        putStrLn "Before"
        printTablePerson conn
        Sql.runSqlM
            ( do
                Sql.deleteWhere (colBirthYear Sql.>. 1800) tablePerson
                error "oops"
            ) conn `catch` (\(_ :: SomeException) -> pure ())
        putStrLn "After"
        printTablePerson conn
  where
    printTablePerson conn =
        mapM_ print =<< Sql.runSqlM (Sql.selectAll tablePerson) conn
