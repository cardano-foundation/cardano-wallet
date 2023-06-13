{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Delegations.MigrationSpec where

import Prelude

import Cardano.Wallet.DB.LayerSpec
    ( withinCopiedFile )
import Cardano.Wallet.DB.Migration
    ( runMigrations )
import Cardano.Wallet.DB.Sqlite.Migration.New
    ( newMigrationInterface )
import Cardano.Wallet.DB.Store.Delegations.Migration
    ( migrateDelegations )
import Control.Tracer
    ( nullTracer )
import Data.String.Interpolate
    ( i )
import Data.Text
    ( Text )
import Database.Persist.Sql
    ( Single (Single) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite

spec :: Spec
spec =
    describe "Delegations table migration"
        $ it "'migrate' db new delegation table"
        $ testMigrationDelegationsTable
            "before_new_delegation.sqlite"

testMigrationDelegationsTable :: FilePath -> IO ()
testMigrationDelegationsTable dbName = do
    let performMigrations path =
            runMigrations (newMigrationInterface nullTracer)
                path migrateDelegations
        testOnCopiedAndMigrated test = fmap snd
            $ withinCopiedFile dbName $ \path _  -> do
                performMigrations path
                test path
    testOnCopiedAndMigrated testDelegationsTableExists
    testOnCopiedAndMigrated testDelegationsTableHasExpectedData
    testOnCopiedAndMigrated testOldDelegationTablesAreDeleted

    where

        testDelegationsTableExists path = do
            [Single (count :: Int)] <- Sqlite.runSqlite (T.pack path) $
                    Sqlite.rawSql delegationsTableExists []
            count `shouldBe` 1
            pure ()

        testDelegationsTableHasExpectedData path = do
            delegations <- Sqlite.runSqlite (T.pack path) $
                    Sqlite.rawSql delegationsTableData []
            delegations `shouldBe` expectedValues
            pure ()

        testOldDelegationTablesAreDeleted path = do
            [Single (count :: Int)]  <- Sqlite.runSqlite (T.pack path) $
                    Sqlite.rawSql oldDelegationTablesAreDeleted []
            count `shouldBe` 0
            pure ()

        delegationsTableExists = [i|
            SELECT EXISTS (
                SELECT
                    name
                FROM
                    sqlite_schema
                WHERE
                    type='table' AND
                    name='delegations'
            );
            |]

        delegationsTableData = [i|
            SELECT
                slot,
                status,
                pool
            FROM
                delegations
            ORDER BY
                slot ASC;
            |]

        oldDelegationTablesAreDeleted = [i|
            SELECT EXISTS (
                SELECT
                    name
                FROM
                    sqlite_schema
                WHERE
                    type='table' AND
                    name ='delegation_certificate' or
                    name='stake_key_certificate'
            );
            |]


        expectedValues :: [(Single Int, Single Text, Maybe (Single Text))]
        expectedValues =
            [
                ( Single 21812897
                , Single "active"
                , Just (Single "8aa469088eaf5c38c3d4faf0d3516ca670cd6df5545fafea2f70258b")
                )
            ,
                ( Single 23116273
                , Single "inactive"
                , Nothing
                )
            ,
                ( Single 23116274
                , Single "registered"
                , Nothing
                )
            ,
                ( Single 23116275
                , Single "active"
                , Just (Single "8aa469088eaf5c38c3d4faf0d3516ca670cd6df5545fafea2f70258b")
                )
            ]
