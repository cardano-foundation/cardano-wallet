{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite
    ( migrateAll )
import Conduit
    ( MonadUnliftIO, ResourceT, runResourceT )
import Control.Monad.Logger
    ( LoggingT, runStderrLoggingT )
import Control.Monad.Reader
    ( ReaderT )
import Data.Text
    ( Text )
import Database.Persist.Sqlite
    ( SqlBackend, runMigration, runSqlConn, withSqliteConn )
import Test.Hspec
    ( Spec, describe, it, shouldReturn )

runSqlite'
    :: (MonadUnliftIO m)
    => Text
    -> ReaderT SqlBackend (LoggingT (ResourceT m)) a
    -> m a
runSqlite' connstr =
    runResourceT . runStderrLoggingT . withSqliteConn connstr . runSqlConn

testMigrate :: IO ()
testMigrate = runSqlite' ":memory:" $ do
    runMigration migrateAll

spec :: Spec
spec = do
    describe "Generated SQL schema" $ do
        it "looks like SQL" $ do
            testMigrate `shouldReturn` ()
