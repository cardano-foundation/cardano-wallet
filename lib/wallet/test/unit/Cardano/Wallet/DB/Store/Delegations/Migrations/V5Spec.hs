{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.Delegations.Migrations.V5Spec where

import Prelude

import Cardano.Wallet.DB.LayerSpec
    ( withinCopiedFile
    )
import Cardano.Wallet.DB.Migration
    ( runMigrations
    )
import Cardano.Wallet.DB.Sqlite.Migration.New
    ( newMigrationInterface
    , newStyleMigrations
    )
import Cardano.Wallet.DB.Store.Delegations.Schema
    ( Delegations (..)
    )
import Cardano.Wallet.DB.Store.Delegations.Types
    ( DelegationStatusEnum (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Logger
    ( NoLoggingT (..)
    )
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Control.Tracer
    ( nullTracer
    )
import Database.Persist.Sql
    ( Entity (..)
    , PersistStoreWrite (..)
    , insertMany_
    , selectList
    )
import Database.Persist.Sqlite
    ( withSqliteConn
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    )
import Test.QuickCheck
    ( Gen
    , InfiniteList (..)
    , Positive (..)
    , arbitrary
    , elements
    , generate
    , oneof
    )
import UnliftIO.Resource
    ( runResourceT
    )

import Cardano.Api
    ( SlotNo
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId
    )
import Data.List
    ( maximumBy
    , sortOn
    )
import Data.Ord
    ( Down (..)
    , comparing
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec =
    describe "Delegations table migration"
        $ it "'migrate' db new delegation table"
        $ testMigrationDelegationsTable
            "before_new_delegation.sqlite"

testMigrationDelegationsTable :: FilePath -> IO ()
testMigrationDelegationsTable dbName = do
    let performMigrations path =
            runMigrations
                (newMigrationInterface nullTracer)
                path
                newStyleMigrations
        testOnCopiedAndMigrated test = fmap snd
            $ withinCopiedFile dbName
            $ \path _ -> do
                performMigrations path
                test path
    testOnCopiedAndMigrated persistentTableIsCompatible
  where
    persistentTableIsCompatible :: FilePath -> IO ()
    persistentTableIsCompatible path = do
        runResourceT . runNoLoggingT
            $ withSqliteConn (T.pack path)
            $ runReaderT
            $ do
                -- delegations from the migration
                oldDelegations <-
                    fmap entityVal
                        <$> selectList [] []
                liftIO $ length oldDelegations `shouldNotBe` 0
                -- last used slot
                let latest = maximumBy (comparing delegationSlot) oldDelegations
                -- 10 delegations to insert
                delegations <-
                    sortOn delegationOrd
                        <$> liftIO (someDelegations 10 $ delegationSlot latest)
                insertMany_ delegations
                -- last 10 delegations
                delegations' <-
                    reverse
                        . take 10
                        . sortOn (Down . delegationOrd)
                        . fmap entityVal
                        <$> selectList [] []
                liftIO $ delegations' `shouldBe` delegations

-- generate n delegations with a slot number greater than the given slot and
-- all different slots
generateDelegations :: Int -> SlotNo -> Gen [Delegations]
generateDelegations 0 _ = pure []
generateDelegations n lslot = do
    status <-
        elements
            [InactiveE, RegisteredE, ActiveE, ActiveAndVotedE, VotedE]
    drep <- oneof [pure Nothing, Just <$> arbitrary]
    pool <- oneof [pure Nothing, Just <$> arbitraryDRep]
    Positive dslot <- arbitrary
    let slot = lslot + dslot
    (Delegations slot status drep pool :) <$> generateDelegations (n - 1) lslot

someDelegations :: Int -> SlotNo -> IO [Delegations]
someDelegations n lslot = generate $ generateDelegations n lslot

arbitraryDRepID :: Gen DRepID
arbitraryDRepID = do
    InfiniteList bytes _ <- arbitrary
    oneof
        [ pure $ DRepFromKeyHash $ DRepKeyHash $ BS.pack $ take 28 bytes
        , pure $ DRepFromScriptHash $ DRepScriptHash $ BS.pack $ take 28 bytes
        ]

arbitraryDRep :: Gen DRep
arbitraryDRep =
    oneof [pure Abstain, pure NoConfidence, FromDRepID <$> arbitraryDRepID]

delegationOrd
    :: Delegations
    -> (SlotNo, DelegationStatusEnum, Maybe PoolId, Maybe DRep)
delegationOrd (Delegations slot status drep pool) = (slot, status, drep, pool)
