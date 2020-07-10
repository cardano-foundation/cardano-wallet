{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.
--
-- To test individual properties in GHCi, use the shorthand type aliases. For
-- example:
--
-- >>> db <- newMemoryDBLayer :: IO TestDBSeq
-- >>> quickCheck $ prop_sequential db

module Cardano.Wallet.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigTesting )
import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.DB.Sqlite
    ( DBLog (..), SqliteContext, destroyDBLayer, fieldName )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchWallet (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( InitialCheckpoint (..), KeyValPairs (..) )
import Cardano.Wallet.DB.Properties
    ( properties, withDB )
import Cardano.Wallet.DB.Sqlite
    ( DefaultFieldValues (..)
    , PersistState
    , newDBFactory
    , newDBLayer
    , withDBLayer
    )
import Cardano.Wallet.DB.StateMachine
    ( prop_parallel, prop_sequential )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0, dummyGenesisParameters, dummyProtocolParameters, mockHash )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PersistPrivateKey
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( KnownAddresses (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..), defaultAddressPoolGap, mkSeqStateFromRootXPrv )
import Cardano.Wallet.Primitive.Model
    ( FilteredBlock (..)
    , Wallet
    , applyBlock
    , availableBalance
    , currentTip
    , getState
    , initWallet
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , PassphraseScheme (..)
    , ProtocolParameters
    , Range
    , SlotId (..)
    , SortOrder (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (TxMeta, amount, direction)
    , TxOut (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , toTxHistory
    , wholeRange
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( forkIO, killThread, threadDelay )
import Control.Concurrent.Async
    ( concurrently, concurrently_ )
import Control.Concurrent.MVar
    ( isEmptyMVar, newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( SomeException, handle, throwIO )
import Control.Monad
    ( forM_, forever, replicateM_, unless, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, mapExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText )
import Data.Time.Clock
    ( getCurrentTime )
import Database.Persist.Sql
    ( DBName (..), PersistEntity (..), fieldDB )
import GHC.Conc
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( copyFile, doesFileExist, listDirectory, removeFile )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hClose, withFile )
import System.IO.Error
    ( isUserError )
import System.IO.Temp
    ( emptySystemTempFile, withSystemTempDirectory, withSystemTempFile )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Random
    ( randomRIO )
import Test.Hspec
    ( Expectation
    , Spec
    , SpecWith
    , before
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldNotBe
    , shouldNotContain
    , shouldReturn
    , shouldThrow
    , xit
    )
import Test.QuickCheck
    ( Property, arbitrary, generate, property, (==>) )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( captureLogging )

import qualified Cardano.Wallet.DB.Sqlite.TH as DB
import qualified Cardano.Wallet.Primitive.AddressDerivation.Jormungandr as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified GHC.Conc as TVar

spec :: Spec
spec = do
    sqliteSpecSeq
    sqliteSpecRnd
    loggingSpec
    fileModeSpec
    describe "Manual migrations" $ do
        it "'migrate' db with no passphrase scheme set."
            testMigrationPassphraseScheme

sqliteSpecSeq :: Spec
sqliteSpecSeq = withDB newMemoryDBLayer $ do
    describe "Sqlite" properties
    describe "Sqlite State machine tests" $ do
        it "Sequential" (prop_sequential :: TestDBSeq -> Property)
        xit "Parallel" prop_parallel

sqliteSpecRnd :: Spec
sqliteSpecRnd = withDB newMemoryDBLayer $ do
    describe "Sqlite (RndState)" $ do
        it "insertState . selectState (regression account index)"
            testRegressionInsertSelectRndState
    describe "Sqlite State machine (RndState)" $ do
        it "Sequential state machine tests"
            (prop_sequential :: TestDBRnd -> Property)

testRegressionInsertSelectRndState
    :: DBLayer IO (RndState 'Mainnet) ByronKey
    -> IO ()
testRegressionInsertSelectRndState db = do
    -- NOTE Abusing the index type here, for the sake of testing.
    old  <- (\s -> s { accountIndex = Index 0 }) <$> generate arbitraryRndState
    wid  <- generate arbitrary
    cp   <- getInitialCheckpoint <$> generate arbitrary
    meta <- generate arbitrary

    new <- db & \DBLayer{..} -> atomically $ do
        unsafeRunExceptT $ initializeWallet wid cp meta mempty pp
        unsafeRunExceptT $ putCheckpoint wid (updateState old cp)
        (fmap getState) <$> readCheckpoint wid

    (accountIndex <$> new) `shouldBe` Just (minBound :: Index 'Hardened 'AccountK)

  where
    arbitraryRndState = arbitrary @(RndState 'Mainnet)

testMigrationPassphraseScheme
    :: forall s k. (k ~ ShelleyKey, s ~ SeqState 'Mainnet k)
    => IO ()
testMigrationPassphraseScheme = do
    let orig = $(getTestData) </> "passphraseScheme-v2020-03-16.sqlite"
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        copyFile orig path
        (logs, (a,b,c,d)) <- captureLogging $ \tr -> do
            withDBLayer @s @k tr defaultFieldValues (Just path)
                $ \(_ctx, db) -> db & \DBLayer{..} -> atomically
                $ do
                    Just a <- readWalletMeta $ PrimaryKey walNeedMigration
                    Just b <- readWalletMeta $ PrimaryKey walNewScheme
                    Just c <- readWalletMeta $ PrimaryKey walOldScheme
                    Just d <- readWalletMeta $ PrimaryKey walNoPassphrase
                    pure (a,b,c,d)

        -- Migration is visible from the logs
        let migrationMsg = filter isMsgManualMigration logs
        length migrationMsg `shouldBe` 1

        -- The first wallet is stored in the database with only a
        -- 'passphraseLastUpdatedAt' field, but no 'passphraseScheme'. So,
        -- after the migration, both should now be `Just`.
        (passphraseScheme <$> passphraseInfo a) `shouldBe` Just EncryptWithPBKDF2

        -- The second wallet was just fine and already has a passphrase
        -- scheme set to use PBKDF2. Nothing should have changed.
        (passphraseScheme <$> passphraseInfo b) `shouldBe` Just EncryptWithPBKDF2

        -- The third wallet had a scheme too, but was using the legacy
        -- scheme. Nothing should have changed.
        (passphraseScheme <$> passphraseInfo c) `shouldBe` Just EncryptWithScrypt

        -- The last wallet had no passphrase whatsoever (restored from
        -- account public key), so it should still have NO scheme.
        (passphraseScheme <$> passphraseInfo d) `shouldBe` Nothing
  where
    isMsgManualMigration :: DBLog -> Bool
    isMsgManualMigration = \case
        MsgManualMigrationNeeded field _ ->
            fieldName field ==
                unDBName (fieldDB $ persistFieldDef DB.WalPassphraseScheme)
        _ ->
            False

    -- Coming from __test/data/passphraseScheme-v2020-03-16.sqlite__:
    --
    --     sqlite3> SELECT wallet_id, passphrase_last_updated_at,passphrase_scheme FROM wallet;
    --
    --     wallet_id                                | passphrase_last_updated_at    | passphrase_scheme
    --     =========================================+===============================+==================
    --     64581f7393190aed462fc3180ce52c3c1fe580a9 | 2020-06-02T14:22:17.48678659  |
    --     5e481f55084afda69fc9cd3863ced80fa83734aa | 2020-06-02T14:22:03.234087113 | EncryptWithPBKDF2
    --     4a6279cd71d5993a288b2c5879daa7c42cebb73d | 2020-06-02T14:21:45.418841818 | EncryptWithScrypt
    --     ba74a7d2c1157ea7f32a93f255dac30e9ebca62b |                               |
    --
    Right walNeedMigration = fromText "64581f7393190aed462fc3180ce52c3c1fe580a9"
    Right walNewScheme     = fromText "5e481f55084afda69fc9cd3863ced80fa83734aa"
    Right walOldScheme     = fromText "4a6279cd71d5993a288b2c5879daa7c42cebb73d"
    Right walNoPassphrase  = fromText "ba74a7d2c1157ea7f32a93f255dac30e9ebca62b"

{-------------------------------------------------------------------------------
                                Logging Spec
-------------------------------------------------------------------------------}

loggingSpec :: Spec
loggingSpec = withLoggingDB @(SeqState 'Mainnet JormungandrKey) @JormungandrKey $ do
    describe "Sqlite query logging" $ do
        it "should log queries at DEBUG level" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCpSeq testMetadata mempty pp
            logs <- getLogs
            logs `shouldHaveMsgQuery` "INSERT"

        it "should not log query parameters" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCpSeq testMetadata mempty pp
            let walletName = T.unpack $ coerce $ name testMetadata
            msgs <- T.unlines . mapMaybe getMsgQuery <$> getLogs
            T.unpack msgs `shouldNotContain` walletName

    describe "Sqlite observables" $ do
        it "should measure query timings" $ \(getLogs, DBLayer{..}) -> do
            let count = 5
            replicateM_ count (atomically listWallets)
            msgs <- findObserveDiffs <$> getLogs
            length msgs `shouldBe` count * 2

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer
    ::  ( PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => IO (DBLayer IO s k)
newMemoryDBLayer = snd . snd <$> newMemoryDBLayer'

newMemoryDBLayer'
    ::  ( PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => IO (TVar [DBLog], (SqliteContext, DBLayer IO s k))
newMemoryDBLayer' = do
    logVar <- newTVarIO []
    (logVar, ) <$>
        newDBLayer (traceInTVarIO logVar) defaultFieldValues Nothing

withLoggingDB
    ::  ( PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => SpecWith (IO [DBLog], DBLayer IO s k)
    -> Spec
withLoggingDB = beforeAll newMemoryDBLayer' . beforeWith clean
  where
    clean (logs, (_, db)) = do
        cleanDB db
        TVar.atomically $ writeTVar logs []
        pure (readTVarIO logs, db)

shouldHaveMsgQuery :: [DBLog] -> Text -> Expectation
shouldHaveMsgQuery msgs str = unless (any match msgs) $
    fail $ "Did not find DB query " ++
        T.unpack str ++ " within " ++ show msgs
  where
    match = maybe False (str `T.isInfixOf`) . getMsgQuery

getMsgQuery :: DBLog -> Maybe Text
getMsgQuery (MsgQuery msg _) = Just msg
getMsgQuery _ = Nothing

findObserveDiffs :: [DBLog] -> [DBLog]
findObserveDiffs = filter isObserveDiff
  where
    isObserveDiff (MsgRun _) = True
    isObserveDiff _ = False

{-------------------------------------------------------------------------------
                                File Mode Spec
-------------------------------------------------------------------------------}

type TestDBSeq = DBLayer IO (SeqState 'Mainnet JormungandrKey) JormungandrKey
type TestDBRnd = DBLayer IO (RndState 'Mainnet) ByronKey

fileModeSpec :: Spec
fileModeSpec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $ do
                db <- Just <$> temporaryDBFile
                (ctx, _) <- newDBLayer' @(SeqState 'Mainnet JormungandrKey) db
                destroyDBLayer ctx

    describe "DBFactory" $ do
        let withDBFactory action = withSystemTempDirectory "DBFactory" $ \dir -> do
                dbf <- newDBFactory nullTracer defaultFieldValues (Just dir)
                action dir dbf

        let whileFileOpened delay f action = do
                opened <- newEmptyMVar
                concurrently_
                    (withFile f ReadMode (\_ -> putMVar opened () >> threadDelay delay))
                    (takeMVar opened >> action)

        it "withDatabase *> removeDatabase works and remove files" $ do
            withDBFactory $ \dir DBFactory{..} -> do
                -- NOTE
                -- Start a concurrent worker which makes action on the DB in
                -- parallel to simulate activity.
                pid <- forkIO $ withDatabase testWid $ \(DBLayer{..} :: TestDBSeq) -> do
                    handle @SomeException (const (pure ())) $ forever $ do
                        atomically $ do
                            liftIO $ threadDelay 10000
                            void $ readCheckpoint $ PrimaryKey testWid

                killThread pid *> removeDatabase testWid
                listDirectory dir `shouldReturn` mempty

        it "removeDatabase still works if file is opened" $ do
            withDBFactory $ \dir DBFactory{..} -> do
                -- set up a database file
                withDatabase testWid $ \(DBLayer{..} :: TestDBSeq) -> pure ()
                files <- listDirectory dir
                files `shouldNotBe` mempty

                -- Try removing the database when it's already opened for
                -- reading for 100ms.
                -- This simulates an antivirus program on windows which may
                -- interfere with file deletion.
                whileFileOpened 100000 (dir </> head files) (removeDatabase testWid)
                listDirectory dir `shouldReturn` mempty

        it "removeDatabase waits for connections to close" $ do
            withDBFactory $ \_ DBFactory{..} -> do
                closed <- newEmptyMVar

                let conn =
                        withDatabase testWid $ \(DBLayer{..} :: TestDBSeq) -> do
                            threadDelay 500000
                            putMVar closed ()
                let rm = do
                        removeDatabase testWid
                        isEmptyMVar closed

                concurrently conn (threadDelay 10 >> rm) `shouldReturn` ((), False)

    describe "Sqlite database file" $ do
        let writeSomething DBLayer{..} = do
                atomically $ unsafeRunExceptT $
                    initializeWallet testPk testCpSeq testMetadata mempty pp
                atomically listWallets `shouldReturn` [testPk]
            tempFilesAbsent fp = do
                doesFileExist fp `shouldReturn` True
                doesFileExist (fp <> "-wal") `shouldReturn` False
                doesFileExist (fp <> "-shm") `shouldReturn` False
            bomb = throwIO (userError "bomb")
        it "is properly closed after withDBLayer" $
            withTestDBFile writeSomething tempFilesAbsent
        it "is properly closed after an exception in withDBLayer" $
            withTestDBFile (\db -> writeSomething db >> bomb) tempFilesAbsent
                `shouldThrow` isUserError

    before temporaryDBFile $
        describe "Check db reading/writing from/to file and cleaning" $ do

        it "create and list wallet works" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCp testMetadata mempty pp
            destroyDBLayer ctx
            testOpeningCleaning f listWallets' [testPk] []

        it "create and get meta works" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            now <- getCurrentTime
            let meta = testMetadata
                   { passphraseInfo = Just $ WalletPassphraseInfo now EncryptWithPBKDF2 }
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCp meta mempty pp
            destroyDBLayer ctx
            testOpeningCleaning f (`readWalletMeta'` testPk) (Just meta) Nothing

        it "create and get private key" $ \f-> do
            (ctx, db@DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCp testMetadata mempty pp
            (k, h) <- unsafeRunExceptT $ attachPrivateKey db testPk
            destroyDBLayer ctx
            testOpeningCleaning f (`readPrivateKey'` testPk) (Just (k, h)) Nothing

        it "put and read tx history (Ascending)" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ do
                unsafeRunExceptT $
                    initializeWallet testPk testCp testMetadata mempty pp
                unsafeRunExceptT $ putTxHistory testPk testTxs
            destroyDBLayer ctx
            testOpeningCleaning
                f
                (\db' -> readTxHistory' db' testPk Ascending wholeRange Nothing)
                testTxs
                mempty

        it "put and read tx history (Decending)" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ do
                unsafeRunExceptT $
                    initializeWallet testPk testCp testMetadata mempty pp
                unsafeRunExceptT $ putTxHistory testPk testTxs
            destroyDBLayer ctx
            testOpeningCleaning
                f
                (\db' -> readTxHistory' db' testPk Descending wholeRange Nothing)
                testTxs
                mempty

        it "put and read checkpoint" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ do
                unsafeRunExceptT $
                    initializeWallet testPk testCp testMetadata mempty pp
                unsafeRunExceptT $ putCheckpoint testPk testCp
            destroyDBLayer ctx
            testOpeningCleaning f (`readCheckpoint'` testPk) (Just testCp) Nothing

        describe "Golden rollback scenarios" $ do
            let dummyHash x = Hash $ x <> BS.pack (replicate (32 - (BS.length x)) 0)
            let dummyAddr x = Address $ x <> BS.pack (replicate (32 - (BS.length x)) 0)

            it "(Regression test #1575) - TxMetas and checkpoints should \
               \rollback to the same place" $ \f -> do
                (_ctx, db@DBLayer{..}) <- newDBLayer' (Just f)

                let ourAddrs = knownAddresses (getState testCp)

                atomically $ unsafeRunExceptT $ initializeWallet
                    testPk testCp testMetadata mempty pp

                let mockApply h mockTxs = do
                        Just cpA <- atomically $ readCheckpoint testPk
                        let slotA = slotId $ currentTip cpA
                        let Quantity bhA = blockHeight $ currentTip cpA
                        let hashA = headerHash $ currentTip cpA
                        let fakeBlock = Block
                                (BlockHeader
                                { slotId = slotA { epochNumber = (epochNumber slotA) + 1}
                                -- Increment blockHeight by steps greater than k
                                -- Such that old checkpoints are always pruned.
                                , blockHeight = Quantity $ bhA + 5000
                                , headerHash = h
                                , parentHeaderHash = hashA
                                })
                                mockTxs
                                []
                        let (FilteredBlock _ txs, cpB) = applyBlock fakeBlock cpA
                        atomically $ do
                            unsafeRunExceptT $ putCheckpoint testPk cpB
                            unsafeRunExceptT $ putTxHistory testPk txs
                            unsafeRunExceptT $ prune testPk

                let mockApplyBlock1 = mockApply (dummyHash "block1")
                            [ Tx (dummyHash "tx1")
                                [(TxIn (dummyHash "faucet") 0, Coin 4)]
                                [ TxOut (head ourAddrs) (Coin 4)
                                ]
                            ]

                -- Slot 1 0
                mockApplyBlock1
                getAvailableBalance db `shouldReturn` 4

                -- Slot 2 0
                mockApply (dummyHash "block2a")
                            [ Tx
                                (dummyHash "tx2a")
                                [ (TxIn (dummyHash "tx1") 0, Coin 4)
                                ]
                                [ TxOut (dummyAddr "faucetAddr2") (Coin 2)
                                , TxOut (ourAddrs !! 1) (Coin 2)
                                ]
                            ]

                -- Slot 3 0
                mockApply (dummyHash "block3a") []
                getAvailableBalance db `shouldReturn` 2
                getTxsInLedger db `shouldReturn` [(Outgoing, 2), (Incoming, 4)]

                atomically . void . unsafeRunExceptT $
                    rollbackTo testPk (SlotId 2 0)
                Just cp <- atomically $ readCheckpoint testPk
                slotId (currentTip cp) `shouldBe` (SlotId 0 0)

                getTxsInLedger db `shouldReturn` []

    describe "random operation chunks property" $ do
        it "realize a random batch of operations upon one db open"
            (property $ prop_randomOpChunks @(SeqState 'Mainnet JormungandrKey))

-- This property checks that executing series of wallet operations in a single
-- SQLite session has the same effect as executing the same operations over
-- multiple sessions.
prop_randomOpChunks
    :: (Eq s, PersistState s, Show s)
    => KeyValPairs (PrimaryKey WalletId) (Wallet s, WalletMetadata)
    -> Property
prop_randomOpChunks (KeyValPairs pairs) =
    not (null pairs) ==> monadicIO (liftIO prop)
  where
    prop = do
        filepath <- temporaryDBFile
        (ctxF, dbF) <- newDBLayer' (Just filepath) >>= cleanDB'
        (ctxM, dbM) <- inMemoryDBLayer >>= cleanDB'
        forM_ pairs (insertPair dbM)
        cutRandomly pairs >>= mapM_ (\chunk -> do
            (ctx, db) <- newDBLayer' (Just filepath)
            forM_ chunk (insertPair db)
            destroyDBLayer ctx)
        dbF `shouldBeConsistentWith` dbM
        destroyDBLayer ctxF *> destroyDBLayer ctxM

    insertPair
        :: DBLayer IO s k
        -> (PrimaryKey WalletId, (Wallet s, WalletMetadata))
        -> IO ()
    insertPair DBLayer{..} (k, (cp, meta)) = do
        keys <- atomically listWallets
        if k `elem` keys then atomically $ do
            unsafeRunExceptT $ putCheckpoint k cp
            unsafeRunExceptT $ putWalletMeta k meta
        else do
            atomically $ unsafeRunExceptT $ initializeWallet k cp meta mempty pp
            Set.fromList <$> atomically listWallets
                `shouldReturn` Set.fromList (k:keys)

    shouldBeConsistentWith :: (Eq s, Show s) => DBLayer IO s k -> DBLayer IO s k -> IO ()
    shouldBeConsistentWith db1@DBLayer{..} db2 = do
        wids1 <- Set.fromList <$> listWallets' db1
        wids2 <- Set.fromList <$> listWallets' db2
        wids1 `shouldBe` wids2

        forM_ wids1 $ \walId -> do
            cps1 <- readCheckpoint' db1 walId
            cps2 <- readCheckpoint' db2 walId
            cps1 `shouldBe` cps2

        forM_ wids1 $ \walId -> do
            meta1 <- readWalletMeta' db1 walId
            meta2 <- readWalletMeta' db2 walId
            meta1 `shouldBe` meta2

-- | Test that data is preserved between open / close of the same database and
-- that cleaning up happens as expected.
testOpeningCleaning
    :: (Show s, Eq s)
    => FilePath
    -> (DBLayer IO (SeqState 'Mainnet JormungandrKey) JormungandrKey -> IO s)
    -> s
    -> s
    -> Expectation
testOpeningCleaning filepath call expectedAfterOpen expectedAfterClean = do
    (ctx1, db1) <- newDBLayer' (Just filepath)
    call db1 `shouldReturn` expectedAfterOpen
    _ <- cleanDB db1
    call db1 `shouldReturn` expectedAfterClean
    destroyDBLayer ctx1
    (ctx2,db2) <- newDBLayer' (Just filepath)
    call db2 `shouldReturn` expectedAfterClean
    destroyDBLayer ctx2


-- | Run a test action inside withDBLayer, then check assertions.
withTestDBFile
    :: (DBLayer IO (SeqState 'Mainnet JormungandrKey) JormungandrKey -> IO ())
    -> (FilePath -> IO a)
    -> IO a
withTestDBFile action expectations = do
    logConfig <- defaultConfigTesting
    trace <- setupTrace (Right logConfig) "connectionSpec"
    withSystemTempFile "spec.db" $ \fp h -> do
        hClose h
        removeFile fp
        withDBLayer
            (trMessageText trace)
            defaultFieldValues
            (Just fp)
            (action . snd)
        expectations fp

inMemoryDBLayer
    :: PersistState s
    => IO (SqliteContext, DBLayer IO s JormungandrKey)
inMemoryDBLayer = newDBLayer' Nothing

temporaryDBFile :: IO FilePath
temporaryDBFile = emptySystemTempFile "cardano-wallet-SqliteFileMode"

defaultFieldValues :: DefaultFieldValues
defaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient = ActiveSlotCoefficient 1.0
    , defaultDesiredNumberOfPool = 0
    , defaultMinimumUTxOValue = Coin 0
    }

newDBLayer'
    :: PersistState s
    => Maybe FilePath
    -> IO (SqliteContext, DBLayer IO s JormungandrKey)
newDBLayer' = newDBLayer nullTracer defaultFieldValues

-- | Clean the database
cleanDB'
    :: Monad m
    => (SqliteContext, DBLayer m s k)
    -> m (SqliteContext, DBLayer m s k)
cleanDB' (ctx, db) =
    cleanDB db $> (ctx, db)

listWallets'
    :: DBLayer m s k
    -> m [PrimaryKey WalletId]
listWallets' DBLayer{..} =
    atomically listWallets

readCheckpoint'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe (Wallet s))
readCheckpoint' DBLayer{..} =
    atomically . readCheckpoint

readWalletMeta'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe WalletMetadata)
readWalletMeta' DBLayer{..} =
    atomically . readWalletMeta

readTxHistory'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> SortOrder
    -> Range SlotId
    -> Maybe TxStatus
    -> m [(Tx, TxMeta)]
readTxHistory' DBLayer{..} a0 a1 a2 =
    atomically . fmap (fmap toTxHistory) . readTxHistory a0 a1 a2

readPrivateKey'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe (k 'RootK XPrv, Hash "encryption"))
readPrivateKey' DBLayer{..} =
    atomically . readPrivateKey

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s JormungandrKey
    -> PrimaryKey WalletId
    -> ExceptT ErrNoSuchWallet IO (JormungandrKey 'RootK XPrv, Hash "encryption")
attachPrivateKey DBLayer{..} wid = do
    let pwd = Passphrase $ BA.convert $ T.encodeUtf8 "simplevalidphrase"
    seed <- liftIO $ generate $ SomeMnemonic <$> genMnemonic @15
    let k = generateKeyFromSeed (seed, Nothing) pwd
    h <- liftIO $ encryptPassphrase pwd
    mapExceptT atomically $ putPrivateKey wid (k, h)
    return (k, h)

cutRandomly :: [a] -> IO [[a]]
cutRandomly = iter []
  where
    iter acc rest
        | L.length rest <= 1 =
            pure $ L.reverse (rest:acc)
        | otherwise = do
            chunksNum <- randomRIO (1, L.length rest)
            let chunk = L.take chunksNum rest
            iter (chunk:acc) (L.drop chunksNum rest)


{-------------------------------------------------------------------------------
                                   Test data
-------------------------------------------------------------------------------}

testCp :: Wallet (SeqState 'Mainnet JormungandrKey)
testCp = snd $ initWallet block0 dummyGenesisParameters initDummyState
  where
    initDummyState :: SeqState 'Mainnet JormungandrKey
    initDummyState = mkSeqStateFromRootXPrv (xprv, mempty) defaultAddressPoolGap
      where
        mw = SomeMnemonic . unsafePerformIO . generate $ genMnemonic @15
        xprv = generateKeyFromSeed (mw, Nothing) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , creationTime = unsafePerformIO getCurrentTime
    , passphraseInfo = Nothing
    , delegation = WalletDelegation NotDelegating []
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

testTxs :: [(Tx, TxMeta)]
testTxs =
    [ ( Tx (mockHash @String "tx2")
        [ (TxIn (mockHash @String "tx1") 0, Coin 1)]
        [ TxOut (Address "addr") (Coin 1) ]
      , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 0) (Quantity 1337144)
      )
    ]

pp :: ProtocolParameters
pp = dummyProtocolParameters

{-------------------------------------------------------------------------------
                    Helpers for golden rollback tests
-------------------------------------------------------------------------------}

getAvailableBalance :: DBLayer IO s k -> IO Word
getAvailableBalance DBLayer{..} = do
    cp <- fmap (fromMaybe (error "nothing")) <$> atomically $ readCheckpoint testPk
    pend <- atomically $ fmap toTxHistory
        <$> readTxHistory testPk Descending wholeRange (Just Pending)
    return $ fromIntegral $ availableBalance (Set.fromList $ map fst pend) cp

getTxsInLedger :: DBLayer IO s k -> IO ([(Direction, Natural)])
getTxsInLedger DBLayer {..} = do
    pend <- atomically $ fmap toTxHistory
        <$> readTxHistory testPk Descending wholeRange (Just InLedger)
    return $ map (\(_, m) -> (direction m, getQuantity $ amount m)) pend

{-------------------------------------------------------------------------------
                           Test data - Sequential AD
-------------------------------------------------------------------------------}

testCpSeq :: Wallet (SeqState 'Mainnet JormungandrKey)
testCpSeq = snd $ initWallet block0 dummyGenesisParameters initDummyStateSeq

initDummyStateSeq :: SeqState 'Mainnet JormungandrKey
initDummyStateSeq = mkSeqStateFromRootXPrv (xprv, mempty) defaultAddressPoolGap
  where
      mw = SomeMnemonic $ unsafePerformIO (generate $ genMnemonic @15)
      xprv = Seq.generateKeyFromSeed (mw, Nothing) mempty

{-------------------------------------------------------------------------------
                      Test data and instances - Random AD
-------------------------------------------------------------------------------}

instance Eq (RndState t) where
    (==)
        (RndState _ idx1 addrs1 pending1 gen1)
        (RndState _ idx2 addrs2 pending2 gen2) =
           idx1 == idx2
        && addrs1 == addrs2
        && pending1 == pending2
        && show gen1 == show gen2
