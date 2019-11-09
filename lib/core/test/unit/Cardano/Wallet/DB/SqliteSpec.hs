{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
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
import Cardano.BM.Data.LogItem
    ( LOContent (..), LOMeta (severity), LogObject (..) )
import Cardano.BM.Data.MonitoringEval
    ( MEvAction (..), MEvExpr (..), Operand (..), Operator (..) )
import Cardano.BM.Data.Observable
    ( ObservableInstance (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.DB.Sqlite
    ( SqliteContext, destroyDBLayer )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), PrimaryKey (..), cleanDB )
import Cardano.Wallet.DB.Arbitrary
    ( KeyValPairs (..) )
import Cardano.Wallet.DB.Properties
    ( properties, withDB )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, newDBLayer, withDBLayer )
import Cardano.Wallet.DB.StateMachine
    ( prop_parallel, prop_sequential )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0, genesisParameters )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PersistPrivateKey
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..), defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , Range
    , SlotId (..)
    , SortOrder (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , wholeRange
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( forM_, replicateM_, unless )
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
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import Data.Time.Clock
    ( getCurrentTime )
import GHC.Conc
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import System.Directory
    ( doesFileExist, removeFile )
import System.IO
    ( hClose )
import System.IO.Error
    ( isUserError )
import System.IO.Temp
    ( emptySystemTempFile, withSystemTempFile )
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
    , shouldNotContain
    , shouldReturn
    , shouldThrow
    , xit
    )
import Test.QuickCheck
    ( Property, property, (==>) )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.Aggregated as CM
import qualified Cardano.BM.Data.AggregatedKind as CM
import qualified Cardano.BM.Data.Backend as CM
import qualified Cardano.BM.Data.SubTrace as CM
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Conc as TVar

spec :: Spec
spec = do
    sqliteSpecSeq
    sqliteSpecRnd
    loggingSpec
    fileModeSpec

sqliteSpecSeq :: Spec
sqliteSpecSeq = withDB newMemoryDBLayer $ do
    describe "Sqlite" properties
    describe "Sqlite State machine tests" $ do
        it "Sequential" (prop_sequential :: TestDBSeq -> Property)
        xit "Parallel" prop_parallel

sqliteSpecRnd :: Spec
sqliteSpecRnd = withDB newMemoryDBLayer $ do
        describe "Sqlite State machine (RndState)" $ do
            it "Sequential state machine tests"
                (prop_sequential :: TestDBRnd -> Property)

{-------------------------------------------------------------------------------
                                Logging Spec
-------------------------------------------------------------------------------}

loggingSpec :: Spec
loggingSpec = withLoggingDB @(SeqState 'Testnet ShelleyKey) @ShelleyKey $ do
    describe "Sqlite query logging" $ do
        it "should log queries at DEBUG level" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCpSeq testMetadata mempty
            logs <- logMessages <$> getLogs
            logs `shouldHaveLog` (Debug, "INSERT")

        it "should not log query parameters" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCpSeq testMetadata mempty
            let walletName = T.unpack $ coerce $ name testMetadata
            msgs <- T.unlines . map snd . logMessages <$> getLogs
            T.unpack msgs `shouldNotContain` walletName

    describe "Sqlite observables" $ do
        it "should measure query timings" $ \(getLogs, DBLayer{..}) -> do
            let count = 5
            replicateM_ count (atomically listWallets)
            -- Commented out until this is fixed:
            --   https://github.com/input-output-hk/iohk-monitoring-framework/issues/391
            -- msg <- T.unlines . map snd . logMessages <$> getLogs
            -- T.unpack msgs `shouldContain` "runQuery monitor works"
            msgs <- findObserveDiffs <$> getLogs
            length msgs `shouldBe` count

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer
    :: ( IsOurs s
       , NFData s
       , Show s
       , PersistState s
       , PersistPrivateKey (k 'RootK)
       )
    => IO (DBLayer IO s k)
newMemoryDBLayer = snd . snd <$> newMemoryDBLayer'

newMemoryDBLayer'
    :: ( IsOurs s
       , NFData s
       , Show s
       , PersistState s
       , PersistPrivateKey (k 'RootK)
       )
    => IO (TVar [LogObject Text], (SqliteContext, DBLayer IO s k))
newMemoryDBLayer' = do
    logConfig <- testingLogConfig
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer logConfig (traceInTVarIO logVar) Nothing

testingLogConfig :: IO CM.Configuration
testingLogConfig = do
    logConfig <- defaultConfigTesting
    CM.setMinSeverity logConfig Debug
    CM.setSetupBackends logConfig [CM.KatipBK, CM.AggregationBK, CM.MonitoringBK]

    CM.setSubTrace logConfig "query"
        (Just $ CM.ObservableTraceSelf [MonotonicClock])

    CM.setBackends logConfig
        "query"
        (Just [CM.AggregationBK])
    CM.setAggregatedKind logConfig
        "query"
        (Just CM.StatsAK) -- statistics AgreggatedKind
    CM.setBackends logConfig
        "#aggregation.query"
        (Just [CM.KatipBK])

    -- This monitor should always trigger.
    CM.setMonitors logConfig $ HM.singleton
        "query.diff"
        ( Nothing
        , Compare "query.diff.timestamp" (GE, (OpMeasurable (CM.Seconds 0)))
        , [CreateMessage Info "runQuery monitor works"]
        )

    CM.setBackends logConfig
        "query"
        (Just [CM.AggregationBK, CM.KatipBK, CM.MonitoringBK])

    pure logConfig

withLoggingDB
    :: ( IsOurs s
       , NFData s
       , Show s
       , PersistState s
       , PersistPrivateKey (k 'RootK)
       )
    => SpecWith (IO [LogObject Text], DBLayer IO s k)
    -> Spec
withLoggingDB = beforeAll newMemoryDBLayer' . beforeWith clean
  where
    clean (logs, (_, db)) = do
        cleanDB db
        TVar.atomically $ writeTVar logs []
        pure (readTVarIO logs, db)

logMessages :: [LogObject a] -> [(Severity, a)]
logMessages = mapMaybe getMessage
  where
    getMessage (LogObject _ m (LogMessage a)) = Just (severity m, a)
    getMessage _ = Nothing

findObserveDiffs :: [LogObject a] -> [LOContent a]
findObserveDiffs = filter isObserveDiff . map loContent
  where
    isObserveDiff (ObserveDiff _) = True
    isObserveDiff _ = False

shouldHaveLog :: [(Severity, Text)] -> (Severity, Text) -> Expectation
shouldHaveLog msgs (sev, str) = unless (any match msgs) $
    fail $ "Did not find " ++ show sev ++ " log " ++
        T.unpack str ++ " within " ++ show msgs
  where
    match (sev', msg) = sev' == sev && str `T.isInfixOf` msg

{-------------------------------------------------------------------------------
                                File Mode Spec
-------------------------------------------------------------------------------}

type TestDBSeq = DBLayer IO (SeqState 'Testnet ShelleyKey) ShelleyKey
type TestDBRnd = DBLayer IO (RndState 'Testnet) ByronKey

fileModeSpec :: Spec
fileModeSpec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $ do
                db <- Just <$> temporaryDBFile
                (ctx, _) <- newDBLayer' @(SeqState 'Testnet ShelleyKey) db
                destroyDBLayer ctx

    describe "Sqlite database file" $ do
        let writeSomething DBLayer{..} = do
                atomically $ unsafeRunExceptT $
                    initializeWallet testPk testCpSeq testMetadata mempty
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
                initializeWallet testPk testCp testMetadata mempty
            destroyDBLayer ctx
            testOpeningCleaning f listWallets' [testPk] []

        it "create and get meta works" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            now <- getCurrentTime
            let meta = testMetadata
                   { passphraseInfo = Just $ WalletPassphraseInfo now }
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCp meta mempty
            destroyDBLayer ctx
            testOpeningCleaning f (`readWalletMeta'` testPk) (Just meta) Nothing

        it "create and get private key" $ \f-> do
            (ctx, db@DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ do
                unsafeRunExceptT $ initializeWallet testPk testCp testMetadata mempty
            (k, h) <- unsafeRunExceptT $ attachPrivateKey db testPk
            destroyDBLayer ctx
            testOpeningCleaning f (`readPrivateKey'` testPk) (Just (k, h)) Nothing

        it "put and read tx history (Ascending)" $ \f -> do
            (ctx, DBLayer{..}) <- newDBLayer' (Just f)
            atomically $ do
                unsafeRunExceptT $ initializeWallet testPk testCp testMetadata mempty
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
                unsafeRunExceptT $ initializeWallet testPk testCp testMetadata mempty
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
                unsafeRunExceptT $ initializeWallet testPk testCp testMetadata mempty
                unsafeRunExceptT $ putCheckpoint testPk testCp
            destroyDBLayer ctx
            testOpeningCleaning f (`readCheckpoint'` testPk) (Just testCp) Nothing

    describe "random operation chunks property" $ do
        it "realize a random batch of operations upon one db open"
            (property $ prop_randomOpChunks @(SeqState 'Testnet ShelleyKey))

-- This property checks that executing series of wallet operations in a single
-- SQLite session has the same effect as executing the same operations over
-- multiple sessions.
prop_randomOpChunks
    :: (Eq s, IsOurs s, NFData s, Show s, PersistState s)
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
            atomically $ unsafeRunExceptT $ initializeWallet k cp meta mempty
            Set.fromList <$> atomically listWallets
                `shouldReturn` Set.fromList (k:keys)

    shouldBeConsistentWith :: (Eq s) => DBLayer IO s k -> DBLayer IO s k -> IO ()
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
    -> (DBLayer IO (SeqState 'Testnet ShelleyKey) ShelleyKey -> IO s)
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
    :: (DBLayer IO (SeqState 'Testnet ShelleyKey) ShelleyKey -> IO ())
    -> (FilePath -> IO a)
    -> IO a
withTestDBFile action expectations = do
    logConfig <- defaultConfigTesting
    trace <- setupTrace (Right logConfig) "connectionSpec"
    withSystemTempFile "spec.db" $ \fp handle -> do
        hClose handle
        removeFile fp
        withDBLayer logConfig trace (Just fp) action
        expectations fp

inMemoryDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s)
    => IO (SqliteContext, DBLayer IO s ShelleyKey)
inMemoryDBLayer = newDBLayer' Nothing

temporaryDBFile :: IO FilePath
temporaryDBFile = emptySystemTempFile "cardano-wallet-SqliteFileMode"

newDBLayer'
    :: (IsOurs s, NFData s, Show s, PersistState s)
    => Maybe FilePath
    -> IO (SqliteContext, DBLayer IO s ShelleyKey)
newDBLayer' fp = do
    logConfig <- CM.empty
    newDBLayer logConfig nullTracer fp

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
    atomically . readTxHistory a0 a1 a2

readPrivateKey'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe (k 'RootK XPrv, Hash "encryption"))
readPrivateKey' DBLayer{..} =
    atomically . readPrivateKey

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s ShelleyKey
    -> PrimaryKey WalletId
    -> ExceptT ErrNoSuchWallet IO (ShelleyKey 'RootK XPrv, Hash "encryption")
attachPrivateKey DBLayer{..} wid = do
    let Right pwd = fromText "simplevalidphrase"
    let k = generateKeyFromSeed (coerce pwd, coerce pwd) pwd
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

testCp :: Wallet (SeqState 'Testnet ShelleyKey)
testCp = snd $ initWallet block0 genesisParameters initDummyState
  where
    initDummyState :: SeqState 'Testnet ShelleyKey
    initDummyState = mkSeqState (xprv, mempty) defaultAddressPoolGap
      where
        bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
        xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , creationTime = unsafePerformIO getCurrentTime
    , passphraseInfo = Nothing
    , delegation = NotDelegating
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

testTxs :: [(Tx, TxMeta)]
testTxs =
    [ ( Tx (Hash "tx2")
        [ (TxIn (Hash "tx1") 0, Coin 1)]
        [ TxOut (Address "addr") (Coin 1) ]
      , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 0) (Quantity 1337144)
      )
    ]

{-------------------------------------------------------------------------------
                           Test data - Sequential AD
-------------------------------------------------------------------------------}

testCpSeq :: Wallet (SeqState 'Testnet ShelleyKey)
testCpSeq = snd $ initWallet block0 genesisParameters initDummyStateSeq

initDummyStateSeq :: SeqState 'Testnet ShelleyKey
initDummyStateSeq = mkSeqState (xprv, mempty) defaultAddressPoolGap
  where
      bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
      xprv = Seq.generateKeyFromSeed (Passphrase bytes, mempty) mempty

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
