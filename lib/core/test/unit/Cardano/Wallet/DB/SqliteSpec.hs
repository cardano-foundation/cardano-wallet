{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
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
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( KeyValPairs (..) )
import Cardano.Wallet.DB.Properties
    ( properties, withDB )
import Cardano.Wallet.DB.Sqlite
    ( PersistState
    , PersistTx
    , SqliteContext
    , destroyDBLayer
    , newDBLayer
    , withDBLayer
    )
import Cardano.Wallet.DB.StateMachine
    ( prop_parallel, prop_sequential )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0, genesisParameters )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..), PersistKey, encryptPassphrase )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..), mkRndState )
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
    , SlotId (..)
    , SortOrder (..)
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
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
    ( ExceptT, runExceptT )
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
    ( TVar, atomically, newTVarIO, readTVarIO, writeTVar )
import System.Directory
    ( doesFileExist, removeFile )
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
import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: Spec
spec = do
    sqliteSpecSeq
    sqliteSpecRnd
    loggingSpec
    fileModeSpec

sqliteSpecSeq :: Spec
sqliteSpecSeq = withDB newMemoryDBLayer $ do
    describe "Sqlite Simple tests (SeqState)" $ simpleSpec testCpSeq
    describe "Sqlite" properties
    describe "Sqlite State machine tests" $ do
        it "Sequential" (prop_sequential :: TestDBSeq -> Property)
        xit "Parallel" prop_parallel

sqliteSpecRnd :: Spec
sqliteSpecRnd = withDB newMemoryDBLayer $ do
        describe "Sqlite simple (RndState)" $ simpleSpec testCpRnd
        describe "Sqlite State machine (RndState)" $ do
            it "Sequential state machine tests"
                (prop_sequential :: TestDBRnd -> Property)

{-------------------------------------------------------------------------------
                                 Simple Specs
-------------------------------------------------------------------------------}

simpleSpec
    :: forall s k.
       ( Show (k 'RootK XPrv)
       , Eq (k 'RootK XPrv)
       , Eq s
       , GenerateTestKey k )
    => Wallet s DummyTarget
    -> SpecWith (DBLayer IO s DummyTarget k)
simpleSpec cp = do
    describe "Wallet table" $ do
        it "create and list works" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk cp testMetadata
            listWallets db `shouldReturn` [testPk]

        it "create and get meta works" $ \db -> do
            now <- getCurrentTime
            let md = testMetadata
                    { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testPk cp md
            readWalletMeta db testPk `shouldReturn` Just md

        it "create twice is handled" $ \db -> do
            let create' = createWallet db testPk cp testMetadata
            runExceptT create' `shouldReturn` (Right ())
            runExceptT create' `shouldReturn`
                (Left (ErrWalletAlreadyExists testWid))

        it "create and get private key" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk cp testMetadata
            readPrivateKey db testPk `shouldReturn` Nothing
            (k, h) <- generateTestKey
            unsafeRunExceptT (putPrivateKey db testPk (k, h))
            readPrivateKey db testPk `shouldReturn` Just (k, h)

        let sortOrder = Descending
        it "put and read tx history" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk cp testMetadata
            runExceptT (putTxHistory db testPk (Map.fromList testTxs))
                `shouldReturn` Right ()
            readTxHistory db testPk
                sortOrder wholeRange `shouldReturn` testTxs

        it "put and read tx history - regression case" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk cp testMetadata
            unsafeRunExceptT $ createWallet db testPk1 cp testMetadata
            runExceptT (putTxHistory db testPk1 (Map.fromList testTxs))
                `shouldReturn` Right ()
            runExceptT (removeWallet db testPk) `shouldReturn` Right ()
            readTxHistory db testPk1
                sortOrder wholeRange `shouldReturn` testTxs

        it "put and read checkpoint" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk cp testMetadata
            runExceptT (putCheckpoint db testPk cp) `shouldReturn` Right ()
            readCheckpoint db testPk `shouldReturn` Just cp

{-------------------------------------------------------------------------------
                                Logging Spec
-------------------------------------------------------------------------------}

loggingSpec :: Spec
loggingSpec = withLoggingDB @(SeqState DummyTarget) @DummyTarget @SeqKey $ do
    describe "Sqlite query logging" $ do
        it "should log queries at DEBUG level" $ \(getLogs, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCpSeq testMetadata
            logs <- logMessages <$> getLogs
            logs `shouldHaveLog` (Debug, "INSERT")

        it "should not log query parameters" $ \(getLogs, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCpSeq testMetadata
            let walletName = T.unpack $ coerce $ name testMetadata
            msgs <- T.unlines . map snd . logMessages <$> getLogs
            T.unpack msgs `shouldNotContain` walletName

    describe "Sqlite observables" $ do
        it "should measure query timings" $ \(getLogs, db) -> do
            let count = 5
            replicateM_ count $ listWallets db
            -- Commented out until this is fixed:
            --   https://github.com/input-output-hk/iohk-monitoring-framework/issues/391
            -- msg <- T.unlines . map snd . logMessages <$> getLogs
            -- T.unpack msgs `shouldContain` "runQuery monitor works"
            msgs <- findObserveDiffs <$> getLogs
            length msgs `shouldBe` count

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => IO (DBLayer IO s t k)
newMemoryDBLayer = snd . snd <$> newMemoryDBLayer'

newMemoryDBLayer'
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => IO (TVar [LogObject Text], (SqliteContext, DBLayer IO s t k))
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
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => SpecWith (IO [LogObject Text], DBLayer IO s t k)
    -> Spec
withLoggingDB = beforeAll newMemoryDBLayer' . beforeWith clean
  where
    clean (logs, (_, db)) = do
        cleanDB db
        atomically $ writeTVar logs []
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

type TestDBSeq = DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey
type TestDBRnd = DBLayer IO (RndState DummyTarget) DummyTarget RndKey

fileModeSpec :: Spec
fileModeSpec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $ do
                db <- Just <$> temporaryDBFile
                (ctx, _) <- newDBLayer' @(SeqState DummyTarget) @DummyTarget db
                destroyDBLayer ctx

    describe "Sqlite database file" $ do
        let writeSomething db = do
                unsafeRunExceptT $ createWallet db testPk testCpSeq testMetadata
                listWallets db `shouldReturn` [testPk]
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
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            destroyDBLayer ctx
            testOpeningCleaning f listWallets [testPk] []

        it "create and get meta works" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            now <- getCurrentTime
            let meta = testMetadata
                   { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testPk testCp meta
            destroyDBLayer ctx
            testOpeningCleaning f (`readWalletMeta` testPk) (Just meta) Nothing

        it "create and get private key" $ \f-> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            (k, h) <- unsafeRunExceptT $ attachPrivateKey db testPk
            destroyDBLayer ctx
            testOpeningCleaning f (`readPrivateKey` testPk) (Just (k, h)) Nothing

        it "put and read tx history (Ascending)" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ putTxHistory db testPk (Map.fromList testTxs)
            destroyDBLayer ctx
            testOpeningCleaning
                f
                (\db' -> readTxHistory db' testPk Ascending wholeRange)
                testTxs
                mempty

        it "put and read tx history (Decending)" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ putTxHistory db testPk (Map.fromList testTxs)
            destroyDBLayer ctx
            testOpeningCleaning
                f
                (\db' -> readTxHistory db' testPk Descending wholeRange)
                testTxs
                mempty

        it "put and read checkpoint" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ putCheckpoint db testPk testCp
            destroyDBLayer ctx
            testOpeningCleaning f (`readCheckpoint` testPk) (Just testCp) Nothing

    describe "random operation chunks property" $ do
        it "realize a random batch of operations upon one db open"
            (property $ prop_randomOpChunks @(SeqState DummyTarget) @DummyTarget)

-- This property checks that executing series of wallet operations in a single
-- SQLite session has the same effect as executing the same operations over
-- multiple sessions.
prop_randomOpChunks
    :: (Eq s, IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => KeyValPairs (PrimaryKey WalletId) (Wallet s t, WalletMetadata)
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
        :: DBLayer IO s t k
        -> (PrimaryKey WalletId, (Wallet s t, WalletMetadata))
        -> IO ()
    insertPair db (k, (cp, meta)) = do
        keys <- listWallets db
        if k `elem` keys then do
            unsafeRunExceptT $ putCheckpoint db k cp
            unsafeRunExceptT $ putWalletMeta db k meta
        else do
            unsafeRunExceptT $ createWallet db k cp meta
            Set.fromList <$> listWallets db `shouldReturn` Set.fromList (k:keys)

    shouldBeConsistentWith :: (Eq s) => DBLayer IO s t k -> DBLayer IO s t k -> IO ()
    shouldBeConsistentWith db1 db2 = do
        expectedWalIds <- Set.fromList <$> listWallets db1
        Set.fromList <$> listWallets db2
            `shouldReturn` expectedWalIds
        forM_ expectedWalIds $ \walId -> do
            expectedCps <- readCheckpoint db1 walId
            readCheckpoint db2 walId
                `shouldReturn` expectedCps
        forM_ expectedWalIds $ \walId -> do
            expectedMetas <- readWalletMeta db1 walId
            readWalletMeta db2 walId
                `shouldReturn` expectedMetas

-- | Test that data is preserved between open / close of the same database and
-- that cleaning up happens as expected.
testOpeningCleaning
    :: (Show s, Eq s)
    => FilePath
    -> (DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey -> IO s)
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
    :: (DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey -> IO ())
    -> (FilePath -> IO a)
    -> IO a
withTestDBFile action expectations = do
    logConfig <- defaultConfigTesting
    trace <- setupTrace (Right logConfig) "connectionSpec"
    withSystemTempFile "spec.db" $ \fp _handle -> do
        removeFile fp
        withDBLayer logConfig trace (Just fp) action
        expectations fp

inMemoryDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => IO (SqliteContext, DBLayer IO s t SeqKey)
inMemoryDBLayer = newDBLayer' Nothing

temporaryDBFile :: IO FilePath
temporaryDBFile = emptySystemTempFile "cardano-wallet-SqliteFileMode"

newDBLayer'
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => Maybe FilePath
    -> IO (SqliteContext, DBLayer IO s t SeqKey)
newDBLayer' fp = do
    logConfig <- CM.empty
    newDBLayer logConfig nullTracer fp

-- | Clean the database
cleanDB'
    :: Monad m
    => (SqliteContext, DBLayer m s t k)
    -> m (SqliteContext, DBLayer m s t k)
cleanDB' (ctx, db) =
    cleanDB db $> (ctx, db)

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s t SeqKey
    -> PrimaryKey WalletId
    -> ExceptT ErrNoSuchWallet IO (SeqKey 'RootK XPrv, Hash "encryption")
attachPrivateKey db wid = do
    let Right pwd = fromText "simplevalidphrase"
    let k = generateKeyFromSeed (coerce pwd, coerce pwd) pwd
    h <- liftIO $ encryptPassphrase pwd
    putPrivateKey db wid (k, h)
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

testCp :: Wallet (SeqState DummyTarget) DummyTarget
testCp = initWallet block0 genesisParameters initDummyState
  where
    initDummyState :: SeqState DummyTarget
    initDummyState = mkSeqState (xprv, mempty) defaultAddressPoolGap
      where
        bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
        xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , creationTime = unsafePerformIO getCurrentTime
    , passphraseInfo = Nothing
    , status = Ready
    , delegation = NotDelegating
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testWid1 :: WalletId
testWid1 = WalletId (hash ("test1" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

testPk1 :: PrimaryKey WalletId
testPk1 = PrimaryKey testWid1

testTxs :: [(Hash "Tx", (Tx, TxMeta))]
testTxs =
    [ (Hash "tx2", (Tx [TxIn (Hash "tx1") 0] [TxOut (Address "addr") (Coin 1)]
      , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 1337144))
      )
    ]

testPassphraseAndHash :: IO (Passphrase "encryption", Hash "encryption")
testPassphraseAndHash = do
    let Right phr = fromText "abcdefghijklmnop"
    h <- encryptPassphrase phr
    pure (phr, h)

class GenerateTestKey (key :: Depth -> * -> *) where
    generateTestKey :: IO (key 'RootK XPrv, Hash "encryption")

{-------------------------------------------------------------------------------
                           Test data - Sequential AD
-------------------------------------------------------------------------------}

testCpSeq :: Wallet (SeqState DummyTarget) DummyTarget
testCpSeq = initWallet block0 genesisParameters initDummyStateSeq

initDummyStateSeq :: SeqState DummyTarget
initDummyStateSeq = mkSeqState (xprv, mempty) defaultAddressPoolGap
  where
      bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
      xprv = Seq.generateKeyFromSeed (Passphrase bytes, mempty) mempty

instance GenerateTestKey SeqKey where
    generateTestKey = do
        (phr, h) <- testPassphraseAndHash
        pure (Seq.unsafeGenerateKeyFromSeed (coerce phr, coerce phr) phr, h)

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

instance GenerateTestKey RndKey where
    generateTestKey = do
        (phr, h) <- testPassphraseAndHash
        pure (Rnd.generateKeyFromSeed (coerce phr) phr, h)

{-# NOINLINE initDummyStateRnd #-}
initDummyStateRnd :: RndState DummyTarget
initDummyStateRnd = mkRndState xprv 0
    where xprv = fst $ unsafePerformIO generateTestKey

testCpRnd :: Wallet (RndState DummyTarget) DummyTarget
testCpRnd = initWallet block0 genesisParameters initDummyStateRnd
