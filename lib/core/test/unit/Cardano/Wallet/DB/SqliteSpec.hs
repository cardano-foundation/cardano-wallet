{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrWalletAlreadyExists (..), PrimaryKey (..), cleanDB )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, PersistTx, newDBLayer, withDBLayer )
import Cardano.Wallet.DB.StateMachine
    ( prop_parallel, prop_sequential )
import Cardano.Wallet.DBSpec
    ( dbPropertyTests, withDB )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..), PersistKey, encryptPassphrase )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..) )
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
    , Block (..)
    , BlockHeader (..)
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
    , slotMinBound
    , wholeRange
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( replicateM_, unless )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
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
    ( withSystemTempFile )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Hspec
    ( Expectation
    , Spec
    , SpecWith
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

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.Aggregated as CM
import qualified Cardano.BM.Data.AggregatedKind as CM
import qualified Cardano.BM.Data.Backend as CM
import qualified Cardano.BM.Data.SubTrace as CM
import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T

spec :: Spec
spec = do
    sqliteSpec
    sqliteSpecRnd
    loggingSpec
    connectionSpec

sqliteSpec :: Spec
sqliteSpec = withDB (fst <$> newMemoryDBLayer) $ do
    describe "Sqlite Simple tests (SeqState)" $
        simpleSpec testCpSeq
    describe "Sqlite" dbPropertyTests
    describe "Sqlite State machine tests" $ do
        it "Sequential" prop_sequential
        xit "Parallel" prop_parallel

sqliteSpecRnd :: Spec
sqliteSpecRnd =
    withDB (fst <$> newMemoryDBLayer @(RndState DummyTarget) @DummyTarget @RndKey) $ do
        describe "Sqlite simple (RndState)" $
            simpleSpec testCpRnd
        describe "Sqlite State machine (RndState)" $ do
            it "Sequential state machine tests" prop_sequential

simpleSpec
    :: forall s k.
       ( Show (k 'RootK XPrv)
       , Eq (k 'RootK XPrv)
       , Eq s
       , GenerateTestKey k )
    => Wallet s DummyTarget
    -> SpecWith (DBLayer IO s DummyTarget k)
simpleSpec testCp = do
    describe "Wallet table" $ do
        it "create and list works" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            listWallets db `shouldReturn` [testPk]

        it "create and get meta works" $ \db -> do
            now <- getCurrentTime
            let md = testMetadata
                    { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testPk testCp md
            readWalletMeta db testPk `shouldReturn` Just md

        it "create twice is handled" $ \db -> do
            let create' = createWallet db testPk testCp testMetadata
            runExceptT create' `shouldReturn` (Right ())
            runExceptT create' `shouldReturn`
                (Left (ErrWalletAlreadyExists testWid))

        it "create and get private key" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            readPrivateKey db testPk `shouldReturn` Nothing
            (k, h) <- generateTestKey
            unsafeRunExceptT (putPrivateKey db testPk (k, h))
            readPrivateKey db testPk `shouldReturn` Just (k, h)

        let sortOrder = Descending
        it "put and read tx history" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            runExceptT (putTxHistory db testPk (Map.fromList testTxs))
                `shouldReturn` Right ()
            readTxHistory db testPk
                sortOrder wholeRange `shouldReturn` testTxs

        it "put and read tx history - regression case" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ createWallet db testPk1 testCp testMetadata
            runExceptT (putTxHistory db testPk1 (Map.fromList testTxs))
                `shouldReturn` Right ()
            runExceptT (removeWallet db testPk) `shouldReturn` Right ()
            readTxHistory db testPk1
                sortOrder wholeRange `shouldReturn` testTxs

        it "put and read checkpoint" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            runExceptT (putCheckpoint db testPk testCp) `shouldReturn` Right ()
            readCheckpoint db testPk `shouldReturn` Just testCp

loggingSpec :: Spec
loggingSpec = withLoggingDB $ do
    describe "Sqlite query logging" $ do
        it "should log queries at DEBUG level" $ \(db, getLogs) -> do
            unsafeRunExceptT $ createWallet db testPk testCpSeq testMetadata
            logs <- logMessages <$> getLogs
            logs `shouldHaveLog` (Debug, "INSERT")

        it "should not log query parameters" $ \(db, getLogs) -> do
            unsafeRunExceptT $ createWallet db testPk testCpSeq testMetadata
            let walletName = T.unpack $ coerce $ name testMetadata
            msgs <- T.unlines . map snd . logMessages <$> getLogs
            T.unpack msgs `shouldNotContain` walletName

    describe "Sqlite observables" $ do
        it "should measure query timings" $ \(db, getLogs) -> do
            let count = 5
            replicateM_ count $ listWallets db
            -- Commented out until this is fixed:
            --   https://github.com/input-output-hk/iohk-monitoring-framework/issues/391
            -- msg <- T.unlines . map snd . logMessages <$> getLogs
            -- T.unpack msgs `shouldContain` "runQuery monitor works"
            msgs <- findObserveDiffs <$> getLogs
            length msgs `shouldBe` count

connectionSpec :: Spec
connectionSpec = describe "Sqlite database file" $ do
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

newMemoryDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => IO (DBLayer IO s t k, TVar [LogObject Text])
newMemoryDBLayer = do
    logConfig <- testingLogConfig
    logs <- newTVarIO []
    db <- snd <$> newDBLayer logConfig (traceInTVarIO logs) Nothing
    pure (db, logs)

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

withLoggingDB :: SpecWith (DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey, IO [LogObject Text]) -> Spec
withLoggingDB = beforeAll newMemoryDBLayer . beforeWith clean
  where
    clean (db, logs) = do
        cleanDB db
        atomically $ writeTVar logs []
        pure (db, readTVarIO logs)

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
                                   Test data
-------------------------------------------------------------------------------}

initDummyBlock0 :: Block Tx
initDummyBlock0 = Block
    { header = BlockHeader
        { slotId = slotMinBound
        , prevBlockHash = Hash "genesis"
        }
    , transactions = []
    }

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
    [ (Hash "tx2"
      , (Tx [TxIn (Hash "tx1") 0] [TxOut (Address "addr") (Coin 1)]
        , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 1337144))) ]

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
testCpSeq = initWallet initDummyBlock0 initDummyStateSeq

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
testCpRnd = initWallet initDummyBlock0 initDummyStateRnd
