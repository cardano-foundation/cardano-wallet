{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ( PersistState
    , PersistTx
    , SqliteContext
    , newDBLayer
    , unsafeRunQuery
    , withDBLayer
    )
import Cardano.Wallet.DB.Sqlite.TH
    ( Checkpoint (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.DB.StateMachine
    ( prop_parallel, prop_sequential )
import Cardano.Wallet.DBSpec
    ( dbPropertyTests, withDB )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0 )
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
    ( Wallet
    , blockHeight
    , currentTip
    , getPending
    , getState
    , initWallet
    , unsafeInitWallet
    , utxo
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
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
    , UTxO (..)
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
    ( replicateM_, unless )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( isNothing, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import Data.Time.Clock
    ( getCurrentTime )
import Database.Persist
    ( Entity, entityVal, selectList )
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
import qualified Cardano.Wallet.DB.Sqlite.TH as TH
import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: Spec
spec = do
    sqliteSpec
    sqliteSpecRnd
    loggingSpec
    connectionSpec
    sqliteDetailedSeqSpec
    sqliteDetailedRndSpec

sqliteDetailedSeqSpec :: Spec
sqliteDetailedSeqSpec = withDBCtx newDBSeqCtx $ do
    describe "Sqlite multiple-checkpoints test (SeqState)" $
        multipleCheckpointsSpec testCpSeq

sqliteDetailedRndSpec :: Spec
sqliteDetailedRndSpec = withDBCtx newDBRndCtx $ do
    describe "Sqlite multiple-checkpoints test (RndState)" $
        multipleCheckpointsSpec testCpRnd

sqliteSpec :: Spec
sqliteSpec = withDB newDBSeq $ do
    describe "Sqlite Simple tests (SeqState)" $
        simpleSpec testCpSeq
    describe "Sqlite" dbPropertyTests
    describe "Sqlite State machine tests" $ do
        it "Sequential" prop_sequential
        xit "Parallel" prop_parallel

sqliteSpecRnd :: Spec
sqliteSpecRnd =
    withDB newDBRnd $ do
        describe "Sqlite simple (RndState)" $
            simpleSpec testCpRnd
        describe "Sqlite State machine (RndState)" $ do
            it "Sequential state machine tests" prop_sequential

withDBCtx
    :: IO (SqliteContext, DBLayer IO s t k)
    -> SpecWith (SqliteContext, DBLayer IO s t k)
    -> Spec
withDBCtx create = beforeAll create . beforeWith
    (\(ctx, db) -> cleanDB db $> (ctx, db))

multipleCheckpointsSpec
    :: forall s k.
       ( Show (k 'RootK XPrv)
       , Eq (k 'RootK XPrv)
       , Eq s
       , IsOurs s
       , NFData s
       , Show s
       , GenerateTestKey k )
    => Wallet s DummyTarget
    -> SpecWith (SqliteContext, DBLayer IO s DummyTarget k)
multipleCheckpointsSpec initialState = do
    describe "put and read checkpoints" $ do
        it "previous checkpoints are also stored" $ \(ctx,db) -> do
            createWalletAndFirstCheckpoint db testPk

            -- the second checkpoint is to be persisted
            let nextSlotId = (SlotId 0 1)
            let utxos = UTxO Map.empty
            cp2 <- putUpdatedCheckpoint db testPk initialState nextSlotId utxos Set.empty
            runExceptT (putCheckpoint db testPk cp2) `shouldReturn` Right ()
            cps <- unsafeRunQuery ctx $ do
                cps :: [Entity Checkpoint] <- selectList [][]
                return $ fmap entityVal cps
            readCheckpoint db testPk `shouldReturn` Just cp2
            map (\(Checkpoint _ slot (BlockId prev) height) ->
                     (slot, prev, height)) cps
                `shouldBe` map getTriple [initialState,cp2]
    describe "put and read tx history" $ do
        it "checking accumulating 2 incoming transactions" $ \(ctx,db) -> do
            createWalletAndFirstCheckpoint db testPk

            let slotNum1 = 1
            let nextSlotId1 = (SlotId 0 slotNum1)
            let txid1 = Hash "tx1"
            let addr1 = Address "addrDest1"
            let ix1 = 0
            let txin1 = TxIn txid1 ix1
            let amt1 = Coin 1
            let txout1 = TxOut addr1 amt1
            let expectedTx1 = (txin1, txout1)
            (cp1, txs1) <-
                acceptIncomingTx (ctx,db) testPk initialState slotNum1
                amt1 txid1 addr1 ix1 1
            let sortOrder = Descending
            readTxHistory db testPk
                sortOrder wholeRange `shouldReturn` txs1
            utxo cp1 `shouldBe` (UTxO $ Map.fromList [expectedTx1])

            --now the next block is applied but has nothing inside for this wallet
            let slotNum2 = 2
            let nextSlotId2 = (SlotId 0 slotNum2)
            let utxos1 = UTxO $ Map.fromList [expectedTx1]
            cp2 <- putUpdatedCheckpoint db testPk cp1 nextSlotId2 utxos1 Set.empty

            -- Transaction number 2 comes and the same flow as above takes place
            let slotNum3 = 3
            let nextSlotId3 = (SlotId 0 slotNum3)
            let txid2 = Hash "tx2"
            let addr2 = Address "addrDest2"
            let ix2 = 1
            let txin2 = TxIn txid2 ix2
            let amt2 = Coin 10
            let txout2 = TxOut addr2 amt2
            let expectedTx2 = (txin2, txout2)
            let utxos2 = UTxO $ Map.fromList [expectedTx1, expectedTx2]
            cp3 <- putUpdatedCheckpoint db testPk cp2 nextSlotId3 utxos2 Set.empty
            utxo cp3 `shouldBe` (UTxO $ Map.fromList [expectedTx1, expectedTx2])
            utxos3 <- unsafeRunQuery ctx $ do
                utxos :: [Entity TH.UTxO] <- selectList [][]
                return $ fmap entityVal utxos
            let getNotSpent =
                    filter (\(TH.UTxO _ _ spent _ _ _ _) -> isNothing spent)
            map TH.utxoSlotSpent (getNotSpent utxos3)
                `shouldBe` [Nothing, Nothing]
            map TH.utxoSlot (getNotSpent utxos3)
                `shouldBe` [nextSlotId1, nextSlotId3]

            --check all checkpoints stored in db
            cps <- unsafeRunQuery ctx $ do
                cps :: [Entity Checkpoint] <- selectList [][]
                return $ fmap entityVal cps
            length cps `shouldBe` 4

        it "checking incoming and outgoing transaction" $ \(ctx,db) -> do
            createWalletAndFirstCheckpoint db testPk
            createWalletAndFirstCheckpoint db testPk1

            let slotNum1 = 1
            let nextSlotId1 = (SlotId 0 slotNum1)
            let txid1 = Hash "tx1"
            let addr1 = Address "addrSrc1"
            let amt1 = Coin 100
            let ix1 = 0
            (cp1,txs1) <-
                acceptIncomingTx (ctx,db) testPk initialState slotNum1
                amt1 txid1 addr1 ix1 1
            let sortOrder = Descending
            readTxHistory db testPk
                sortOrder wholeRange `shouldReturn` txs1

            -- Transaction number 2 is sent
            -- What happens here can be composed in two pieces
            -- I. (a) when signTx is called putCheckpoint with additional change
            --        address and the current slot is put - this step will be
            --        omitted in current test
            --    (b) when submitTx is called checkpoint is updated with pending
            --        tx added to metas
            -- II. applyBlocks is called and the state is updated
            --     which affect utxo, state slot is altered, and tx history
            --     changed. In order to emulate it three things will happen:
            --    (a) checkpoint with new slot will be put
            --    (b) putTxHistory will update tx history
            --    (c) used utxo will be given proper slot-spent
            --    (d) new row of metas and new slot id will be added
            --
            -- At this moment one wallet has 100 in one of its utxos
            -- and putting new checkpoint at this moment
            let slotNum2 = 2
            let nextSlotId2 = (SlotId 0 slotNum2)
            let utxos1 = UTxO $ Map.fromList [(TxIn txid1 ix1, TxOut addr1 amt1)]
            cp2 <- putUpdatedCheckpoint db testPk cp1 nextSlotId2 utxos1 Set.empty
            utxos2 <- unsafeRunQuery ctx $ do
                utxos :: [Entity TH.UTxO] <- selectList [][]
                return $ fmap entityVal utxos
            length utxos2 `shouldBe` 1

            -- now emulating submitTx
            let slotNum3 = 3
            let nextSlotId3 = (SlotId 0 slotNum3)
            let amtWithFee = Quantity 43
            let txin = TxIn txid1 ix1
            let txout1 = TxOut (Address "addrSrc2") (Coin 57)
            let txout2 = TxOut (Address "addrDest1") (Coin 1)
            let pendingTx = ( Tx [txin] [txout1, txout2]
                            , TxMeta Pending Outgoing nextSlotId3 amtWithFee
                            )
            let pendings = Set.fromList [pendingTx]
            cp3 <- putUpdatedCheckpoint db testPk cp2 nextSlotId3 utxos1 pendings
            utxos3 <- unsafeRunQuery ctx $ do
                utxos :: [Entity TH.UTxO] <- selectList [][]
                return $ fmap entityVal utxos
            length utxos3 `shouldBe` 1
            metas <- unsafeRunQuery ctx $ do
                metas :: [Entity TH.TxMeta] <- selectList [][]
                return $ fmap entityVal metas
            length metas `shouldBe` 2

            -- finally at next slot block is applied to both wallets
            let txid2 = Hash "tx2"
            let slotNum4 = 4
            let nextSlotId4 = (SlotId 0 slotNum4)
            -- when block is applied new utxo is used
            let utxosNew = UTxO $ Map.fromList [(TxIn txid2 0, txout1)]
            _cp4 <- putUpdatedCheckpoint db testPk cp3 nextSlotId4 utxosNew Set.empty
            -- at this moment we have still all 100 UTxOs in db with Nothing
            utxos4 <- unsafeRunQuery ctx $ do
                utxos :: [Entity TH.UTxO] <- selectList [][]
                return $ fmap entityVal utxos
            length utxos4 `shouldBe` 2
            let getAll100utxos = getAllutxosOf 100
            length (getAll100utxos utxos4)
                `shouldBe` 1
            map TH.utxoSlot (getAll100utxos  utxos4)
                `shouldBe` [nextSlotId1]
            map TH.utxoSlotSpent (getAll100utxos utxos4)
                `shouldBe` [Nothing]

            let txs2 =
                    [ (txid2
                      , ( Tx [txin] [txout1, txout2]
                        , TxMeta InLedger Outgoing nextSlotId4 amtWithFee
                        )
                      )
                    ]
            runExceptT (putTxHistory db testPk (Map.fromList txs2))
                `shouldReturn` Right ()
            let amtReceived = Quantity 1
            let txs2' =
                    [ (txid2
                      , ( Tx [txin] [txout1, txout2]
                        , TxMeta InLedger Incoming nextSlotId4 amtReceived
                        )
                      )
                    ]
            runExceptT (putTxHistory db testPk1 (Map.fromList txs2'))
                `shouldReturn` Right ()
            allTxs <- readTxHistory db testPk sortOrder wholeRange
            filter (\(_,(_, TxMeta st _ _ _)) -> st == InLedger) allTxs
                `shouldBe` (txs2++txs1)
            readTxHistory db testPk1
                sortOrder wholeRange `shouldReturn` txs2'
            -- at this moment we should have three 100 UTxOs in db with Nothing
            -- and one additional 100 UTxO with slotSpent=(Just nextSlotId4)
            utxos5 <- unsafeRunQuery ctx $ do
                utxos :: [Entity TH.UTxO] <- selectList [][]
                return $ fmap entityVal utxos
            length utxos5 `shouldBe` 2
            length (getAll100utxos utxos5)
                `shouldBe` 1
            map TH.utxoSlot (getAll100utxos utxos5)
                `shouldBe` [nextSlotId1]
            map TH.utxoSlotSpent (getAll100utxos utxos5)
                `shouldBe` [(Just nextSlotId4)]
  where
      getTriple cp =
          let (BlockHeader s h) = currentTip cp
          in (s, h, fromIntegral $ fromQuantity $ blockHeight cp)
      fromQuantity (Quantity a) = a
      getAllutxosOf v =
          filter (\(TH.UTxO _ _ _ _ _ _ amt) -> amt == (Coin v))
      createWalletAndFirstCheckpoint db widKey = do
          -- first checkpoint is to be stored upon wallet creation
          -- there are no utxos available
          unsafeRunExceptT $ createWallet db widKey initialState testMetadata
          readCheckpoint db widKey `shouldReturn` Just initialState
          utxo initialState `shouldBe` (UTxO Map.empty)
      putUpdatedCheckpoint db widKey cp' nextSlotId utxos pendings = do
          let (Quantity blockHeight') = blockHeight cp'
          let (BlockHeader _ blockHash') = currentTip cp'
          let state' = getState cp'
          -- when block is applied new utxo is used
          let cp = unsafeInitWallet
                   utxos pendings
                   (BlockHeader nextSlotId blockHash') state'
                   (Quantity $ blockHeight' + 1)
          runExceptT (putCheckpoint db widKey cp) `shouldReturn` Right ()
          return cp
      acceptIncomingTx (ctx,db) widKey cp' slotNum amt txid addr ix expUtxo = do
          -- Outer transaction comes and gives us utxo
          -- applyBlocks is called and the state is updated
          -- which affect utxo, state slot is altered, and tx history changed
          -- In order to emulate it three things will happen:
          -- (a) checkpoint with new slot will be put
          -- (b) utxo will be inserted
          -- (c) putTxHistory will be called
          let (Quantity blockHeight') = blockHeight cp'
          let (BlockHeader _ blockHash') = currentTip cp'
          let nextSlotId = (SlotId 0 slotNum)
          let state' = getState cp'
          let utxos = utxo cp'
          let pendings = getPending cp'
          let cp = unsafeInitWallet
                   utxos pendings
                   (BlockHeader nextSlotId blockHash') state'
                   (Quantity $ blockHeight' + 1)
          runExceptT (putCheckpoint db widKey cp) `shouldReturn` Right ()
          let txin = TxIn txid ix
          let txout = TxOut addr amt
          let expectedTx = (txin, txout)
          let utxosNew = UTxO $ Map.fromList [expectedTx]
          cp1 <- putUpdatedCheckpoint db widKey cp nextSlotId utxosNew Set.empty
          utxos' <- unsafeRunQuery ctx $ do
              utxos1 :: [Entity TH.UTxO] <- selectList [][]
              return $ fmap entityVal utxos1
          length utxos' `shouldBe` expUtxo
          (Just cp2) <- readCheckpoint db widKey
          cp1 `shouldBe` cp2
          -- change tx history with the same slot number accordingly
          let amt' = Quantity $ fromIntegral $ getCoin amt
          let txs =
                  [ (txid
                    , ( Tx [txin] [txout]
                      , TxMeta InLedger Incoming nextSlotId amt'
                      )
                    )
                  ]
          runExceptT (putTxHistory db widKey (Map.fromList txs))
              `shouldReturn` Right ()
          return (cp2, txs)

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
    => IO ((SqliteContext, DBLayer IO s t k), TVar [LogObject Text])
newMemoryDBLayer = do
    logConfig <- testingLogConfig
    logs <- newTVarIO []
    (ctx,db) <- newDBLayer logConfig (traceInTVarIO logs) Nothing
    pure ((ctx, db), logs)

-- | Type-specialised shorthand. Useful for testing in GHCi.
newDBSeqCtx :: IO (SqliteContext, DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey)
newDBSeqCtx = fst <$> newMemoryDBLayer

-- | Type-specialised shorthand. Useful for testing in GHCi.
newDBRndCtx :: IO (SqliteContext, DBLayer IO (RndState DummyTarget) DummyTarget RndKey)
newDBRndCtx = fst <$> newMemoryDBLayer

-- | Type-specialised shorthand. Useful for testing in GHCi.
newDBSeq :: IO (DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey)
newDBSeq = snd <$> newDBSeqCtx

-- | Type-specialised shorthand. Useful for testing in GHCi.
newDBRnd :: IO (DBLayer IO (RndState DummyTarget) DummyTarget RndKey)
newDBRnd = snd <$> newDBRndCtx

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
    clean ((_,db), logs) = do
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
testCpSeq = initWallet block0 initDummyStateSeq

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
testCpRnd = initWallet block0 initDummyStateRnd
