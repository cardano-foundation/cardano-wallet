{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This benchmark gathers timings of the important database operations, for
-- various sizes of input. The chosen sizes are meant to approach and slightly
-- exceed realistic maximum values for a wallet.
--
-- For example, the number of inputs and outputs of a single transaction is
-- limited by the maximum transaction size. So we don't need to benchmark
-- transactions larger than that. However we could benchmark writing many large
-- transactions.
--
-- Additionally, the highest number of checkpoints that a wallet could ever want
-- is k=2160. However, there could be multiple wallets in the database, and each
-- checkpoint could have a large (fragmented) UTxO.
--
-- The coin selection algorithm attempts to minimize UTxO size, however a small
-- UTxO can't be taken for granted.
--
-- Smaller input sizes are also timed so that we can check that whether
-- operations are accidentally quadratic.
--
-- An important limit is SQLITE_MAX_VARIABLE_NUMBER, which defaults to 999. If
-- there are database statements with more parameters than that, the DBLayer
-- will throw exceptions.

import Prelude

import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.DB.Sqlite
    ( SqliteContext, destroyDBLayer )
import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..), cleanDB )
import Cardano.Wallet.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0, genesisParameters )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), KeyToAddress (..), Passphrase (..), WalletKey (..), XPub )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..), generateKeyFromSeed, unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , SeqState (..)
    , defaultAddressPoolGap
    , emptyPendingIxs
    , mkAddressPool
    , mkSeqState
    )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet, unsafeInitWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , EpochLength (..)
    , Hash (..)
    , Range (..)
    , SlotId (..)
    , SlotNo (unSlotNo)
    , SortOrder (..)
    , SyncProgress (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , fromFlatSlot
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData (..), force )
import Control.Exception
    ( bracket )
import Control.Monad
    ( forM_ )
import Criterion.Main
    ( Benchmark
    , Benchmarkable
    , bench
    , bgroup
    , defaultMain
    , env
    , envWithCleanup
    , perRunEnv
    , whnfIO
    )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.List.Split
    ( chunksOf )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.System
    ( SystemTime (..), systemToUTCTime )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word64 )
import Fmt
    ( build, padLeftF, padRightF, pretty, (+|), (|+) )
import System.Directory
    ( doesFileExist, getFileSize, removeFile )
import System.FilePath
    ( takeFileName )
import System.IO.Temp
    ( emptySystemTempFile )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    defaultMain
        [ withDB bgroupWriteUTxO
        , withDB bgroupReadUTxO
        , withDB bgroupSeqState
        , withDB bgroupWriteTxHistory
        , withDB bgroupReadTxHistory
        ]
    putStrLn "\n--"
    utxoDiskSpaceTests
    txHistoryDiskSpaceTests

----------------------------------------------------------------------------
-- UTxO Benchmarks
--
-- The very max number of checkpoints we are likely to insert per wallet
-- is k=2160.
--
-- Currently the DBLayer will only store a single checkpoint (no rollback), so
-- the #Checkpoints axis is a bit meaningless.
bgroupWriteUTxO :: DBLayerBench -> Benchmark
bgroupWriteUTxO db = bgroup "UTxO (Write)"
    -- A fragmented wallet will have a large number of UTxO. The coin
    -- selection algorithm tries to prevent fragmentation.
    --
    --      #Checkpoints   UTxO Size
    [ bUTxO            1           0
    , bUTxO          100           0
    , bUTxO         1000           0
    , bUTxO           10          10
    , bUTxO          100          10
    , bUTxO         1000          10
    , bUTxO           10         100
    , bUTxO          100         100
    , bUTxO         1000         100
    , bUTxO           10        1000
    , bUTxO          100        1000
    , bUTxO         1000        1000
    , bUTxO            1       10000
    ]
  where
    bUTxO n s = bench lbl $ withCleanDB db $ benchPutUTxO n s
        where lbl = n|+" CP x "+|s|+" UTxO"

bgroupReadUTxO :: DBLayerBench -> Benchmark
bgroupReadUTxO db = bgroup "UTxO (Read)"
    --      #Checkpoints   UTxO Size
    [ bUTxO           10         100
    , bUTxO          100         100
    , bUTxO         1000         100
    , bUTxO           10        1000
    , bUTxO          100        1000
    , bUTxO         1000        1000
    , bUTxO           10       10000
    , bUTxO          100       10000
    , bUTxO         1000       10000
    ]
  where
    bUTxO n s = withUTxO db n s $ bench lbl $ benchReadUTxO db
        where lbl = n|+" CP x "+|s|+" UTxO"

----------------------------------------------------------------------------
-- Wallet State (Sequential Scheme) Benchmarks
--
-- Currently the DBLayer will only store a single checkpoint (no rollback), so
-- the #Checkpoints axis is a bit meaningless.
bgroupSeqState :: DBLayerBench -> Benchmark
bgroupSeqState db = bgroup "SeqState"
    --      #Checkpoints  #Addresses
    [ bSeqState      100          10
    , bSeqState      100         100
    , bSeqState      100        1000
    , bSeqState     1000          10
    , bSeqState     1000         100
    ]
  where
    bSeqState n a = bench lbl $ withCleanDB db $ benchPutSeqState n a
        where lbl = n|+" CP x "+|a|+" addr"

----------------------------------------------------------------------------
-- Tx history Benchmarks
--
-- These benchmarks evaluate the time taken to insert many transactions in an
-- empty database for batches, and transaction sizes of different order of
-- magnitude.
--
-- The number of inputs and outputs of a transaction is limited by the maximum
-- transaction size. So we don't need to benchmark further than that.
--
-- In Byron, the transaction max size is currently of 8kb.
--
-- A transaction size is roughly:
--
-- - headers & CBOR overhead: ~10 bytes
-- - size of all the witnesses: 139 bytes * nInputs
-- - size of all inputs: 42-43 bytes * nInputs
-- - size of all outputs: 53-86 bytes * nOuts on mainnet
--
-- This means a transaction in Byron can't have more than (worst case):
--
-- - ~45 inputs
-- - ~155 outputs
--
-- In Shelley with Jörmungandr, we have a soft max size of 8Kb (64Kb hard).
--
-- A transaction size is roughly:
--
-- - headers & binary overhead: ~10 bytes
-- - size of all the witnesses: 64-128 byte * nInputs (depending on scheme)
-- - size of all inputs: 41 bytes * nInputs
-- - size of all outputs: 41 bytes * nOutputs
--
-- This means a transaction in Shelley can't have more than (worst case):
--
-- - ~78 inputs
-- - ~200 outputs
--
-- The numbers above are really hard boundaries but, it is unrealistic to
-- imagine a transaction with 1 inputs and 200 outputs because, the coin
-- selection algorithm always try to consider output independently. So for
-- 200 outputs, we know that 200 inputs (at least) would be necessary. There are
-- the case for other softwares of course, but we may consider the following
-- numbers as realistic benchmark higher bounds (in the worst case):
--
-- - 50 inputs
-- - 100 outputs
bgroupWriteTxHistory :: DBLayerBench -> Benchmark
bgroupWriteTxHistory db = bgroup "TxHistory (Write)"
    --              #NBatch  #BatchSize #NInputs #NOutputs  #SlotRange
    [ bTxHistory          1         100        1        1     [1..100]
    , bTxHistory          1        1000        1        1     [1..100]
    , bTxHistory         10          10        1        1     [1..100]
    , bTxHistory        100          10        1        1     [1..100]
    , bTxHistory          1         100       10       10     [1..100]
    , bTxHistory          1        1000       10       10    [1..1000]
    , bTxHistory          1       10000       10       10   [1..10000]
    , bTxHistory          1          50       50      100     [1..100]
    , bTxHistory          1         100       50      100     [1..100]
    , bTxHistory          1        1000       50      100     [1..100]
    , bTxHistory          1       10000       50      100     [1..100]
    ]
  where
    bTxHistory n s i o r =
        bench lbl $ withCleanDB db $ benchPutTxHistory n s i o r
      where
        lbl = n|+" x "+|s|+" w/ "+|i|+"i + "+|o|+"o ["+|inf|+".."+|sup|+"]"
        inf = head r
        sup = last r

bgroupReadTxHistory :: DBLayerBench -> Benchmark
bgroupReadTxHistory db = bgroup "TxHistory (Read)"
    --             #NTxs  #SlotRange  #SortOrder  #Status  #SearchRange
    [ bTxHistory    1000    [1..100]  Descending  Nothing  wholeRange
    , bTxHistory    1000    [1..100]   Ascending  Nothing  wholeRange
    , bTxHistory    1000   [1..1000]  Descending  Nothing  wholeRange
    , bTxHistory    1000    [1..100]  Descending  pending  wholeRange
    , bTxHistory    1000    [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000  [1..10000]  Descending  Nothing  (Just 42, Just 1337)
    , bTxHistory   10000    [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory   10000  [1..10000]  Descending  Nothing  (Just 42, Just 1337)
    ]
  where
    wholeRange = (Nothing, Nothing)
    pending = Just Pending
    bTxHistory n r o st s =
        withTxHistory db n r $ bench lbl $ benchReadTxHistory db o s st
      where
        lbl = unwords [show n, range, ord, mstatus, search]
        range = let inf = head r in let sup = last r in "["+|inf|+".."+|sup|+"]"
        ord = case o of Descending -> "DESC"; Ascending -> "ASC"
        mstatus = maybe "-" pretty st
        search = case s of
            (Nothing, Nothing) -> "*"
            (Just inf, Nothing) -> inf|+".."
            (Nothing, Just sup) -> ".."+|sup|+""
            (Just inf, Just sup) -> inf|+".."+|sup|+""

----------------------------------------------------------------------------
-- Criterion env functions for database setup

-- | Sets up a benchmark environment with the SQLite DBLayer using a file
-- database in a temporary location.
withDB :: (DBLayerBench -> Benchmark) -> Benchmark
withDB bm = envWithCleanup setupDB cleanupDB (\ ~(_, _, db) -> bm db)

setupDB :: IO (FilePath, SqliteContext, DBLayerBench)
setupDB = do
    logConfig <- CM.empty
    f <- emptySystemTempFile "bench.db"
    (ctx, db) <- newDBLayer logConfig nullTracer (Just f)
    pure (f, ctx, db)

cleanupDB :: (FilePath, SqliteContext, DBLayerBench) -> IO ()
cleanupDB (db, _, _) = mapM_ remove [db, db <> "-shm", db <> "-wal"]
  where
    remove f = doesFileExist f >>= \case
        True -> removeFile f
        False -> pure ()

-- | Cleans the database before running the benchmark.
-- It also cleans the database after running the benchmark. That is just to
-- exercise the delete functions.
withCleanDB
    :: NFData b
    => DBLayerBench
    -> (DBLayerBench -> IO b)
    -> Benchmarkable
withCleanDB db = perRunEnv (walletFixture db $> db)

walletFixture :: DBLayerBench -> IO ()
walletFixture db = do
    cleanDB db
    unsafeRunExceptT $ createWallet db testPk testCp testMetadata mempty

----------------------------------------------------------------------------
-- TxHistory benchmarks

benchPutTxHistory
    :: Int
    -> Int
    -> Int
    -> Int
    -> [Word64]
    -> DBLayerBench
    -> IO ()
benchPutTxHistory numBatches batchSize numInputs numOutputs range db = do
    let batches = mkTxHistory (numBatches*batchSize) numInputs numOutputs range
    unsafeRunExceptT $ forM_ (chunksOf batchSize batches) $ putTxHistory db testPk

benchReadTxHistory
    :: DBLayerBench
    -> SortOrder
    -> (Maybe Word64, Maybe Word64)
    -> Maybe TxStatus
    -> Benchmarkable
benchReadTxHistory db sortOrder (inf, sup) mstatus =
    whnfIO $ readTxHistory db testPk sortOrder range mstatus
  where
    range = Range
        (fromFlatSlot epochLength <$> inf)
        (fromFlatSlot epochLength <$> sup)

mkTxHistory :: Int -> Int -> Int -> [Word64] -> [(Tx, TxMeta)]
mkTxHistory numTx numInputs numOutputs range =
    [ ( force (Tx (mkInputs i numInputs) (mkOutputs i numOutputs))
      , force TxMeta
          { status = [InLedger, Pending, Invalidated] !! (i `mod` 3)
          , direction = Incoming
          , slotId = sl i
          , blockHeight = Quantity $ fromIntegral $ unSlotNo $ slotNumber $ sl i
          , amount = Quantity (fromIntegral numOutputs)
          }
      )
    | !i <- [1..numTx]
    ]
  where
    sl i = fromFlatSlot epochLength (range !! (i `mod` length range))

mkInputs :: Int -> Int -> [TxIn]
mkInputs prefix n =
    [force (TxIn (Hash (label lbl i)) (fromIntegral i)) | !i <- [1..n]]
  where
    lbl = show prefix <> "in"

mkOutputs :: Int -> Int -> [TxOut]
mkOutputs prefix n =
    [force (TxOut (Address (label lbl i)) (Coin 1)) | !i <- [1..n]]
  where
    lbl = show prefix <> "in"

withTxHistory :: DBLayerBench -> Int -> [Word64] -> Benchmark -> Benchmark
withTxHistory db bSize range = env setup . const
  where
    setup = do
        cleanDB db
        unsafeRunExceptT $ createWallet db testPk testCp testMetadata mempty
        let (nInps, nOuts) = (20, 20)
        let txs = force (mkTxHistory bSize nInps nOuts range)
        unsafeRunExceptT $ putTxHistory db testPk txs
        pure db

----------------------------------------------------------------------------
-- UTxO benchmarks

benchPutUTxO :: Int -> Int -> DBLayerBench -> IO ()
benchPutUTxO numCheckpoints utxoSize db = do
    let cps = mkCheckpoints numCheckpoints utxoSize
    unsafeRunExceptT $ mapM_ (putCheckpoint db testPk) cps

mkCheckpoints :: Int -> Int -> [WalletBench]
mkCheckpoints numCheckpoints utxoSize =
    [ force (cp i) | !i <- [1..numCheckpoints] ]
  where
    cp i = unsafeInitWallet
        (UTxO (utxo i))
        (BlockHeader
            (fromFlatSlot epochLength (fromIntegral i))
            (Quantity $ fromIntegral i)
            (Hash $ label "parentHeaderHash" i)
            (Hash $ label "headerHash" i)
        )
        initDummyState
        genesisParameters

    utxo i = force (Map.fromList (zip (mkInputs i utxoSize) (mkOutputs i utxoSize)))

benchReadUTxO :: DBLayerBench -> Benchmarkable
benchReadUTxO db = whnfIO $ readCheckpoint db testPk

-- Set up a database with some UTxO in checkpoints.
withUTxO :: DBLayerBench -> Int -> Int -> Benchmark -> Benchmark
withUTxO db numCheckpoints utxoSize = env setup . const
  where
    setup = do
        cleanDB db
        unsafeRunExceptT $ createWallet db testPk testCp testMetadata mempty
        let cps = mkCheckpoints numCheckpoints utxoSize
        unsafeRunExceptT $ mapM_ (putCheckpoint db testPk) cps
        pure db

----------------------------------------------------------------------------
-- SeqState Address Discovery

benchPutSeqState :: Int -> Int -> DBLayerBench -> IO ()
benchPutSeqState numCheckpoints numAddrs db =
    unsafeRunExceptT $ mapM_ (putCheckpoint db testPk)
        [ snd $ initWallet block0 genesisParameters $
            SeqState (mkPool numAddrs i) (mkPool numAddrs i) emptyPendingIxs
        | i <- [1..numCheckpoints]
        ]

mkPool
    :: forall t c. (KeyToAddress t SeqKey, Typeable c)
    => Int -> Int -> AddressPool t c
mkPool numAddrs i = mkAddressPool ourAccount defaultAddressPoolGap addrs
  where
    addrs =
        [ Address (label "addr-" (show i ++ "-" ++ show j))
        | j <- [1..numAddrs] ]

----------------------------------------------------------------------------
-- Disk space usage tests
--
-- These are not proper criterion benchmarks but use the benchmark test data to
-- measure size on disk of the database and its temporary files.

utxoDiskSpaceTests :: IO ()
utxoDiskSpaceTests = do
    putStrLn "Database disk space usage tests for UTxO\n"
    sequence_
        --      #Checkpoints   UTxO Size
        [ bUTxO          100           0
        , bUTxO         1000           0
        , bUTxO           10          10
        , bUTxO          100          10
        , bUTxO         1000          10
        , bUTxO           10         100
        , bUTxO          100         100
        , bUTxO         1000         100
        , bUTxO           10        1000
        , bUTxO          100        1000
        , bUTxO         1000        1000
        ]
  where
    bUTxO n s = benchDiskSize $ \db -> do
        putStrLn ("File size /"+|n|+" CP x "+|s|+" UTxO")
        walletFixture db
        benchPutUTxO n s db

txHistoryDiskSpaceTests :: IO ()
txHistoryDiskSpaceTests = do
    putStrLn "Database disk space usage tests for TxHistory\n"
    sequence_
        --       #NTransactions  #NInputs #NOutputs
        [ bTxs             100         10        20
        , bTxs            1000         10        20
        , bTxs           10000         10        20
        , bTxs          100000         10        20
        , bTxs             100         50       100
        , bTxs            1000         50       100
        , bTxs           10000         50       100
        , bTxs          100000         50       100
        ]
  where
    bTxs n i o = benchDiskSize $ \db -> do
        putStrLn ("File size /"+|n|+" w/ "+|i|+"i + "+|o|+"o")
        walletFixture db
        benchPutTxHistory 1 n i o [1..100] db

benchDiskSize :: (DBLayerBench -> IO ()) -> IO ()
benchDiskSize action = bracket setupDB cleanupDB $ \(f, ctx, db) -> do
    action db
    mapM_ (printFileSize "") [f, f <> "-shm", f <> "-wal"]
    destroyDBLayer ctx
    printFileSize " (closed)" f
    putStrLn ""
  where
    printFileSize sfx f = do
        size <- doesFileExist f >>= \case
            True -> Just <$> getFileSize f
            False -> pure Nothing
        putStrLn $ "  " +|
            padRightF 28 ' ' (takeFileName f ++ sfx) <>
            padLeftF 20 ' ' (maybe "-" sizeF size)

    sizeF size
        | size < kb = build size <> " B"
        | size < mb = build (size `div` kb) <> " KB"
        | size < gb = build (size `div` mb) <> " MB"
        | otherwise = build (size `div` gb) <> " GB"
      where
        kb = 1024
        mb = 1024*kb
        gb = 1024*mb


----------------------------------------------------------------------------
-- Mock data to use for benchmarks

type DBLayerBench = DBLayer IO (SeqState DummyTarget) DummyTarget SeqKey
type WalletBench = Wallet (SeqState DummyTarget) DummyTarget

instance NFData (DBLayer m s t k) where
    rnf _ = ()

instance NFData SqliteContext where
    rnf _ = ()

testCp :: WalletBench
testCp = snd $ initWallet block0 genesisParameters initDummyState

initDummyState :: SeqState DummyTarget
initDummyState =
    mkSeqState (xprv, mempty) defaultAddressPoolGap
  where
    bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
    xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , passphraseInfo = Nothing
    , status = Ready
    , delegation = NotDelegating
    , creationTime = systemToUTCTime (MkSystemTime 0 0)
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

ourAccount :: SeqKey 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where seed = Passphrase $ BA.convert $ BS.replicate 32 0

-- | Make a prefixed bytestring for use as a Hash or Address.
label :: Show n => String -> n -> B8.ByteString
label prefix n = B8.pack (prefix <> show n)

-- | Arbitrary epoch length for testing
epochLength :: EpochLength
epochLength = EpochLength 500
