{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
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
import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..), cleanDB )
import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..), newDBLayer )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0 )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, KeyToAddress (..), Passphrase (..), XPub, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( generateKeyFromSeed, unsafeGenerateKeyFromSeed )
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
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletState (..)
    , fromFlatSlot
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM_ )
import Criterion.Main
    ( Benchmark
    , Benchmarkable
    , bench
    , bgroup
    , defaultMain
    , envWithCleanup
    , perRunEnv
    )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.List.Split
    ( chunksOf )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.System
    ( SystemTime (..), systemToUTCTime )
import Data.Typeable
    ( Typeable )
import Fmt
    ( (+|), (|+) )
import System.Directory
    ( doesFileExist, removeFile )
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
main = defaultMain
    [ withDB bgroupUTxO
    , withDB bgroupSeqState
    , withDB bgroupTxHistory
    ]

----------------------------------------------------------------------------
-- UTxO Benchmarks
--
-- The very max number of checkpoints we are likely to insert per wallet
-- is k=2160.
--
-- Currently the DBLayer will only store a single checkpoint (no rollback), so
-- the #Checkpoints axis is a bit meaningless.
bgroupUTxO :: DBLayerBench -> Benchmark
bgroupUTxO db = bgroup "UTxO"
    -- A fragmented wallet will have a large number of UTxO. The coin
    -- selection algorithm tries to prevent fragmentation.
    --
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
    bUTxO n s = bench lbl $ withCleanDB db $ benchPutUTxO n s
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
bgroupTxHistory :: DBLayerBench -> Benchmark
bgroupTxHistory db = bgroup "TxHistory"
    --           #NBatch  #BatchSize #NInputs #NOutputs
    [ bTxHistory       1         100        1        1
    , bTxHistory       1        1000        1        1
    , bTxHistory      10          10        1        1
    , bTxHistory     100          10        1        1
    , bTxHistory       1         100       10       10
    , bTxHistory       1        1000       10       10
    , bTxHistory       1       10000       10       10
    , bTxHistory       1         100       50      100
    , bTxHistory       1        1000       50      100
    , bTxHistory       1       10000       50      100
    ]
  where
    bTxHistory nBatch bSize nInps nOuts =
        bench lbl $ withCleanDB db $ benchPutTxHistory nBatch bSize nInps nOuts
      where lbl = nBatch |+" x "+| bSize |+" w/ "+| nInps |+"i + "+| nOuts |+"o"

----------------------------------------------------------------------------
-- Criterion env functions for database setup

-- | Sets up a benchmark environment with the SQLite DBLayer using a file
-- database in a temporary location.
withDB :: (DBLayerBench -> Benchmark) -> Benchmark
withDB bm = envWithCleanup setup cleanup (\ ~(_, db) -> bm db)
  where
    setup = do
        logConfig <- CM.empty
        f <- emptySystemTempFile "bench.db"
        (_, db) <- newDBLayer logConfig nullTracer (Just f)
        pure (f, db)
    cleanup (f, _) = mapM_ remove [f, f <> "-shm", f <> "-wal"]
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
withCleanDB db = perRunEnv $ do
    cleanDB db
    unsafeRunExceptT $ createWallet db testPk testCp testMetadata
    pure db

----------------------------------------------------------------------------
-- TxHistory benchmarks

benchPutTxHistory :: Int -> Int -> Int -> Int -> DBLayerBench -> IO ()
benchPutTxHistory numBatches batchSize numInputs numOutputs db = do
    let batches = mkTxHistory (numBatches*batchSize) numInputs numOutputs
    unsafeRunExceptT $ forM_ (chunksOf batchSize batches) $ \txs -> do
        putTxHistory db testPk (Map.fromList txs)

mkTxHistory :: Int -> Int -> Int -> [(Hash "Tx", (Tx, TxMeta))]
mkTxHistory numTx numInputs numOutputs =
    [ ( Hash (label "tx-" i)
      , ( Tx (mkInputs numInputs) (mkOutputs numOutputs)
        , TxMeta
            { status = InLedger
            , direction = Incoming
            , slotId = fromFlatSlot epochLength (fromIntegral i)
            , amount = Quantity (fromIntegral numOutputs)
            }
        )
      )
    | i <- [1..numTx] ]

mkInputs :: Int -> [TxIn]
mkInputs n = [TxIn (Hash (label "in" i)) (fromIntegral i) | i <- [1..n]]

mkOutputs :: Int -> [TxOut]
mkOutputs n = [TxOut (Address (label "addr" i)) (Coin 1) | i <- [1..n]]

----------------------------------------------------------------------------
-- UTxO benchmarks

benchPutUTxO :: Int -> Int -> DBLayerBench -> IO ()
benchPutUTxO numCheckpoints utxoSize db = do
    let cps = mkCheckpoints numCheckpoints utxoSize
    unsafeRunExceptT $ mapM_ (putCheckpoint db testPk) cps

mkCheckpoints :: Int -> Int -> [WalletBench]
mkCheckpoints numCheckpoints utxoSize = [ cp i | i <- [1..numCheckpoints]]
  where
    cp i = unsafeInitWallet (UTxO utxo) mempty
        (BlockHeader
            (fromFlatSlot epochLength (fromIntegral i))
            (Hash $ label "prevBlockHash" i)
        )
        initDummyState
    utxo = Map.fromList $ zip (mkInputs utxoSize) (mkOutputs utxoSize)

----------------------------------------------------------------------------
-- SeqState Address Discovery

benchPutSeqState :: Int -> Int -> DBLayerBench -> IO ()
benchPutSeqState numCheckpoints numAddrs db =
    unsafeRunExceptT $ mapM_ (putCheckpoint db testPk)
        [ initWallet block0 $
            SeqState (mkPool numAddrs i) (mkPool numAddrs i) emptyPendingIxs
        | i <- [1..numCheckpoints]
        ]

mkPool
    :: forall t chain. (KeyToAddress t, Typeable chain)
    => Int -> Int -> AddressPool t chain
mkPool numAddrs i = mkAddressPool ourAccount defaultAddressPoolGap addrs
  where
    addrs =
        [ Address (label "addr-" (show i ++ "-" ++ show j))
        | j <- [1..numAddrs] ]

----------------------------------------------------------------------------
-- Mock data to use for benchmarks

type DBLayerBench = DBLayer IO (SeqState DummyTarget) DummyTarget
type WalletBench = Wallet (SeqState DummyTarget) DummyTarget

instance NFData (DBLayer m s t) where
    rnf _ = ()

instance PersistTx DummyTarget where
    resolvedInputs = flip zip (repeat Nothing) . inputs
    mkTx _ inps = Tx (fst <$> inps)

testCp :: WalletBench
testCp = initWallet block0 initDummyState

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

ourAccount :: Key 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where seed = Passphrase $ BA.convert $ BS.replicate 32 0

-- | Make a prefixed bytestring for use as a Hash or Address.
label :: Show n => B8.ByteString -> n -> B8.ByteString
label prefix n = prefix <> B8.pack (show n)

-- | Arbitrary epoch length for testing
epochLength :: EpochLength
epochLength = EpochLength 500
