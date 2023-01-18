{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
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

module Main where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubFromBytes )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Trace
    ( Trace )
import Cardano.BM.Data.Tracer
    ( Tracer, filterSeverity )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
import Cardano.DB.Sqlite
    ( ConnectionPool
    , SqliteContext (..)
    , newSqliteContext
    , withConnectionPool
    )
import Cardano.Mnemonic
    ( EntropySize, SomeMnemonic (..), entropyToMnemonic, genEntropy )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.DB
    ( DBLayer (..), cleanDB )
import Cardano.Wallet.DB.Layer
    ( CacheBehavior (..)
    , PersistAddressBook
    , WalletDBLog (..)
    , newDBLayerWith
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( migrateAll )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0, dummyGenesisParameters, mkTxId )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , PersistPrivateKey
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed, unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..), mkRndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqAddressPool (..)
    , SeqState (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , mkSeqStateFromAccountXPub
    , mkSeqStateFromRootXPrv
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet, unsafeInitWallet )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, hoistTimeInterpreter, mkSingleEraInterpreter )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , Range (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , SortOrder (..)
    , StartTime (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..), TransactionInfo, Tx (..), TxMeta (..), TxStatus (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic, unsafeRunExceptT )
import Control.DeepSeq
    ( NFData (..), deepseq, force )
import Control.Monad
    ( join )
import Control.Monad.Trans.Except
    ( mapExceptT )
import Control.Tracer
    ( contramap )
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
import Data.Either
    ( fromRight )
import Data.Functor
    ( ($>), (<&>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List
    ( foldl' )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Time.Clock.System
    ( SystemTime (..), systemToUTCTime )
import Data.Word
    ( Word64 )
import Fmt
    ( build, padLeftF, padRightF, pretty, (+|), (|+) )
import System.Directory
    ( doesFileExist, getFileSize )
import System.FilePath
    ( takeFileName )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Random
    ( mkStdGen, randoms )
import Test.Utils.Resource
    ( unBracket )
import UnliftIO.Exception
    ( bracket, throwIO )
import UnliftIO.Temporary
    ( withSystemTempFile )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = withUtf8Encoding $ withLogging $ \trace -> do
    let tr = filterSeverity (pure . const Error) $ trMessageText trace
    defaultMain
        [ withDB tr bgroupWriteUTxO
        , withDB tr bgroupReadUTxO
        , withDB tr bgroupWriteSeqState
        , withDB tr bgroupWriteRndState
        , withDB tr bgroupWriteTxHistory
        , withDB tr bgroupReadTxHistory
        ]
    putStrLn "\n--"
    utxoDiskSpaceTests tr
    txHistoryDiskSpaceTests tr

----------------------------------------------------------------------------
-- UTxO Benchmarks
--
-- The very max number of checkpoints we are likely to insert per wallet
-- is k=2160.
--
-- Currently the DBLayer will only store a single checkpoint (no rollback), so
-- the #Checkpoints axis is a bit meaningless.
bgroupWriteUTxO
    :: DBLayerBench
    -> Benchmark
bgroupWriteUTxO db = bgroup "UTxO (Write)"
    -- A fragmented wallet will have a large number of UTxO. The coin
    -- selection algorithm tries to prevent fragmentation.
    --
    --      #Checkpoints   UTxO Size   #NAssets
    [ bUTxO            1           0          0
    , bUTxO            1          10          0
    , bUTxO            1         100          0
    , bUTxO            1        1000          0
    , bUTxO            1       10000          0
    , bUTxO            1       10000         10
    , bUTxO            1       10000         20
    , bUTxO            1      100000          0
    , bUTxO           10        1000          0
    , bUTxO          100        1000          0
    ]
  where
    bUTxO n s a = bench lbl $ withCleanDB db walletFixture $
        benchPutUTxO n s a . fst
      where lbl | a == 0    = n|+" CP (ada-only) x "+|s|+" UTxO"
                | otherwise = n|+" CP ("+|a|+" assets per output) x "+|s|+" UTxO"

bgroupReadUTxO
    :: DBLayerBench
    -> Benchmark
bgroupReadUTxO db = bgroup "UTxO (Read)"
    --      #Checkpoints   UTxO Size   #NAssets
    [ bUTxO            1           0          0
    , bUTxO            1          10          0
    , bUTxO            1         100          0
    , bUTxO            1        1000          0
    , bUTxO            1       10000          0
    , bUTxO            1       10000         10
    , bUTxO            1       10000         20
    , bUTxO            1      100000          0
    ]
  where
    bUTxO n s a = bench lbl $ withUTxO db n s a benchReadUTxO
        where lbl | a == 0    = n|+" CP (ada-only) x "+|s|+" UTxO"
                  | otherwise = n|+" CP ("+|a|+" assets per output) x "+|s|+" UTxO"

benchPutUTxO :: Int -> Int -> Int -> DBLayerBench -> IO ()
benchPutUTxO numCheckpoints utxoSize numAssets DBLayer{..} = do
    let cps = mkCheckpoints numCheckpoints utxoSize numAssets
    unsafeRunExceptT $ mapExceptT atomically $ mapM_ (putCheckpoint testWid) cps

mkCheckpoints :: Int -> Int -> Int -> [WalletBench]
mkCheckpoints numCheckpoints utxoSize numAssets =
    [ force (cp i) | !i <- [1..numCheckpoints] ]
  where
    cp i = unsafeInitWallet
        (UTxO (utxo i))
        (BlockHeader
            (SlotNo $ fromIntegral i)
            (Quantity $ fromIntegral i)
            (Hash $ label "parentHeaderHash" i)
            (Just $ Hash $ label "headerHash" i)
        )
        initDummySeqState

    utxo i = Map.fromList $ zip
        (map fst $ mkInputs i utxoSize)
        (mkOutputs i utxoSize numAssets)

benchReadUTxO :: DBLayerBench -> IO (Maybe WalletBench)
benchReadUTxO DBLayer{..} = atomically $ readCheckpoint testWid

-- Set up a database with some UTxO in checkpoints.
withUTxO
    :: NFData b
    => DBLayerBench
    -> Int
    -> Int
    -> Int
    -> (DBLayerBench -> IO b)
    -> Benchmarkable
withUTxO  db n s a = perRunEnv (utxoFixture db n s a $> db)

utxoFixture :: DBLayerBench -> Int -> Int -> Int -> IO ()
utxoFixture db@DBLayer{..} numCheckpoints utxoSize numAssets = do
    walletFixture db
    let cps = mkCheckpoints numCheckpoints utxoSize numAssets
    unsafeRunExceptT $ mapM_ (mapExceptT atomically . putCheckpoint testWid) cps

----------------------------------------------------------------------------
-- Wallet State (Sequential Scheme) Benchmarks
--
bgroupWriteSeqState :: DBLayerBench -> Benchmark
bgroupWriteSeqState db = bgroup "SeqState"
    --      #Checkpoints  #Addresses
    [ bSeqState        1          10
    , bSeqState        1         100
    , bSeqState        1        1000
    , bSeqState        1       10000
    , bSeqState        1      100000
    , bSeqState       10        1000
    , bSeqState      100        1000
    ]
  where
    bSeqState n a = bench lbl $ withCleanDB db fixture (uncurry benchPutSeqState)
      where
        lbl = n|+" CP x "+|a|+" addr"
        fixture db_ = do
            walletFixture db_
            pure cps
        cps :: [WalletBench]
        cps =
            [ snd $ initWallet (withMovingSlot i block0) $ mkSeqState a i
            | i <- [1..n]
            ]

benchPutSeqState :: DBLayerBench -> [WalletBench] -> IO ()
benchPutSeqState DBLayer{..} cps = do
    unsafeRunExceptT $ mapExceptT atomically $ mapM_ (putCheckpoint testWid) cps

mkSeqState :: Int -> Int -> SeqState 'Mainnet ShelleyKey
mkSeqState numAddrs _ = s
    { internalPool = fillPool (internalPool s)
    , externalPool = fillPool (externalPool s)
    }
  where
    s = mkSeqStateFromAccountXPub @'Mainnet
        ourAccount Nothing purposeCIP1852 defaultAddressPoolGap
    fillPool (SeqAddressPool pool0) = SeqAddressPool $
        foldl' (\p ix -> AddressPool.update (gen ix) p) pool0 [0 .. numAddrs-1]
      where
        gen ix = AddressPool.addressFromIx pool0 $ toEnum ix

----------------------------------------------------------------------------
-- Wallet State (Random Scheme) Benchmarks
--

bgroupWriteRndState :: DBLayerBenchByron -> Benchmark
bgroupWriteRndState db = bgroup "RndState"
    --      #Checkpoints  #Addresses  #Pending
    [ bRndState        1          10        10
    , bRndState        1         100       100
    , bRndState        1        1000      1000
    , bRndState        1       10000     10000
    , bRndState        1      100000    100000
    , bRndState       10        1000      1000
    , bRndState      100        1000      1000
    ]
  where
    bRndState checkpoints numAddrs numPending =
        bench lbl $ withCleanDB db fixture (uncurry benchPutRndState)
      where
        lbl = checkpoints|+" CP x "+|numAddrs|+" addr x "+|numPending|+" pending"
        fixture db_ = walletFixtureByron db_ $> cps
        cps :: [Wallet (RndState 'Mainnet)]
        cps =
            [ snd $ initWallet (withMovingSlot i block0) $
                RndState
                    { hdPassphrase = dummyPassphrase
                    , accountIndex = minBound
                    , gen = mkStdGen 42
                    , pendingAddresses = Map.fromList (drop numAddrs addresses)
                    , discoveredAddresses =
                        (,Used) <$> Map.fromList (take numAddrs addresses)
                    }
            | i <- [1 .. checkpoints]
            , let addresses = [1 .. numAddrs + numPending] <&> \j ->
                    force ((toEnum i, toEnum j), mkByronAddress i j)
            ]

benchPutRndState :: DBLayerBenchByron -> [Wallet (RndState 'Mainnet)] -> IO ()
benchPutRndState DBLayer{..} cps =
    unsafeRunExceptT $ mapExceptT atomically $ mapM_ (putCheckpoint testWid) cps

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
-- In Jormungandr with Jörmungandr, we have a soft max size of 8Kb (64Kb hard).
--
-- A transaction size is roughly:
--
-- - headers & binary overhead: ~10 bytes
-- - size of all the witnesses: 64-128 byte * nInputs (depending on scheme)
-- - size of all inputs: 41 bytes * nInputs
-- - size of all outputs: 41 bytes * nOutputs
--
-- This means a transaction in Jormungandr can't have more than (worst case):
--
-- - ~78 inputs
-- - ~200 outputs
--
-- The numbers above are really hard boundaries but, it is unrealistic to
-- imagine a transaction with 1 inputs and 200 outputs because, the coin
-- selection algorithm always try to consider output independently. So for
-- 200 outputs, we know that 200 inputs (at least) would be necessary. There are
-- the case for other software of course, but we may consider the following
-- numbers as realistic benchmark higher bounds (in the worst case):
--
-- - 50 inputs
-- - 100 outputs
bgroupWriteTxHistory
    :: DBLayerBench
    -> Benchmark
bgroupWriteTxHistory db = bgroup "TxHistory (Write)"
    --                   #NTxs #NInputs #NOutputs #NAssets  #SlotRange
    [ bTxHistory             1        1        1         0     [1..10]
    , bTxHistory            10        1        1         0     [1..10]
    , bTxHistory            10       10       10         0     [1..10]
    , bTxHistory            10       50      100         0     [1..10]
    , bTxHistory           100       10       10         0    [1..100]
    , bTxHistory           100       50      100         0    [1..100]
    , bTxHistory          1000       10       10         0   [1..1000]
    , bTxHistory          1000       50      100         0   [1..1000]
    , bTxHistory          1000       50      100        10   [1..1000]
    , bTxHistory          1000       50      100        20   [1..1000]
    , bTxHistory         10000       10       10         0  [1..10000]
    ]
  where
    bTxHistory n i o a r =
        bench lbl $ withCleanDB db walletFixture $
            benchPutTxHistory n i o a r . fst
      where
        lbl = n|+" w/ "+|i|+"i + "+|o|+"o ["+|inf|+".."+|sup|+"]"
        inf = head r
        sup = last r

bgroupReadTxHistory
    :: DBLayerBench
    -> Benchmark
bgroupReadTxHistory db = bgroup "TxHistory (Read)"
    --             #NTxs  #NAssets #SlotRange  #SortOrder  #Status  #SearchRange
    [ bTxHistory    1000         0   [1..100]  Descending  Nothing  wholeRange
    , bTxHistory    1000         0   [1..100]   Ascending  Nothing  wholeRange
    , bTxHistory    1000         0  [1..1000]  Descending  Nothing  wholeRange
    , bTxHistory    1000         0   [1..100]  Descending  pending  wholeRange
    , bTxHistory    1000         0   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000        10   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000        20   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000         0 [1..10000]  Descending  Nothing  (Just 42, Just 1337)
    , bTxHistory   10000         0   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory   10000         0 [1..10000]  Descending  Nothing  (Just 42, Just 1337)
    ]
  where
    wholeRange = (Nothing, Nothing)
    pending = Just Pending
    bTxHistory n a r o st s =
        bench lbl $ withTxHistory db n a r $ benchReadTxHistory o s st
      where
        lbl = unwords [show n, show a, range, ord, mstatus, search]
        range = let inf = head r in let sup = last r in "["+|inf|+".."+|sup|+"]"
        ord = case o of Descending -> "DESC"; Ascending -> "ASC"
        mstatus = maybe "-" pretty st
        search = case s of
            (Nothing, Nothing) -> "*"
            (Just inf, Nothing) -> inf|+".."
            (Nothing, Just sup) -> ".."+|sup|+""
            (Just inf, Just sup) -> inf|+".."+|sup|+""

benchPutTxHistory
    :: Int
    -> Int
    -> Int
    -> Int
    -> [Word64]
    -> DBLayerBench
    -> IO ()
benchPutTxHistory numTxs numInputs numOutputs numAssets range DBLayer{..} = do
    let txs = mkTxHistory numTxs numInputs numOutputs numAssets range
    unsafeRunExceptT $ mapExceptT atomically $ putTxHistory testWid txs

benchReadTxHistory
    :: SortOrder
    -> (Maybe Word64, Maybe Word64)
    -> Maybe TxStatus
    -> DBLayerBench
    -> IO [TransactionInfo]
benchReadTxHistory sortOrder (inf, sup) mstatus DBLayer{..} =
    atomically $ readTxHistory testWid Nothing sortOrder range mstatus
  where
    range = Range
        (SlotNo . fromIntegral <$> inf)
        (SlotNo . fromIntegral <$> sup)

mkTxHistory
    :: Int
    -> Int
    -> Int
    -> Int
    -> [Word64]
    -> [(Tx, TxMeta)]
mkTxHistory numTx numInputs numOutputs numAssets range =
    [ force
        ( Tx
            { txId = mkTxId resolvedInputs outputs mempty Nothing
            , txCBOR = Nothing
            , fee = Nothing
            , resolvedInputs
            , resolvedCollateralInputs =
                -- TODO: (ADP-957)
                []
            , outputs
            , collateralOutput =
                -- TODO: [ADP-1670]
                Nothing
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }
        , TxMeta
            { status = [InLedger, Pending] !! (i `mod` 2)
            , direction = Incoming
            , slotNo = sl i
            , blockHeight = Quantity $ fromIntegral i
            , amount = Coin (fromIntegral numOutputs)
            , expiry = Nothing
            }
        )
    | !i <- [1..numTx]
    , let resolvedInputs = mkInputs  i numInputs
    , let outputs = mkOutputs i numOutputs numAssets
    ]
  where
    sl i = SlotNo $ range !! (i `mod` length range)

mkInputs :: Int -> Int -> [(TxIn, Maybe TxOut)]
mkInputs prefix n =
    [ force
        ( TxIn (Hash (label lbl i)) (fromIntegral i)
        , Just $ mkTxOut n
        )
    | !i <- [1..n]]
  where
    lbl = show prefix <> "in"
    mkTxOut i = TxOut
        (mkAddress prefix i)
        (TokenBundle.TokenBundle (Coin $ fromIntegral i) mempty)

-- | Creates transaction outputs with multi-asset token bundles.
mkOutputs :: Int -> Int -> Int -> [TxOut]
mkOutputs prefix nOuts nAssets =
    [ force (mkTxOut i)
    | !i <- [1..nOuts]
    ]
  where
    mkTxOut i = TxOut
        (mkAddress prefix i)
        (TokenBundle.TokenBundle (Coin 1) (TokenMap.fromFlatList tokens))
    tokens =
        [ ( TokenMap.AssetId (mkTokenPolicyId (ac `mod` 10)) (mkTokenName ac)
          , TokenQuantity 42
          )
        | !ac <- [1 .. nAssets]
        ]
    mkTokenName =
        UnsafeTokenName . B8.singleton . Char.chr
    mkTokenPolicyId = fromRight (error "Couldn't decode tokenPolicyId")
        . fromText
        . T.pack
        . take tokenPolicyIdHexStringLength
        . join
        . replicate tokenPolicyIdHexStringLength
        . show
    tokenPolicyIdHexStringLength = 56

withTxHistory
    :: NFData b
    => DBLayerBench
    -> Int
    -> Int
    -> [Word64]
    -> (DBLayerBench -> IO b)
    -> Benchmarkable
withTxHistory db s a r =
    perRunEnv (txHistoryFixture db s a r $> db)

txHistoryFixture
    :: DBLayerBench
    -> Int
    -> Int
    -> [Word64]
    -> IO ()
txHistoryFixture db@DBLayer{..} bSize nAssets range = do
    walletFixture db
    let (nInps, nOuts) = (20, 20)
    let txs = mkTxHistory bSize nInps nOuts nAssets range
    atomically $ unsafeRunExceptT $ putTxHistory testWid txs

----------------------------------------------------------------------------
-- Criterion env functions for database setup

-- | Sets up a benchmark environment with the SQLite DBLayer using a file
-- database in a temporary location.
withDB
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO WalletDBLog
    -> (DBLayer IO s k -> Benchmark)
    -> Benchmark
withDB tr bm = envWithCleanup (setupDB tr) cleanupDB $
    \benchEnv -> bm (dbLayer benchEnv)

data BenchEnv s k = BenchEnv
    { cleanupDB :: IO ()
    , dbFile :: FilePath
    , dbLayer :: DBLayer IO s k
    }

instance NFData (BenchEnv s k) where
    rnf env = deepseq (rnf $ dbFile env) $ deepseq (rnf $ dbLayer env) ()

withTempSqliteFile :: (FilePath -> IO a) -> IO a
withTempSqliteFile action = withSystemTempFile "bench.db" $ \fp _ -> action fp

setupDB
    :: forall s k.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO WalletDBLog
    -> IO (BenchEnv s k)
setupDB tr = do
    (createPool, destroyPool) <- unBracket withSetup
    uncurry (BenchEnv destroyPool) <$> createPool
  where
    withSetup action = withTempSqliteFile $ \fp -> do
        let trDB = contramap MsgDB tr
        withConnectionPool trDB fp $ \pool -> do
            ctx <- either throwIO pure =<< newSqliteContext trDB pool [] migrateAll
            db <- newDBLayerWith NoCache tr singleEraInterpreter ctx
            action (fp, db)

singleEraInterpreter :: TimeInterpreter IO
singleEraInterpreter = hoistTimeInterpreter (pure . runIdentity) $
    mkSingleEraInterpreter
        (StartTime $ posixSecondsToUTCTime 0)
        (SlottingParameters
        { getSlotLength = SlotLength 1
        , getEpochLength = EpochLength 21600
        , getActiveSlotCoefficient = ActiveSlotCoefficient 1
        , getSecurityParameter = Quantity 2160
        })

-- | Cleans the database before running the benchmark.
-- It also cleans the database after running the benchmark. That is just to
-- exercise the delete functions.
withCleanDB
    :: NFData fixture
    => DBLayer IO s k
    -> (DBLayer IO s k -> IO fixture)
    -> ((DBLayer IO s k, fixture) -> IO ())
    -> Benchmarkable
withCleanDB db fixture =
    perRunEnv $ (db,) <$> fixture db

walletFixture :: DBLayerBench -> IO ()
walletFixture db@DBLayer{..} = do
    cleanDB db
    atomically $ unsafeRunExceptT $ initializeWallet
        testWid
        testCp
        testMetadata
        mempty
        dummyGenesisParameters

walletFixtureByron :: DBLayerBenchByron -> IO ()
walletFixtureByron db@DBLayer{..} = do
    cleanDB db
    atomically $ unsafeRunExceptT $ initializeWallet
        testWid
        testCpByron
        testMetadata
        mempty
        dummyGenesisParameters

----------------------------------------------------------------------------
-- Disk space usage tests
--
-- These are not proper criterion benchmarks but use the benchmark test data to
-- measure size on disk of the database and its temporary files.

utxoDiskSpaceTests :: Tracer IO WalletDBLog -> IO ()
utxoDiskSpaceTests tr = do
    putStrLn "Database disk space usage tests for UTxO\n"
    sequence_
        --      #Checkpoints   UTxO Size
        [ bUTxO            1          10
        , bUTxO           10          10
        , bUTxO            1         100
        , bUTxO           10         100
        , bUTxO            1        1000
        , bUTxO           10        1000
        , bUTxO            1       10000
        , bUTxO           10       10000
        , bUTxO            1      100000
        , bUTxO           10      100000
        ]
  where
    bUTxO n s = benchDiskSize tr $ \db -> do
        putStrLn ("File size /"+|n|+" CP x "+|s|+" UTxO")
        walletFixture db
        benchPutUTxO n s 0 db

txHistoryDiskSpaceTests :: Tracer IO WalletDBLog -> IO ()
txHistoryDiskSpaceTests tr = do
    putStrLn "Database disk space usage tests for TxHistory\n"
    sequence_
        --       #NTransactions  #NInputs  #NOutputs
        [ bTxs             100         20         20
        , bTxs            1000         20         20
        , bTxs           10000         20         20
        , bTxs          100000         20         20
        ]
  where
    bTxs n i o = benchDiskSize tr $ \db -> do
        putStrLn ("File size /"+|n|+" w/ "+|i|+"i + "+|o|+"o")
        walletFixture db
        benchPutTxHistory n i o 0 [1..100] db

benchDiskSize :: Tracer IO WalletDBLog -> (DBLayerBench -> IO ()) -> IO ()
benchDiskSize tr action = bracket (setupDB tr) cleanupDB
    $ \(BenchEnv destroyPool f db) -> do
        action db
        mapM_ (printFileSize "") [f, f <> "-shm", f <> "-wal"]
        destroyPool
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

type DBLayerBench = DBLayer IO (SeqState 'Mainnet ShelleyKey) ShelleyKey
type DBLayerBenchByron = DBLayer IO (RndState 'Mainnet) ByronKey
type WalletBench = Wallet (SeqState 'Mainnet ShelleyKey)
type WalletBenchByron = Wallet (RndState 'Mainnet)

instance NFData (DBLayer m s k) where
    rnf _ = ()

instance NFData SqliteContext where
    rnf _ = ()

instance NFData ConnectionPool where
    rnf _ = ()

testCp :: WalletBench
testCp = snd $ initWallet block0 initDummySeqState

testCpByron :: WalletBenchByron
testCpByron = snd $ initWallet block0 initDummyRndState

{-# NOINLINE initDummySeqState #-}
initDummySeqState :: SeqState 'Mainnet ShelleyKey
initDummySeqState =
    mkSeqStateFromRootXPrv (xprv, mempty) purposeCIP1852 defaultAddressPoolGap
  where
    mnemonic = unsafePerformIO
        $ SomeMnemonic . entropyToMnemonic @15
        <$> genEntropy @(EntropySize 15)
    xprv = generateKeyFromSeed (mnemonic, Nothing) mempty

{-# NOINLINE initDummyRndState #-}
initDummyRndState :: RndState 'Mainnet
initDummyRndState =
    mkRndState rootK 42
  where
    rootK = Byron.generateKeyFromSeed mnemonic mempty
    mnemonic = unsafePerformIO $
        SomeMnemonic . entropyToMnemonic @12 <$> genEntropy @(EntropySize 12)

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , passphraseInfo = Nothing
    , creationTime = systemToUTCTime (MkSystemTime 0 0)
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

defaultPrefix :: DerivationPrefix
defaultPrefix = DerivationPrefix
    ( purposeCIP1852
    , coinTypeAda
    , minBound
    )

ourAccount :: ShelleyKey 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, Nothing) mempty
  where
    seed = someDummyMnemonic (Proxy @15)

rewardAccount :: ShelleyKey 'CredFromKeyK XPub
rewardAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, Nothing) mempty
  where
    seed = someDummyMnemonic (Proxy @15)

-- | Make a prefixed bytestring for use as a Hash or Address.
label :: Show n => String -> n -> B8.ByteString
label prefix n = B8.take 32 $ B8.pack (prefix <> show n) <> B8.replicate 32 '0'

dummyPassphrase :: Passphrase any
dummyPassphrase = Passphrase "dummy-passphrase"

-- | Make sure to generate
withMovingSlot :: Int -> Block -> Block
withMovingSlot i b@(Block h _ _) = b
    { header = h
        { slotNo = SlotNo (fromIntegral i)
        , blockHeight = Quantity (fromIntegral i)
        }
    }

mkAddress :: Int -> Int -> Address
mkAddress i j =
    delegationAddress @'Mainnet
        (ShelleyKey $ unsafeXPub $ B8.pack $ take 64 $ randoms $ mkStdGen seed)
        rewardAccount
  where
    -- Generate a seed using two prime numbers and a pair of index. This should
    -- lead to a satisfactory entropy.
    seed = 1459*i + 1153*j
    unsafeXPub = fromMaybe (error "xpubFromBytes error") . xpubFromBytes

mkByronAddress :: Int -> Int -> Address
mkByronAddress i j =
    paymentAddress @'Mainnet @ByronKey @'CredFromKeyK
        (ByronKey
            (unsafeXPub (B8.pack $ take 64 $ randoms g))
            (Index acctIx, Index addrIx)
            (Passphrase $ BA.convert $ BS.pack $ replicate 32 0)
        )
  where
    -- Generate a seed using two prime numbers and a pair of index. This should
    -- lead to a satisfactory entropy.
    g = mkStdGen $ 1459*i + 1153*j
    unsafeXPub = fromMaybe (error "xpubFromBytes error") . xpubFromBytes
    [acctIx, addrIx] = take 2 $ randoms g

-- | Run an action with logging available and configured. When the action is
-- finished (normally or otherwise), log messages are flushed.
withLogging
    :: (Trace IO Text -> IO a)
    -- ^ The action to run with logging configured.
    -> IO a
withLogging action = bracket before after between
  where
    before = do
        cfg <- do
            c <- defaultConfigStdout
            CM.setMinSeverity c Debug
            CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
            pure c
        (tr, sb) <- setupTrace_ cfg "bench-db"
        pure (sb, tr)

    after =
        shutdown . fst

    between =
        action . snd
