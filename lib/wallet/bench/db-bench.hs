{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
#endif

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
    ( XPub
    , xpubFromBytes
    )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Trace
    ( Trace
    )
import Cardano.BM.Data.Tracer
    ( Tracer
    , filterSeverity
    )
import Cardano.BM.Extra
    ( trMessageText
    )
import Cardano.BM.Setup
    ( setupTrace_
    , shutdown
    )
import Cardano.DB.Sqlite
    ( SqliteContext (..)
    )
import Cardano.Mnemonic
    ( EntropySize
    , SomeMnemonic (..)
    , entropyToMnemonic
    , genEntropy
    )
import Cardano.Wallet
    ( putWalletCheckpoints
    )
import Cardano.Wallet.Address.Derivation
    ( DelegationAddress (..)
    , Depth (..)
    , Index (..)
    , PaymentAddress (..)
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState (..)
    , mkRndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( DerivationPrefix (..)
    , SeqAddressPool (..)
    , SeqState (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , mkSeqStateFromAccountXPub
    , purposeCIP1852
    )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( publicKey
    )
import Cardano.Wallet.BenchShared
    ( withTempSqliteFile
    )
import Cardano.Wallet.DB
    ( DBFresh (..)
    , DBLayer (..)
    , DBLayerParams (..)
    )
import Cardano.Wallet.DB.Layer
    ( DefaultFieldValues (..)
    , PersistAddressBook
    , WalletDBLog (..)
    , withDBFresh
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0
    , dummyGenesisParameters
    , mkTxId
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    , WalletFlavor (..)
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , initWallet
    , unsafeInitWallet
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    , SNetworkId (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , hoistTimeInterpreter
    , mkSingleEraInterpreter
    )
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
    ( Address (..)
    , AddressState (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( RootCredentials (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxMeta (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic
    , unsafeRunExceptT
    )
import Control.DeepSeq
    ( NFData (..)
    , force
    )
import Control.Monad
    ( join
    )
import Criterion.Main
    ( Benchmark
    , Benchmarkable
    , bench
    , bgroup
    , defaultMain
    , perRunEnvWithCleanup
    )
import Crypto.Hash
    ( hash
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either
    ( fromRight
    )
import Data.Functor
    ( (<&>)
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.List
    ( foldl'
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( fromText
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Data.Time.Clock.System
    ( SystemTime (..)
    , systemToUTCTime
    )
import Data.Word
    ( Word64
    )
import Fmt
    ( build
    , padLeftF
    , padRightF
    , pretty
    , (+|)
    , (|+)
    )
import GHC.Num
    ( Natural
    )
import Main.Utf8
    ( withUtf8
    )
import System.Directory
    ( doesFileExist
    , getFileSize
    )
import System.FilePath
    ( takeFileName
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )
import System.Random
    ( mkStdGen
    , randoms
    )
import Test.Utils.Resource
    ( unBracket
    )
import UnliftIO.Exception
    ( bracket
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = withUtf8 $ withLogging $ \trace -> do
    let tr = filterSeverity (pure . const Error) $ trMessageText trace
    defaultMain
        [ bgroupWriteUTxO tr
        , bgroupReadUTxO tr
        , bgroupWriteSeqState tr
        , bgroupWriteRndState tr
        , bgroupWriteTxHistory tr
        , bgroupReadTxHistory tr
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
    :: Tracer IO WalletDBLog
    -> Benchmark
bgroupWriteUTxO tr = bgroup "UTxO (Write)"
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
    bUTxO n s a = bench lbl $ withCleanDB tr (fmap (, ()) <$> walletFixture) $
        benchPutUTxO n s a . fst
      where lbl | a == 0    = n|+" CP (ada-only) x "+|s|+" UTxO"
                | otherwise = n|+" CP ("+|a|+" assets per output) x "+|s|+" UTxO"

bgroupReadUTxO :: Tracer IO WalletDBLog -> Benchmark
bgroupReadUTxO tr = bgroup "UTxO (Read)"
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
    bUTxO n s a = bench lbl $ withCleanDB tr (fmap (, ()) <$> utxoFixture n s a)
        $ benchReadUTxO . fst
        where lbl | a == 0    = n|+" CP (ada-only) x "+|s|+" UTxO"
                  | otherwise = n|+" CP ("+|a|+" assets per output) x "+|s|+" UTxO"

benchPutUTxO :: Int -> Int -> Int -> DBLayerBench -> IO ()
benchPutUTxO numCheckpoints utxoSize numAssets db =
    putWalletCheckpoints db $ mkCheckpoints numCheckpoints utxoSize numAssets

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

benchReadUTxO :: DBLayerBench -> IO WalletBench
benchReadUTxO DBLayer{..} = atomically readCheckpoint

utxoFixture ::  Int -> Int -> Int -> DBFreshBench -> IO DBLayerBench
utxoFixture  numCheckpoints utxoSize numAssets dbf = do
    db <- walletFixture dbf
    putWalletCheckpoints db $ mkCheckpoints numCheckpoints utxoSize numAssets
    pure db

----------------------------------------------------------------------------
-- Wallet State (Sequential Scheme) Benchmarks

bgroupWriteSeqState :: Tracer IO WalletDBLog -> Benchmark
bgroupWriteSeqState tr = bgroup "SeqState"
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
    bSeqState n a = bench lbl $ withCleanDB tr fixture (uncurry benchPutSeqState)
      where
        lbl = n|+" CP x "+|a|+" addr"
        fixture dbf = do
            db <- walletFixture dbf
            pure (db, cps)
        cps :: [WalletBench]
        cps =
            [ snd $ initWallet (withMovingSlot i block0) $ mkSeqState a i
            | i <- [1..n]
            ]

benchPutSeqState :: DBLayerBench -> [WalletBench] -> IO ()
benchPutSeqState = putWalletCheckpoints

mkSeqState :: Int -> Int -> SeqState 'Mainnet ShelleyKey
mkSeqState numAddrs _ = s
    { internalPool = fillPool (internalPool s)
    , externalPool = fillPool (externalPool s)
    }
  where
    s = mkSeqStateFromAccountXPub @'Mainnet
        ourAccount Nothing purposeCIP1852 defaultAddressPoolGap
    fillPool :: SeqAddressPool r ShelleyKey -> SeqAddressPool r ShelleyKey
    fillPool (SeqAddressPool pool0) = SeqAddressPool $
        foldl' (\p ix -> AddressPool.update (gen ix) p) pool0 [0 .. numAddrs-1]
      where
        gen ix = AddressPool.addressFromIx pool0 $ toEnum ix

----------------------------------------------------------------------------
-- Wallet State (Random Scheme) Benchmarks
--

bgroupWriteRndState :: Tracer IO WalletDBLog -> Benchmark
bgroupWriteRndState tr = bgroup "RndState"
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
        bench lbl $ withCleanDB tr fixture (uncurry benchPutRndState)
      where
        lbl = checkpoints|+" CP x "+|numAddrs|+" addr x "+|numPending|+" pending"
        fixture dbf = walletFixtureByron dbf <&> (, cps)
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
benchPutRndState = putWalletCheckpoints

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

bgroupWriteTxHistory :: Tracer IO WalletDBLog -> Benchmark
bgroupWriteTxHistory tr = bgroup "TxHistory (Write)"
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
        bench lbl $ withCleanDB tr (fmap (, ()) <$> walletFixture) $
            benchPutTxHistory n i o a r . fst
      where
        lbl = n|+" w/ "+|i|+"i + "+|o|+"o ["+|inf|+".."+|sup|+"]"
        inf = head r
        sup = last r

bgroupReadTxHistory :: Tracer IO WalletDBLog -> Benchmark
bgroupReadTxHistory tr = bgroup "TxHistory (Read)"
    --             #NTxs  #NAssets #SlotRange  #SortOrder  #Status  #SearchRange
    [ bTxHistory    1000         0   [1..100]  Descending  Nothing  wholeRange
    , bTxHistory    1000         0   [1..100]   Ascending  Nothing  wholeRange
    , bTxHistory    1000         0  [1..1000]  Descending  Nothing  wholeRange
    -- , bTxHistory    1000         0   [1..100]  Descending  (Just Pending)  wholeRange
    -- TODO This is currently broken because the pending txs are not stored via
    -- putTxHistory. We need to fix this. ADP-2830
    , bTxHistory    1000         0   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000        10   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000        20   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory    1000         0 [1..10000]  Descending  Nothing  (Just 42, Just 1337)
    , bTxHistory   10000         0   [1..100]  Descending  Nothing  (Just 40, Just 60)
    , bTxHistory   10000         0 [1..10000]  Descending  Nothing  (Just 42, Just 1337)
    ]
  where
    wholeRange = (Nothing, Nothing)
    -- pending = Just Pending
    bTxHistory n a r o st s =
        bench lbl $ withCleanDB tr (fmap (, ()) <$> txHistoryFixture n a r) $
            benchReadTxHistory o s st Nothing . fst
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
    atomically $ putTxHistory txs

benchReadTxHistory
    :: SortOrder
    -> (Maybe Word64, Maybe Word64)
    -> Maybe TxStatus
    -> Maybe Natural
    -> DBLayerBench
    -> IO [TransactionInfo]
benchReadTxHistory sortOrder (inf, sup) mstatus mlimit DBLayer{..} =
    atomically $ readTransactions Nothing sortOrder range mstatus mlimit Nothing
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
        (TokenBundle.TokenBundle (Coin.unsafeFromIntegral i) mempty)

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

txHistoryFixture
    :: Int
    -> Int
    -> [Word64]
    -> DBFreshBench
    -> IO DBLayerBench
txHistoryFixture  bSize nAssets range dbf = do
    db@DBLayer{..} <- walletFixture dbf
    let (nInps, nOuts) = (20, 20)
    let txs = mkTxHistory bSize nInps nOuts nAssets range
    atomically $ putTxHistory txs
    pure db

walletFixture :: DBFreshBench -> IO DBLayerBench
walletFixture DBFresh{bootDBLayer} = do
    unsafeRunExceptT $ bootDBLayer $ DBLayerParams
        testCp
        testMetadata
        mempty
        dummyGenesisParameters

walletFixtureByron :: DBFreshBenchByron  -> IO DBLayerBenchByron
walletFixtureByron DBFresh{bootDBLayer} = do
    unsafeRunExceptT $ bootDBLayer $ DBLayerParams
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
    bUTxO n s = benchDiskSize tr $ \dbf -> do
        putStrLn ("File size /"+|n|+" CP x "+|s|+" UTxO")
        db <- walletFixture dbf
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
    bTxs n i o = benchDiskSize tr $ \dbf -> do
        putStrLn ("File size /"+|n|+" w/ "+|i|+"i + "+|o|+"o")
        db <- walletFixture dbf
        benchPutTxHistory n i o 0 [1..100] db

benchDiskSize :: Tracer IO WalletDBLog -> (DBFreshBench -> IO ()) -> IO ()
benchDiskSize tr action = bracket (setupDB tr) dbDown
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
-- Criterion env functions for database setup

data BenchEnv s = BenchEnv
    { dbDown :: IO ()
    , dbFile :: FilePath
    , dbFresh :: DBFresh IO s
    }

instance NFData (BenchEnv s) where
    rnf :: BenchEnv s -> ()
    rnf _ = ()

setupDB
    :: forall s
     . ( PersistAddressBook s
       , WalletFlavor s
       )
    => Tracer IO WalletDBLog
    -> IO (BenchEnv s)
setupDB tr = do
    (createPool, destroyPool) <- unBracket withSetup
    uncurry (BenchEnv destroyPool) <$> createPool
  where
    withSetup action = withTempSqliteFile $ \fp -> do
        withDBFresh (walletFlavor @s)
            tr (Just defaultFieldValues) fp singleEraInterpreter testWid
                $ \db -> action (fp, db)

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

-- | Runs a benchmark on (a series of) freshly created 'DBLayer's.
withCleanDB
    :: ( NFData c
       , PersistAddressBook s
       , WalletFlavor s
       , NFData b
       )
    => Tracer IO WalletDBLog
    -- ^ db messages tracer
    -> (DBFresh IO s -> IO (DBLayer IO s, b))
    -- ^ fixture setup, always run before the action
    -> ((DBLayer IO s, b) -> IO c)
    -- ^ action to run
    -> Benchmarkable
withCleanDB tr f g = perRunEnvWithCleanup setup (dbDown . fst) $
    g . snd
  where
    setup = do
        be@BenchEnv {..} <- setupDB tr
        x <- f dbFresh
        pure (be , x)

defaultFieldValues :: DefaultFieldValues
defaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient = ActiveSlotCoefficient 1.0
    , defaultDesiredNumberOfPool = 0
    , defaultMinimumUTxOValue = Coin 1000000
    , defaultHardforkEpoch = Nothing
    , defaultKeyDeposit = Coin 2000000
    }
----------------------------------------------------------------------------
-- Mock data to use for benchmarks

type DBFreshBench = DBFresh IO (SeqState 'Mainnet ShelleyKey)
type DBLayerBench = DBLayer IO (SeqState 'Mainnet ShelleyKey)
type DBLayerBenchByron = DBLayer IO (RndState 'Mainnet)
type DBFreshBenchByron = DBFresh IO (RndState 'Mainnet)
type WalletBench = Wallet (SeqState 'Mainnet ShelleyKey)
type WalletBenchByron = Wallet (RndState 'Mainnet)

instance NFData (DBLayer m s) where
    rnf _ = ()

instance NFData SqliteContext where
    rnf _ = ()

testCp :: WalletBench
testCp = snd $ initWallet block0 initDummySeqState

testCpByron :: WalletBenchByron
testCpByron = snd $ initWallet block0 initDummyRndState

{-# NOINLINE initDummySeqState #-}
initDummySeqState :: SeqState 'Mainnet ShelleyKey
initDummySeqState = mkSeqStateFromRootXPrv
    ShelleyKeyS (RootCredentials xprv mempty) purposeCIP1852 defaultAddressPoolGap
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
ourAccount = publicKey ShelleyKeyS
    $ unsafeGenerateKeyFromSeed (seed, Nothing) mempty
  where
    seed = someDummyMnemonic (Proxy @15)

rewardAccount :: ShelleyKey 'CredFromKeyK XPub
rewardAccount = publicKey ShelleyKeyS
    $ unsafeGenerateKeyFromSeed (seed, Nothing) mempty
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
    delegationAddress SMainnet
        (ShelleyKey $ unsafeXPub $ B8.pack $ take 64 $ randoms $ mkStdGen seed)
        rewardAccount
  where
    -- Generate a seed using two prime numbers and a pair of index. This should
    -- lead to a satisfactory entropy.
    seed = 1459*i + 1153*j
    unsafeXPub = fromMaybe (error "xpubFromBytes error") . xpubFromBytes

mkByronAddress :: Int -> Int -> Address
mkByronAddress i j =
    paymentAddress @ByronKey @'CredFromKeyK SMainnet
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
