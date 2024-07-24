{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2023–2023 IOHK

This benchmark measures the running time of various functions from
the main module "Cardano.Wallet". The benchmark loads wallet states
from database files inside a given directory. These wallets are not
actively synchronized to the blockchain.

This benchmark design is useful for the following tasks:

* Compare the performance for wallet states with different ``shapes'',
  e.g. wallets with many addresses and few UTxO, or vice-versa.
* Detect performance regressions or improvements as the codebase changes.
* Develop performance improvements, by enabling speedy testing of
  hypotheses on the code. (In order to keep recompilation times
  short, the benchmark exercises the "Cardano.Wallet" module,
  but not the full HTTP endpoints.)

This benchmark is not so useful for the following tasks:

* Accurately measure the absolute running time for wallets of
  realistic sizes. (Unless we generate a corresponding wallet state file.)
* Accurately measure the absolute running time of all HTTP API endpoints,
  as this is at odds with the goal of allowing speedy hypothesis testing.
  (We want the "Cardano.Wallet" module to faithfully represent the API
  and expect that only a small cost of serializing/deserializing to/from JSON
  is incurred comparing to the HTTP API.)

-}
module Main where

import Prelude

import Cardano.BM.Data.Tracer
    ( filterSeverity
    )
import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , overToTextTracer
    , withToTextTracer
    )
import Cardano.BM.Tracing
    ( Severity (..)
    )
import Cardano.Wallet
    ( WalletLayer (..)
    , readWalletMeta
    )
import Cardano.Wallet.Address.Derivation
    ( DelegationAddress (..)
    , Depth (..)
    , delegationAddressS
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState (..)
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState (..)
    )
import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Reporter (addSemantic, report)
    , Result (Result)
    , Semantic
    , Unit (Seconds)
    , mkSemantic
    , newReporterFromEnv
    )
import Cardano.Wallet.BenchShared
    ( Time
    , bench
    , runBenchmarks
    , unTime
    )
import Cardano.Wallet.DB.Layer
    ( PersistAddressBook
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyLedgerProtocolParameters
    , dummyNetworkLayer
    , dummyProtocolParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    )
import Cardano.Wallet.Flavor
    ( KeyFlavor
    , KeyOf
    , WalletFlavor (..)
    , keyFlavor
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Model
    ( totalUTxO
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , NetworkDiscriminant (..)
    , NetworkId (..)
    , SNetworkId (..)
    , networkIdVal
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , hoistTimeInterpreter
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..)
    , WalletId
    , WalletMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( HistogramBar (..)
    , UTxOStatistics (..)
    )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT
    )
import Control.Monad
    ( forM
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Aeson
    ( ToJSON (..)
    , genericToJSON
    , (.=)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text
    ( Text
    )
import Fmt
    ( Buildable
    , Builder
    , blockListF'
    , build
    , genericF
    , nameF
    , pretty
    )
import GHC.Generics
    ( Generic
    )
import Main.Utf8
    ( withUtf8
    )
import Numeric.Natural
    ( Natural
    )
import Say
    ( sayErr
    )
import System.IO
    ( stdout
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.DB.Layer as DB
import qualified Cardano.Wallet.DB.Layer as Sqlite
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Transaction as Tx
import qualified Data.Aeson as Aeson
import Data.Functor
    ( (<&>)
    )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Internal.Cardano.Write.Tx as Write
    ( MaybeInRecentEra (InRecentEraBabbage)
    )
import qualified System.Environment as Sys
import qualified System.Exit as Sys

{-------------------------------------------------------------------------------
    Main function
-------------------------------------------------------------------------------}
main :: IO ()
main = withUtf8 $ evalContT $ do
    tr <- withToTextTracer (Left stdout) Nothing
    args <- liftIO Sys.getArgs
    case args of
        [] -> do
            sayErr "expected 1 argument: directory with wallet database files"
            liftIO Sys.exitFailure
        (dir: _) -> benchmarkApi tr dir

benchmarkApi :: ToTextTracer -> FilePath -> ContT r IO ()
benchmarkApi ttr@(ToTextTracer tr) dir = do
    report <- newReporterFromEnv tr $ mkSemantic ["wallet","api"]
    let onlyErrors = overToTextTracer (filterSeverity (const $ pure Error)) ttr
    liftIO $ withSNetworkId (NTestnet 0) $ \networkId ->
        runBenchmarks
            [ benchmarkWallets report "sequential"
                dir onlyErrors networkId benchmarksSeq
            , benchmarkWallets report "shared"
                dir onlyErrors networkId benchmarksShared
            , benchmarkWallets report "random"
                dir onlyErrors networkId benchmarksRnd
            ]

{-------------------------------------------------------------------------------
    Wallet API benchmarks
-------------------------------------------------------------------------------}
data WalletOverview = WalletOverview
    { utxo :: UTxOStatistics
    , addresses :: Word
    , transactions :: Word
    } deriving (Show, Generic)

walletOverviewSemantic :: WalletOverview -> Semantic
walletOverviewSemantic WalletOverview{addresses,transactions} =
    mkSemantic
        [ T.pack $ show addresses <> "-addresses"
        , T.pack $ show transactions <> "-transactions"
        ]

instance Buildable WalletOverview where
    build WalletOverview{utxo,addresses,transactions} =
        blockListF' "" id
            [ nameF "number of addresses" (build addresses)
            , nameF "number of transactions" (build transactions)
            , build utxo
            ]

instance ToJSON WalletOverview where
    toJSON WalletOverview{utxo,addresses,transactions} = Aeson.object
        [ "utxo" .= jsonFromUTxOStatistics utxo
        , "addresses" .= addresses
        , "transactions" .= transactions
        ]

jsonFromUTxOStatistics :: UTxOStatistics -> Aeson.Value
jsonFromUTxOStatistics UTxOStatistics{histogram,allStakes,boundType} =
    Aeson.object
        [ "total" .=
            (Quantity (fromIntegral allStakes) :: Quantity "lovelace" Natural)
        , "scale" .= genericToJSON Aeson.defaultOptions boundType
        , "distribution" .=
            Map.fromList (map (\(HistogramBar k v)-> (k,v)) histogram)
        ]

{-------------------------------------------------------------------------------
    Wallet API benchmarks
        SeqState
-------------------------------------------------------------------------------}
data BenchSeqResults = BenchSeqResults
    { benchName :: Text
    , walletOverview :: WalletOverview
    , readWalletTime :: Time
    , getWalletUtxoSnapshotTime :: Time
    , listAddressesTime :: Time
    , listAssetsTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , createMigrationPlanTime :: Time
    , delegationFeeTime :: Time
    } deriving (Show, Generic)

mkSeqBenchmarks :: BenchSeqResults -> [Benchmark]
mkSeqBenchmarks BenchSeqResults{..} =
    [ readWalletTime
    , getWalletUtxoSnapshotTime
    , listAddressesTime
    , listAssetsTime
    , listTransactionsTime
    , listTransactionsLimitedTime
    , createMigrationPlanTime
    , delegationFeeTime
    ]
        <&> \x ->
            Benchmark
                (walletOverviewSemantic walletOverview)
                (Result (unTime x) Seconds 1)

instance Buildable BenchSeqResults where
    build = genericF

instance ToJSON BenchSeqResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksSeq
    :: forall n s .
        ( s ~ SeqState n ShelleyKey
        , HasSNetworkId n
        , DelegationAddress ShelleyKey 'CredFromKeyK
        )
    => Reporter IO
    -> BenchmarkConfig n s
    -> IO BenchSeqResults
benchmarksSeq reporter BenchmarkConfig{benchmarkName,ctx} = do
    ((cp, pending), readWalletTime) <- bench "readWallet" $ do
        (cp, _, pending) <- W.readWallet ctx
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (_, getWalletUtxoSnapshotTime) <- bench "getWalletUtxoSnapshot"
        $ length
        <$> W.getWalletUtxoSnapshot ctx

    (addresses, listAddressesTime) <- bench "listAddresses"
        $ fromIntegral . length
        <$> W.listAddresses ctx (const pure)

    (_, listAssetsTime) <- bench "listAssets"
        $ length
        <$> W.listAssets @s ctx

    (transactions, listTransactionsTime) <- bench "listTransactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions ctx
            Nothing Nothing Nothing Descending Nothing Nothing

    (_, listTransactionsLimitedTime) <- bench "listTransactions (max_count=50)"
        $ unsafeRunExceptT
        $ W.listTransactions ctx
            Nothing Nothing Nothing Descending (Just 50) Nothing

    (_, createMigrationPlanTime) <- bench "createMigrationPlan"
        $ W.createMigrationPlan ctx Tx.NoWithdrawal

    (_, delegationFeeTime) <- bench "delegationFee" $ do
        W.delegationFee
            (W.dbLayer_ ctx) (W.networkLayer_ ctx)
            (W.defaultChangeAddressGen (delegationAddressS @n))

    let seqResult = BenchSeqResults
            { benchName = benchmarkName
            , walletOverview = WalletOverview{utxo,addresses,transactions}
            , readWalletTime
            , getWalletUtxoSnapshotTime
            , listAddressesTime
            , listAssetsTime
            , listTransactionsTime
            , listTransactionsLimitedTime
            , createMigrationPlanTime
            , delegationFeeTime
            }
    report reporter $ mkSeqBenchmarks seqResult
    pure seqResult

{-------------------------------------------------------------------------------
    Wallet API benchmarks
        SharedState
-------------------------------------------------------------------------------}
data BenchSharedResults = BenchSharedResults
    { benchName :: Text
    , walletOverview :: WalletOverview
    , readWalletTime :: Time
    , getWalletUtxoSnapshotTime :: Time
    , listAddressesTime :: Time
    , listAssetsTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    } deriving (Show, Generic)

benchSharedResultsToBenchmarks :: BenchSharedResults -> [Benchmark]
benchSharedResultsToBenchmarks BenchSharedResults{..} =
    [ readWalletTime
    , getWalletUtxoSnapshotTime
    , listAddressesTime
    , listAssetsTime
    , listTransactionsTime
    , listTransactionsLimitedTime
    ]
        <&> \x ->
            Benchmark
                (walletOverviewSemantic walletOverview)
                (Result (unTime x) Seconds 1)

instance Buildable BenchSharedResults where
    build = genericF

instance ToJSON BenchSharedResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksShared
    :: forall n s
     . ( s ~ SharedState n SharedKey
       , HasSNetworkId n
       )
    => Reporter IO
    -> BenchmarkConfig n s
    -> IO BenchSharedResults
benchmarksShared reporter BenchmarkConfig{benchmarkName,ctx} = do
    ((cp, pending), readWalletTime) <- bench "readWallet" $ do
        (cp, _, pending) <- W.readWallet ctx
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (_, getWalletUtxoSnapshotTime) <- bench "getWalletUtxoSnapshot"
        $ length
        <$> W.getWalletUtxoSnapshot ctx

    (addresses, listAddressesTime) <- bench "listAddresses"
        $ fromIntegral . length
        <$> W.listAddresses ctx (const pure)

    (_, listAssetsTime) <- bench "listAssets"
        $ length
        <$> W.listAssets @s ctx

    (transactions, listTransactionsTime) <- bench "listTransactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions ctx
            Nothing Nothing Nothing Descending Nothing Nothing

    (_, listTransactionsLimitedTime) <- bench "listTransactions (max_count=50)"
        $ unsafeRunExceptT
        $ W.listTransactions ctx
            Nothing Nothing Nothing Descending (Just 50) Nothing
    let sharedResult = BenchSharedResults
            { benchName = benchmarkName
            , walletOverview = WalletOverview{utxo,addresses,transactions}
            , readWalletTime
            , getWalletUtxoSnapshotTime
            , listAddressesTime
            , listAssetsTime
            , listTransactionsTime
            , listTransactionsLimitedTime
            }
    report reporter $ benchSharedResultsToBenchmarks sharedResult
    pure sharedResult

{-------------------------------------------------------------------------------
    Wallet API benchmarks
        RndState
-------------------------------------------------------------------------------}
data BenchRndResults = BenchRndResults
    { benchName :: Text
    , walletOverview :: WalletOverview
    , readWalletTime :: Time
    , getWalletUtxoSnapshotTime :: Time
    , listAddressesTime :: Time
    , listAssetsTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , createMigrationPlanTime :: Time
    } deriving (Show, Generic)

benchRndResultToBenchmarks :: BenchRndResults -> [Benchmark]
benchRndResultToBenchmarks BenchRndResults{..} =
    [ readWalletTime
    , getWalletUtxoSnapshotTime
    , listAddressesTime
    , listAssetsTime
    , listTransactionsTime
    , listTransactionsLimitedTime
    , createMigrationPlanTime
    ] <&> \x -> Benchmark
        (walletOverviewSemantic walletOverview)
        (Result (unTime x) Seconds 1)

instance Buildable BenchRndResults where
    build = genericF

instance ToJSON BenchRndResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksRnd
    :: forall n s
     . s ~ RndState n
    => Reporter IO
    -> BenchmarkConfig n s
    -> IO BenchRndResults
benchmarksRnd reporter BenchmarkConfig{benchmarkName,ctx} = do
    ((cp, pending), readWalletTime) <- bench "readWallet" $ do
        (cp, _, pending) <- W.readWallet ctx
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (_, getWalletUtxoSnapshotTime) <- bench "getWalletUtxoSnapshot"
        $ length
        <$> W.getWalletUtxoSnapshot ctx

    (addresses, listAddressesTime) <- bench "listAddresses"
        $ fromIntegral . length
        <$> W.listAddresses ctx (const pure)

    (_, listAssetsTime) <- bench "listAssets"
        $ length
        <$> W.listAssets @s ctx

    (transactions, listTransactionsTime) <- bench "listTransactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions ctx
            Nothing Nothing Nothing Descending Nothing Nothing

    (_, listTransactionsLimitedTime) <- bench "listTransactions (max_count=50)"
        $ unsafeRunExceptT
        $ W.listTransactions ctx
            Nothing Nothing Nothing Descending (Just 50) Nothing

    (_, createMigrationPlanTime) <- bench "createMigrationPlan"
        $ W.createMigrationPlan ctx Tx.NoWithdrawal

    let rndResult = BenchRndResults
            { benchName = benchmarkName
            , walletOverview = WalletOverview{utxo,addresses,transactions}
            , readWalletTime
            , getWalletUtxoSnapshotTime
            , listAddressesTime
            , listAssetsTime
            , listTransactionsTime
            , listTransactionsLimitedTime
            , createMigrationPlanTime
            }
    report reporter $ benchRndResultToBenchmarks rndResult
    pure rndResult

{-------------------------------------------------------------------------------
    Benchmark Harness
-------------------------------------------------------------------------------}
newtype SomeBenchmarkResults = SomeBenchmarkResults Builder

instance Buildable SomeBenchmarkResults where
    build (SomeBenchmarkResults x) = x

data BenchmarkConfig (n :: NetworkDiscriminant) s =
    BenchmarkConfig
        { benchmarkName :: Text
        , networkId :: SNetworkId n
        , ctx :: WalletLayer IO s
        , wid :: WalletId
        }

-- | Run benchmarks on all wallet databases in a given directory.
benchmarkWallets
    :: forall n s results
     . ( PersistAddressBook s
       , KeyFlavor (KeyOf s)
       , Buildable results
       , ToJSON results
       , Show results
       , WalletFlavor s
       )
    => Reporter IO
    -> Text
        -- ^ Benchmark name (used for naming resulting files)
    -> FilePath
        -- ^ Directory from which to load database files
    -> ToTextTracer
        -- ^ For wallet tracing
    -> SNetworkId n
    -> ( Reporter IO -> BenchmarkConfig n s -> IO results )
        -- ^ Benchmark to run
    -> IO [SomeBenchmarkResults]
benchmarkWallets reporter benchName dir walletTr networkId action = do
    withWalletsFromDirectory dir walletTr networkId
        $ \ctx@(WalletLayer{dbLayer_=DB.DBLayer{atomically,walletState}}) wid
        -> do
            WalletMetadata{name} <- atomically $ readWalletMeta walletState
            let config = BenchmarkConfig
                    { benchmarkName = benchName <> " " <> pretty name
                    , networkId = networkId
                    , ctx
                    , wid
                    }
            sayErr $ "*** " <> benchmarkName config
            let sem = mkSemantic [benchName, T.replace " " "-" $ pretty name]
            results <- action (addSemantic reporter sem) config
            print results
            saveBenchmarkPoints (benchmarkName config) results
            pure $ SomeBenchmarkResults (build results)
  where
    saveBenchmarkPoints :: ToJSON a => Text -> a -> IO ()
    saveBenchmarkPoints benchname =
        Aeson.encodeFile (T.unpack benchname <> ".json")

mockNetworkLayer :: NetworkLayer IO Read.ConsensusBlock
mockNetworkLayer = dummyNetworkLayer
    { timeInterpreter = hoistTimeInterpreter liftIO mockTimeInterpreter
    , currentSlottingParameters = pure dummySlottingParameters
    , currentProtocolParameters = pure dummyProtocolParameters
    , currentProtocolParametersInRecentEras =
        pure $ Write.InRecentEraBabbage dummyLedgerProtocolParameters
    , currentNodeEra = pure $ Cardano.anyCardanoEra Cardano.BabbageEra
    , currentNodeTip = pure Read.BlockTip
        { Read.slotNo = Read.SlotNo 123456789
        , Read.blockNo = Read.BlockNo 12345
        , Read.headerHash = mockHash
        }
    }

mockHash :: Read.RawHeaderHash
mockHash = Read.mockRawHeaderHash 0

mockTimeInterpreter :: TimeInterpreter IO
mockTimeInterpreter = dummyTimeInterpreter

withWalletsFromDirectory
    :: forall n s k a
     . ( PersistAddressBook s
       , WalletFlavor s
       , k ~ KeyOf s
       , KeyFlavor k
       )
    => FilePath
        -- ^ Directory of database files
    -> ToTextTracer
    -> SNetworkId n
    -> (WalletLayer IO s -> WalletId -> IO a)
    -> IO [a]
withWalletsFromDirectory dir (ToTextTracer tr) networkId action = do
    DB.DBFactory{listDatabases,withDatabaseLoad}
        <- DB.newDBFactory (walletFlavor @s)
            tr migrationDefaultValues ti (Just dir)
    wids <- listDatabases
    forM wids $ \wid ->
        withDatabaseLoad wid $
            flip action wid . mkMockWalletLayer
  where
    mkMockWalletLayer =
        WalletLayer tr genesis mockNetworkLayer tl
    genesis = error "not implemented: genesis data"
    ti = mockTimeInterpreter
    tl = newTransactionLayer
            (keyFlavor @k) (networkIdVal networkId)
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        , Sqlite.defaultDesiredNumberOfPool = 0
        , Sqlite.defaultMinimumUTxOValue = Coin 1000
        , Sqlite.defaultHardforkEpoch = Nothing
        , Sqlite.defaultKeyDeposit = Coin 0
        }
