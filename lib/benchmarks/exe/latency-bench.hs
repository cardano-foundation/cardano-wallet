{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (ConwayEra)
    )
import Cardano.Balance.Tx.Eras
    ( MaybeInRecentEra (InRecentEraConway)
    )
import Cardano.Launcher.Node
    ( cardanoNodeConn
    , nodeSocketFile
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Ledger.BaseTypes
    ( StrictMaybe (SJust)
    , TxIx (..)
    , unsafeNonZero
    )
import Cardano.Ledger.Api
    ( addrTxOutL
    , coinTxOutL
    , auxDataHashTxBodyL
    , auxDataTxL
    , bodyTxL
    , feeTxBodyL
    , hashTxAuxData
    , inputsTxBodyL
    , metadataTxAuxDataL
    , mkBasicTxAuxData
    , outputsTxBodyL
    , ppMaxBBSizeL
    , ppMaxTxSizeL
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (sgNetworkMagic)
    )
import Cardano.Ledger.TxIn
    ( TxIn (..)
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Api.Types.Dapp.Context
    ( ApiDappContextNetwork (..)
    , ApiDappHex (..)
    , ApiDappOutpoint (ApiDappOutpoint)
    , ApiDappProvenance (Pending)
    , ApiDappSubmissionRequest (..)
    , ApiDappTransactionContextRequest (..)
    , ApiDappTransactionContextResponse
    , decodeTransactionContextResponse
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiError (ApiError)
    , ApiErrorInfo (DappContextUnavailable, DappInvalidRequest)
    , ApiErrorMessage (ApiErrorMessage)
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    , ApiWalletMigrationPlanPostData (..)
    , ApiWalletMigrationPostData (..)
    , PostTransactionFeeOldData (..)
    , PostTransactionOldData (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (..)
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra
    )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiAddress (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiSerialisedTransaction (..)
    , ApiSignTransactionPostData (..)
    )
import qualified Cardano.Wallet.Api.Http.Shelley.TransactionContext as TxContext
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    )
import Cardano.Wallet.Application
    ( Tracers
    , Tracers' (..)
    , serveWallet
    , serveWalletWithNetworkDecorator
    )
import Cardano.Wallet.Application.CLI
    ( Port (..)
    )
import Cardano.Wallet.Application.Server
    ( Listen (ListenOnPort)
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( keyTypeDescriptor
    )
import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Reporter (..)
    , Result (..)
    , Unit (Milliseconds)
    , mkSemantic
    , newReporterFromEnv
    , report
    )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    )
import Cardano.Wallet.DB.Layer
    ( withLoadDBLayerFromFile
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( DappSubmissionInputRole (..)
    , DappSubmissionStatusEnum (..)
    )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DurableSubmission (..)
    , DurableSubmissionInput (..)
    , DurableSubmissionInsert (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( hoistTimeInterpreter
    , mkSingleEraInterpreter
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    , WalletFlavorS (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Benchmarks.Latency.BenchM
    ( BenchCtx (..)
    , BenchM
    , finallyDeleteWallet
    , fixtureMultiAssetWallet
    , fixtureWallet
    , fixtureWalletWith
    , request
    , requestWithError
    )
import Cardano.Wallet.Benchmarks.Latency.Measure
    ( meanAvg
    , withLatencyLogging
    )
import Cardano.Wallet.DRep.Metadata
    ( defaultIpfsGatewayUrl
    )
import Cardano.Wallet.Faucet
    ( Faucet (massiveWalletMnemonic)
    )
import Cardano.Wallet.Launch.Cluster
    ( Config (..)
    , FaucetFunds (..)
    , RunningNode (..)
    , defaultPoolConfigs
    , testnetMagicToNatural
    , withCluster
    , withSingleNodeCluster
    , withFaucet
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( clusterConfigsDirParser
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( OsNamedPipe (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , mkRelDirOf
    , newAbsolutizer
    , toFilePath
    )
import Cardano.Wallet.Network
    ( DappTransactionContext (..)
    , NetworkLayer (..)
    , ErrDappTransactionContext (..)
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Network.Ports
    ( portFromURL
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromGenesisData
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Cardano.Wallet.Primitive.Types
    ( GenesisParameters (getGenesisBlockDate, getGenesisBlockHash)
    , NetworkParameters (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (Incoming)
    , TxStatus (InLedger)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMetadata
    ( TxMetadataValue (..)
    , toShelleyMetadata
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (serialisedTx)
    , sealedTxFromBytes
    )
import Cardano.Wallet.Network.Implementation
    ( runBoundedQuery
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (Error)
    )
import Cardano.Wallet.Tracing.Data.Tracer
    ( filterSeverity
    )
import Cardano.Wallet.Tracing.Extra
    ( stdoutTextTracer
    , trMessage
    )
import Cardano.Wallet.Tracing.ToTextTracer
    ( ToTextTracer (..)
    , overToTextTracer
    , withToTextTracer
    )
import Cardano.Wallet.Tracing.Trace
    ( nullTracer
    , traceInTVarIO
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Cardano.Read.Ledger.Tx.CBOR
    ( TxWithOutputBytes (..)
    , deserializeTxWithOutputBytes
    , serializeTx
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Control.DeepSeq
    ( force
    , rnf
    )
import Control.Exception
    ( displayException
    , evaluate
    , finally
    )
import Cardano.Wallet.Read
    ( Conway
    )
import Control.Applicative
    ( (<**>)
    )
import Control.Concurrent
    ( ThreadId
    , myThreadId
    , threadDelay
    )
import Control.Monad
    ( forM
    , replicateM
    , replicateM_
    , void
    , when
    )
import Control.Monad.Catch
    ( Exception
    , MonadThrow (..)
    )
import Control.Monad.Cont
    ( evalContT
    )
import Data.Functor.Identity
    ( runIdentity
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.IO.Unlift
    ( toIO
    )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Control.Monad.Reader
    ( MonadReader (..)
    , ReaderT (..)
    , lift
    )
import Data.Bifunctor
    ( bimap
    )
import Data.Aeson
    ( eitherDecode
    , encode
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.List
    ( sort
    , stripPrefix
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , set
    , view
    , (^.)
    )
import Data.Generics.Labels
    (
    )
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import Data.Time
    ( NominalDiffTime
    )
import Data.Text.Class
    ( toText
    )
import Fmt
    ( Builder
    , build
    )
import GHC.Conc
    ( getNumCapabilities
    )
import GHC.Clock
    ( getMonotonicTimeNSec
    )
import GHC.Stats
    ( RTSStats (..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import Main.Utf8
    ( withUtf8
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Numeric.Natural
    ( Natural
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientEnv (..)
    , ClientError (..)
    , ClientM
    , Scheme (..)
    , mkClientEnv
    )
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.Environment.Extended
    ( isEnvSet
    )
import System.Environment
    ( getEnvironment
    , getExecutablePath
    , lookupEnv
    , setEnv
    )
import System.Exit
    ( ExitCode (..)
    )
import System.Info
    ( arch
    , os
    )
import System.IO
    ( stdout
    )
import System.Process
    ( CreateProcess (..)
    , proc
    , readCreateProcessWithExitCode
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import System.Path
    ( absDir
    , absFile
    , relDir
    , (</>)
    )
import Text.Read
    ( readMaybe
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , eventually
    , faucetAmt
    , fixturePassphrase
    , minUTxOValue
    , pickAnAsset
    , runResourceT
    , shouldBe
    , utxoStatisticsFromCoins
    )
import UnliftIO.Async
    ( async
    , cancel
    , race_
    , wait
    , waitCatch
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import "extra" System.IO.Extra
    ( withTempFile
    )
import Prelude

import qualified Cardano.Wallet.Api.Clients.Network as CN
import qualified Cardano.Ledger.Coin as LedgerCoin
import qualified Cardano.Wallet.DB.Sqlite.Types as DB
import qualified Data.ByteString.Lazy as BL
import qualified System.FilePath as FP
import qualified Cardano.Ledger.Api.Tx.Address as LedgerAddress
import qualified Cardano.Wallet.Api.Clients.Testnet.Id as C
import qualified Cardano.Wallet.Api.Clients.Shelley as WalletClient
import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C
import qualified Cardano.Wallet.Benchmarks.Latency.Measure as Measure
import qualified Cardano.Wallet.Faucet as Faucet
import Cardano.Network.NodeToClient.Version
    ( NodeToClientVersionData (..)
    )
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Options.Applicative as O
import qualified Data.ByteString as BS
import qualified Cardano.Read.Ledger.Tx.Tx as LedgerTx
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Read.Hash as ReadHash
import qualified Cardano.Wallet.Unsafe as Unsafe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Cardano.Wallet.Primitive.Model as Wallet
import qualified Cardano.Wallet.Primitive.Types.UTxO as Wallet
import qualified Data.Yaml as Yaml

splitComma :: String -> [String]
splitComma xs = case break (== ',') xs of
    (field, []) -> [field]
    (field, _ : rest) -> field : splitComma rest

contextMetric :: String -> String -> String -> Map.Map String Integer
contextMetric label field output =
    Map.fromList $ mapMaybe parse $ lines output
  where
    parse line = case splitComma line of
        prefix : name : fields | prefix == label ->
            case mapMaybe (stripPrefix $ field <> "=") fields of
                [value] -> Just (name, fromMaybe
                    (error $ "context benchmark: invalid " <> field <> ": " <> line)
                    $ readMaybe value)
                _ -> error $ "context benchmark: missing " <> field <> ": " <> line
        _ -> Nothing

checkContextBudget
    :: String -> (String -> Map.Map String Integer)
    -> [String] -> String -> IO ()
checkContextBudget label metric baselines finalRun = do
    let baseline = metric <$> baselines
        final = metric finalRun
    case baseline of
        [first, second, third] -> do
            if Map.null first || any ((/= Map.keysSet first) . Map.keysSet)
                [second, third, final]
                then error $ "context benchmark: incomplete " <> label <> " results"
                else mapM_ (verify second third final) $ Map.toList first
        _ -> error "context benchmark: expected exactly three fresh baseline processes"
  where
    verify second third final (name, first) = do
        let values = [first, second Map.! name, third Map.! name]
            largest = maximum values
            smallest = minimum values
            budget = (6 * largest + 4) `div` 5
            observed = final Map.! name
        putStrLn $ "context_budget," <> label <> "," <> name
            <> ",baseline=" <> show values
            <> ",limit=" <> show budget
            <> ",final=" <> show observed
        if smallest <= 0 || 5 * largest > 6 * smallest
            then error $ "context benchmark: baseline variation exceeds 20%: "
                <> label <> "/" <> name <> "; remove machine load and repeat"
            else if observed > budget
                then error $ "context benchmark: regression above budget: "
                    <> label <> "/" <> name
                else pure ()

-- Each measured wallet/API/RTS runs in a fresh OS process with its own
-- disposable DB while the pinned local node stays alive for all four runs.
runContextCoordinator :: Bool -> FilePath -> FilePath -> Int -> IO ()
runContextCoordinator enlarged clusterRoot socket faucetPort = do
    baseline <- lookupEnv "CONTEXT_BENCH_BASELINE_EXE"
        >>= maybe (error "context benchmark: missing optimized baseline executable")
            pure
    corpus <- lookupEnv "CONTEXT_CORPUS_DIR"
        >>= maybe (error "context benchmark: missing frozen corpus directory") pure
    workerCpus <- lookupEnv "CONTEXT_BENCH_WORKER_CPUSET"
    putStrLn "context_cadence,fresh_native_tip_alignment,immediate_recheck,settle_us=0,post-request-or-wave_cooldown_us=250000"
    let corpusSession = corpus FP.</> FP.takeFileName clusterRoot
    createDirectoryIfMissing True corpusSession
    finalBinary <- getExecutablePath
    inherited <- getEnvironment
    let profile = if enlarged then "enlarged" else "standard"
        overrides index =
            [ ("CONTEXT_BENCH_CHILD", "1")
            , ("CONTEXT_BENCH_COORDINATE", "0")
            , ("CONTEXT_BENCH_NODE_SOCKET", socket)
            , ("CONTEXT_BENCH_CLUSTER_DIR", clusterRoot)
            , ("CONTEXT_BENCH_FAUCET_PORT", show faucetPort)
            , ("CONTEXT_BENCH_PROFILE", profile)
            , ("CONTEXT_BENCH_RUN_INDEX", show index)
            , ("CONTEXT_CORPUS_DIR", corpusSession)
            ]
        runProcess index = do
            let binary = if index <= 3 then baseline else finalBinary
                additions = overrides index
                childEnv = additions <> filter
                    (\(key, _) -> key `notElem` (fst <$> additions)) inherited
                binaryArgs = ["+RTS", "-N8", "-A32m", "-F1.2", "-T"]
                childProcess = case workerCpus of
                    Nothing -> proc binary binaryArgs
                    Just cpus ->
                        proc "taskset" (["-c", cpus, binary] <> binaryArgs)
            putStrLn $ "context_process," <> profile <> ",run=" <> show index
                <> ",role=" <> (if index <= 3 then "baseline" else "final")
                <> ",binary=" <> binary
                <> ",worker_cpus=" <> fromMaybe "inherited" workerCpus
                <> ",rts=" <> unwords binaryArgs
            (status, stdoutText, stderrText) <-
                readCreateProcessWithExitCode
                    childProcess { env = Just childEnv }
                    ""
            let record = stdoutText <> stderrText
            writeFile (corpusSession FP.</> profile <> "-run-"
                <> show index <> ".log") record
            putStr stdoutText
            putStr stderrText
            case status of
                ExitSuccess -> pure stdoutText
                ExitFailure code -> error $ "context benchmark: worker "
                    <> profile <> "/" <> show index
                    <> " exited " <> show code
    reports <- forM [1 .. 4 :: Int] runProcess
    case reports of
        [a, b, c, result] -> do
            checkContextBudget "p95_ns"
                (contextMetric "context_summary" "p95")
                [a, b, c] result
            checkContextBudget "maximum_residency_bytes"
                (contextMetric "context_rts" "process_peak_live")
                [a, b, c] result
            checkContextBudget "peak_resident_bytes"
                (contextMetric "context_residency"
                    "process_peak_resident_bytes")
                [a, b, c] result
        _ -> error "context benchmark: missing fresh worker run"

main :: IO ()
main = withUtf8 $ evalContT $ do
    contextBench <- liftIO $ (== Just "1") <$> lookupEnv "CONTEXT_BENCH"
    tr <- withToTextTracer (Left stdout) Nothing
    let ToTextTracer onlyErrors =
            overToTextTracer (filterSeverity (const $ pure Error)) tr
        setupTracers tvar =
            Tracers
                { apiServerTracer = if contextBench
                    then onlyErrors
                    else trMessage $ snd >$< traceInTVarIO tvar
                , applicationTracer = onlyErrors
                , tokenMetadataTracer = onlyErrors
                , walletEngineTracer = onlyErrors
                , walletDbTracer = onlyErrors
                , poolsEngineTracer = onlyErrors
                , poolsDbTracer = onlyErrors
                , ntpClientTracer = onlyErrors
                , networkTracer = onlyErrors
                }
    (tracers, capture) <- withLatencyLogging setupTracers
    let ToTextTracer tr' = tr
        semantic = mkSemantic ["latency"]
    reporter <- newReporterFromEnv tr' semantic
    let run enlarged =
            withShelleyServer tracers enlarged
                $ \massiveWalletMnemonic' ctx dbDir exhaustion queries ->
                    runReaderT
                        ( runResourceT
                            $ if contextBench
                                then walletContextBench reporter enlarged dbDir exhaustion queries massiveWalletMnemonic'
                                else walletApiBench reporter massiveWalletMnemonic'
                        )
                        $ BenchCtx ctx capture
    workerProfile <- liftIO $ lookupEnv "CONTEXT_BENCH_PROFILE"
    liftIO $ if contextBench
        then case workerProfile of
            Just "standard" -> run False
            Just "enlarged" -> run True
            Nothing -> run False >> run True
            _ -> error "context benchmark: unknown child cluster profile"
        else run False
-- The context profile uses the same live server and reporter as the original
walletContextBench
    :: Reporter IO -> Bool -> FilePath -> IORef Int -> IORef Int
    -> SomeMnemonic -> BenchM ()
walletContextBench reporter enlarged dbDir exhaustion queries mnemonic = do
    w <- contextGenesisWallet mnemonic
    BenchCtx ctx _ <- ask
    addresses <- map (view #id) <$> request (C.listAddresses (w ^. #id) Nothing)
    allInputs <- eventually "benchmark genesis wallet has 250 confirmed outpoints"
        $ fundedWalletInputs w 250
    let fundedInputs = take 50 allInputs
    let NetworkParameters genesis _ _ = _networkParameters ctx
        Hash genesisBytes = getGenesisBlockHash genesis
        network =
            ApiDappContextNetwork
                0
                (fromIntegral $ testnetMagicToNatural $ _testnetMagic ctx)
                (ApiDappHex genesisBytes)
        mkRequest bytes =
            ApiDappTransactionContextRequest 1 network (ApiDappHex <$> bytes)
        ordinary = ordinaryContextTx (head addresses) <$> fundedInputs
        fingerprint name bytes = liftIO $ do
            let json = BL.toStrict $ encode $ mkRequest bytes
                digest = BL.toStrict $ encode $ ApiDappHex $ blake2b256 json
            frozen <- lookupEnv "CONTEXT_CORPUS_DIR"
            disposition <- case frozen of
                Nothing -> pure "not-persisted"
                Just dir -> do
                    createDirectoryIfMissing True dir
                    let path = dir FP.</>
                            ((if enlarged then "enlarged-" else "standard-")
                                <> name <> ".json")
                    existing <- doesFileExist path
                    if existing
                        then do
                            previous <- BS.readFile path
                            if previous /= json
                                then error $ "context benchmark: frozen request changed: "
                                    <> name <> ",expected="
                                    <> show (BL.toStrict $ encode $ ApiDappHex
                                        $ blake2b256 previous)
                                    <> ",actual=" <> show digest
                                else pure "matched"
                        else BS.writeFile path json >> pure "recorded"
            putStrLn $ "context_corpus," <> name
                <> ",blake2b256=" <> show digest
                <> ",json_bytes=" <> show (BS.length json)
                <> ",frozen=" <> disposition
        clientFor wallet bytes =
            C.transactionContext (wallet ^. #id) (mkRequest bytes)
        client = clientFor w
    capabilities <- liftIO getNumCapabilities
    liftIO $ putStrLn $ "context_fixture,genesis_hash="
        <> show (BL.toStrict $ encode $ ApiDappHex genesisBytes)
        <> ",rts_capabilities=" <> show capabilities
        <> ",os=" <> os <> ",arch=" <> arch
        <> ",funded_inputs=" <> show (length fundedInputs)
        <> ",ordinary_request_bytes=" <> show (BL.length $ encode $ mkRequest ordinary)
    fingerprint "ordinary-50" ordinary
    if enlarged
        then do
            liftIO $ putStrLn "context genesis: enlarged maxTxSize=131072 maxBlockBodySize=1048576"
            let large = [exactContextTx n (head addresses) input 65_536 | (n, input) <- zip [0 ..] fundedInputs]
            fingerprint "exact-65536-50" large
            if all ((== 65_536) . BS.length) large && length (Set.fromList large) == 50
                then do
                    liftIO $ putStrLn $ "context_fixture,workload=exact-65536-50"
                        <> ",transactions=" <> show (length large)
                        <> ",inputs=50,outputs=50,cbor_bytes="
                        <> show (sum $ BS.length <$> large)
                        <> ",request_bytes=" <> show (BL.length $ encode $ mkRequest large)
                    liftIO $ mapM_ (pureDecodePhase "exact-65536") large
                    contextScene reporter "exact-65536-50" [w] $ client large
                    pureValidatePhase "exact-65536-50" (mkRequest large)
                        $ client large
                else error "context benchmark: maximum fixture size/identity incorrect"
            let manyWallet = w
                manyAddresses = addresses
                manyInputs = allInputs
            let groups = chunksOfN 5 manyInputs
                many =
                    [ exactFromContextTx n
                        (manyContextTx (head manyAddresses) group) 65_536
                    | (n, group) <- zip [0 ..] groups
                    ]
            fingerprint "exact-65536-many-50" many
            if length many /= 50 || any ((/= 65_536) . BS.length) many
                || length (Set.fromList many) /= 50
                then error "context benchmark: invalid many-input/output maximum fixture"
                else do
                    liftIO $ putStrLn $ "context_fixture,workload=exact-65536-many-50"
                        <> ",transactions=" <> show (length many)
                        <> ",inputs=" <> show (length manyInputs)
                        <> ",outputs=" <> show (5 * length many)
                        <> ",cbor_bytes=" <> show (sum $ BS.length <$> many)
                        <> ",request_bytes=" <> show (BL.length $ encode $ mkRequest many)
                    liftIO $ mapM_ (pureDecodePhase "exact-65536-many") many
                    contextScene reporter "exact-65536-many-50" [manyWallet]
                        $ clientFor manyWallet many
                    pureValidatePhase "exact-65536-many-50" (mkRequest many)
                        $ clientFor manyWallet many
            cancelContextScene queries (clientFor manyWallet many)
            contextRecovery "after-cancel" $ clientFor manyWallet [head many]
            boundaryBeforeQuery queries "bytes-65537" $ client [BS.snoc (head large) 0]
        else do
            liftIO $ putStrLn "context genesis: standard generated limits"
            contextScene reporter "empty" [w] $ client []
            pureValidatePhase "empty" (mkRequest []) $ client []
            contextScene reporter "ordinary-1" [w] $ client (take 1 ordinary)
            contextScene reporter "ordinary-50" [w] $ client ordinary
            pureValidatePhase "ordinary-50" (mkRequest ordinary)
                $ client ordinary
            liftIO $ mapM_ (pureDecodePhase "ordinary") ordinary
            liftIO proveBoundedQueryTimeout
            liftIO $ writeIORef exhaustion 3
            boundaryScene "query-exhaustion" 503
                (ApiError DappContextUnavailable
                    $ ApiErrorMessage "Wallet context unavailable")
                $ client []
            remaining <- liftIO $ readIORef exhaustion
            if remaining /= 0
                then error $ "context benchmark: expected 3 exhausted queries, remaining=" <> show remaining
                else contextRecovery "after-query-exhaustion" $ client []
            mapM_
                (\n -> contextBurst reporter ("one-wallet-" <> show n) [w]
                    (replicate n (client ordinary)) (client ordinary))
                [1, 4, 8]
            others <- replicateM 3 fixtureWallet
            contextBurst reporter "four-wallets-4" (w : others)
                [clientFor wallet ordinary | wallet <- w : others]
                (client ordinary)
            mapM_ (\wallet -> void $ request $ C.deleteWallet (wallet ^. #id)) others
            pendingContextScene reporter dbDir w network
                (head addresses) allInputs client
            boundaryBeforeQuery queries "count-51" $ client (ordinary <> take 1 ordinary)
            boundaryBeforeQuery queries "malformed" $ client [BS.pack [0xff]]
            boundaryBeforeQuery queries "unsupported-envelope"
                $ client [BS.pack [0x83, 0xa0, 0xa0, 0xf6]]
-- Exercise the same 30-second race used for LSQ, including cancellation of
-- its client worker; query exhaustion is a different (three-attempt) gate.
proveBoundedQueryTimeout :: IO ()
proveBoundedQueryTimeout = do
    running <- newIORef False
    started <- getMonotonicTimeNSec
    outcome <- runBoundedQuery
        (threadDelay 30_000_000)
        (writeIORef running True
            >> (threadDelay 60_000_000 `finally` writeIORef running False))
        (threadDelay 60_000_000 >> pure ())
    ended <- getMonotonicTimeNSec
    orphan <- readIORef running
    putStrLn $ "context_timeout,elapsed_ns=" <> show (ended - started)
        <> ",outcome=" <> show outcome <> ",orphan_client=" <> show orphan
    if outcome /= Nothing || orphan || ended - started < 29_000_000_000
        then error "context benchmark: bounded node query did not clean up"
        else pure ()

pureDecodePhase :: String -> BS.ByteString -> IO ()
pureDecodePhase name bytes = do
    start <- getMonotonicTimeNSec
    case TxContext.decodeDappTx (ApiDappHex bytes) of
        Left reason ->
            error $ "context benchmark: pure decode rejected " <> name
                <> ": " <> show reason
        Right tx -> do
            -- Force every decoded field, not just the Set/Map spine; the
            -- ledger transaction and exact output spans are part of this cost.
            inspected <- evaluate $ force $ show
                ( TxContext.transaction tx
                , TxContext.txId tx
                , TxContext.normal tx
                , TxContext.collateral tx
                , TxContext.reference tx
                , TxContext.outputs tx
                , TxContext.expiry tx
                , TxContext.valid tx
                )
            end <- getMonotonicTimeNSec
            putStrLn $ "context_phase,pure-decode-and-inspect," <> name <> ","
                <> show (end - start) <> ",characters=" <> show (length inspected)
pureValidatePhase
    :: String
    -> ApiDappTransactionContextRequest
    -> ClientM ApiDappTransactionContextResponse
    -> BenchM ()
pureValidatePhase name frozen action = do
    decoded <- case traverse TxContext.decodeDappTx (frozen ^. #transactions) of
        Left reason -> error $ "context benchmark: pure assembly decode failed: "
            <> name <> ": " <> show reason
        Right transactions -> pure transactions
    assemblyStart <- liftIO getMonotonicTimeNSec
    assembled <- case TxContext.buildBatchOverlay decoded [] Map.empty of
        Left reason -> error $ "context benchmark: pure assembly failed: "
            <> name <> ": " <> show reason
        Right result -> liftIO $ evaluate $ force $ show result
    assemblyEnd <- liftIO getMonotonicTimeNSec
    liftIO $ putStrLn $ "context_phase,pure-batch-overlay-assembly,"
        <> name <> "," <> show (assemblyEnd - assemblyStart)
        <> ",characters=" <> show (length assembled)
    response <- request action
    start <- liftIO getMonotonicTimeNSec
    validated <- liftIO $ evaluate $ force
        $ TxContext.validateTransactionContextResponseForRequest frozen response
    end <- liftIO getMonotonicTimeNSec
    case validated of
        Left reason -> error $ "context benchmark: failed response proof: "
            <> name <> ": " <> reason
        Right () -> liftIO $ putStrLn $ "context_phase,pure-response-proof,"
            <> name <> "," <> show (end - start)

cancelContextScene
    :: IORef Int -> ClientM ApiDappTransactionContextResponse -> BenchM ()
cancelContextScene queries action = do
    env <- ask
    initial <- liftIO $ readIORef queries
    worker <- liftIO $ async $ runReaderT (runResourceT $ requestWithError action) env
    let awaitNodeQuery 0 = pure False
        awaitNodeQuery attempts = do
            count <- readIORef queries
            if count > initial
                then pure True
                else threadDelay 1_000 >> awaitNodeQuery (attempts - 1)
    liftIO $ do
        entered <- awaitNodeQuery (5_000 :: Int)
        cancel worker
        outcome <- waitCatch worker
        putStrLn $ "context_cancel,entered_node_query=" <> show entered
            <> "," <> either displayException (const "completed-before-cancel")
                outcome
        if not entered || either (const False) (const True) outcome
            then error "context benchmark: cancellation missed node query"
            else pure ()
-- The benchmark-only faucet funds 250 fixed external addresses from this
-- mnemonic directly in genesis: no tip-dependent TTL, change output or funding
-- transaction is introduced before freezing the request corpus.
contextGenesisWallet :: SomeMnemonic -> BenchM ApiWallet
contextGenesisWallet mnemonic = do
    wallet <- request
        $ C.postWallet
        $ WalletOrAccountPostData
        $ Left
        $ WalletPostData
            { addressPoolGap = Just $ ApiT $ toEnum 301
            , mnemonicSentence = ApiMnemonicT mnemonic
            , mnemonicSecondFactor = Nothing
            , name = ApiT $ unsafeFromText "Context benchmark genesis wallet"
            , passphrase = ApiT $ unsafeFromText fixturePassphrase
            , oneChangeAddressMode = Nothing
            , singleAddressMode = Nothing
            , restorationMode = Nothing
            }
    finallyDeleteWallet wallet
    pure wallet

fundedWalletInputs :: ApiWallet -> Int -> BenchM [TxIn]
fundedWalletInputs wallet expected = do
    addresses <- map (view #id)
        <$> request (C.listAddresses (wallet ^. #id) Nothing)
    incoming <- request $ listAllTransactions (wallet ^. #id)
    let outpoints =
            [ TxIn
                (Read.txIdFromHash $ fromMaybe
                    (error "context benchmark: invalid funding transaction hash")
                    $ ReadHash.hashFromBytes hashBytes)
                (TxIx $ fromIntegral ix)
            | tx <- incoming
            , tx ^. #status . #getApiT == InLedger
            , tx ^. #direction . #getApiT == Incoming
            , let ApiT (Hash hashBytes) = tx ^. #id
            , (ix, output) <- zip [0 :: Int ..] (tx ^. #outputs)
            , output ^. #address `elem` addresses
            ]
    if length outpoints /= expected
        || length (Set.fromList outpoints) /= expected
        then error $ "context benchmark: expected " <> show expected
            <> " funded inputs, got " <> show (length outpoints)
        else pure $ sort outpoints

chunksOfN :: Int -> [a] -> [[a]]
chunksOfN _ [] = []
chunksOfN n xs = take n xs : chunksOfN n (drop n xs)

-- A genesis UTxO of ten ada supplies each distinct input.
ordinaryContextTx :: ApiAddress C.Testnet42 -> TxIn -> BS.ByteString
ordinaryContextTx =
    contextTx (LedgerCoin.Coin 5_000_000) (LedgerCoin.Coin 5_000_000)

contextTx
    :: LedgerCoin.Coin -> LedgerCoin.Coin
    -> ApiAddress C.Testnet42 -> TxIn -> BS.ByteString
contextTx fee outputCoin destination input =
    BL.toStrict $ serializeTx (LedgerTx.Tx body :: Read.Tx Conway)
  where
    TxWithOutputBytes{transaction = LedgerTx.Tx original} =
        either (error . show) Prelude.id
            $ deserializeTxWithOutputBytes @Conway
            $ BL.fromStrict
            $ Unsafe.unsafeFromHex
                "84a30081825820111111111111111111111111111111111111111111111111111111111111111100018182581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000f42400200a0f5f6"
    ledgerAddress =
        fromMaybe (error "context benchmark: invalid wallet destination")
            $ LedgerAddress.decodeAddrLenient
            $ unAddress
            $ apiAddress destination
    body :: LedgerTx.TxT Conway
    body =
        set (bodyTxL . feeTxBodyL) fee
            $ set (bodyTxL . inputsTxBodyL) (Set.singleton input)
            $ over (bodyTxL . outputsTxBodyL)
                (fmap $ set coinTxOutL outputCoin . set addrTxOutL ledgerAddress)
                original
manyContextTx :: ApiAddress C.Testnet42 -> [TxIn] -> BS.ByteString
manyContextTx _ [] = error "context benchmark: missing many-input fixture"
manyContextTx destination inputs@(first : _) =
    BL.toStrict $ serializeTx (LedgerTx.Tx body :: Read.Tx Conway)
  where
    TxWithOutputBytes{transaction = LedgerTx.Tx original} =
        either (error . show) Prelude.id
            $ deserializeTxWithOutputBytes @Conway
            $ BL.fromStrict
            $ ordinaryContextTx destination first
    originalOutputs = view (bodyTxL . outputsTxBodyL) original
    body :: LedgerTx.TxT Conway
    body =
        set (bodyTxL . feeTxBodyL) (LedgerCoin.Coin 25_000_000)
            $ set (bodyTxL . inputsTxBodyL) (Set.fromList inputs)
            $ set (bodyTxL . outputsTxBodyL)
                (mconcat $ replicate 5 originalOutputs)
                original


-- The metadata-heavy wire-limit transaction needs a fee greater than the
-- linear minimum at 65,536 bytes; preserve the ten-ada input balance.
exactContextTx :: Int -> ApiAddress C.Testnet42 -> TxIn -> Int -> BS.ByteString
exactContextTx n destination input =
    exactFromContextTx n
        (contextTx (LedgerCoin.Coin 8_000_000)
            (LedgerCoin.Coin 2_000_000) destination input)

exactFromContextTx :: Int -> BS.ByteString -> Int -> BS.ByteString
exactFromContextTx n source target = fit 0
  where
    TxWithOutputBytes{transaction = LedgerTx.Tx original} =
        either (error . show) Prelude.id
            $ deserializeTxWithOutputBytes @Conway
            $ BL.fromStrict source
    make payload filler =
        let chunks k
                | k <= 0 = []
                | otherwise =
                    TxMetaBytes (BS.replicate (min 64 k) (fromIntegral n))
                        : chunks (k - 64)
            aux =
                set metadataTxAuxDataL
                    (toShelleyMetadata $ Map.singleton 1
                        (TxMetaList $ chunks payload
                            <> replicate filler (TxMetaNumber 0)))
                    mkBasicTxAuxData
            tx :: LedgerTx.TxT Conway
            tx =
                set auxDataTxL (SJust aux)
                    $ set (bodyTxL . auxDataHashTxBodyL)
                        (SJust $ hashTxAuxData aux)
                        original
        in BL.toStrict $ serializeTx (LedgerTx.Tx tx :: Read.Tx Conway)
    -- Extra typed integer elements bridge CBOR list/chunk length transitions.
    fit filler
        | filler > 4 = error "context benchmark: no exact-size typed metadata"
        | otherwise = maybe (fit $ filler + 1) Prelude.id
            $ search filler 0 target
    search filler low high
        | low > high = Nothing
        | otherwise =
            let mid = (low + high) `div` 2
                bytes = make mid filler
            in case compare (BS.length bytes) target of
                EQ -> Just bytes
                LT -> search filler (mid + 1) high
                GT -> search filler low (mid - 1)

-- Linux high-water resident set, rather than the RTS's live-heap high-water.
-- It is process-wide and cumulative across workloads in this fixed order.
processPeakResidentBytes :: IO Integer
processPeakResidentBytes = do
    status <- readFile "/proc/self/status"
    case [read number * 1024
         | entry <- lines status
         , "VmHWM:" : number : _ <- [words entry]
         ] of
        [bytes] -> pure bytes
        _ -> error "context benchmark: missing process resident high-water"

-- Outside the HTTP stopwatch, let each real chain follower finish applying
-- the node's point. The node keeps producing blocks during the timed request.
alignContextWallets :: String -> [ApiWallet] -> BenchM ()
alignContextWallets name wallets = do
    slotSeconds <- liftIO $ lookupEnv "CONTEXT_BENCH_SLOT_SECONDS"
        >>= maybe (error "context benchmark: missing generated slot length")
            (pure . Prelude.read @Double)
    initial <- request CN.networkInformation
    let initialPoint = initial ^. #nodeTip
    started <- liftIO getMonotonicTimeNSec
    let align = do
            before <- request CN.networkInformation
            let candidate = before ^. #nodeTip
            current <- mapM (\wallet -> request $ C.getWallet (wallet ^. #id)) wallets
            let caughtUp = candidate /= initialPoint
                    && all ((== candidate) . view #tip) current
            when (not caughtUp) $ liftIO $ threadDelay 10_000
            after <- request CN.networkInformation
            ended <- liftIO getMonotonicTimeNSec
            let point = after ^. #nodeTip
            if caughtUp && candidate == point
                then liftIO $ putStrLn $ "context_alignment," <> name
                    <> ",elapsed_ns=" <> show (ended - started)
                    <> ",wallets=" <> show (length wallets)
                    <> ",slot_length_s=" <> show slotSeconds
                    <> ",settle_us=0,immediate_recheck"
                    <> ",initial_point=" <> show initialPoint
                    <> ",released_point=" <> show point
                else if ended - started >= 30_000_000_000
                    then error $ "context benchmark: fresh wallet/node alignment timeout: " <> name
                    else align
    align

-- Record each sample, including failures; percentiles count only complete
-- runs and the caller refuses to treat failures as valid baseline data.
contextScene
    :: Reporter IO
    -> String
    -> [ApiWallet]
    -> ClientM ApiDappTransactionContextResponse
    -> BenchM ()
contextScene reporter name wallets action = do
    replicateM_ 10 $ do
        alignContextWallets name wallets
        warmup <- requestWithError action
        liftIO $ putStrLn $ "context_warmup," <> name <> ","
            <> either show (const "ok") warmup
        liftIO $ threadDelay 250_000
    statsEnabled <- liftIO getRTSStatsEnabled
    before <- if statsEnabled then Just <$> liftIO getRTSStats else pure Nothing
    samples <- replicateM 100 $ do
        alignContextWallets name wallets
        start <- liftIO getMonotonicTimeNSec
        result <- requestWithError action
        bytes <- liftIO $ case result of
            Left e -> evaluate (length $ show e) >> pure 0
            Right response -> evaluate $ BL.length $ encode response
        end <- liftIO getMonotonicTimeNSec
        case result of
            Left _ -> pure ()
            Right response -> liftIO $ do
                pureStart <- getMonotonicTimeNSec
                validated <- evaluate
                    $ length
                    $ show
                    $ decodeTransactionContextResponse
                    $ encode response
                pureEnd <- getMonotonicTimeNSec
                putStrLn $ "context_phase,response-validation," <> name <> "," <> show (pureEnd - pureStart) <> "," <> show validated
        -- Leave one generated cluster slot for the previous context capture
        -- and chain follower before measuring another request.
        liftIO $ threadDelay 250_000
        pure (end - start, bytes, either show (const "ok") result)
    let times = sort [t | (t, _, "ok") <- samples]
        at p = times !! ((length times - 1) * p `div` 100)
    liftIO $ mapM_
        (\(t, bytes, outcome) -> putStrLn $ "context_raw," <> name <> "," <> show t <> "," <> show bytes <> "," <> outcome)
        samples
    case before of
        Just initial -> do
            final <- liftIO getRTSStats
            liftIO $ putStrLn $ "context_rts," <> name
                <> ",allocated=" <> show (allocated_bytes final - allocated_bytes initial)
                <> ",cpu_ns=" <> show (cpu_ns final - cpu_ns initial)
                <> ",gc_ns=" <> show (gc_elapsed_ns final - gc_elapsed_ns initial)
                <> ",gcs=" <> show (gcs final - gcs initial)
                <> ",process_gcs=" <> show (gcs final)
                <> ",major_gcs=" <> show (major_gcs final - major_gcs initial)
                <> ",process_major_gcs=" <> show (major_gcs final)
                <> ",process_peak_live=" <> show (max_live_bytes final)
            peak <- liftIO processPeakResidentBytes
            liftIO $ putStrLn $ "context_residency," <> name
                <> ",process_peak_resident_bytes=" <> show peak
        Nothing -> liftIO $ putStrLn
            "context_rts: pass +RTS -T for allocation/cpu"
    if length times /= 100
        then error $ "context benchmark: failed success sample: " <> name
        else do
            liftIO $ putStrLn $ "context_summary," <> name <> ",p50=" <> show (at 50) <> ",p95=" <> show (at 95) <> ",p99=" <> show (at 99) <> ",max=" <> show (last times)
            liftIO $ report reporter $ pure $ Benchmark
                (mkSemantic ["context", T.pack name])
                (Result (fromIntegral (at 95) / 1_000_000) Milliseconds 100)

-- Cancellation, exhaustion and burst recovery are correctness checks, not
-- extra latency workloads in the fixed 10-warmup/100-sample matrix.
contextRecovery
    :: String -> ClientM ApiDappTransactionContextResponse -> BenchM ()
contextRecovery name action = do
    start <- liftIO getMonotonicTimeNSec
    response <- request action
    bytes <- liftIO $ evaluate $ BL.length $ encode response
    end <- liftIO getMonotonicTimeNSec
    liftIO $ putStrLn $ "context_recovery," <> name
        <> ",elapsed_ns=" <> show (end - start)
        <> ",response_bytes=" <> show bytes

boundaryScene :: String -> Int -> ApiError -> ClientM a -> BenchM ()
boundaryScene name expectedStatus expectedError action = do
    result <- requestWithError action
    liftIO $ putStrLn $ "context_boundary," <> name <> ","
        <> either show (const "unexpected-success") result
    case result of
        Left (FailureResponse _ response)
            | response ^. #responseStatusCode . #statusCode == expectedStatus
            , Right actualError <- eitherDecode
                (response ^. #responseBody)
            , actualError == expectedError -> pure ()
        _ -> error $ "context benchmark: wrong boundary outcome: " <> name

boundaryBeforeQuery :: IORef Int -> String -> ClientM a -> BenchM ()
boundaryBeforeQuery queries name action = do
    before <- liftIO $ readIORef queries
    boundaryScene name 400
        (ApiError DappInvalidRequest $ ApiErrorMessage "Invalid backend request")
        action
    after <- liftIO $ readIORef queries
    if after == before
        then pure ()
        else error $ "context benchmark: invalid input reached node query: " <> name
contextBurst
    :: Reporter IO
    -> String
    -> [ApiWallet]
    -> [ClientM ApiDappTransactionContextResponse]
    -> ClientM ApiDappTransactionContextResponse
    -> BenchM ()
contextBurst reporter name wallets actions after = do
    env <- ask
    let runWave wave = do
            alignContextWallets name wallets
            gate <- liftIO newEmptyMVar
            ready <- liftIO newEmptyMVar
            workers <- liftIO $ mapM
                (\action -> async $ do
                    putMVar ready ()
                    readMVar gate
                    start <- getMonotonicTimeNSec
                    result <- runReaderT (runResourceT $ requestWithError action) env
                    case result of
                        Right response -> void $ evaluate $ BL.length $ encode response
                        Left e -> void $ evaluate $ length $ show e
                    end <- getMonotonicTimeNSec
                    pure (end - start, either show (const "ok") result))
                wave
            liftIO $ replicateM_ (length wave) $ takeMVar ready
            liftIO $ putMVar gate ()
            healthStart <- liftIO getMonotonicTimeNSec
            health <- requestWithError CN.networkInformation
            healthEnd <- liftIO getMonotonicTimeNSec
            results <- liftIO $ mapM wait workers
            liftIO $ putStrLn $ "context_health," <> name <> ","
                <> show (healthEnd - healthStart) <> ","
                <> either show (const "ok") health
            result <- case health of
                Left _ -> error $ "context benchmark: health failure: " <> name
                Right _ -> pure results
            liftIO $ threadDelay 250_000
            pure result
    mapM_
        (\action -> do
            alignContextWallets name wallets
            warmup <- requestWithError action
            liftIO $ putStrLn $ "context_warmup," <> name <> ","
                <> either show (const "ok") warmup
            liftIO $ threadDelay 250_000)
        (take 10 $ cycle actions)
    statsEnabled <- liftIO getRTSStatsEnabled
    before <- if statsEnabled then Just <$> liftIO getRTSStats else pure Nothing
    samples <- concat <$> mapM runWave
        (chunksOfN (length actions) $ take 100 $ cycle actions)
    liftIO $ mapM_
        (\(ns, outcome) -> putStrLn $ "context_burst," <> name <> ","
            <> show ns <> "," <> outcome)
        samples
    let times = sort [ns | (ns, "ok") <- samples]
    if length times /= 100
        then error $ "context benchmark: failed burst sample: " <> name
        else do
            let at p = times !! ((length times - 1) * p `div` 100)
            liftIO $ putStrLn $ "context_summary," <> name
                <> ",p50=" <> show (at 50)
                <> ",p95=" <> show (at 95)
                <> ",p99=" <> show (at 99)
                <> ",max=" <> show (last times)
            liftIO $ report reporter $ pure $ Benchmark
                (mkSemantic ["context", T.pack name])
                (Result (fromIntegral (at 95) / 1_000_000) Milliseconds 100)
            case before of
                Just initial -> do
                    final <- liftIO getRTSStats
                    peak <- liftIO processPeakResidentBytes
                    liftIO $ do
                        putStrLn $ "context_rts," <> name
                            <> ",allocated=" <> show (allocated_bytes final - allocated_bytes initial)
                            <> ",cpu_ns=" <> show (cpu_ns final - cpu_ns initial)
                            <> ",gc_ns=" <> show (gc_elapsed_ns final - gc_elapsed_ns initial)
                            <> ",gcs=" <> show (gcs final - gcs initial)
                            <> ",process_gcs=" <> show (gcs final)
                            <> ",major_gcs=" <> show (major_gcs final - major_gcs initial)
                            <> ",process_major_gcs=" <> show (major_gcs final)
                            <> ",process_peak_live=" <> show (max_live_bytes final)
                        putStrLn $ "context_residency," <> name
                            <> ",process_peak_resident_bytes=" <> show peak
                Nothing -> pure ()
            contextRecovery ("after-" <> name) after
-- Insert through the same DBLayer journal operation as durable submissions,
-- using a separate connection to the server's disposable wallet database.
pendingContextScene
    :: Reporter IO
    -> FilePath
    -> ApiWallet
    -> ApiDappContextNetwork
    -> ApiAddress C.Testnet42
    -> [TxIn]
    -> ([BS.ByteString] -> ClientM ApiDappTransactionContextResponse)
    -> BenchM ()
pendingContextScene reporter dbDir wallet network destination inputs client = do
    BenchCtx ctx _ <- ask
    let NetworkParameters genesis slotting _ = _networkParameters ctx
        interpreter = hoistTimeInterpreter (pure . runIdentity)
            $ mkSingleEraInterpreter (getGenesisBlockDate genesis) slotting
    let ApiT walletId = wallet ^. #id
        file = dbDir FP.</> keyTypeDescriptor ShelleyKeyS
            <> "." <> T.unpack (toText walletId) <> ".sqlite"
        sign input = do
            sealed <- either (error . show) pure
                $ sealedTxFromBytes
                $ ordinaryContextTx destination input
            response <- request $ C.signTransaction
                (wallet ^. #id)
                (ApiSignTransactionPostData
                    (ApiT sealed)
                    (ApiT $ unsafeFromText fixturePassphrase)
                    Nothing)
            let ApiSerialisedTransaction (ApiT signed) _ = response
            pure $ serialisedTx signed
        asSubmission input bytes =
            let TxWithOutputBytes{transaction = tx} =
                    either (error . show) Prelude.id
                        $ deserializeTxWithOutputBytes @Conway
                        $ BL.fromStrict bytes
                txid = DB.TxId $ Hash $ ReadHash.hashToBytes
                    $ Read.hashFromTxId $ Read.getTxId tx
                TxIn funding (TxIx ix) = input
                fundingId = DB.TxId $ Hash $ ReadHash.hashToBytes
                    $ Read.hashFromTxId funding
                sealed = either (error . show) Prelude.id $ sealedTxFromBytes bytes
            in ( DurableSubmission walletId txid sealed Nothing True
                    AuthorizedE 0 Nothing Nothing Nothing Nothing
               , [DurableSubmissionInput fundingId (fromIntegral ix) NormalInputE]
               )
        withJournal
            :: forall a
             . (DBLayer IO (SeqState C.Testnet42 ShelleyKey) -> IO a)
            -> IO a
        withJournal action =
            withLoadDBLayerFromFile
                (ShelleyWallet :: WalletFlavorS (SeqState C.Testnet42 ShelleyKey))
                nullTracer interpreter walletId Nothing file action
    runIndex <- liftIO $ maybe 0 Prelude.read
        <$> lookupEnv "CONTEXT_BENCH_RUN_INDEX"
    if runIndex < 0 || runIndex > 4 || length inputs /= 250
        then error "context benchmark: missing independent submission outpoints"
        else pure ()
    let spare = inputs !! (249 - runIndex)
    signed <- mapM sign $ take 20 inputs <> [spare]
    let entries = zipWith asSubmission (take 20 inputs) (take 20 signed)
    insertStart <- liftIO getMonotonicTimeNSec
    decisions <- liftIO $ withJournal $ \DBLayer{atomically, insertDurableSubmission} ->
        mapM (\(submission, claims) ->
            atomically $ insertDurableSubmission submission claims) entries
    insertEnd <- liftIO getMonotonicTimeNSec
    liftIO $ putStrLn $ "context_phase,db-insert-20," <> show (insertEnd - insertStart)
        <> ",total_bytes=" <> show (sum $ BS.length <$> take 20 signed)
        <> ",decisions=" <> show decisions
    if any (/= DurableSubmissionAuthorized) decisions
        then error "context benchmark: durable DB insert failed"
        else pure ()
    (captureNs, confirmNs, capturedInputs, capturedPending) <- liftIO
        $ withJournal
        $ \DBLayer{atomicallyReadContext, readCheckpoint, readDurableSubmissions} -> do
            captureStart <- getMonotonicTimeNSec
            ((checkpoint, rows), clock) <- atomicallyReadContext
                $ (,) <$> readCheckpoint <*> readDurableSubmissions
            _ <- evaluate $ force
                ( Set.size $ Wallet.dom $ Wallet.utxo checkpoint
                , length rows
                , show clock
                )
            captureEnd <- getMonotonicTimeNSec
            confirmStart <- getMonotonicTimeNSec
            (_, confirmed) <- atomicallyReadContext $ pure ()
            _ <- evaluate $ force $ show confirmed
            confirmEnd <- getMonotonicTimeNSec
            pure (captureEnd - captureStart, confirmEnd - confirmStart,
                Set.size $ Wallet.dom $ Wallet.utxo checkpoint, length rows)
    liftIO $ putStrLn $ "context_phase,db-fixture-capture,"
        <> show captureNs <> ",inputs=" <> show capturedInputs
        <> ",pending=" <> show capturedPending
    liftIO $ putStrLn $ "context_phase,db-fixture-confirm," <> show confirmNs
    let TxWithOutputBytes{transaction = parent} =
            either (error . show) Prelude.id
                $ deserializeTxWithOutputBytes @Conway
                $ BL.fromStrict $ head signed
        parentId = ReadHash.hashToBytes
            $ Read.hashFromTxId $ Read.getTxId parent
        parentOutput = TxIn (Read.getTxId parent) (TxIx 0)
        child = contextTx (LedgerCoin.Coin 2_000_000)
            (LedgerCoin.Coin 3_000_000) destination parentOutput
        visible response = any
            (\output ->
                output ^. #outpoint == ApiDappOutpoint (ApiDappHex parentId) 0
                    && Pending `elem` (output ^. #provenance))
            (response ^. #outputs)
    overlay <- request $ client [child]
    let pending = overlay ^. #pendingOverlay
    liftIO $ putStrLn $ "context_pending_inventory,transactions="
        <> show (length $ pending ^. #transactions)
        <> ",reserved_inputs=" <> show (length $ pending ^. #spentWalletInputs)
        <> ",pending_child_output_visible=" <> show (visible overlay)
    if length (pending ^. #transactions) /= 20
        || length (pending ^. #spentWalletInputs) /= 20
        || not (null $ pending ^. #producedWalletOutputs)
        || not (visible overlay)
        then error "context benchmark: pending outputs/claims not visible"
        else contextScene reporter "pending-20" [wallet] (client [child])
    pureValidatePhase "pending-20"
        (ApiDappTransactionContextRequest 1 network [ApiDappHex child])
        (client [child])
    liftIO $ withJournal $ \DBLayer{atomically, updateDurableSubmission} ->
        mapM_ (atomically . updateDurableSubmission
            . (\(submission, _) -> submission{durableStatus = ExpiredDappE})) entries
    -- The measured baseline workers leave the shared node UTxO untouched.
    -- Exercise release-to-ledger after the final worker's pending samples,
    -- spending only an input outside the frozen ordinary request corpus.
    when (runIndex == 0 || runIndex == 4) $ do
        let lastSigned = signed !! 20
        receipt <- request $ WalletClient.postDappSubmission (wallet ^. #id)
            (ApiDappSubmissionRequest 1 network $ ApiDappHex lastSigned)
        let txidBytes = receipt ^. #transactionId
        liftIO $ putStrLn $ "context_released_then_submitted," <> show txidBytes
        eventually "released fixture permits subsequent in-ledger submission" $ do
            let ApiDappHex raw = txidBytes
            tx <- request $ C.getTransaction (wallet ^. #id)
                (ApiTxId $ ApiT $ Hash raw) False
            tx ^. #status . #getApiT `shouldBe` InLedger

-- Creates n fixture wallets and return 3 of them

walletApiBench :: Reporter IO -> SomeMnemonic -> BenchM ()
walletApiBench reporter massiveMnemonic = do
    let runScenarioR semSeg scen = do
            let sem = mkSemantic [T.pack semSeg]
            runScenario (addSemantic reporter sem) scen

    fmtTitle "Non-cached run"
    runWarmUpScenario

    fmtTitle "Latencies for 2 fixture wallets scenarioR"
    runScenarioR "2-fixture" (nFixtureWallet 2)

    fmtTitle "Latencies for 10 fixture wallets scenarioR"
    runScenarioR "10-fixture" (nFixtureWallet 10)

    fmtTitle "Latencies for 100 fixture wallets"
    runScenarioR "100-fixture" (nFixtureWallet 100)

    fmtTitle "Latencies for 2 fixture wallets with 10 txs scenarioR"
    runScenarioR "2-fixture-10-txs" (nFixtureWalletWithTxs 2 10)

    fmtTitle "Latencies for 2 fixture wallets with 20 txs scenarioR"
    runScenarioR "2-fixture-20-txs" (nFixtureWalletWithTxs 2 20)

    fmtTitle "Latencies for 2 fixture wallets with 100 txs scenarioR"
    runScenarioR "2-fixture-100-txs" (nFixtureWalletWithTxs 2 100)

    fmtTitle "Latencies for 10 fixture wallets with 10 txs scenarioR"
    runScenarioR "10-fixture-10-txs" (nFixtureWalletWithTxs 10 10)

    fmtTitle "Latencies for 10 fixture wallets with 20 txs scenarioR"
    runScenarioR "10-fixture-20-txs" (nFixtureWalletWithTxs 10 20)

    fmtTitle "Latencies for 10 fixture wallets with 100 txs scenarioR"
    runScenarioR "10-fixture-100-txs" (nFixtureWalletWithTxs 10 100)

    fmtTitle "Latencies for 2 fixture wallets with 100 utxos scenarioR"
    runScenarioR "2-fixture-100-utxos" (nFixtureWalletWithUTxOs 2 100)

    fmtTitle "Latencies for 2 fixture wallets with 200 utxos scenarioR"
    runScenarioR "2-fixture-200-utxos" (nFixtureWalletWithUTxOs 2 200)

    fmtTitle "Latencies for 2 fixture wallets with 500 utxos scenarioR"
    runScenarioR "2-fixture-500-utxos" (nFixtureWalletWithUTxOs 2 500)

    fmtTitle "Latencies for 2 fixture wallets with 1000 utxos scenarioR"
    runScenarioR "2-fixture-1000-utxos" (nFixtureWalletWithUTxOs 2 1_000)

    fmtTitle
        $ "Latencies for 2 fixture wallets with "
            <> build massiveWalletUTxOSize
            <> " utxos scenario"
    runScenarioR "2-fixture-massive-utxos"
        $ massiveFixtureWallet massiveMnemonic

nFixtureWallet
    :: Int
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
nFixtureWallet n = do
    wal1 : wal2 : _ <- replicateM n fixtureWallet
    walMA <- fixtureMultiAssetWallet
    maWalletToMigrate <- fixtureMultiAssetWallet
    pure (wal1, wal2, walMA, maWalletToMigrate)

-- Creates n fixture wallets and send 1-ada transactions to one of them
-- (m times). The money is sent in batches (see batchSize below) from
-- additionally created source fixture wallet. Then we wait for the money
-- to be accommodated in recipient wallet. After that the source fixture
-- wallet is removed.
nFixtureWalletWithTxs
    :: Int
    -> Int
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
nFixtureWalletWithTxs n m = do
    (wal1, wal2, walMA, maWalletToMigrate) <- nFixtureWallet n

    let amt = minUTxOValue era
    let batchSize = 10
    let whole10Rounds = div m batchSize
    let lastBit = mod m batchSize
    let amtExp val = ((amt * fromIntegral val) + faucetAmt) :: Natural
    let expInflows =
            if whole10Rounds > 0
                then [x * batchSize | x <- [1 .. whole10Rounds]] ++ [lastBit]
                else [lastBit]
    let expInflows' = filter (/= 0) expInflows

    mapM_ (repeatPostTx wal1 amt batchSize . amtExp) expInflows'
    pure (wal1, wal2, walMA, maWalletToMigrate)

nFixtureWalletWithUTxOs
    :: Int
    -> Int
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
nFixtureWalletWithUTxOs n utxoNumber = do
    let utxoExp = replicate utxoNumber (minUTxOValue era)
    wal1 <- fixtureWalletWith utxoExp
    (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet n

    eventually "Wallet balance is as expected" $ do
        rWal1 <- request $ C.getWallet (wal1 ^. #id)
        rWal1 ^. #balance . #available . #toNatural `shouldBe` sum utxoExp

    rStat <- request $ C.getWalletUtxoStatistics (wal1 ^. #id)
    utxoStatisticsFromCoins (fromIntegral <$> utxoExp) `shouldBe` rStat
    pure (wal1, wal2, walMA, maWalletToMigrate)

massiveFixtureWallet
    :: SomeMnemonic -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
massiveFixtureWallet massiveMnemonic = do
    (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet 2

    wal1 <-
        request
            $ C.postWallet
            $ WalletOrAccountPostData
            $ Left
            $ WalletPostData
                { addressPoolGap = Just $ ApiT $ toEnum 10_001
                , mnemonicSentence = ApiMnemonicT massiveMnemonic
                , mnemonicSecondFactor = Nothing
                , name = ApiT $ unsafeFromText "Massive wallet"
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , oneChangeAddressMode = Nothing
                , singleAddressMode = Nothing
                , restorationMode = Nothing
                }
    finallyDeleteWallet wal1
    wal1
        ^. #balance
            . #available
            . #toNatural
            `shouldBe` (fromIntegral massiveWalletUTxOSize * unCoin massiveWalletAmt)

    pure (wal1, wal2, walMA, maWalletToMigrate)

repeatPostTx :: ApiWallet -> Natural -> Int -> Natural -> BenchM ()
repeatPostTx wDest amtToSend batchSize amtExp = do
    wSrcId <- view #id <$> fixtureWallet
    replicateM_ batchSize
        $ do
            addrs <- request $ C.listAddresses (wDest ^. #id) Nothing
            let destination = addrs !! 1 ^. #id
                amount =
                    AddressAmount
                        { address = destination
                        , amount = ApiAmount amtToSend
                        , assets = ApiWalletAssets []
                        }
                payload =
                    PostTransactionOldData
                        { payments = pure amount
                        , passphrase = ApiT $ unsafeFromText fixturePassphrase
                        , withdrawal = Nothing
                        , metadata = Nothing
                        , timeToLive = Nothing
                        , preferredCollateral = Nothing
                        }
            request $ C.postTransaction wSrcId payload

    eventually "repeatPostTx: wallet balance is as expected" $ do
        rWal1 <- request $ C.getWallet $ wDest ^. #id
        rWal1 ^. #balance . #available . #toNatural `shouldBe` amtExp

    void $ request $ C.deleteWallet wSrcId

iterations :: Int
iterations = 10

scene
    :: Reporter IO -> String -> BenchM (Either ClientError a) -> BenchM ()
scene reporter title scenario = do
    ts <- measureApiLogs iterations scenario
    let avg = meanAvg ts
        semantic = mkSemantic [T.pack title]
    liftIO
        $ report reporter
        $ pure
        $ Benchmark semantic
        $ Result avg Milliseconds iterations
    fmtResult title ts

sceneOfClientM :: Reporter IO -> String -> ClientM a -> BenchM ()
sceneOfClientM reporter title action =
    scene reporter title $ requestWithError action

listAllTransactions
    :: ApiT WalletId -> ClientM [ApiTransaction C.Testnet42]
listAllTransactions walId =
    C.listTransactions
        walId
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        False

pend :: Applicative m => m () -> m ()
pend = const $ pure ()

runScenario
    :: Reporter IO
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
    -> BenchM ()
runScenario reporter scenario = lift . runResourceT $ do
    let sceneOfClientMR :: String -> ClientM a -> BenchM ()
        sceneOfClientMR = do sceneOfClientM reporter

        -- \| Decrease likelihood of failing with `no_utxos_available`
        -- by calling this short delay after creating txs.
        waitForChange :: BenchM ()
        waitForChange = liftIO $ threadDelay 5_000_000

    (wal1, wal2, walMA, maWalletToMigrate) <- scenario
    let wal1Id = wal1 ^. #id
        wal2Id = wal2 ^. #id
        walMAId = walMA ^. #id
        maWalletToMigrateId = maWalletToMigrate ^. #id
        amt = minUTxOValue era
    sceneOfClientMR "listWallets" C.listWallets
    sceneOfClientMR "getWallet" $ C.getWallet wal1Id
    sceneOfClientMR "getUTxOsStatistics"
        $ C.getWalletUtxoStatistics wal1Id
    sceneOfClientMR "listAddresses" $ C.listAddresses wal1Id Nothing
    sceneOfClientMR "listTransactions" $ listAllTransactions wal1Id

    txs <- request $ listAllTransactions wal1Id
    sceneOfClientMR "getTransaction"
        $ C.getTransaction wal1Id (ApiTxId $ txs !! 1 ^. #id) False
    waitForChange

    addrs <- request $ C.listAddresses wal2Id Nothing
    let destination = addrs !! 1 ^. #id
        amount =
            AddressAmount
                { address = destination
                , amount = ApiAmount amt
                , assets = ApiWalletAssets []
                }
        payload =
            PostTransactionFeeOldData
                { payments = pure amount
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                , preferredCollateral = Nothing
                }
    sceneOfClientMR "postTransactionFee"
        $ C.postTransactionFee wal1Id payload
    waitForChange

    let payloadTx =
            PostTransactionOldData
                { payments = pure amount
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                , preferredCollateral = Nothing
                }
    sceneOfClientMR "postTransaction" $ C.postTransaction wal1Id payloadTx
    waitForChange

    let payments =
            replicate 5
                $ AddressAmount
                    { address = destination
                    , amount = ApiAmount amt
                    , assets = ApiWalletAssets []
                    }
        payloadTxTo5Addr =
            PostTransactionOldData
                { payments = NE.fromList payments
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                , preferredCollateral = Nothing
                }
    sceneOfClientMR "postTransactionTo5Addrs"
        $ C.postTransaction wal1Id payloadTxTo5Addr
    waitForChange

    let
        assetToSend = over _Unwrapped pick $ walMA ^. #assets . #total
        pick (x : _xs) = pure $ set #quantity (minUTxOValue era) x
        pick [] = error "No assets to pick from"
        paymentsMA =
            AddressAmount
                { address = destination
                , amount = ApiAmount amt
                , assets = assetToSend
                }
        payloadMA =
            PostTransactionOldData
                { payments = pure paymentsMA
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                , preferredCollateral = Nothing
                }
    -- Todo ADP-3293
    pend
        $ sceneOfClientMR "postTransactionMA"
        $ C.postTransaction walMAId payloadMA

    sceneOfClientMR "listStakePools"
        $ C.listPools
        $ ApiT <$> arbitraryStake

    sceneOfClientMR "getNetworkInfo" CN.networkInformation

    sceneOfClientMR "listAssets" $ C.getAssets walMAId

    let assetsSrc = walMA ^. #assets . #total
        (polId, assName) =
            bimap unsafeFromText unsafeFromText
                $ fst
                $ pickAnAsset assetsSrc
    sceneOfClientMR "getAsset"
        $ C.getAsset walMAId (ApiT polId) (ApiT assName)

    let addresses = replicate 5 destination
        migrationPlanPayload =
            ApiWalletMigrationPlanPostData $ NE.fromList addresses

    sceneOfClientMR "postMRigrationPlan"
        $ C.planMigration maWalletToMigrateId migrationPlanPayload

    let migrationPayload =
            ApiWalletMigrationPostData
                { addresses = NE.fromList addresses
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                }
    -- Todo ADP-3293
    pend
        $ sceneOfClientMR "postMRigration"
        $ C.migrate maWalletToMigrateId migrationPayload

fmtResult :: String -> [NominalDiffTime] -> BenchM ()
fmtResult title ts = liftIO $ Measure.fmtResult title ts

fmtTitle :: Builder -> BenchM ()
fmtTitle = liftIO . Measure.fmtTitle

arbitraryStake :: Maybe Coin
arbitraryStake = Just $ ada 10_000
  where
    ada = Coin . (1_000_000 *)

measureApiLogs
    :: Exception e => Int -> BenchM (Either e a) -> BenchM [NominalDiffTime]
measureApiLogs count action = do
    BenchCtx _ctx capture <- ask
    run <- toIO $ do
        r <- action
        case r of
            Left e -> throwM e
            Right q -> pure q
    liftIO $ Measure.measureApiLogs count capture run

runWarmUpScenario :: BenchM ()
runWarmUpScenario = do
    -- this one is to have comparable results from first to last measurement
    -- in runScenario
    t <-
        measureApiLogs iterations $ requestWithError CN.networkInformation
    fmtResult "getNetworkInfo     " t

-- The server's actual wallet DB handle, not the disposable fixture connection.
-- The capture reads a checkpoint; confirmation reads only the context clock.
decorateContextDB :: IORef (Map.Map ThreadId Bool) -> DBFactory IO s -> DBFactory IO s
decorateContextDB active factory = factory
    { withDatabaseLoad = \wid use ->
        withDatabaseLoad factory wid (use . timedLayer)
    , withDatabaseBoot = \wid params use ->
        withDatabaseBoot factory wid params (use . timedLayer)
    }
  where
    timedLayer DBLayer
        { readCheckpoint = originalCheckpoint
        , atomicallyReadContext = originalReadContext
        , ..
        } = DBLayer
        { readCheckpoint = do
            tid <- liftIO myThreadId
            liftIO $ atomicModifyIORef' active $ \threads ->
                (Map.adjust (const True) tid threads, ())
            originalCheckpoint
        , atomicallyReadContext = \action -> do
            tid <- myThreadId
            atomicModifyIORef' active $ \threads ->
                (Map.insert tid False threads, ())
            start <- getMonotonicTimeNSec
            result <- originalReadContext action
            _ <- evaluate $ force $ show $ snd result
            end <- getMonotonicTimeNSec
            captured <- atomicModifyIORef' active $ \threads ->
                (Map.delete tid threads, Map.findWithDefault False tid threads)
            putStrLn $ "context_phase,db-"
                <> (if captured then "capture" else "confirm")
                <> "," <> show (end - start)
            pure result
        , ..
        }

withShelleyServer
    :: Tracers IO -> Bool
    -> (SomeMnemonic -> Context -> FilePath -> IORef Int -> IORef Int -> IO ())
    -> IO ()
withShelleyServer tracers enlarged action = do
    faucetPort <- lookupEnv "CONTEXT_BENCH_FAUCET_PORT"
    case faucetPort of
        Nothing -> withFaucet $ withShelleyServerFaucet tracers enlarged action
        Just port -> do
            manager <- newManager defaultManagerSettings
            withShelleyServerFaucet tracers enlarged action
                $ mkClientEnv manager
                $ BaseUrl Http "localhost" (Prelude.read port) ""

-- Fully evaluate the admitted ledger snapshot without rendering protocol
-- parameters, cost-model contexts and UTxO to a pretty-printed String.
forceNetworkContext
    :: Either ErrDappTransactionContext DappTransactionContext -> ()
forceNetworkContext (Left err) = err `seq` ()
forceNetworkContext (Right ctx) =
    case (contextEra ctx, contextProtocolParameters ctx, contextUTxO ctx) of
        ( AnyCardanoEra ConwayEra
            , Read.EraValue (Read.PParams pp :: Read.PParams era)
            , InRecentEraConway utxo
            ) -> case Read.theEra @era of
                Read.Conway -> rnf pp `seq` rnf utxo
                _ -> error "context benchmark: protocol parameters outside Conway"
        _ -> error "context benchmark: captured snapshot outside Conway"

withShelleyServerFaucet
    :: Tracers IO -> Bool
    -> (SomeMnemonic -> Context -> FilePath -> IORef Int -> IORef Int -> IO ())
    -> ClientEnv -> IO ()
withShelleyServerFaucet tracers enlarged action faucetClientEnv = do
    contextBench <- (== Just "1") <$> lookupEnv "CONTEXT_BENCH"
    exhaustion <- newIORef 0
    queries <- newIORef 0
    dbReads <- newIORef Map.empty
    let decorator layer = pure layer
            { getDappTransactionContext = \point inputs -> do
                start <- getMonotonicTimeNSec
                atomicModifyIORef' queries $ \count -> (count + 1, ())
                failThisQuery <- atomicModifyIORef' exhaustion
                    $ \remaining -> (max 0 (remaining - 1), remaining > 0)
                result <- if failThisQuery
                    then pure $ Left ErrDappTransactionContextPointUnavailable
                    else getDappTransactionContext layer point inputs
                _ <- evaluate $ forceNetworkContext result
                end <- getMonotonicTimeNSec
                putStrLn $ "context_phase,node-query," <> show (end - start)
                    <> ",inputs=" <> show (Set.size inputs)
                    <> "," <> either show (const "ok") result
                pure result
            }
        startWallet = if contextBench
            then serveWalletWithNetworkDecorator decorator (decorateContextDB dbReads)
            else serveWallet
    externalSocket <- lookupEnv "CONTEXT_BENCH_NODE_SOCKET"
    faucetFunds <- case externalSocket of
        Just _ -> pure $ FaucetFunds [] [] []
        Nothing -> Faucet.runFaucetM faucetClientEnv
            $ mkFaucetFunds contextBench

    ctx <- newEmptyMVar
    faucet <- Faucet.initFaucet faucetClientEnv
    massiveWalletMnemonic' <- massiveWalletMnemonic faucet
    let testnetMagic = Cluster.TestnetMagic 42
    let setupContext np dbDir socket baseUrl = do
            let sixtySeconds = 60 * 1_000_000 -- 60s in microseconds
            manager <-
                newManager
                    defaultManagerSettings
                        { managerResponseTimeout = responseTimeoutMicro sixtySeconds
                        }
            putMVar ctx
                ( Context
                    { _manager = (baseUrl, manager)
                    , _walletPort = Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _networkParameters = np
                    , _testnetMagic = testnetMagic
                    , _nodeSocketPath = socket
                    , _poolGarbageCollectionEvents =
                        error "poolGarbageCollectionEvents not available"
                    , _smashUrl = ""
                    , _mainEra = maxBound
                    , _mintSeaHorseAssets = error "mintSeaHorseAssets not available"
                    , _preprodWallets = []
                    }
                , dbDir
                )
    race_
        (takeMVar ctx >>= \(context, dbDir) ->
            action massiveWalletMnemonic' context dbDir exhaustion queries)
        (void $ withServer startWallet testnetMagic faucetFunds setupContext)
  where
    mkFaucetFunds contextBench = do
        shelleyFunds <- Faucet.shelleyFunds shelleyTestnet
        massiveFunds <-
            if contextBench
                then Faucet.massiveWalletFunds (Coin 10_000_000) 250 shelleyTestnet
                else Faucet.massiveWalletFunds massiveWalletAmt 10_000 shelleyTestnet
        maryAllegraFunds <-
            Faucet.maryAllegraFunds (Coin 10_000_000) shelleyTestnet
        pure
            FaucetFunds
                { pureAdaFunds = shelleyFunds
                , maryAllegraFunds
                , massiveWalletFunds = massiveFunds
                }

    withServer startWallet cfgTestnetMagic faucetFunds setupAction = do
        isContextProfile <- (== Just "1") <$> lookupEnv "CONTEXT_BENCH"
        skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
        withSystemTempDir stdoutTextTracer "latency" skipCleanup $ \dir -> do
            let testDir = absDir dir
                db = testDir </> relDir "wallets"
            createDirectory $ toFilePath db
            externalSocket <- lookupEnv "CONTEXT_BENCH_NODE_SOCKET"
            case externalSocket of
                Just socket -> do
                    clusterRoot <- lookupEnv "CONTEXT_BENCH_CLUSTER_DIR"
                        >>= maybe (error "context benchmark: missing live cluster path") pure
                    genesisData <- Yaml.decodeFileThrow
                        $ clusterRoot FP.</> "shelley-genesis.json"
                    conn <- either error pure $ cardanoNodeConn socket
                    let versionData = NodeToClientVersionData
                            { networkMagic = NetworkMagic $ sgNetworkMagic genesisData
                            , query = False
                            }
                    onClusterStart startWallet cfgTestnetMagic setupAction db socket
                        (RunningNode conn genesisData versionData)
                Nothing -> do
                    CommandLineOptions{clusterConfigsDir} <- parseCommandLineOptions
                    clusterEra <- Cluster.clusterEraFromEnv
                    cfgNodeLogging <-
                        Cluster.logFileConfigFromEnv
                            $ Just
                            $ mkRelDirOf
                            $ Cluster.clusterEraToString clusterEra
                    withTempFile $ \socket -> do
                        let clusterConfig =
                                Cluster.Config
                                    { cfgStakePools = pure (NE.head defaultPoolConfigs)
                                    , cfgLastHardFork = clusterEra
                                    , cfgNodeLogging
                                    , cfgClusterDir = DirOf testDir
                                    , cfgClusterConfigs = clusterConfigsDir
                                    , cfgTestnetMagic
                                    , cfgShelleyGenesisMods =
                                        [ over #sgSlotLength (const 0.2)
                                        , over #sgSecurityParam (const (unsafeNonZero 100))
                                        ]
                                            <> [ over #sgProtocolParams
                                                    ( set ppMaxBBSizeL 1_048_576
                                                        . set ppMaxTxSizeL 131_072
                                                    )
                                               | enlarged
                                               ]
                                    , cfgTracer = stdoutTextTracer
                                    , cfgNodeOutputFile = Nothing
                                    , cfgRelayNodePath = mkRelDirOf "relay"
                                    , cfgClusterLogFile = Nothing
                                    , cfgNodeToClientSocket =
                                        UnixPipe
                                            $ FileOf
                                            $ absFile socket
                                    }
                        (if isContextProfile then withSingleNodeCluster else withCluster)
                            clusterConfig
                            faucetFunds
                            (onClusterStart startWallet cfgTestnetMagic setupAction db socket)

    onClusterStart startWallet testnetMagic setupAction db socket node = do
        let (RunningNode conn genesisData vData) = node
        let (networkParameters, block0, _gp) = fromGenesisData genesisData
        isContextProfile <- (== Just "1") <$> lookupEnv "CONTEXT_BENCH"
        when isContextProfile $ setEnv "CONTEXT_BENCH_SLOT_SECONDS"
            $ show (realToFrac (genesisData ^. #sgSlotLength) :: Double)
        when isContextProfile $ putStrLn $ "context_genesis,max_tx_size="
            <> show (genesisData ^. #sgProtocolParams . ppMaxTxSizeL)
            <> ",max_block_body_size="
            <> show (genesisData ^. #sgProtocolParams . ppMaxBBSizeL)
            <> ",slot_length_s=" <> show (genesisData ^. #sgSlotLength)
            <> ",active_slots_coeff=" <> show (genesisData ^. #sgActiveSlotsCoeff)
        genesisRoot <- fromMaybe (FP.takeDirectory $ toFilePath db)
            <$> lookupEnv "CONTEXT_BENCH_CLUSTER_DIR"
        when isContextProfile $ mapM_
            (\name -> do
                let path = genesisRoot FP.</> name
                raw <- BS.readFile path
                putStrLn $ "context_genesis_file," <> name
                    <> ",blake2b256="
                    <> show (BL.toStrict $ encode $ ApiDappHex $ blake2b256 raw)
            )
            [ "byron-genesis.json"
            , "shelley-genesis.json"
            , "alonzo-genesis.json"
            , "conway-genesis.json"
            ]
        coordinating <- (== Just "1")
            <$> lookupEnv "CONTEXT_BENCH_COORDINATE"
        child <- (== Just "1") <$> lookupEnv "CONTEXT_BENCH_CHILD"
        if coordinating && not child
            then runContextCoordinator enlarged
                (FP.takeDirectory $ toFilePath db) (nodeSocketFile conn)
                (baseUrlPort $ baseUrl faucetClientEnv)
            else
                void $ startWallet
                    (NodeSource conn vData (SyncTolerance 10))
                    networkParameters
                    tunedForMainnetPipeliningStrategy
                    (NTestnet . fromIntegral $ testnetMagicToNatural testnetMagic)
                    [] -- pool certificates
                    tracers
                    (Just $ toFilePath db)
                    Nothing -- db decorator
                    "127.0.0.1"
                    (ListenOnPort 8_090)
                    Nothing
                    Nothing -- tls configuration
                    Nothing -- settings
                    Nothing -- token metadata server
                    defaultIpfsGatewayUrl
                    block0
                    (setupAction networkParameters (toFilePath db) socket)

massiveWalletUTxOSize :: Int
massiveWalletUTxOSize = 10_000

massiveWalletAmt :: Coin
massiveWalletAmt = ada 1_000
  where
    ada x = Coin $ x * 1000_000

era :: ApiEra
era = maxBound

--------------------------------------------------------------------------------
-- Command line options --------------------------------------------------------

newtype CommandLineOptions = CommandLineOptions
    {clusterConfigsDir :: DirOf "cluster-configs"}
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    absolutizer <- newAbsolutizer
    O.execParser
        $ O.info
            ( fmap CommandLineOptions (clusterConfigsDirParser absolutizer)
                <**> O.helper
            )
            (O.progDesc "Cardano Wallet's Latency Benchmark")
