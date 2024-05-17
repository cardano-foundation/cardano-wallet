{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}

module Cardano.Wallet.Launch.Cluster.Http.ServiceSpec
    ( spec
    )
where

import Prelude

import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , newToTextTracer
    )
import Cardano.Launcher
    ( Command (..)
    , IfToSendSigINT (..)
    , TimeoutInSecs (..)
    , withBackendProcess
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds (FaucetFunds)
    , FileOf (..)
    , RunningNode (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet.Serialize
    ( saveFunds
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( RunFaucetQ
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( MonitorQ (..)
    , RunMonitorQ (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Service
    ( ServiceConfiguration (..)
    , withService
    , withServiceClient
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer (currentNodeTip)
    )
import Cardano.Wallet.Network.Implementation
    ( withNetworkLayer
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber
    , getRandomPort
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( CardanoBlock
    , StandardCrypto
    , fromGenesisData
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    , SNetworkId (SMainnet)
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (SyncTolerance)
    )
import Control.Monad
    ( forM_
    , unless
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Fix
    ( fix
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Control.Tracer
    ( Tracer
    , nullTracer
    , traceWith
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import System.IO
    ( IOMode (..)
    , withFile
    )
import System.IO.Extra
    ( withTempFile
    )
import System.Path
    ( absFile
    )
import System.Process
    ( StdStream (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import UnliftIO.Async
    ( async
    , wait
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Directory
    ( createDirectoryIfMissing
    )

import qualified Cardano.Wallet.Network.Implementation as NL

testService
    :: MonitorState
    -> (Tracer IO Phase -> RunMonitorQ IO -> IO ())
    -> IO ()
testService w f =
    evalContT $ do
        (tracer, (query, _)) <-
            withService
                SMainnet
                (error "No connection")
                (error "No cluster")
                nullTracer
                $ ServiceConfiguration Nothing w
        liftIO $ f tracer query

localClusterCommand
    :: FilePath
    -- ^ filename to append to the logs dir
    -> PortNumber
    -- ^ monitoring port
    -> FilePath
    -- ^ faucet funds path
    -> ContT r IO (Maybe FilePath, Command)
localClusterCommand name port faucetFundsPath = do
    configsPath <- liftIO getClusterConfigsPathFromEnv
    mLogsPath <- liftIO getClusterLogsFilePathFromEnv
    mMinSeverity <- liftIO getClusterLogsMinSeverity
    (clusterStdout, logsPathName) <- case mLogsPath of
        Nothing -> pure (NoStream, Nothing)
        Just logsPath -> do
            let logsPathName = logsPath </> name
            fmap (\h -> (UseHandle h, Just logsPathName))
                $ ContT
                $ withFile (logsPath </> name <> "-stdout" <.> "log") WriteMode

    pure
        $ (logsPathName,)
        $ Command
            { cmdName = "local-cluster"
            , cmdArgs =
                [ "--faucet-funds"
                , faucetFundsPath
                , "--monitoring-port"
                , show port
                , "--cluster-configs"
                , configsPath
                ]
                    <> case mLogsPath of
                        Nothing -> []
                        Just logsPath ->
                            [ "--cluster-logs"
                            , logsPath </> name <.> "log"
                            ]
                    <> case mMinSeverity of
                        Nothing -> []
                        Just minSeverity -> ["--min-severity", show minSeverity]
            , cmdSetup = pure ()
            , cmdInput = NoStream
            , cmdOutput = clusterStdout
            }

getClusterConfigsPathFromEnv :: IO FilePath
getClusterConfigsPathFromEnv = do
    lookupEnv "LOCAL_CLUSTER_CONFIGS" >>= \case
        Just path -> pure path
        Nothing -> error "LOCAL_CLUSTER_CONFIGS not set"

getClusterLogsFilePathFromEnv :: IO (Maybe FilePath)
getClusterLogsFilePathFromEnv = do
    mp <- lookupEnv "CLUSTER_LOGS_DIR_PATH"
    forM_ mp $ \dir ->
        createDirectoryIfMissing True dir
    pure mp

getClusterLogsMinSeverity :: IO (Maybe String)
getClusterLogsMinSeverity = lookupEnv "CLUSTER_LOGS_MIN_SEVERITY"

testServiceWithCluster
    :: FilePath
    -> FaucetFunds
    -> ContT () IO ((RunMonitorQ IO, RunFaucetQ IO), ToTextTracer)
testServiceWithCluster name faucetFundsValue = do
    port <- liftIO getRandomPort
    faucetFundsPath <- ContT withTempFile
    liftIO $ saveFunds (FileOf $ absFile faucetFundsPath) faucetFundsValue
    (logsPathName, command) <- localClusterCommand name port faucetFundsPath
    ToTextTracer processLogs <- case logsPathName of
        Nothing -> pure $ ToTextTracer nullTracer
        Just path ->
            ContT
                $ newToTextTracer
                    (path <> "-process" <.> "log")
                    Nothing
    _ <-
        ContT
            $ withBackendProcess
                processLogs
                command
                NoTimeout
                DoNotSendSigINT
    queries <- withSNetworkId (NTestnet 42)
        $ \network -> withServiceClient network port nullTracer
    pure (queries, ToTextTracer processLogs)

withNetwork
    :: Tracer IO NL.Log
    -> RunningNode
    -> ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto))
withNetwork tr (RunningNode sock genesisData vData) = do
    let (np, _, _) = fromGenesisData genesisData
    let sTol = SyncTolerance 60
    ContT
        $ withNetworkLayer
            tr
            tunedForMainnetPipeliningStrategy
            np
            sock
            vData
            sTol

noFunds :: FaucetFunds
noFunds = FaucetFunds [] [] []

spec :: Spec
spec = do
    describe "withService control" $ do
        it "can start" $ do
            testService Step $ \_ _ -> pure ()
        it "can query" $ do
            testService Step $ \_ (RunQuery query) -> do
                result <- query ReadyQ
                result `shouldBe` False
        it "can trace" $ do
            testService Run $ \tracer _ -> do
                traceWith tracer RetrievingFunds
        it "can report readiness" $ do
            testService Run $ \tracer (RunQuery query) -> do
                traceWith tracer (Cluster Nothing)
                result <- query ReadyQ
                result `shouldBe` True
        it "can step the tracer thread" $ do
            testService Step $ \tracer (RunQuery query) -> do
                tracer' <- async $ do
                    traceWith tracer (Cluster Nothing)
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ query StepQ >> loop
                wait tracer'
        it "can report the phase history" $ do
            testService Run $ \tracer (RunQuery query) -> do
                traceWith tracer RetrievingFunds
                traceWith tracer Metadata
                traceWith tracer Genesis
                traceWith tracer Pool0
                traceWith tracer Funding
                traceWith tracer Pools
                traceWith tracer Relay
                traceWith tracer (Cluster Nothing)
                threadDelay 10000
                (History phases, state) <- query ObserveQ
                snd <$> phases
                    `shouldBe` [ RetrievingFunds
                               , Metadata
                               , Genesis
                               , Pool0
                               , Funding
                               , Pools
                               , Relay
                               , Cluster Nothing
                               ]
                state `shouldBe` Run
        it "can switch from step to run" $ do
            testService Step $ \tracer (RunQuery query) -> do
                tracer' <- async $ do
                    traceWith tracer RetrievingFunds
                state <- query SwitchQ
                state `shouldBe` Run
                wait tracer'
                (History phases, _state) <- query ObserveQ
                snd <$> phases `shouldBe` [RetrievingFunds]
    describe "withService application" $ do
        it "can start and stop" $ evalContT $ do
            ((RunQuery query, _), _) <-
                testServiceWithCluster
                    "can-start-and-stop"
                    noFunds
            liftIO $ do
                result <- query ReadyQ
                result `shouldBe` False
        it "can wait for cluster ready before ending" $ evalContT $ do
            ((RunQuery query, _), _) <-
                testServiceWithCluster
                    "can-wait-for-cluster-ready-before-ending"
                    noFunds
            liftIO $ do
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ threadDelay 10000 >> loop
    describe "withNetwork" $ do
        it "can start and stop" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                testServiceWithCluster
                    "withNetwork-can-start-and-stop"
                    noFunds
            node <- liftIO $ waitForNode query
            nl <- withNetwork tr node
            tip <- liftIO $ currentNodeTip nl
            tip `seq` pure ()

waitForNode :: RunMonitorQ IO -> IO RunningNode
waitForNode (RunQuery query) = fix $ \loop -> do
    (history', _) <- query ObserveQ
    case getNode history' of
        Nothing -> threadDelay 10000 >> loop
        Just node -> pure node

getNode :: History -> Maybe RunningNode
getNode (History phases) = case phases of
    [] -> Nothing
    (_time, phase) : _ -> case phase of
        Cluster (Just node) -> Just node
        _ -> Nothing
