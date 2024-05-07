{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Launch.Cluster.Http.ServiceSpec
    ( spec
    )
where

import Prelude

import Cardano.Launcher
    ( Command (..)
    , ProcessHandles (..)
    , withBackendProcess
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
import Cardano.Wallet.Network.Ports
    ( PortNumber
    , getRandomPort
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    , SNetworkId (SMainnet)
    , withSNetworkId
    )
import Control.Exception
    ( finally
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
    ( (</>)
    )
import System.Process
    ( StdStream (..)
    , cleanupProcess
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
    -> IO Command
localClusterCommand name port = do
    configsPath <- getClusterConfigsPathFromEnv
    mLogsPath <- getClusterLogsFilePathFromEnv
    mMinSeverity <- getClusterLogsMinSeverity
    pure
        $ Command
            { cmdName = "local-cluster"
            , cmdArgs =
                [ "--monitoring-port"
                , show port
                , "--cluster-configs"
                , configsPath
                ]
                    <> case mLogsPath of
                        Nothing -> []
                        Just logsPath -> ["--cluster-logs", logsPath </> name]
                    <> case mMinSeverity of
                        Nothing -> []
                        Just minSeverity -> ["--min-severity", show minSeverity]
            , cmdSetup = pure ()
            , cmdInput = NoStream
            , cmdOutput = NoStream
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
    -> ((RunMonitorQ IO, RunFaucetQ IO) -> IO ())
    -> IO ()
testServiceWithCluster name = runContT $ do
    port <- liftIO getRandomPort
    command <- liftIO $ localClusterCommand name port
    ProcessHandles in' out err kill <- do
        ContT $ withBackendProcess nullTracer command
    queries <- withSNetworkId (NTestnet 42)
        $ \network -> withServiceClient network port nullTracer
    ContT $ \k -> do
        k queries `finally` cleanupProcess (in', out, err, kill)

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
        it "can start and stop" $ do
            testServiceWithCluster
                "can-start-and-stop.log"
                $ \(RunQuery query, _) -> do
                    result <- query ReadyQ
                    result `shouldBe` False
        it "can wait for cluster ready before ending" $ do
            testServiceWithCluster
                "can-wait-for-cluster-ready-before-ending.log"
                $ \(RunQuery query, _) -> do
                    fix $ \loop -> do
                        result <- query ReadyQ
                        unless result $ threadDelay 10000 >> loop
