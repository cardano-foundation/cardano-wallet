module Cardano.Wallet.Launch.Cluster.Monitoring.MonitorSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( Query (..)
    , RunQuery (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Monitor
    ( MonitorConfiguration (..)
    , withMonitoring
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Control.Monad
    ( unless
    )
import Control.Monad.Cont
    ( evalContT
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

testMonitoring
    :: MonitorState
    -> (Tracer IO Phase -> RunQuery IO -> IO ())
    -> IO ()
testMonitoring w f =
    evalContT $ do
        (tracer, query) <-
            withMonitoring nullTracer
                $ MonitorConfiguration Nothing w
        liftIO $ f tracer query

spec :: Spec
spec = do
    describe "withMonitoring" $ do
        it "can start" $ do
            testMonitoring Step $ \_ _ -> pure ()
        it "can query" $ do
            testMonitoring Step $ \_ (RunQuery query) -> do
                result <- query ReadyQ
                result `shouldBe` False
        it "can trace" $ do
            testMonitoring Run $ \tracer _ -> do
                traceWith tracer RetrievingFunds
        it "can report readiness" $ do
            testMonitoring Run $ \tracer (RunQuery query) -> do
                traceWith tracer (Cluster Nothing)
                result <- query ReadyQ
                result `shouldBe` True
        it "can step the tracer thread" $ do
            testMonitoring Step $ \tracer (RunQuery query) -> do
                tracer' <- async $ do
                    traceWith tracer (Cluster Nothing)
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ query StepQ >> loop
                wait tracer'
        it "can report the phase history" $ do
            testMonitoring Run $ \tracer (RunQuery query) -> do
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
            testMonitoring Step $ \tracer (RunQuery query) -> do
                tracer' <- async $ do
                    traceWith tracer RetrievingFunds
                state <- query SwitchQ
                state `shouldBe` Run
                wait tracer'
                (History phases, _state) <- query ObserveQ
                snd <$> phases `shouldBe` [RetrievingFunds]
