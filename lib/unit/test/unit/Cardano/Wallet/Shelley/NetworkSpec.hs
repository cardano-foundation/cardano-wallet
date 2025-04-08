{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Shelley.NetworkSpec (spec) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Trace
    ( nullTracer
    , traceInTVarIO
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , isWindows
    , mkWindowsPipeName
    )
import Cardano.Wallet.Launch.Cluster
    ( ClusterEra (..)
    , ClusterLog (..)
    , Config (..)
    , FaucetFunds (..)
    , LogFileConfig (..)
    , RunningNode (..)
    , defaultPoolConfigs
    , localClusterConfigsFromEnv
    , withCluster
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( OsNamedPipe (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , mkRelDirOf
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Network.Implementation
    ( Observer (..)
    , ObserverLog (..)
    , newObserver
    , withNetworkLayer
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromGenesisData
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..)
    )
import Control.Exception
    ( finally
    )
import Control.Monad
    ( replicateM
    , unless
    , void
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Trans
    ( MonadIO
    , lift
    )
import Control.Tracer
    ( Tracer
    , contramap
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Fmt
    ( build
    , fmt
    , indentF
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData
    )
import System.Directory
    ( removePathForcibly
    )
import System.Environment.Extended
    ( isEnvSet
    )
import "extra" System.IO.Extra
    ( withTempFile
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import System.Path
    ( absDir
    , absFile
    )
import System.Random
    ( randomIO
    )
import Test.Hspec
    ( Spec
    , beforeAll
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.Hspec.Core.Spec
    ( sequential
    )
import Test.QuickCheck
    ( counterexample
    , property
    )
import Test.QuickCheck.Monadic
    ( PropertyM
    , assert
    , monadicIO
    , monitor
    , run
    )
import Test.Utils.Trace
    ( traceSpec
    )
import UnliftIO.Async
    ( async
    , race_
    , waitAnyCancel
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import UnliftIO.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )

import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Data.Map as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    concurrentConnectionSpec
    observerSpec

concurrentConnectionSpec :: Spec
concurrentConnectionSpec = describe "NetworkLayer regression test #1708" $ do
    traceSpec $ it "Parallel local socket connections" $ \tr ->
        withTestNode nullTracer $ \np sock vData -> do
            let sTol = SyncTolerance 60
            tasks <- replicateM 10
                $ async
                $ withNetworkLayer
                    tr
                    tunedForMainnetPipeliningStrategy
                    np
                    sock
                    vData
                    sTol
                $ \nl -> do
                    -- Wait for the first tip result from the node
                    waiter <- newEmptyMVar
                    race_
                        (watchNodeTip nl (putMVar waiter))
                        (takeMVar waiter)
            void $ waitAnyCancel tasks

observerSpec :: Spec
observerSpec = sequential $ describe "Observer" $ do
    it "can fetch all observed keys, but not any other keys"
        $ property
        $ \keys1 keys2 -> monadicIO $ do
            (observer, refresh, _trVar) <- run mockObserver
            run $ mapM_ (startObserving observer) keys1
            run $ refresh True

            let allNothing = fromKeysWith (const Nothing)
            let unobservedKeys = Set.difference keys2 keys1
            unobservedValues <- run $ queryKeys observer unobservedKeys

            observedValues <- run $ queryKeys observer keys1

            assertEqual
                "observed keys return expected values"
                observedValues
                (fromKeysWith (Just . length) keys1)

            assertEqual
                "unobserved keys are all Nothing when queried"
                unobservedValues
                (allNothing unobservedKeys)

    describe "typical use" $ beforeAll mockObserver $ do
        -- Using monadic-property tests here /just/ for the sake of testing
        -- with multiple keys seem worthless.
        --
        -- State machine tests might be suitable on the other hand...
        --
        -- NOTE: We make sure to test conditions both before and after
        -- calling @refresh@, as it can be called arbitrarily without our
        -- (the observers') knowledge.
        --
        -- NOTE: These tests are stateful.
        -- They also use smaller @it@ blocks, with more @describe@ nesting,
        -- than much of the rest of the wallet tests. This is done for
        -- concise and readable test output.
        let k = ("k" :: String)
        let v = length k
        describe "startObserving" $ do
            it "(query k) returns Nothing before startObserving"
                $ \(observer, refresh, trVar) -> do
                    (query observer k) `shouldReturn` Nothing
                    trVar `shouldHaveTraced` []
                    refresh True
                    (query observer k) `shouldReturn` Nothing
                    shouldHaveTraced
                        trVar
                        [ MsgWillFetch Set.empty
                        , MsgDidFetch Map.empty
                        ]

            it "(query k) returns v after (startObserving k >> refresh)"
                $ \(observer, refresh, _) -> do
                    startObserving observer k
                    refresh True
                    let expectedValue = length k
                    query observer k `shouldReturn` Just expectedValue

            -- NOTE: Depends on the @refresh@ call from the previous test.
            it "traced MsgAddedObserver, MsgWillFetch, MsgDidFetch"
                $ \(_, _, trVar) -> do
                    shouldHaveTraced
                        trVar
                        [ MsgAddedObserver k
                        , MsgWillFetch $ Set.singleton k
                        , MsgDidFetch $ Map.singleton k v
                        , MsgDidChange $ Map.singleton k v
                        ]

        describe "calling startObserving a second time" $ do
            it "(query k) is still v"
                $ \(observer, refresh, trVar) -> do
                    startObserving observer k
                    query observer k `shouldReturn` Just v
                    refresh True
                    query observer k `shouldReturn` Just v
                    shouldHaveTraced
                        trVar
                        [ MsgWillFetch $ Set.singleton k
                        , MsgDidFetch $ Map.singleton k v
                        ]

        describe "when refresh fails" $ do
            it "(query k) returns the existing v"
                $ \(observer, refresh, _) -> do
                    refresh False
                    query observer k `shouldReturn` Just v

            it "only MsgWillFetch is traced"
                $ \(_, _, trVar) -> do
                    shouldHaveTraced
                        trVar
                        [ MsgWillFetch $ Set.singleton k
                        ]

        describe "stopObserving"
            $ it "makes (query k) return Nothing"
            $ \(observer, refresh, _) -> do
                stopObserving observer k
                query observer k `shouldReturn` Nothing
                refresh True
                query observer k `shouldReturn` Nothing
  where
    -- \| Expects given messages to have been traced /and/ clears the @TVar@.
    --
    -- NOTE: Reverses the contents in the @TVar@ to get a chronological order.
    shouldHaveTraced :: (Show log, Eq log) => TVar [log] -> [log] -> IO ()
    shouldHaveTraced trVar expected = do
        actual <- atomically ((readTVar trVar) <* (writeTVar trVar []))
        (reverse actual) `shouldBe` expected

    fromKeysWith :: Ord k => (k -> v) -> Set k -> Map k v
    fromKeysWith f =
        Map.fromList
            . map (\k -> (k, f k))
            . Set.toList

    queryKeys :: (Monad m, Ord k) => Observer m k v -> Set k -> m (Map k (Maybe v))
    queryKeys observer keys =
        Map.fromList
            <$> mapM
                (\k -> query observer k >>= \v -> return (k, v))
                (Set.toList keys)

    mockObserver
        :: IO
            ( Observer IO String Int
            , Bool -> IO ()
            , TVar [ObserverLog String Int]
            )
    mockObserver = do
        trVar <- newTVarIO []
        (ob, refresh) <- newObserver (traceInTVarIO trVar) fetch
        return (ob, refresh, trVar)
      where
        fetch True keys =
            pure
                $ Just
                $ Map.fromList
                $ map (\x -> (x, length x))
                $ Set.toList keys
        fetch False _ = pure Nothing

    -- Assert equiality in monadic properties with nice counterexamples
    --
    -- E.g.
    -- >> observed keys return expected values ✗
    -- >>      fromList [("",Just 0)]
    -- >>      /=
    -- >>      fromList [("",Just 1)]
    assertEqual :: (Eq a, Show a) => String -> a -> a -> PropertyM IO ()
    assertEqual description a b = do
        let condition = a == b
        let flag = if condition then "✓" else "✗"
        monitor (counterexample $ description <> " " <> flag)
        unless condition $ do
            monitor
                $ counterexample
                $ fmt
                $ indentF 4
                $ mconcat
                    [ build $ show a
                    , "\n/=\n"
                    , build $ show b
                    ]
        assert condition

randomName :: MonadIO m => m String
randomName = do
    n :: Int  <- randomIO
    pure $ "test-" <> show n

withTestNode
    :: Tracer IO ClusterLog
    -> (NetworkParameters -> CardanoNodeConn -> NodeToClientVersionData -> IO a)
    -> IO a
withTestNode tr action = evalContT $ do
    skipCleanup <- lift $ SkipCleanup <$> isEnvSet "NO_CLEANUP"
    dir <-
        ContT
            $ withSystemTempDir
                (contramap MsgTempDir tr)
                "network-spec"
                skipCleanup
    socketPath <-
        if isWindows
            then ContT $ \k -> do
                fp <- randomName
                let pipeName = mkWindowsPipeName fp
                k (WindowsPipe pipeName) `finally`
                    removePathForcibly pipeName
        else do
            socket <- ContT withTempFile
            pure $ UnixPipe $ FileOf $ absFile socket
    cfgClusterConfigs <- lift localClusterConfigsFromEnv
    let clusterConfig =
            Cluster.Config
                { cfgStakePools = defaultPoolConfigs
                , cfgLastHardFork = BabbageHardFork
                , cfgNodeLogging = LogFileConfig Info Nothing Info
                , cfgClusterDir = DirOf @"cluster" $ absDir dir
                , cfgClusterConfigs
                , cfgTestnetMagic = Cluster.TestnetMagic 42
                , cfgShelleyGenesisMods = []
                , cfgTracer = tr
                , cfgNodeOutputFile = Nothing
                , cfgRelayNodePath = mkRelDirOf "relay"
                , cfgClusterLogFile = Nothing
                , cfgNodeToClientSocket = socketPath
                }
    RunningNode sock genesisData vData <-
        ContT $ withCluster clusterConfig (FaucetFunds [] [] [])
    let (np, _, _) = fromGenesisData genesisData
    lift $ action np sock vData
