{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}

module Cardano.Wallet.Launch.Cluster.Http.ServiceSpec
    ( spec
    )
where

import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    )
import Cardano.Binary
    ( serialize'
    )
import Cardano.Chain.Common
    ( unsafeGetLovelace
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    )
import Cardano.Read.Ledger.Tx.Outputs
    ( Outputs (..)
    , getEraOutputs
    )
import Cardano.Wallet.Faucet.Gen.Address
    ( NetworkTag (..)
    , genAddress
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds (FaucetFunds)
    , RunningNode (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( FaucetQ (SendFaucetAssetsQ)
    , RunFaucetQ (RunFaucetQ)
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets (SendFaucetAssets)
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( MonitorQ (..)
    , RunMonitorQ (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Service
    ( ServiceConfiguration (..)
    , withService
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Cardano.Wallet.Launch.Cluster.Process
    ( WalletPresence (..)
    , defaultEnvVars
    , withLocalCluster
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
import Cardano.Wallet.Network.Rollback.One
    ( oneHistory
    )
import Cardano.Wallet.Network.Streaming
    ( ChainStream
    , eraBlockS
    , eraTxS
    , forChainStream
    , forConsensusS
    , newTMVarBuffer
    , scanChainStream
    , withStreamingFromBlockChain
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromGenesisData
    )
import Cardano.Wallet.Primitive.NetworkId
    ( SNetworkId (SMainnet)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (SyncTolerance)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Read
    ( Allegra
    , Alonzo
    , Babbage
    , Conway
    , Era (..)
    , EraValue
    , IsEra (..)
    , Mary
    , Shelley
    , Tx
    , applyEraFun
    , (:*:) (..)
    )
import Control.Monad
    ( join
    , replicateM
    , unless
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Fix
    ( fix
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Control.Tracer
    ( Tracer
    , nullTracer
    , traceWith
    )
import Data.ByteString
    ( ByteString
    )
import Data.Foldable
    ( toList
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Streaming
    ( MonadIO (liftIO)
    , Of
    , Stream
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    )
import Test.QuickCheck
    ( generate
    )
import UnliftIO.Async
    ( async
    , race
    , wait
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import Prelude

import qualified Cardano.Address as Addr
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Alonzo.TxOut as Alonzo
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Network.Implementation as NL
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streaming.Prelude as S

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
            testService Step $ \_ (RunMonitorQ query) -> do
                result <- query ReadyQ
                result `shouldBe` False
        it "can trace" $ do
            testService Run $ \tracer _ -> do
                traceWith tracer RetrievingFunds
        it "can report readiness" $ do
            testService Run $ \tracer (RunMonitorQ query) -> do
                traceWith tracer (Cluster Nothing)
                result <- query ReadyQ
                result `shouldBe` True
        it "can step the tracer thread" $ do
            testService Step $ \tracer (RunMonitorQ query) -> do
                tracer' <- async $ do
                    traceWith tracer (Cluster Nothing)
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ query StepQ >> loop
                wait tracer'
        it "can report the phase history" $ do
            testService Run $ \tracer (RunMonitorQ query) -> do
                traceWith tracer RetrievingFunds
                traceWith tracer Metadata
                traceWith tracer Genesis
                traceWith tracer Pool0
                traceWith tracer Funding
                traceWith tracer Pools
                traceWith tracer Relay
                traceWith tracer (Cluster Nothing)
                threadDelay 10_000
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
            testService Step $ \tracer (RunMonitorQ query) -> do
                tracer' <- async $ do
                    traceWith tracer RetrievingFunds
                state <- query SwitchQ
                state `shouldBe` Run
                wait tracer'
                (History phases, _state) <- query ObserveQ
                snd <$> phases `shouldBe` [RetrievingFunds]
    describe "withService application" $ do
        it "can start and stop" $ evalContT $ do
            ((RunMonitorQ query, _), _) <-
                withLocalCluster
                    "can-start-and-stop"
                    NoWallet
                    defaultEnvVars
                    noFunds
            liftIO $ do
                result <- query ReadyQ
                result `shouldBe` False
        it "can wait for cluster ready before ending" $ evalContT $ do
            ((RunMonitorQ query, _), _) <-
                withLocalCluster
                    "can-wait-for-cluster-ready-before-ending"
                    NoWallet
                    defaultEnvVars
                    noFunds
            liftIO $ do
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ threadDelay 1_000_000 >> loop
    describe "withNetwork" $ do
        it "can start and stop" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                withLocalCluster
                    "withNetwork-can-start-and-stop"
                    NoWallet
                    defaultEnvVars
                    noFunds
            node <- liftIO $ waitForNode query
            network <- withNetwork tr node
            tip <- liftIO $ currentNodeTip network
            tip `seq` pure ()
        it "can get the first block" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                withLocalCluster
                    "withNetwork-can-get-collect-the-incoming-blocks"
                    NoWallet
                    defaultEnvVars
                    noFunds
            node <- liftIO $ waitForNode query
            network <- withNetwork tr node
            blocks <- withStreamingFromBlockChain network tr newTMVarBuffer
            firstBlock <-
                liftIO
                    $ S.head_ -- get the first element
                    $ elements blocks
            liftIO $ join firstBlock `shouldNotBe` Nothing
        it "can get the first non-empty balance" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                withLocalCluster
                    "withNetwork-can-get-collect-the-incoming-blocks"
                    NoWallet
                    defaultEnvVars
                    noFunds
            node <- liftIO $ waitForNode query
            network <- withNetwork tr node
            blocks <- withStreamingFromBlockChain network tr newTMVarBuffer
            firstBalance <-
                liftIO
                    $ S.head_
                    $ balance
                    $ outputs
                    $ eraTxS
                    $ eraBlockS
                    $ forConsensusS blocks
            liftIO $ firstBalance `shouldNotBe` Nothing
    describe "send faucet assets" $ do
        it "can send assets to a node" $ evalContT $ do
            ((query, RunFaucetQ faucet), ToTextTracer tr) <-
                withLocalCluster
                    "send-faucet-assets-can-send-assets-to-a-node"
                    NoWallet
                    defaultEnvVars
                    noFunds
            node <- liftIO $ waitForNode query
            addrs <- liftIO $ replicateM 2 $ generate $ genAddress [TestnetTag]
            liftIO
                $ faucet
                $ SendFaucetAssetsQ
                $ SendFaucetAssets
                    1
                    [ ( Address . Addr.unAddress $ addr
                      , (TokenBundle (W.Coin 1_234_567) mempty, mempty)
                      )
                    | addr <- addrs
                    ]
            network <- withNetwork tr node
            blocks <-
                withStreamingFromBlockChain network tr newTMVarBuffer
            eBalances <-
                liftIO
                    $ race (threadDelay 100_000_000)
                    $ waitForAddressesBalance
                        (Set.fromList $ Addr.unAddress <$> addrs)
                    $ balance
                    $ outputs
                    $ eraTxS
                    $ eraBlockS
                    $ forConsensusS blocks
            liftIO
                $ eBalances
                `shouldBe` Right
                    ( Map.fromList
                        $ map (,1_234_567) (Addr.unAddress <$> addrs)
                    )

waitForNode :: RunMonitorQ IO -> IO RunningNode
waitForNode (RunMonitorQ query) = fix $ \loop -> do
    (history', _) <- query ObserveQ
    case getNode history' of
        Nothing -> threadDelay 10_000 >> loop
        Just node -> pure node

getNode :: History -> Maybe RunningNode
getNode (History phases) = case phases of
    [] -> Nothing
    (_time, phase) : _ -> case phase of
        Cluster (Just node) -> Just node
        _ -> Nothing

waitForAddressesBalance
    :: Monad m
    => Set ByteString
    -> Stream (Of (Map ByteString Integer)) m ()
    -> m (Map ByteString Integer)
waitForAddressesBalance addrs s = do
    mm <- S.head_ . S.dropWhile f $ s
    pure $ case mm of
        Nothing -> mempty
        Just m -> Map.filterWithKey (\k _ -> k `Set.member` addrs) m
  where
    f m = not $ addrs `Set.isSubsetOf` Map.keysSet m

data TxOut = TxOut
    { address :: ByteString
    , value :: Integer
    }
    deriving stock (Show, Eq)

outputs
    :: Monad m
    => ChainStream (EraValue (ctx :*: Tx)) m r
    -> ChainStream TxOut m r
outputs = forChainStream $ S.each . applyEraFun f
  where
    f :: IsEra era => (ctx :*: Tx) era -> [TxOut]
    f (_bh :*: tx) = txOutFromOutput $ getEraOutputs tx

-- a bit of a hack to drop the first element of a stream
-- because we know it's a rollback and we're only interested in the
-- forward
elements
    :: Monad m
    => ChainStream a m r
    -> Stream (Of (Maybe a)) m r
elements = S.drop 1 . scanChainStream (const Just) (oneHistory Nothing)

balance
    :: Monad m
    => ChainStream TxOut m r
    -> Stream (Of (Map ByteString Integer)) m r
balance =
    scanChainStream
        (\m (TxOut addr val) -> Map.insertWith (+) addr val m)
        $ oneHistory Map.empty

txOutFromOutput :: forall era. IsEra era => Outputs era -> [TxOut]
txOutFromOutput = case theEra :: Era era of
    Byron -> \(Outputs os) -> fromByronTxOut <$> toList os
    Shelley -> \(Outputs os) -> fromShelleyTxOut <$> toList os
    Allegra -> \(Outputs os) -> fromAllegraTxOut <$> toList os
    Mary -> \(Outputs os) -> fromMaryTxOut <$> toList os
    Alonzo -> \(Outputs os) -> fromAlonzoTxOut <$> toList os
    Babbage -> \(Outputs os) -> fromBabbageTxOut <$> toList os
    Conway -> \(Outputs os) -> fromConwayTxOut <$> toList os
  where
    fromByronTxOut :: Byron.TxOut -> TxOut
    fromByronTxOut (Byron.TxOut addr amount) =
        TxOut (serialize' addr) (fromIntegral $ unsafeGetLovelace amount)

    fromShelleyTxOut :: SL.ShelleyTxOut Shelley -> TxOut
    fromShelleyTxOut (SL.ShelleyTxOut addr (Coin amount)) =
        TxOut (SL.serialiseAddr addr) amount

    fromAllegraTxOut :: SL.ShelleyTxOut Allegra -> TxOut
    fromAllegraTxOut (SL.ShelleyTxOut addr (Coin amount)) =
        TxOut (SL.serialiseAddr addr) amount

    fromMaryTxOut :: SL.ShelleyTxOut Mary -> TxOut
    fromMaryTxOut (SL.ShelleyTxOut addr (MaryValue (Coin amount) _)) =
        TxOut (SL.serialiseAddr addr) amount

    fromAlonzoTxOut :: Alonzo.AlonzoTxOut Alonzo -> TxOut
    fromAlonzoTxOut (Alonzo.AlonzoTxOut addr (MaryValue (Coin amount) _) _) =
        TxOut (SL.serialiseAddr addr) amount

    fromBabbageTxOut :: Babbage.BabbageTxOut Babbage -> TxOut
    fromBabbageTxOut
        (Babbage.BabbageTxOut addr (MaryValue (Coin amount) _) _ _) =
            TxOut (SL.serialiseAddr addr) amount

    fromConwayTxOut :: Babbage.BabbageTxOut Conway -> TxOut
    fromConwayTxOut
        (Babbage.BabbageTxOut addr (MaryValue (Coin amount) _) _ _) =
            TxOut (SL.serialiseAddr addr) amount
