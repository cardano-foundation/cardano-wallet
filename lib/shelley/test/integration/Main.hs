{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Plugin
    ( loadPlugin )
import Cardano.BM.Trace
    ( appendName )
import Cardano.CLI
    ( LogOutput (..)
    , Port (..)
    , ekgEnabled
    , getEKGURL
    , getPrometheusURL
    , withLogging
    )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Startup
    ( setDefaultFilePermissions, withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( EncodeAddress (..) )
import Cardano.Wallet.Logging
    ( BracketLog (..), bracketTracer, trMessageText )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.Faucet
    ( initFaucet )
import Cardano.Wallet.Shelley.Launch
    ( ClusterLog
    , RunningNode (..)
    , moveInstantaneousRewardsTo
    , nodeMinSeverityFromEnv
    , oneMillionAda
    , poolConfigsFromEnv
    , sendFaucetFundsTo
    , testLogDirFromEnv
    , testMinSeverityFromEnv
    , walletMinSeverityFromEnv
    , withCluster
    , withSMASH
    , withSystemTempDir
    )
import Control.Arrow
    ( first )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Either.Combinators
    ( whenLeft )
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( (</>) )
import System.IO
    ( BufferMode (..), hSetBuffering, stdout )
import Test.Hspec
    ( Spec, SpecWith, describe, hspec, parallel )
import Test.Hspec.Extra
    ( aroundAll )
import Test.Integration.Faucet
    ( genRewardAccounts, mirMnemonics, shelleyIntegrationTestFunds )
import Test.Integration.Framework.Context
    ( Context (..), PoolGarbageCollectionEvent (..) )
import Test.Utils.Paths
    ( inNixBuild )
import UnliftIO.Async
    ( race )
import UnliftIO.Exception
    ( SomeException, throwIO, withException )
import UnliftIO.MVar
    ( newEmptyMVar, putMVar, takeMVar )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Data.Text as T
import qualified Test.Integration.Scenario.API.Byron.Addresses as ByronAddresses
import qualified Test.Integration.Scenario.API.Byron.CoinSelections as ByronCoinSelections
import qualified Test.Integration.Scenario.API.Byron.HWWallets as ByronHWWallets
import qualified Test.Integration.Scenario.API.Byron.Migrations as ByronMigrations
import qualified Test.Integration.Scenario.API.Byron.Transactions as ByronTransactions
import qualified Test.Integration.Scenario.API.Byron.Wallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.CoinSelections as CoinSelections
import qualified Test.Integration.Scenario.API.Shelley.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Shelley.Migrations as Migrations
import qualified Test.Integration.Scenario.API.Shelley.Network as Network_
import qualified Test.Integration.Scenario.API.Shelley.Settings as Settings
import qualified Test.Integration.Scenario.API.Shelley.StakePools as StakePools
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Shelley.HWWallets as HWWalletsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Wallets as WalletsCLI

main :: forall n. (n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $ withTracers $ \tracers -> do
    hSetBuffering stdout LineBuffering
    setDefaultFilePermissions
    nix <- inNixBuild
    hspec $ do
        describe "No backend required" $
            parallelIf (not nix) $ describe "Miscellaneous CLI tests"
                MiscellaneousCLI.spec
        specWithServer tracers $ do
            describe "API Specifications" $ do
                parallel $ do
                    Addresses.spec @n
                    CoinSelections.spec @n
                    ByronAddresses.spec @n
                    ByronCoinSelections.spec @n
                    Wallets.spec @n
                    ByronWallets.spec @n
                    HWWallets.spec @n
                    Migrations.spec @n
                    ByronMigrations.spec @n
                    Transactions.spec @n
                    Network.spec
                    Network_.spec
                    StakePools.spec @n
                    ByronTransactions.spec @n
                    ByronHWWallets.spec @n

                -- possible conflict with StakePools
                Settings.spec @n

            -- Hydra runs tests with code coverage enabled. CLI tests run
            -- multiple processes. These processes can try to write to the
            -- same .tix file simultaneously, causing errors.
            --
            -- Because of this, don't run the CLI tests in parallel in hydra.
            parallelIf (not nix) $ describe "CLI Specifications" $ do
                AddressesCLI.spec @n
                TransactionsCLI.spec @n
                WalletsCLI.spec @n
                HWWalletsCLI.spec @n
                PortCLI.spec
                NetworkCLI.spec
  where
    parallelIf :: forall a. Bool -> SpecWith a -> SpecWith a
    parallelIf flag = if flag then parallel else id

specWithServer
    :: (Tracer IO TestsLog, Tracers IO)
    -> SpecWith Context
    -> Spec
specWithServer (tr, tracers) = aroundAll withContext
  where
    withContext :: (Context -> IO ()) -> IO ()
    withContext action = bracketTracer' tr "withContext" $ do
        ctx <- newEmptyMVar
        poolGarbageCollectionEvents <- newIORef []
        let dbEventRecorder =
                recordPoolGarbageCollectionEvents poolGarbageCollectionEvents
        let setupContext np wAddr = bracketTracer' tr "setupContext" $ do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                prometheusUrl <- (maybe "none" (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)) <$> getPrometheusURL
                ekgUrl <- (maybe "none" (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)) <$> getEKGURL
                traceWith tr $ MsgBaseUrl baseUrl ekgUrl prometheusUrl
                let fiveMinutes = 300*1000*1000 -- 5 minutes in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro fiveMinutes
                    })
                faucet <- initFaucet

                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                    , _faucet = faucet
                    , _feeEstimator = error "feeEstimator: unused in shelley specs"
                    , _networkParameters = np
                    , _poolGarbageCollectionEvents = poolGarbageCollectionEvents
                    }
        let action' = bracketTracer' tr "spec" . action
        res <- race
            (withServer dbEventRecorder setupContext)
            (takeMVar ctx >>= action')
        whenLeft res (throwIO . ProcessHasExited "integration")

    -- A decorator for the pool database that records all calls to the
    -- 'removeRetiredPools' operation.
    --
    -- The parameters and return value of each call are recorded by appending
    -- a 'PoolGarbageCollectionEvent' value to the start of the given log.
    --
    recordPoolGarbageCollectionEvents
        :: IORef [PoolGarbageCollectionEvent]
        -> Pool.DBDecorator IO
    recordPoolGarbageCollectionEvents eventsRef = Pool.DBDecorator decorate
      where
        decorate Pool.DBLayer {..} =
            Pool.DBLayer {removeRetiredPools = removeRetiredPoolsDecorated, ..}
          where
            removeRetiredPoolsDecorated epochNo = do
                certificates <- removeRetiredPools epochNo
                let event = PoolGarbageCollectionEvent epochNo certificates
                liftIO $ do
                    traceWith tr $ MsgPoolGarbageCollectionEvent event
                    atomicModifyIORef' eventsRef ((, ()) . (event :))
                pure certificates

    withServer dbDecorator action = bracketTracer' tr "withServer" $ withSMASH tr' $ do
        minSev <- nodeMinSeverityFromEnv
        testPoolConfigs <- poolConfigsFromEnv
        withSystemTempDir tr' "test" $ \dir -> do
            extraLogDir <- (fmap (,Info)) <$> testLogDirFromEnv
            withCluster
                tr'
                minSev
                testPoolConfigs
                dir
                extraLogDir
                onByron
                (afterFork dir)
                (onClusterStart action dir dbDecorator)

    tr' = contramap MsgCluster tr
    onByron _ = pure ()
    afterFork dir _ = do
        traceWith tr MsgSettingUpFaucet
        let rewards = (,Coin $ fromIntegral oneMillionAda) <$>
                concatMap genRewardAccounts mirMnemonics
        moveInstantaneousRewardsTo tr' dir rewards
        let encodeAddr = T.unpack . encodeAddress @'Mainnet
        let addresses = map (first encodeAddr) shelleyIntegrationTestFunds
        sendFaucetFundsTo tr' dir addresses


    onClusterStart action dir dbDecorator node = do
        let db = dir </> "wallets"
        createDirectory db
        -- NOTE: We may want to keep a wallet running across the fork, but
        -- having three callbacks like this might not work well for that.
        serveWallet
            (SomeNetworkDiscriminant $ Proxy @'Mainnet)
            tracers
            (SyncTolerance 10)
            (Just db)
            (Just dbDecorator)
            "127.0.0.1"
            ListenOnRandomPort
            Nothing
            Nothing
            socketPath
            block0
            (gp, vData)
            (action gp)
            `withException` (traceWith tr . MsgServerError)
      where
        RunningNode socketPath block0 (gp, vData) = node

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data TestsLog
    = MsgBracket Text BracketLog
    | MsgBaseUrl Text Text Text
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    | MsgPoolGarbageCollectionEvent PoolGarbageCollectionEvent
    | MsgServerError SomeException
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBracket name b -> name <> ": " <> toText b
        MsgBaseUrl walletUrl ekgUrl prometheusUrl -> mconcat
            [ "Wallet url: " , walletUrl
            , ", EKG url: " , ekgUrl
            , ", Prometheus url:", prometheusUrl
            ]
        MsgSettingUpFaucet -> "Setting up faucet..."
        MsgCluster msg -> toText msg
        MsgPoolGarbageCollectionEvent e -> mconcat
            [ "Intercepted pool garbage collection event for epoch "
            , toText (poolGarbageCollectionEpochNo e)
            , ". "
            , case poolGarbageCollectionCertificates e of
                [] -> "No pools were removed from the database."
                ps -> mconcat
                    [ "The following pools were removed from the database: "
                    , T.unwords (T.pack . show <$> ps)
                    ]
            ]
        MsgServerError e -> T.pack (show e)

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgBracket _ _ -> Debug
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl {} -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
        MsgPoolGarbageCollectionEvent _ -> Info
        MsgServerError{} -> Critical

withTracers
    :: ((Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTracers action = do
    let getLogOutputs getMinSev name = do
            minSev <- getMinSev
            logDir <- testLogDirFromEnv
            let logToFile dir = LogToFile (dir </> name) (min minSev Info)
            pure (LogToStdout minSev:maybe [] (pure . logToFile) logDir)

    walletLogOutputs <- getLogOutputs walletMinSeverityFromEnv "wallet.log"
    testLogOutputs <- getLogOutputs testMinSeverityFromEnv "test.log"

    withLogging walletLogOutputs $ \(sb, (cfg, walTr)) -> do
        ekgEnabled >>= flip when (EKG.plugin cfg walTr sb >>= loadPlugin sb)
        withLogging testLogOutputs $ \(_, (_, testTr)) -> do
            let trTests = appendName "integration" testTr
            let tracers = setupTracers (tracerSeverities (Just Info)) walTr
            action (trMessageText trTests, tracers)

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
