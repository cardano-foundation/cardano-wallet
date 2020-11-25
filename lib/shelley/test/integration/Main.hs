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
import Cardano.BM.Trace
    ( appendName )
import Cardano.CLI
    ( LogOutput (..), Port (..), withLogging )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Startup
    ( withUtf8Encoding )
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
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley )
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
    , withSystemTempDir
    , withTempDir
    )
import Control.Arrow
    ( first )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
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
import Test.Integration.Framework.DSL
    ( KnownCommand (..) )
import Test.Utils.Paths
    ( inNixBuild )

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

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Shelley where
    commandName = "cardano-wallet"

main :: forall t n . (t ~ Shelley, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $ withTracers $ \tracers -> do
    hSetBuffering stdout LineBuffering
    nix <- inNixBuild
    hspec $ do
        describe "No backend required" $
            parallelIf (not nix) $ describe "Miscellaneous CLI tests" $
                MiscellaneousCLI.spec @t
        specWithServer tracers $ do
            parallel $ describe "API Specifications" $ do
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
                PortCLI.spec @t
                NetworkCLI.spec @t
  where
    parallelIf :: forall a. Bool -> SpecWith a -> SpecWith a
    parallelIf flag = if flag then parallel else id

specWithServer
    :: (Tracer IO TestsLog, Tracers IO)
    -> SpecWith (Context Shelley)
    -> Spec
specWithServer (tr, tracers) = aroundAll withContext
  where
    withContext :: (Context Shelley -> IO ()) -> IO ()
    withContext action = bracketTracer' tr "withContext" $ do
        ctx <- newEmptyMVar
        poolGarbageCollectionEvents <- newIORef []
        let dbEventRecorder =
                recordPoolGarbageCollectionEvents poolGarbageCollectionEvents
        let setupContext np wAddr = bracketTracer' tr "setupContext" $ do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                traceWith tr $ MsgBaseUrl baseUrl
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
                    , _target = Proxy
                    , _poolGarbageCollectionEvents = poolGarbageCollectionEvents
                    }
        let action' = bracketTracer' tr "spec" . action
        race
            (takeMVar ctx >>= action')
            (withServer dbEventRecorder setupContext) >>=
            (either pure (throwIO . ProcessHasExited "integration"))

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

    withServer dbDecorator action = bracketTracer' tr "withServer" $ do
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
        let encodeAddr = T.unpack . encodeAddress @'Mainnet
        let addresses = map (first encodeAddr) shelleyIntegrationTestFunds
        sendFaucetFundsTo tr' dir addresses

        let rewards = (,Coin $ fromIntegral oneMillionAda) <$>
                concatMap genRewardAccounts mirMnemonics
        moveInstantaneousRewardsTo tr' dir rewards

    onClusterStart
        action dir dbDecorator (RunningNode socketPath block0 (gp, vData)) = do
        -- NOTE: We may want to keep a wallet running across the fork, but
        -- having three callbacks like this might not work well for that.
        withTempDir tr' dir "wallets" $ \db -> do
            serveWallet @(IO Shelley)
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

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data TestsLog
    = MsgBracket Text BracketLog
    | MsgBaseUrl Text
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    | MsgPoolGarbageCollectionEvent PoolGarbageCollectionEvent
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBracket name b -> name <> ": " <> toText b
        MsgBaseUrl txt -> txt
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

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgBracket _ _ -> Debug
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl _ -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
        MsgPoolGarbageCollectionEvent _ -> Info

withTracers
    :: ((Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTracers action = do
    walletMinSeverity <- walletMinSeverityFromEnv
    testMinSeverity <- testMinSeverityFromEnv

    let extraOutput name = (maybe [] (\f -> [LogToFile (f </> name) Info]))
            <$> testLogDirFromEnv

    walletLogOutputs <- ([LogToStdout walletMinSeverity] ++) <$>
        extraOutput "wallet.log"
    testLogOutputs <- ([LogToStdout testMinSeverity] ++) <$>
        extraOutput "test.log"

    withLogging walletLogOutputs $ \(_, walTr) -> do
        withLogging testLogOutputs $ \(_, testTr) -> do
            let trTests = appendName "integration" testTr
            let tracers = setupTracers
                    (tracerSeverities (Just Info)) walTr
            action (trMessageText trTests, tracers)

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
