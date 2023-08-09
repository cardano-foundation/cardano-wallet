{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    ( installSignalHandlersNoLogging
    , setDefaultFilePermissions
    , withUtf8Encoding
    )
import Cardano.Wallet.Faucet
    ( byronIntegrationTestFunds
    , genRewardAccounts
    , hwWalletFunds
    , maryIntegrationTestAssets
    , mirMnemonics
    , seaHorseTestAssets
    , shelleyIntegrationTestFunds
    )
import Cardano.Wallet.Faucet.Shelley
    ( initFaucet )
import Cardano.Wallet.Launch
    ( withSystemTempDir )
import Cardano.Wallet.Launch.Cluster
    ( ClusterLog
    , Credential (..)
    , FaucetFunds (..)
    , RunningNode (..)
    , clusterEraFromEnv
    , clusterEraToString
    , clusterToApiEra
    , localClusterConfigFromEnv
    , moveInstantaneousRewardsTo
    , oneMillionAda
    , sendFaucetAssetsTo
    , testLogDirFromEnv
    , testMinSeverityFromEnv
    , walletListenFromEnv
    , walletMinSeverityFromEnv
    , withCluster
    , withSMASH
    )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer, stdoutTextTracer, trMessageText )
import Cardano.Wallet.Network.Ports
    ( portFromURL )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..), NetworkId (..), SNetworkId (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( Tracers, serveWallet, setupTracers, tracerSeverities )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( decodeAddress, encodeAddress )
import Cardano.Wallet.TokenMetadata.MockServer
    ( queryServerStatic, withMetadataServer )
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
import Data.Maybe
    ( fromMaybe )
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
import Network.URI
    ( URI )
import Ouroboros.Network.Client.Wallet
    ( tunedForMainnetPipeliningStrategy )
import System.Directory
    ( createDirectory )
import System.Environment
    ( setEnv )
import System.FilePath
    ( (</>) )
import Test.Hspec.Core.Spec
    ( Spec, SpecWith, describe, parallel, sequential )
import Test.Hspec.Extra
    ( aroundAll, hspecMain )
import Test.Integration.Framework.Context
    ( Context (..), PoolGarbageCollectionEvent (..) )
import Test.Utils.Paths
    ( getTestData, inNixBuild )
import UnliftIO.Async
    ( race )
import UnliftIO.Exception
    ( SomeException, isAsyncException, throwIO, withException )
import UnliftIO.MVar
    ( newEmptyMVar, newMVar, putMVar, takeMVar, withMVar )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Data.Text as T
import qualified Test.Integration.Scenario.API.Blocks as Blocks
import qualified Test.Integration.Scenario.API.Byron.Addresses as ByronAddresses
import qualified Test.Integration.Scenario.API.Byron.CoinSelections as ByronCoinSelections
import qualified Test.Integration.Scenario.API.Byron.HWWallets as ByronHWWallets
import qualified Test.Integration.Scenario.API.Byron.Migrations as ByronMigrations
import qualified Test.Integration.Scenario.API.Byron.Transactions as ByronTransactions
import qualified Test.Integration.Scenario.API.Byron.Wallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shared.Addresses as SharedAddresses
import qualified Test.Integration.Scenario.API.Shared.Transactions as SharedTransactions
import qualified Test.Integration.Scenario.API.Shared.Wallets as SharedWallets
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.CoinSelections as CoinSelections
import qualified Test.Integration.Scenario.API.Shelley.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Shelley.Migrations as Migrations
import qualified Test.Integration.Scenario.API.Shelley.Network as Network_
import qualified Test.Integration.Scenario.API.Shelley.Settings as Settings
import qualified Test.Integration.Scenario.API.Shelley.StakePools as StakePools
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.TransactionsNew as TransactionsNew
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Shelley.HWWallets as HWWalletsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Wallets as WalletsCLI

main :: forall n. (n ~ 'Mainnet) => IO ()
main = withTestsSetup $ \testDir tracers -> do
    nix <- inNixBuild
    hspecMain $ do
        describe "No backend required" $
            parallelIf (not nix) $ describe "Miscellaneous CLI tests"
                MiscellaneousCLI.spec
        specWithServer testDir tracers $ do
            describe "API Specifications" $ do
                parallel $ do
                    Addresses.spec @n
                    CoinSelections.spec @n
                    Blocks.spec
                    ByronAddresses.spec @n
                    ByronCoinSelections.spec @n
                    Wallets.spec @n
                    SharedWallets.spec @n
                    SharedAddresses.spec @n
                    SharedTransactions.spec @n
                    ByronWallets.spec @n
                    HWWallets.spec @n
                    Migrations.spec @n
                    ByronMigrations.spec @n
                    Transactions.spec @n
                    TransactionsNew.spec @n
                    Network.spec
                    Network_.spec
                    StakePools.spec @n
                    ByronTransactions.spec @n
                    ByronHWWallets.spec @n

            -- Possible conflict with StakePools - mark as not parallizable
            sequential Settings.spec

            -- CI runs tests with code coverage enabled. CLI tests run
            -- multiple processes. These processes can try to write to the
            -- same .tix file simultaneously, causing errors.
            --
            -- Because of this, don't run the CLI tests in parallel in CI.
            parallelIf (not nix) $ describe "CLI Specifications" $ do
                AddressesCLI.spec @n
                TransactionsCLI.spec @n
                WalletsCLI.spec @n
                HWWalletsCLI.spec @n
                PortCLI.spec
                NetworkCLI.spec
  where
    parallelIf flag = if flag then parallel else sequential

-- | Do all the program setup required for integration tests, create a temporary
-- directory, and pass this info to the main hspec action.
withTestsSetup :: (FilePath -> (Tracer IO TestsLog, Tracers IO) -> IO a) -> IO a
withTestsSetup action = do
    -- Handle SIGTERM properly
    installSignalHandlersNoLogging
    -- Stop cardano-cli complaining about file permissions
    setDefaultFilePermissions
    -- Enables small test-specific workarounds, like timing out faster if wallet
    -- deletion fails.
    setEnv "CARDANO_WALLET_TEST_INTEGRATION" "1"

    -- Flush test output as soon as a line is printed.
    -- Set UTF-8, regardless of user locale.
    withUtf8Encoding $
        -- This temporary directory will contain logs, and all other data
        -- produced by the integration tests.
        withSystemTempDir stdoutTextTracer "test" $ \testDir ->
            withTracers testDir $ action testDir

specWithServer
    :: FilePath
    -> (Tracer IO TestsLog, Tracers IO)
    -> SpecWith Context
    -> Spec
specWithServer testDir (tr, tracers) = aroundAll withContext
  where
    withContext :: (Context -> IO ()) -> IO ()
    withContext action = bracketTracer' tr "withContext" $ do
        ctx <- newEmptyMVar
        poolGarbageCollectionEvents <- newIORef []
        let dbEventRecorder =
                recordPoolGarbageCollectionEvents poolGarbageCollectionEvents
        let setupContext smashUrl conn np baseUrl = bracketTracer' tr "setupContext" $ do
                prometheusUrl <- (maybe "none" (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)) <$> getPrometheusURL
                ekgUrl <- (maybe "none" (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)) <$> getEKGURL
                traceWith tr $ MsgBaseUrl baseUrl ekgUrl prometheusUrl smashUrl
                let fiveMinutes = 300 * 1_000 * 1_000 -- 5 minutes in microseconds
                manager <- newManager $ defaultManagerSettings
                    { managerResponseTimeout = responseTimeoutMicro fiveMinutes
                    }
                faucet <- initFaucet

                era <- clusterEraFromEnv

                mintSeaHorseAssetsLock <- newMVar ()

                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = (baseUrl, manager)
                    , _walletPort = Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _networkParameters = np
                    , _poolGarbageCollectionEvents = poolGarbageCollectionEvents
                    , _mainEra = clusterToApiEra era
                    , _smashUrl = smashUrl
                    , _mintSeaHorseAssets = \nPerAddr batchSize c addrs ->
                        withMVar mintSeaHorseAssetsLock $ \() ->
                            sendFaucetAssetsTo tr' conn testDir era batchSize
                                $ encodeAddresses
                                $ seaHorseTestAssets nPerAddr c addrs
                    , _moveRewardsToScript = \(script, coin) ->
                            moveInstantaneousRewardsTo tr' conn testDir era
                            [(ScriptCredential script, coin)]
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
                    atomicModifyIORef' eventsRef ((,()) . (event :))
                pure certificates

    withServer dbDecorator onReady = bracketTracer' tr "withServer" $
        withSMASH tr' testDir $ \smashUrl -> do
            clusterCfg <- localClusterConfigFromEnv
            withCluster tr' testDir clusterCfg faucetFunds
                $ onClusterStart (onReady $ T.pack smashUrl) dbDecorator

    tr' = contramap MsgCluster tr
    encodeAddresses = map (first (T.unpack . encodeAddress SMainnet))

    faucetFunds = FaucetFunds
        { pureAdaFunds =
            shelleyIntegrationTestFunds
             <> byronIntegrationTestFunds
             <> map (first unsafeDecodeAddr) hwWalletFunds
        , maFunds =
            maryIntegrationTestAssets (Coin 10_000_000)
        , mirFunds =
            first KeyCredential
            . (,Coin $ fromIntegral oneMillionAda)
            <$> concatMap genRewardAccounts mirMnemonics
        }

    unsafeDecodeAddr = either (error . show) id . decodeAddress SMainnet

    onClusterStart action dbDecorator (RunningNode conn block0 (gp, vData) genesisPools) = do
        let db = testDir </> "wallets"
        createDirectory db
        listen <- walletListenFromEnv
        let testMetadata = $(getTestData) </> "token-metadata.json"
        withMetadataServer (queryServerStatic testMetadata) $ \tokenMetaUrl ->
            serveWallet
                (NodeSource conn vData (SyncTolerance 10))
                gp
                tunedForMainnetPipeliningStrategy
                NMainnet
                genesisPools
                tracers
                (Just db)
                (Just dbDecorator)
                "127.0.0.1"
                listen
                Nothing
                Nothing
                (Just tokenMetaUrl)
                block0
                (action conn gp)
                `withException` (traceWith tr . MsgServerError)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data TestsLog
    = MsgBracket Text BracketLog
    | MsgBaseUrl URI Text Text Text
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    | MsgPoolGarbageCollectionEvent PoolGarbageCollectionEvent
    | MsgServerError SomeException
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBracket name b -> name <> ": " <> toText b
        MsgBaseUrl walletUrl ekgUrl prometheusUrl smashUrl -> T.unlines
            [ "Wallet url: " <> T.pack (show walletUrl)
            , "EKG url: " <> ekgUrl
            , "Prometheus url: " <> prometheusUrl
            , "SMASH url: " <> smashUrl
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
        MsgServerError e
            | isAsyncException e -> "Server thread cancelled"
            | otherwise -> T.pack (show e)

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgBracket _ _ -> Debug
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl {} -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
        MsgPoolGarbageCollectionEvent _ -> Info
        MsgServerError e
            | isAsyncException e -> Info
            | otherwise -> Critical

withTracers
    :: FilePath
    -> ((Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTracers testDir action = do
    let getLogOutputs getMinSev name = do
            minSev <- getMinSev
            eraStr <- clusterEraToString <$> clusterEraFromEnv
            logDir <- fromMaybe testDir <$> testLogDirFromEnv (Just eraStr)
            pure
                [ LogToFile (logDir </> name) (min minSev Info)
                , LogToStdStreams minSev
                ]

    walletLogOutputs <- getLogOutputs walletMinSeverityFromEnv "wallet.log"
    testLogOutputs <- getLogOutputs testMinSeverityFromEnv "test.log"

    withLogging walletLogOutputs $ \(sb, (cfg, walTr)) -> do
        ekgEnabled >>= flip when (EKG.plugin cfg walTr sb >>= loadPlugin sb)
        withLogging testLogOutputs $ \(_, (_, testTr)) -> do
            let trTests = appendName "integration" testTr
            let tracers = setupTracers (tracerSeverities (Just Debug)) walTr
            action (trMessageText trTests, tracers)

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
