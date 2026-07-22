{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Integration.Framework.Setup
    ( TestingCtx (..)
    , legacyV1ApiWalletId
    , legacyV1CliWalletId
    , legacyV1SharedWalletId
    , legacyV1Passphrase
    , withContext
    , withTestsSetup
    ) where

import Cardano.Address
    ( NetworkTag (..)
    )
import Cardano.Address.Script
    ( Cosigner (..)
    , Script (RequireSignatureOf)
    , ScriptTemplate (..)
    )
import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.BM.Data.Tracer
    ( nullTracer
    )
import Cardano.BM.Extra
    ( bracketTracer
    , stdoutTextTracer
    )
import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    )
import Cardano.Crypto.Wallet
    ( XPrv
    )
import Cardano.Faucet.Mnemonics
    ( unsafeMnemonic
    )
import Cardano.Launcher
    ( ProcessHasExited (..)
    )
import Cardano.Ledger.Shelley.Genesis
    ( sgNetworkMagic
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Startup
    ( installSignalHandlersNoLogging
    , setDefaultFilePermissions
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK)
    , HardDerivation (deriveAccountPrivateKey)
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery
    ( ChangeAddressMode (IncreasingChangeAddresses)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    , defaultAddressPoolGap
    , purposeCIP1852
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState
    )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Address.Keys.Shared
    ( mkSharedStateFromRootXPrv
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( digest
    , getRawKey
    , publicKey
    )
import Cardano.Wallet.Api.Types
    ( ApiPoolSpecifier (..)
    , ApiT (..)
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra (..)
    )
import Cardano.Wallet.Application
    ( Tracers
    , serveWallet
    )
import Cardano.Wallet.Application.CLI
    ( Port (..)
    , getEKGURL
    , getPrometheusURL
    )
import Cardano.Wallet.Application.Server
    ( walletListenFromEnv
    )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , DBLayerParams (..)
    )
import Cardano.Wallet.DB.Layer
    ( withBootDBLayerFromFile
    )
import Cardano.Wallet.Faucet
    ( FaucetM
    , runFaucetM
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (SharedKeyS, ShelleyKeyS)
    , WalletFlavorS (SharedWallet, ShelleyWallet)
    )
import Cardano.Wallet.Launch.Cluster
    ( ClusterEra (..)
    , FaucetFunds (..)
    , RunningNode (..)
    , withFaucet
    , withSMASH
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( TestnetMagic
    , testnetMagicToNatural
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , absolutize
    , toFilePath
    )
import Cardano.Wallet.Launch.Cluster.Process
    ( RunFaucetQ
    , RunMonitorQ
    , WalletPresence (..)
    , defaultEnvVars
    , waitForRunningNode
    , withLocalCluster
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Network.Ports
    ( portFromURL
    )
import Cardano.Wallet.Network.RestorationMode
    ( RestorationPoint (RestorationPointAtGenesis)
    )
import Cardano.Wallet.Pools
    ( StakePool
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromGenesisData
    )
import Cardano.Wallet.Primitive.Model
    ( initWallet
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (Testnet)
    , NetworkId (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( encryptPassphrase
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseScheme (EncryptWithPBKDF2)
    , WalletPassphraseInfo (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( hoistTimeInterpreter
    , mkSingleEraInterpreter
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block
    , GenesisParameters (getGenesisBlockDate)
    , NetworkParameters (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials (HashedCredentialsV1)
    , RootCredentials (..)
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.TokenMetadata.MockServer
    ( queryServerStatic
    , withMetadataServer
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Lens
    ( view
    )
import Control.Monad
    ( forM_
    , void
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , traceWith
    )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Text.Encoding
    ( encodeUtf8
    )
import Data.Time.Clock
    ( getCurrentTime
    )
import Main.Utf8
    ( withUtf8
    )
import Network.HTTP.Client
    ( Manager
    , defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.URI
    ( URI
    )
import Servant.Client
    ( ClientEnv
    )
import System.Directory
    ( createDirectory
    )
import System.Environment
    ( lookupEnv
    , setEnv
    )
import System.Environment.Extended
    ( envFromText
    , isEnvSet
    )
import System.Exit
    ( ExitCode (..)
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import System.Path
    ( absRel
    , relDir
    , relFile
    , (</>)
    )
import Test.Integration.Framework.Context
    ( Context (..)
    , PoolGarbageCollectionEvent (..)
    )
import Test.Integration.Framework.DSL
    ( Payload (..)
    , arbitraryStake
    , fixturePassphrase
    , joinStakePool
    , runResourceT
    , unsafeRequest
    , walletFromMnemonic
    )
import Test.Integration.Framework.LocalCluster.SendFaucetAssets
    ( sendFaucetAssets
    )
import Test.Integration.Framework.Logging
    ( TestsLog (..)
    , withTracers
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Exception
    ( throwIO
    , withException
    )
import UnliftIO.MVar
    ( MVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    , withMVar
    )
import Prelude

import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Layer as Pool
import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Address.Derivation.Shared as Shared
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Application.CLI as CLI
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Data.ByteArray as BA
import qualified Data.Functor.Identity as Identity
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.FilePath as FilePath

-- | Persisted V1 root keys, prepared before the wallet server starts.
--
-- Separate Shelley API, Shelley CLI, and Shared API fixtures mean all
-- migratable wallet kinds exercise the migration independently.
legacyV1ApiWalletId :: WalletId
legacyV1ApiWalletId = legacyV1WalletId legacyV1ApiMnemonic

legacyV1Passphrase :: Text
legacyV1Passphrase = "legacy-root-key-passphrase"

legacyV1ApiMnemonic :: [Text]
legacyV1ApiMnemonic =
    [ "pulp"
    , "ten"
    , "light"
    , "rhythm"
    , "replace"
    , "vessel"
    , "slow"
    , "drift"
    , "kingdom"
    , "amazing"
    , "negative"
    , "join"
    , "auction"
    , "ugly"
    , "symptom"
    ]

legacyV1CliMnemonic :: [Text]
legacyV1CliMnemonic =
    [ "vintage"
    , "poem"
    , "topic"
    , "machine"
    , "hazard"
    , "cement"
    , "dune"
    , "glimpse"
    , "fix"
    , "brief"
    , "account"
    , "badge"
    , "mass"
    , "silly"
    , "business"
    ]

legacyV1CliWalletId :: WalletId
legacyV1CliWalletId = legacyV1WalletId legacyV1CliMnemonic

legacyV1SharedWalletId :: WalletId
legacyV1SharedWalletId =
    WalletId
        $ digest SharedKeyS
        $ publicKey SharedKeyS legacyV1SharedRootKey

legacyV1WalletId :: [Text] -> WalletId
legacyV1WalletId mnemonic =
    WalletId
        $ digest ShelleyKeyS
        $ publicKey ShelleyKeyS
        $ legacyV1RootKey mnemonic

legacyV1RootKey :: [Text] -> ShelleyKey 'RootK XPrv
legacyV1RootKey mnemonic =
    Shelley.generateKeyFromSeed
        ( SomeMnemonic $ unsafeMnemonic @15 mnemonic
        , Just legacyV1SecondFactor
        )
        (preparePassphrase EncryptWithPBKDF2 legacyV1Passphrase')

-- | Keep the persisted fixtures' IDs distinct from the fixed mnemonics used
-- by the regular integration scenarios.
legacyV1SecondFactor :: SomeMnemonic
legacyV1SecondFactor =
    SomeMnemonic $ unsafeMnemonic @15 legacyV1ApiMnemonic

legacyV1SharedRootKey :: SharedKey 'RootK XPrv
legacyV1SharedRootKey =
    Shared.generateKeyFromSeed
        ( SomeMnemonic $ unsafeMnemonic @15 legacyV1CliMnemonic
        , Just legacyV1SecondFactor
        )
        (preparePassphrase EncryptWithPBKDF2 legacyV1Passphrase')

legacyV1Passphrase' :: Passphrase "user"
legacyV1Passphrase' =
    Passphrase $ BA.convert $ encodeUtf8 legacyV1Passphrase

seedLegacyV1Wallets
    :: FilePath
    -> Block
    -> NetworkParameters
    -> IO ()
seedLegacyV1Wallets dbDir block0 networkParameters = do
    forM_ [legacyV1ApiMnemonic, legacyV1CliMnemonic] $ \mnemonic -> do
        now <- getCurrentTime
        (_, passphraseHash) <- encryptPassphrase legacyV1Passphrase'
        let wid = legacyV1WalletId mnemonic
        let rootKey = legacyV1RootKey mnemonic
        let state :: SeqState ('Testnet 42) ShelleyKey
            state =
                mkSeqStateFromRootXPrv
                    ShelleyKeyS
                    ( RootCredentials
                        rootKey
                        (preparePassphrase EncryptWithPBKDF2 legacyV1Passphrase')
                    )
                    purposeCIP1852
                    defaultAddressPoolGap
                    IncreasingChangeAddresses
        let params =
                DBLayerParams
                    (snd $ initWallet block0 state)
                    RestorationPointAtGenesis
                    WalletMetadata
                        { name = WalletName "Legacy V1 migration fixture"
                        , creationTime = now
                        , passphraseInfo =
                            Just
                                $ WalletPassphraseInfo
                                    { lastUpdatedAt = now
                                    , passphraseScheme = EncryptWithPBKDF2
                                    }
                        }
                    mempty
                    (genesisParameters networkParameters)
        let dbFile =
                dbDir
                    FilePath.</> ("she." <> T.unpack (toText wid) <> ".sqlite")
        let timeInterpreter =
                hoistTimeInterpreter (pure . Identity.runIdentity)
                    $ mkSingleEraInterpreter
                        (getGenesisBlockDate $ genesisParameters networkParameters)
                        (slottingParameters networkParameters)
        withBootDBLayerFromFile
            ShelleyWallet
            nullTracer
            timeInterpreter
            wid
            Nothing
            params
            dbFile
            $ \DBLayer{atomically, walletState} ->
                atomically
                    $ Wallet.putPrivateKey
                        walletState
                        (HashedCredentialsV1 rootKey passphraseHash)

    now <- getCurrentTime
    (_, passphraseHash) <- encryptPassphrase legacyV1Passphrase'
    let rootKey = legacyV1SharedRootKey
    let accountXPub =
            publicKey SharedKeyS
                $ deriveAccountPrivateKey
                    (preparePassphrase EncryptWithPBKDF2 legacyV1Passphrase')
                    rootKey
                    minBound
    let state :: SharedState ('Testnet 42) SharedKey
        state =
            mkSharedStateFromRootXPrv
                SharedKeyS
                ( RootCredentials
                    rootKey
                    (preparePassphrase EncryptWithPBKDF2 legacyV1Passphrase')
                )
                minBound
                IncreasingChangeAddresses
                defaultAddressPoolGap
                ( ScriptTemplate
                    (Map.singleton (Cosigner 0) $ getRawKey SharedKeyS accountXPub)
                    (RequireSignatureOf $ Cosigner 0)
                )
                Nothing
    let params =
            DBLayerParams
                (snd $ initWallet block0 state)
                RestorationPointAtGenesis
                WalletMetadata
                    { name = WalletName "Legacy V1 shared migration fixture"
                    , creationTime = now
                    , passphraseInfo =
                        Just
                            $ WalletPassphraseInfo
                                { lastUpdatedAt = now
                                , passphraseScheme = EncryptWithPBKDF2
                                }
                    }
                mempty
                (genesisParameters networkParameters)
    let dbFile =
            dbDir
                FilePath.</> ("sha." <> T.unpack (toText legacyV1SharedWalletId) <> ".sqlite")
    let timeInterpreter =
            hoistTimeInterpreter (pure . Identity.runIdentity)
                $ mkSingleEraInterpreter
                    (getGenesisBlockDate $ genesisParameters networkParameters)
                    (slottingParameters networkParameters)
    withBootDBLayerFromFile
        SharedWallet
        nullTracer
        timeInterpreter
        legacyV1SharedWalletId
        Nothing
        params
        dbFile
        $ \DBLayer{atomically, walletState} ->
            atomically
                $ Wallet.putPrivateKey
                    walletState
                    (HashedCredentialsV1 rootKey passphraseHash)

-- | Do all the program setup required for integration tests, create a temporary
-- directory, and pass this info to the main hspec action.
withTestsSetup
    :: (DirOf "cluster" -> (Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTestsSetup action = do
    -- Handle SIGTERM properly
    installSignalHandlersNoLogging
    -- Stop cardano-cli complaining about file permissions
    setDefaultFilePermissions
    -- Enables small test-specific workarounds, like timing out faster if wallet
    -- deletion fails.
    setEnv "CARDANO_WALLET_TEST_INTEGRATION" "1"

    skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
    -- Flush test output as soon as a line is printed.
    -- Set UTF-8, regardless of user locale.
    withUtf8
        $ do
            mEnv <- lookupEnv "INTEGRATION_TEST_DIR"
            -- This temporary directory will contain logs, and all other data
            -- produced by the integration tests.
            let run fp = do
                    fpa <- absolutize $ absRel fp
                    let testDir = DirOf fpa
                    withTracers testDir $ action testDir
            case mEnv of
                Just env -> run env
                Nothing ->
                    withSystemTempDir
                        stdoutTextTracer
                        "test"
                        skipCleanup
                        run

mkFaucetFunds :: TestnetMagic -> FaucetM FaucetFunds
mkFaucetFunds testnetMagic = do
    let networkTag =
            NetworkTag . fromIntegral $ testnetMagicToNatural testnetMagic
    shelleyFunds <- Faucet.shelleyFunds shelleyTestnet
    byronFunds <- Faucet.byronFunds networkTag
    icarusFunds <- Faucet.icarusFunds networkTag
    onlyDustWallet <- Faucet.onlyDustWallet shelleyTestnet
    bigDustWallet <- Faucet.bigDustWallet shelleyTestnet
    preregKeyWallet <- Faucet.preregKeyWallet shelleyTestnet
    rewardWalletFunds <- Faucet.rewardWalletFunds shelleyTestnet
    massiveWallet <- Faucet.massiveWalletFunds (Coin 0) 0 shelleyTestnet
    maryAllegraFunds <-
        Faucet.maryAllegraFunds
            (Coin 10__000_000)
            shelleyTestnet
    pure
        FaucetFunds
            { pureAdaFunds =
                mconcat
                    [ shelleyFunds
                    , byronFunds
                    , Faucet.byronIntegrationTestFunds networkTag
                    , Faucet.hwLedgerFunds networkTag
                    , icarusFunds
                    , onlyDustWallet
                    , bigDustWallet
                    , preregKeyWallet
                    , rewardWalletFunds
                    ]
            , maryAllegraFunds
            , massiveWalletFunds = massiveWallet
            }

data TestingCtx = TestingCtx
    { testnetMagic :: TestnetMagic
    , testDir :: DirOf "cluster"
    , tr :: Tracer IO TestsLog
    , tracers :: Tracers IO
    , localClusterEra :: ClusterEra
    , testDataDir :: DirOf "test-data"
    }

-- A decorator for the pool database that records all calls to the
-- 'removeRetiredPools' operation.
--
-- The parameters and return value of each call are recorded by appending
-- a 'PoolGarbageCollectionEvent' value to the start of the given log.
--

recordPoolGarbageCollectionEvents
    :: TestingCtx
    -> IORef [PoolGarbageCollectionEvent]
    -> Pool.DBDecorator m
recordPoolGarbageCollectionEvents TestingCtx{..} eventsRef =
    Pool.DBDecorator decorate
  where
    decorate Pool.DBLayer{..} =
        Pool.DBLayer{removeRetiredPools = removeRetiredPoolsDecorated, ..}
      where
        removeRetiredPoolsDecorated epochNo = do
            certificates <- removeRetiredPools epochNo
            let event = PoolGarbageCollectionEvent epochNo certificates
            liftIO $ do
                traceWith tr $ MsgPoolGarbageCollectionEvent event
                atomicModifyIORef' eventsRef ((,()) . (event :))
            pure certificates

withServer
    :: TestingCtx
    -> FaucetFunds
    -> Pool.DBDecorator IO
    -> ContT () IO (T.Text, NetworkParameters, URI, RunFaucetQ IO)
withServer
    ctx@TestingCtx{..}
    faucetFunds
    dbDecorator = do
        _ <- ContT $ \k -> bracketTracer' tr "withServer" $ k ()
        ((runMonitorQ, runFaucetQ), ToTextTracer clog) <-
            withLocalCluster
                "main-integration-tests"
                NoWallet
                defaultEnvVars
                faucetFunds
        smashUrl <- ContT $ withSMASH clog (toFilePath . absDirOf $ testDir)
        (np, uri) <-
            onClusterStart
                ctx
                dbDecorator
                runMonitorQ
        pure (T.pack smashUrl, np, uri, runFaucetQ)

onClusterStart
    :: TestingCtx
    -> Pool.DBDecorator IO
    -> RunMonitorQ IO
    -> ContT () IO (NetworkParameters, URI)
onClusterStart
    TestingCtx{..}
    dbDecorator
    runMonitorQ =
        do
            RunningNode nodeConnection genesisData vData <-
                liftIO $ waitForRunningNode runMonitorQ

            let (networkParameters, block0, genesisPools) =
                    fromGenesisData genesisData
            let db = absDirOf testDir </> relDir "wallets"
            liftIO $ createDirectory $ toFilePath db
            liftIO
                $ seedLegacyV1Wallets
                    (toFilePath db)
                    block0
                    networkParameters
            listen <- liftIO $ walletListenFromEnv envFromText
            let testMetadata = absDirOf testDataDir </> relFile "token-metadata.json"
            tokenMetaUrl <-
                ContT
                    $ withMetadataServer (queryServerStatic $ toFilePath testMetadata)
            ContT $ \k -> do
                end <-
                    serveWallet
                        (NodeSource nodeConnection vData (SyncTolerance 10))
                        networkParameters
                        tunedForMainnetPipeliningStrategy
                        (NTestnet (fromIntegral (sgNetworkMagic genesisData)))
                        genesisPools
                        tracers
                        (Just $ toFilePath db)
                        (Just dbDecorator)
                        "127.0.0.1"
                        listen
                        Nothing
                        Nothing
                        Nothing
                        (Just tokenMetaUrl)
                        block0
                        (\uri -> k (networkParameters, uri))
                        `withException` (traceWith tr . MsgServerError)
                case end of
                    ExitSuccess -> pure ()
                    failure -> throwIO $ ProcessHasExited "integration" failure

-- threadDelay $ 3 * 60 * 1_000_000 -- Wait 3 minutes for the node to start
-- exitSuccess

-- | Convert @ClusterEra@ to a @ApiEra@.
clusterToApiEra :: ClusterEra -> ApiEra
clusterToApiEra = \case
    ConwayHardFork -> ApiConway

httpManager :: IO Manager
httpManager = do
    let fiveMinutes = 300 * 1_000 * 1_000 -- 5 min in microseconds
    newManager
        $ defaultManagerSettings
            { managerResponseTimeout =
                responseTimeoutMicro fiveMinutes
            }

setupContext
    :: TestingCtx
    -> RunFaucetQ IO
    -> MVar Context
    -> ClientEnv
    -> IORef [PoolGarbageCollectionEvent]
    -> T.Text
    -> NetworkParameters
    -> URI
    -> IO ()
setupContext
    TestingCtx{..}
    runFaucetQ
    ctx
    faucetClientEnv
    poolGarbageCollectionEvents
    smashUrl
    networkParameters
    baseUrl =
        bracketTracer' tr "setupContext" $ do
            faucet <- Faucet.initFaucet faucetClientEnv
            prometheusUrl <-
                let packPort (h, p) =
                        T.pack h <> ":" <> toText @(Port "Prometheus") p
                in  maybe "none" packPort <$> getPrometheusURL
            ekgUrl <-
                let packPort (h, p) =
                        T.pack h <> ":" <> toText @(Port "EKG") p
                in  maybe "none" packPort <$> getEKGURL
            traceWith tr $ MsgBaseUrl baseUrl ekgUrl prometheusUrl smashUrl
            manager <- httpManager
            mintSeaHorseAssetsLock <- newMVar ()
            putMVar
                ctx
                Context
                    { _manager = (baseUrl, manager)
                    , _walletPort = CLI.Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _networkParameters = networkParameters
                    , _testnetMagic = testnetMagic
                    , _poolGarbageCollectionEvents = poolGarbageCollectionEvents
                    , _mainEra = clusterToApiEra localClusterEra
                    , _smashUrl = smashUrl
                    , _mintSeaHorseAssets = \nPerAddr batchSize c addrs ->
                        withMVar mintSeaHorseAssetsLock $ \() ->
                            sendFaucetAssets
                                runFaucetQ
                                batchSize
                                (nPerAddr, c, addrs)
                    , _preprodWallets = []
                    }

withContext :: TestingCtx -> (Context -> IO ()) -> IO ()
withContext testingCtx@TestingCtx{..} action = do
    bracketTracer' tr "withContext" $ withFaucet $ \faucetClientEnv -> do
        ctx <- newEmptyMVar
        poolGarbageCollectionEvents <- newIORef []
        faucetFunds <- runFaucetM faucetClientEnv $ mkFaucetFunds testnetMagic
        let dbEventRecorder =
                recordPoolGarbageCollectionEvents
                    testingCtx
                    poolGarbageCollectionEvents
            delegation = do
                x <- takeMVar ctx
                bracketTracer' tr "spec" $ do
                    setupDelegation faucetClientEnv x
                    action x
            wallet = evalContT $ do
                (smashUrl, networkParams, walletURI, runFaucetQ) <-
                    withServer
                        testingCtx
                        faucetFunds
                        dbEventRecorder
                liftIO
                    $ setupContext
                        testingCtx
                        runFaucetQ
                        ctx
                        faucetClientEnv
                        poolGarbageCollectionEvents
                        smashUrl
                        networkParams
                        walletURI
        void $ race wallet delegation
  where
    -- \| Setup delegation for 'rewardWallet' / 'rewardWalletMnemonics'.
    --
    -- Rewards take 4-5 epochs (here ~2 min) to accrue from delegating. By
    -- doing this up-front, the rewards are likely available by the time
    -- 'rewardWallet' is called, and we save time.
    setupDelegation :: ClientEnv -> Context -> IO ()
    setupDelegation faucetClientEnv ctx = do
        mnemonics <- runFaucetM faucetClientEnv Faucet.rewardWalletMnemonics
        pool : _ : _ <-
            map (view #id . getApiT) . snd
                <$> unsafeRequest @[ApiT StakePool]
                    ctx
                    (Link.listStakePools arbitraryStake)
                    Empty

        -- We only delete the wallets together at the end of the loop by
        -- using this /outer/ 'runResourceT'. While this may be taxing on
        -- resources, it enables tx submission to ensure all txs make it into
        -- the chain.
        runResourceT $ do
            forM_ mnemonics $ \mw -> do
                w <- walletFromMnemonic ctx mw
                (httpStatus, res) <-
                    joinStakePool
                        @('Testnet 0) -- protocol magic doesn't matter
                        ctx
                        (SpecificPool pool)
                        (w, fixturePassphrase)
                liftIO $ case res of
                    Left err ->
                        traceWith tr
                            $ MsgRewardWalletDelegationFailed
                            $ T.pack
                            $ show (httpStatus, err)
                    Right _ -> return ()

            -- Extra time to ensure the final txs make it into the chain
            let second = 1_000_000
            liftIO $ threadDelay $ 10 * second

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer $ contramap (MsgBracket name) tr
