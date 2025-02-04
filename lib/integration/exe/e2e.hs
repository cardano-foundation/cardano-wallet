{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude

import qualified Cardano.Launcher.Wallet as Launcher
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Test.Integration.Scenario.Preprod as Preprod

import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , isWindows
    , withCardanoNode
    )
import Cardano.Launcher.Wallet
    ( CardanoWalletConfig (..)
    , CardanoWalletConn (CardanoWalletConn)
    , withCardanoWallet
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiEra (..)
    )
import Cardano.Wallet.Application.CLI
    ( Port (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (Testnet)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( contramap
    , nullTracer
    , stdoutTracer
    )
import Data.FileEmbed
    ( embedDir
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.MaybeK
    ( MaybeK (..)
    )
import Main.Utf8
    ( withUtf8
    )
import Network.URI
    ( parseURI
    )
import System.Directory
    ( createDirectoryIfMissing
    , getCurrentDirectory
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( takeDirectory
    , (</>)
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
    , aroundAll
    , hspec
    )
import Test.Integration.Framework.Context
    ( Context (..)
    )
import Test.Integration.Framework.DSL
    ( PreprodSetupLog (..)
    , setupPreprodWallets
    )
import Test.Integration.Framework.Setup
    ( httpManager
    )
import Test.Utils.Paths
    ( getTestDataPath
    )

-- ENV configuration -----------------------------------------------------------

data E2EConfig = E2EConfig
    { walletDbDir :: FilePath
    , nodeDir :: FilePath
    , nodeDbDir :: FilePath
    , preprodMnemonics :: [SomeMnemonic]
    }

getConfig :: IO E2EConfig
getConfig = do
    walletDbDir <- fromMaybe defaultWalletDbDir <$> lookupEnv "WALLET_DB_DIR"
    nodeDbDir <- fromMaybe defaultNodeDbDir <$> lookupEnv "NODE_DB_DIR"
    nodeDir <- fromMaybe defaultNodeDir <$> lookupEnv "NODE_DIR"
    preprodMnemonics <- getPreprodMnemonics
    pure $ E2EConfig {..}
  where
    -- Probably too fine grained control with all these settings, but works for now
    defaultNodeDbDir   = repoRoot </> "test" </> "e2e" </> "state" </> "node" </> "db"
    defaultNodeDir   = repoRoot </> "test" </> "e2e" </> "state" </> "node"
    defaultWalletDbDir = repoRoot </> "test" </> "e2e" </> "state" </> "wallet"

    repoRoot = ".." </> ".." -- works when run with 'cabal test'

    getPreprodMnemonics :: IO [SomeMnemonic]
    getPreprodMnemonics
        = map (SomeMnemonic . unsafeMkMnemonic @15 . T.words)
        . T.split isNewlineOrSemicolon
        . T.pack
        . fromMaybe (error errMsg)
        <$> lookupEnv envVarName
      where
        isNewlineOrSemicolon c = c == '\n' || c == ';'
        envVarName = "HAL_E2E_PREPROD_MNEMONICS"
        errMsg = unlines
            [ envVarName <> " is not set."
            , "Please set it to a '\\n' or ';'-separated list of ' '-separated mnemonics."
            , ""
            , "Example:"
            , "fish fish fish fish fish fish fish fish fish fish fish fish fish fish fish"
            , "buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo"
            ]

-- main ------------------------------------------------------------------------

main :: IO ()
main = withUtf8 $ do
    config <- getConfig
    hspec (spec' config)
  where
    spec' :: E2EConfig -> Spec
    spec' config =
        aroundAll (configureContext config)
            $ Preprod.spec @('Testnet 1)

-- setup -----------------------------------------------------------------------

configureContext :: E2EConfig -> (Context -> IO ()) -> IO ()
configureContext E2EConfig{walletDbDir,nodeDbDir,nodeDir,preprodMnemonics} action = do
    cwd <- getCurrentDirectory
    let nodeSocket = if isWindows
            then "\\\\.\\pipe\\socket"
            else cwd </> nodeDbDir </> "node.socket"
    withConfigDir $ \configDir -> do
        let nodeConfig =
                CardanoNodeConfig
                    { nodeDir = cwd </> nodeDir
                    , nodeConfigFile = cwd </> configDir </> "config.json"
                    , nodeTopologyFile = cwd </> configDir </> "topology.json"
                    , nodeDatabaseDir = cwd </> nodeDbDir
                    , nodeDlgCertFile = Nothing
                    , nodeSignKeyFile = Nothing
                    , nodeOpCertFile = Nothing
                    , nodeKesKeyFile = Nothing
                    , nodeVrfKeyFile = Nothing
                    , nodePort = Nothing
                    , nodeLoggingHostname = Nothing
                    , nodeExecutable = Nothing
                    , nodeOutputFile = Nothing
                    , nodeSocketPathFile = JustK nodeSocket
                    }

        withCardanoNode nullTracer nodeConfig $ \(JustK node) -> do
            let walletConfig =
                    CardanoWalletConfig
                        { walletPort = 8090
                        , walletDatabaseDir = walletDbDir
                        , walletNetwork = Launcher.Testnet $
                            configDir </> "byron-genesis.json"
                        , executable = Nothing
                        , workingDir = Nothing
                        , extraArgs = []
                        }
            withCardanoWallet nullTracer node walletConfig $ \wallet -> do
                action =<< contextFromNetwork wallet
  where
    contextFromNetwork :: CardanoWalletConn -> IO Context
    contextFromNetwork (CardanoWalletConn port _) = do
        manager <- httpManager
        let mUri = parseURI $ "http://localhost:" <> show port <> "/"
        let baseUri = case mUri of
              Just uri -> uri
              Nothing  -> error "Invalid URI"

        let ctx = Context
                { _manager = (baseUri, manager)
                , _walletPort =
                    Port $ fromIntegral port
                , _mainEra =
                    ApiConway
                , _faucet =
                    error "_faucet not implemented"
                , _networkParameters =
                    error "_networkParameters not implemented"
                , _testnetMagic =
                    error "_testnetMagic not implemented"
                , _poolGarbageCollectionEvents =
                    error "_poolGarbageCollectionEvents not implemented"
                , _smashUrl =
                    error "_smashUrl not implemented"
                , _mintSeaHorseAssets =
                    error "_mintSeaHorseAssets not implemented"
                , _preprodWallets =
                    []
                }
        ctx' <- setupPreprodWallets setupTr preprodMnemonics ctx
        return ctx'

    setupTr = contramap setupAsBuildkiteSections stdoutTracer

    setupAsBuildkiteSections :: PreprodSetupLog -> String
    setupAsBuildkiteSections WaitingForNodeConnection = "--- Waiting for node connection"
    setupAsBuildkiteSections CreatingWallets          = "--- Creating wallets"
    setupAsBuildkiteSections WaitingForWalletsToSync  = "--- Syncing wallets"
    setupAsBuildkiteSections PreprodSetupReady        = "--- Running tests"

-- node configs ----------------------------------------------------------------

-- | Get access to a tmp directory containing node configs.
--
-- The configs are based on the configs in the repository, embedded at
-- compile-time, and tweaked for less verbose logs.
withConfigDir :: (FilePath -> IO a) -> IO a
withConfigDir action = liftIO $
  withSystemTempDirectory "e2e-node-configs" $ \tmpDir -> do
    -- Write the embedded config dir to the temporary dir
    forM_ embeddedConfigs $ \(relPath, content) -> do
      let dest = tmpDir </> relPath
      createDirectoryIfMissing True (takeDirectory dest)
      BS.writeFile dest content

    -- Customize node config; we don't need this verbose logs
    adjustKeysToFalse (tmpDir </> "config.json")
        [ "TraceConnectionManager"
        , "TraceDNSResolver"
        , "TraceDNSSubscription"
        , "TraceHandshake"
        , "TraceInboundGovernor"
        , "TraceLocalInboundGovernor"
        , "TraceLedgerPeers"
        , "TraceLocalConnectionManager"
        , "TraceLocalRootPeers"
        , "TraceMempool"
        , "TraceMempoolSynced"
        , "TracePeerSelection"
        , "TracePeerSelectionActions"
        , "TracePeerSelectionCounters"
        , "TracePublicRootPeers"
        ]
    action tmpDir
  where
    adjustKeysToFalse :: FilePath -> [KeyMap.Key] -> IO ()
    adjustKeysToFalse filePath keys = do
        content <- BL.readFile filePath
        case Aeson.decode content :: Maybe Aeson.Value of
          Just (Aeson.Object o) -> do
            let o' = foldr (\key acc -> KeyMap.insert key (Aeson.Bool False) acc) o keys
            BL.writeFile filePath (Aeson.encode (Aeson.Object o'))
          _ -> error "config.json is not a JSON object or failed to parse."

-- | Embed all files in the config directory at compile time.
--   This gives a list of pairs (relative filepath, file contents).
embeddedConfigs :: [(FilePath, BS.ByteString)]
embeddedConfigs = $(embedDir
    $(getTestDataPath ("configs" </> "cardano" </> "preprod")))
