{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude

import qualified Cardano.Launcher.Wallet as Launcher
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
import Control.Tracer
    ( contramap
    , nullTracer
    , stdoutTracer
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
    ( getCurrentDirectory
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( (</>)
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

-- ENV configuration -----------------------------------------------------------

data E2EConfig = E2EConfig
    { walletDbDir :: FilePath
    , nodeDir :: FilePath
    , nodeDbDir :: FilePath
    , configDir :: FilePath
    , preprodMnemonics :: [SomeMnemonic]
    }

getConfig :: IO E2EConfig
getConfig = do
    walletDbDir <- fromMaybe defaultWalletDbDir <$> lookupEnv "WALLET_DB_DIR"
    nodeDbDir <- fromMaybe defaultNodeDbDir <$> lookupEnv "NODE_DB_DIR"
    nodeDir <- fromMaybe defaultNodeDir <$> lookupEnv "NODE_DIR"
    configDir <- fromMaybe defaultConfigDir <$> lookupEnv "E2E_CONFIG_DIR"
    preprodMnemonics <- getPreprodMnemonics
    pure $ E2EConfig {..}
  where
    -- Probably too fine grained control with all these settings, but works for now
    defaultNodeDbDir   = repoRoot </> "test" </> "e2e" </> "state" </> "node" </> "db"
    defaultNodeDir   = repoRoot </> "test" </> "e2e" </> "state" </> "node"
    defaultWalletDbDir = repoRoot </> "test" </> "e2e" </> "state" </> "wallet"
    defaultConfigDir   = repoRoot </> "configs" </> "cardano" </> "preprod"

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
configureContext E2EConfig{walletDbDir,nodeDbDir,nodeDir,configDir,preprodMnemonics} action = do
    cwd <- getCurrentDirectory
    let nodeSocket = if isWindows
            then "\\\\.\\pipe\\socket"
            else cwd </> nodeDbDir </> "node.socket"
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
