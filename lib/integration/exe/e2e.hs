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

import Cardano.Launcher.Mithril
    ( downloadLatestSnapshot
    , downloadMithril
    )
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
    , when
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
    , isNothing
    )
import Data.MaybeK
    ( MaybeK (..)
    )
import Main.Utf8
    ( withUtf8
    )
import Network.Socket
    ( PortNumber
    )
import Network.URI
    ( parseURI
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.Environment
    ( lookupEnv
    , setEnv
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
import Text.Read
    ( readMaybe
    )

-- ENV configuration -----------------------------------------------------------

data E2EConfig = E2EConfig
    { preprodMnemonics :: [SomeMnemonic]
    , alreadyRunningWallet :: Maybe PortNumber
    }

getConfig :: IO E2EConfig
getConfig = do
    preprodMnemonics <- getPreprodMnemonics
    alreadyRunningWallet <- (readMaybe =<<) <$> lookupEnv "HAL_E2E_ALREADY_RUNNING_WALLET_PORT"

    -- Needed for mithril-client
    setEnvIfMissing "GENESIS_VERIFICATION_KEY" "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
    setEnvIfMissing "AGGREGATOR_ENDPOINT" "https://aggregator.release-preprod.api.mithril.network/aggregator"

    pure $ E2EConfig {..}
  where
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

    setEnvIfMissing :: String -> String -> IO ()
    setEnvIfMissing var value = do
        isUnset <- isNothing <$> lookupEnv var
        when isUnset $ setEnv var value

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
configureContext (E2EConfig preprodMnemonics alreadyRunningWallet) action =
    case alreadyRunningWallet of
        Just port -> action =<< contextFromWalletPort port
        Nothing -> launchNodeAndWalletViaMithril
  where
    launchNodeAndWalletViaMithril :: IO ()
    launchNodeAndWalletViaMithril = withConfigDir $ \dir -> do
        putStrLn "~~~ Downloading latest mithril snapshot"
        downloadLatestSnapshot dir =<< downloadMithril dir
        let nodeSocket = if isWindows
                then "\\\\.\\pipe\\socket"
                else "node.socket"
        let nodeConfig =
                CardanoNodeConfig
                    { nodeDir = dir
                    , nodeConfigFile = dir </> "config.json"
                    , nodeTopologyFile = dir </> "topology.json"
                    , nodeDatabaseDir = dir </> "db"
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
                        , walletDatabaseDir = "wallet-db"
                        , walletNetwork = Launcher.Testnet "byron-genesis.json"
                        , executable = Nothing
                        , workingDir = Just dir
                        , extraArgs = []
                        }
            withCardanoWallet nullTracer node walletConfig
                $ \(CardanoWalletConn walletPort _)  -> do
                    action =<< contextFromWalletPort walletPort

    contextFromWalletPort :: PortNumber -> IO Context
    contextFromWalletPort walletPort = do
        manager <- httpManager
        let mUri = parseURI ("http://localhost:" <> show walletPort <> "/")
        let baseUri = case mUri of
              Just uri -> uri
              Nothing  -> error "Invalid URI"

        let ctx = Context
                { _manager = (baseUri, manager)
                , _walletPort =
                    Port $ fromIntegral walletPort
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
    setupAsBuildkiteSections PreprodSetupReady        = "+++ Running tests"

-- node configs ----------------------------------------------------------------

-- | Get access to a tmp directory containing node configs.
--
-- The configs are based on the configs in the repository, embedded at
-- compile-time, and tweaked for less verbose logs.
withConfigDir :: (FilePath -> IO a) -> IO a
withConfigDir action = liftIO $
  withSystemTempDirectory "e2e" $ \tmpDir -> do
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
