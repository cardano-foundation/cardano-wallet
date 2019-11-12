{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch the jormungandr node backend for integration
-- tests.

module Cardano.Wallet.Jormungandr.Launch
    ( withConfig
    , withBackendOnly
    , testDataDir
    , spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrConfig (..), JormungandrConnParams, withJormungandr )
import Cardano.Wallet.Network.Ports
    ( PortNumber, getRandomPort )
import Control.Exception
    ( bracket, throwIO )
import Data.Aeson
    ( Value (..), object, (.=) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Function
    ( (&) )
import System.Directory
    ( doesDirectoryExist, removeDirectoryRecursive )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( FilePath, (</>) )
import System.IO
    ( IOMode (..), hClose, openFile )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

testDataDir :: FilePath
testDataDir = "test" </> "data" </> "jormungandr"

-- | Starts jormungandr on a random port using the integration tests config.
-- The data directory will be stored in a unique location under the system
-- temporary directory.
withConfig :: (JormungandrConfig -> IO a) -> IO a
withConfig = bracket setupConfig teardownConfig

setupConfig :: IO JormungandrConfig
setupConfig = do
    tmp <- getCanonicalTemporaryDirectory
    configDir <- createTempDirectory tmp "cardano-wallet-jormungandr"
    logFile <- openFile (configDir </> "jormungandr.log") WriteMode
    let cfg = JormungandrConfig
            configDir
            (Right $ testDataDir </> "block0.bin")
            Nothing
            (UseHandle logFile)
            ["--secret", testDataDir </> "secret.yaml"]
    genConfigYaml cfg
    pure cfg

teardownConfig :: JormungandrConfig -> IO ()
teardownConfig (JormungandrConfig d _ _ output _) = do
    case output of
        UseHandle h -> hClose h
        _ -> pure ()
    override <- maybe False (not . null) <$> lookupEnv "NO_CLEANUP"
    exists <- doesDirectoryExist d
    case (override, exists) of
        (True, _) -> putStrLn $ "Not cleaning up temporary directory " ++ d
        (_, True) -> removeDirectoryRecursive d
        _ -> pure ()

-- | Launches jörmungandr with a test config, but not the wallet.
withBackendOnly :: (JormungandrConnParams -> IO a) -> IO a
withBackendOnly cb = withConfig $ \jmConfig -> do
    withJormungandr nullTracer jmConfig cb >>= either throwIO pure

{-------------------------------------------------------------------------------
                           Generate YAML config file
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "genConfigFile integration tests helper" $ do
    it "example configuration" $ do
        let stateDir = "/state-dir"
        genConfigFile stateDir 8081 `shouldBe` [aesonQQ|{
            "storage": "/state-dir/chain",
            "p2p": {
                "trusted_peers": [],
                "topics_of_interest": {
                    "messages": "low",
                    "blocks": "normal"
                },
                "public_address" : "/ip4/127.0.0.1/tcp/8081"
            }
        }|]

genConfigYaml :: JormungandrConfig -> IO ()
genConfigYaml (JormungandrConfig stateDir _ _ _ _) = do
    p2pPort <- getRandomPort
    genConfigFile stateDir p2pPort
        & Yaml.encodeFile nodeConfigFile
  where
    nodeConfigFile = stateDir </> "jormungandr-config.yaml"

-- | Generate a configuration file for Jörmungandr@0.3.999
genConfigFile
    :: FilePath
    -> PortNumber
    -> Aeson.Value
genConfigFile stateDir addressPort = object
    [ "storage" .= (stateDir </> "chain")
    , "p2p" .= object
        [ "trusted_peers" .= ([] :: [()])
        , "topics_of_interest" .= object
            [ "messages" .= String "low"
            , "blocks" .= String "normal"
            ]
        , "public_address" .= String publicAddress
        ]
    ]
  where
    publicAddress = T.pack $ mconcat ["/ip4/127.0.0.1/tcp/", show addressPort]
