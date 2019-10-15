{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch the jormungandr node backend for integration
-- tests.

module Cardano.Wallet.Jormungandr.Launch
    ( setupConfig
    , teardownConfig
    , withBackendOnly
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Wallet.Jormungandr.Compatibility
    ( genConfigFile, localhostBaseUrl )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrConfig (..), JormungandrConnParams, withJormungandr )
import Cardano.Wallet.Network.Ports
    ( getRandomPort )
import Control.Exception
    ( bracket, throwIO )
import System.Directory
    ( doesDirectoryExist, removeDirectoryRecursive )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hClose, openFile )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )

import qualified Data.Yaml as Yaml

-- | Starts jormungandr on a random port using the integration tests config.
-- The data directory will be stored in a unique location under the system
-- temporary directory.
setupConfig :: IO (JormungandrConfig, StdStream, FilePath)
setupConfig = do
    tmp <- getCanonicalTemporaryDirectory
    configDir <- createTempDirectory tmp "cardano-wallet-jormungandr"
    logFile <- openFile (configDir </> "jormungandr.log") WriteMode
    apiPort <- getRandomPort
    p2pPort <- getRandomPort
    let configFile = configDir </> "config.yaml"
    let baseUrl = localhostBaseUrl $ fromIntegral apiPort
    Yaml.encodeFile configFile $ genConfigFile configDir p2pPort baseUrl
    let output = UseHandle logFile
    let args =
            [ "--config", configFile
            , "--secret", "test/data/jormungandr/secret.yaml"
            , "--genesis-block", "test/data/jormungandr/block0.bin"
            ]
    pure (JormungandrConfig args output, output, configDir)

teardownConfig :: (JormungandrConfig, StdStream, FilePath) -> IO ()
teardownConfig (_, output, d) = do
    case output of
        UseHandle h -> hClose h
        _ -> pure ()
    override <- maybe False (not . null) <$> lookupEnv "NO_CLEANUP"
    exists <- doesDirectoryExist d
    case (override, exists) of
        (True, _) -> putStrLn $ "Not cleaning up temporary directory " ++ d
        (_, True) -> removeDirectoryRecursive d
        _ -> pure ()

-- | Launches jörmungandr, but not the wallet.
withBackendOnly :: (JormungandrConnParams -> IO a) -> IO a
withBackendOnly cb =
    bracket setupConfig teardownConfig $ \(jmConfig, _, _) ->
        (withJormungandr nullTracer jmConfig cb >>= either throwIO pure)
