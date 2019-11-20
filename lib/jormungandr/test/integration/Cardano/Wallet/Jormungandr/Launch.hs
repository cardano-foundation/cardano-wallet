{-# LANGUAGE DataKinds #-}

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
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrConfig (..), JormungandrConnParams, withJormungandr )
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
            [ "--secret", testDataDir </> "secret.yaml"
            , "--config" , testDataDir </> "config.yaml"
            ]
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
