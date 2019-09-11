{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch the jormungandr node backend for integration
-- tests.

module Cardano.Wallet.Jormungandr.Launch
    ( launchJormungandr
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), Scheme (..) )
import Control.Concurrent.Async
    ( Async, async )
import Control.Monad
    ( void )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( FilePath, (</>) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import Test.Utils.Ports
    ( randomUnusedTCPPorts )

import qualified Cardano.Wallet.Jormungandr.Compatibility as Jormungandr
import qualified Data.ByteString.Char8 as B8
import qualified Data.Yaml as Yaml

-- | Starts jormungandr on a random port using the integration tests config.
-- The data directory will be stored in a unique location under the system
-- temporary directory.
launchJormungandr :: StdStream -> IO (Async (), BaseUrl, Port "node")
launchJormungandr output = do
    (baseUrl, port, configYaml) <- setupConfig
    let dir = "test/data/jormungandr"
    handle <- async $ void $ launch
        [ Command "jormungandr"
            [ "--genesis-block", dir ++ "/block0.bin"
            , "--config", configYaml
            , "--secret", dir ++ "/secret.yaml"
            ] (return ())
            output
        ]
    pure (handle, baseUrl, port)

setupConfig :: IO (BaseUrl, Port "node", FilePath)
setupConfig = do
    [apiPort, nodePort] <- randomUnusedTCPPorts 2
    tmp <- getCanonicalTemporaryDirectory
    configDir <- createTempDirectory tmp "cardano-wallet-jormungandr"
    let storageDir = configDir </> "storage"
        configYaml = configDir </> "config.yaml"
        url = BaseUrl Http "127.0.0.1" apiPort "/api"
        jormConfig = Jormungandr.genConfigFile storageDir nodePort url
    B8.writeFile configYaml (Yaml.encode jormConfig)
    createDirectory storageDir
    pure (url, Port apiPort, configYaml)
