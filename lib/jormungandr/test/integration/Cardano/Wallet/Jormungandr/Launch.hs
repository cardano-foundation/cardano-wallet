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

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), Scheme (..) )
import Control.Concurrent.Async
    ( Async, async )
import Control.Monad
    ( void )
import Data.Aeson
    ( ToJSON (..), Value (..), object, (.=) )
import Data.Yaml
    ( encodeFile )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( FilePath, (</>) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import Test.Utils.Ports
    ( randomUnusedTCPPorts )

-- | Starts jormungandr on a random port using the integration tests config.
-- The data directory will be stored in a unique location under the system
-- temporary directory.
launchJormungandr :: StdStream -> IO (Async (), BaseUrl)
launchJormungandr output = do
    (baseUrl, configYaml) <- setupConfig
    let dir = "test/data/jormungandr"
    handle <- async $ void $ launch
        [ Command "jormungandr"
            [ "--genesis-block", dir ++ "/block0.bin"
            , "--config", configYaml
            , "--secret", dir ++ "/secret.yaml"
            ] (return ())
            output
        ]
    pure (handle, baseUrl)

-- | Config for the test jormungandr.
data JormConfig = JormConfig
    { jmStorageDir :: FilePath
    , jmRestApiPort :: Int
    , jmNodeApiPort :: Int
    } deriving (Show, Eq)

instance ToJSON JormConfig where
    toJSON cfg = object
        [ "storage" .= jmStorageDir cfg
        , "rest" .= object
            [ "listen" .= ("127.0.0.1:" ++ show (jmRestApiPort cfg))
            , "prefix" .= String "api"
            ]
        , "p2p" .= object
          [ "trusted_peers" .= ([] :: [Value])
          , "topics_of_interest" .= object
              [ "messages" .= String "low"
              , "blocks" .= String "normal"
              ]
          , "public_address" .=
              ("/ip4/127.0.0.1/tcp/" ++ show (jmNodeApiPort cfg))
          ]
        ]

setupConfig :: IO (BaseUrl, FilePath)
setupConfig = do
    [apiPort, nodePort] <- randomUnusedTCPPorts 2
    tmp <- getCanonicalTemporaryDirectory
    configDir <- createTempDirectory tmp "cardano-wallet-jormungandr"
    let storageDir = configDir </> "storage"
        configYaml = configDir </> "config.yaml"
        jormConfig = JormConfig storageDir apiPort nodePort
        url = BaseUrl Http "localhost" apiPort "/api"
    encodeFile configYaml jormConfig
    createDirectory storageDir
    pure (url, configYaml)
