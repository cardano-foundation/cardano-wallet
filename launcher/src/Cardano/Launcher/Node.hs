{-# LANGUAGE TupleSections #-}
-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch @cardano-node@.

module Cardano.Launcher.Node
    ( withCardanoNode
    , CardanoNodeConfig (..)
    , defaultCardanoNodeConfig
    , CardanoNodeConn (..)
    , NodePort (..)
    ) where

import Prelude

import Cardano.Launcher
    ( LauncherLog, ProcessHasExited, withBackendCreateProcess )
import Control.Tracer
    ( Tracer (..) )
import Data.Maybe
    ( maybeToList )
import System.Environment
    ( getEnvironment )
import System.FilePath
    ( takeFileName, (</>) )
import System.Info
    ( os )
import System.Process
    ( CreateProcess (..), proc )

-- | Parameters for connecting to the node.
newtype CardanoNodeConn = CardanoNodeConn
    { nodeSocketFile :: FilePath
    } deriving (Show, Eq)

newtype NodePort = NodePort { unNodePort :: Int }
    deriving (Show, Eq)

-- | A subset of the @cardano-node@ CLI parameters, used for starting the
-- backend.
data CardanoNodeConfig = CardanoNodeConfig
    { nodeDir             :: FilePath
    , nodeConfigFile      :: FilePath
    , nodeTopologyFile    :: FilePath
    , nodeDatabaseDir     :: FilePath
    , nodeDlgCertFile     :: Maybe FilePath
    , nodeSignKeyFile     :: Maybe FilePath
    , nodeOpCertFile      :: Maybe FilePath
    , nodeKesKeyFile      :: Maybe FilePath
    , nodeVrfKeyFile      :: Maybe FilePath
    , nodePort            :: Maybe NodePort
    , nodeLoggingHostname :: Maybe String
    } deriving (Show, Eq)

defaultCardanoNodeConfig :: CardanoNodeConfig
defaultCardanoNodeConfig = CardanoNodeConfig
    { nodeDir             = "."
    , nodeConfigFile      = "configuration.json"
    , nodeTopologyFile    = "topology.json"
    , nodeDatabaseDir     = "db"
    , nodeDlgCertFile     = Nothing
    , nodeSignKeyFile     = Nothing
    , nodeOpCertFile      = Nothing
    , nodeKesKeyFile      = Nothing
    , nodeVrfKeyFile      = Nothing
    , nodePort            = Nothing
    , nodeLoggingHostname = Nothing
    }

-- | Spawns a @cardano-node@ process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Tracer IO LauncherLog
    -- ^ Trace for subprocess control logging
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO (Either ProcessHasExited a)
withCardanoNode tr cfg action = do
    let socketPath = nodeSocketPath (nodeDir cfg)
    cp <- cardanoNodeProcess cfg socketPath
    withBackendCreateProcess tr cp $ \_ _ -> action $ CardanoNodeConn socketPath

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: CardanoNodeConfig -> FilePath -> IO CreateProcess
cardanoNodeProcess cfg socketPath = do
    myEnv <- getEnvironment
    let env' = ("CARDANO_NODE_LOGGING_HOSTNAME",) <$> nodeLoggingHostname cfg

    pure $ (proc "cardano-node" args)
        { env = Just $ maybeToList env' ++ myEnv
        , cwd = Just $ nodeDir cfg
        }
  where
    args =
        [ "run"
        , "--config", nodeConfigFile cfg
        , "--topology", nodeTopologyFile cfg
        , "--database-path", nodeDatabaseDir cfg
        , "--socket-path", socketPath
        ]
        ++ opt "--port" (show . unNodePort <$> nodePort cfg)
        ++ opt "--signing-key" (nodeSignKeyFile cfg)
        ++ opt "--delegation-certificate" (nodeDlgCertFile cfg)
        ++ opt "--shelley-operational-certificate" (nodeOpCertFile cfg)
        ++ opt "--shelley-kes-key" (nodeKesKeyFile cfg)
        ++ opt "--shelley-vrf-key" (nodeVrfKeyFile cfg)

    opt _ Nothing = []
    opt arg (Just val) = [arg, val]

-- | Generate a 'FilePath' for the @cardano-node@ domain socket/named pipe.
nodeSocketPath
    :: FilePath -- ^ @cardano-node@ state directory
    -> FilePath -- ^ UNIX socket file path or Windows named pipe name
nodeSocketPath dir
    | os == "mingw32" = "\\\\.\\pipe\\" ++ takeFileName dir
    | otherwise = dir </> "node.socket"
