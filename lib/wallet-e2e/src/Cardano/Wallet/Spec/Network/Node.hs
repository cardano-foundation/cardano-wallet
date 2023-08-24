{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Node where

import qualified Data.String as String

import Path
    ( Abs, Dir, File, Path, relfile, toFilePath, (</>) )
import System.Process.Typed
    ( Process, shell, startProcess, stopProcess )

newtype NodeApi = NodeApi (Path Abs File)

nodeApiSocket :: NodeApi -> Path Abs File
nodeApiSocket (NodeApi socket) = socket

data NodeProcessConfig = NodeProcessConfig
    { nodeDir :: Path Abs Dir
    , nodeConfig :: Path Abs File
    , nodeTopology :: Path Abs File
    , nodeDatabase :: Path Abs Dir
    }

newtype NodeInstance = NodeInstance (Process () () ())

start :: NodeProcessConfig -> IO (NodeInstance, NodeApi)
start NodeProcessConfig{..} = do
    putTextLn "Starting node"
    let nodeSocket = nodeDir </> [relfile|node.sock|]
    nodeProcess <-
        startProcess . shell
            $ String.unwords
                [ "cardano-node"
                , "run"
                , "--config"
                , toFilePath nodeConfig
                , "--topology"
                , toFilePath nodeTopology
                , "--database-path"
                , toFilePath nodeDatabase
                , "--socket-path"
                , toFilePath nodeSocket
                , "+RTS"
                , "-N4"
                , "-RTS"
                ]
    pure (NodeInstance nodeProcess, NodeApi nodeSocket)

stop :: NodeInstance -> IO ()
stop (NodeInstance process) = do
    putTextLn "Stopping node"
    stopProcess process
