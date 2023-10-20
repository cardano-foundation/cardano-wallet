{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Cli.Launcher where

import Prelude

import Path
    ( Abs
    , Dir
    , File
    , Path
    , relfile
    , toFilePath
    , (</>)
    )
import System.IO
    ( IOMode (AppendMode)
    , openFile
    )
import System.Process.Typed
    ( Process
    , proc
    , setStderr
    , setStdout
    , startProcess
    , stopProcess
    , useHandleClose
    )

start :: NodeProcessConfig -> IO (NodeInstance, NodeApi)
start NodeProcessConfig{..} = do
    let nodeSocket = nodeDir </> [relfile|node.sock|]
    let nodeLog = nodeDir </> [relfile|node.log|]
    handle <- openFile (toFilePath nodeLog) AppendMode
    putStrLn $ "Writing node logs to " <> toFilePath nodeLog
    nodeProcess <-
        startProcess
            $ setStderr (useHandleClose handle)
            $ setStdout (useHandleClose handle)
            $ proc
                "cardano-node"
                [ "run"
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
    stopProcess process

--------------------------------------------------------------------------------
-- Data types ------------------------------------------------------------------

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
