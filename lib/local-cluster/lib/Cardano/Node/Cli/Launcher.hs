{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Cli.Launcher where

import Prelude

import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf
    , absFilePathOf
    , toFilePath
    )
import System.IO
    ( IOMode (AppendMode)
    , openFile
    )
import System.Path
    ( AbsFile
    , relFile
    , (</>)
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
    let nodeSocket = absDirOf nodeDir </> relFile "node.sock"
    let nodeLog = absDirOf nodeDir </> relFile "node.log"
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
                , absFilePathOf nodeConfig
                , "--topology"
                , absFilePathOf nodeTopology
                , "--database-path"
                , toFilePath $ absDirOf nodeDatabase
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

newtype NodeApi = NodeApi AbsFile

nodeApiSocket :: NodeApi -> AbsFile
nodeApiSocket (NodeApi socket) = socket

data NodeProcessConfig = NodeProcessConfig
    { nodeDir :: DirOf "node"
    , nodeConfig :: FileOf "node-config"
    , nodeTopology :: FileOf "node-topology"
    , nodeDatabase :: DirOf "db"
    }

newtype NodeInstance = NodeInstance (Process () () ())
