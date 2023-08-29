{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Node where

import qualified Data.String as String

import Path
    ( Abs, Dir, File, Path, relfile, toFilePath, (</>) )
import System.IO
    ( openFile )
import System.Process.Typed
    ( Process
    , setStderr
    , setStdout
    , shell
    , startProcess
    , stopProcess
    , useHandleClose
    )

start :: NodeProcessConfig -> IO (NodeInstance, NodeApi)
start NodeProcessConfig{..} = do
    let nodeSocket = nodeDir </> [relfile|node.sock|]
    let nodeLog = nodeDir </> [relfile|node.log|]
    handle <- openFile (toFilePath nodeLog) AppendMode
    putTextLn $ "Writing node logs to " <> toText (toFilePath nodeLog)
    nodeProcess <-
        startProcess
            $ setStderr (useHandleClose handle)
            $ setStdout (useHandleClose handle)
            $ shell
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
