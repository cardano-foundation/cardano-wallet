{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Cli.Launcher
    ( start
    , stop
    , WalletInstance (..)
    , WalletApi (..)
    , WalletProcessConfig (..)
    ) where

import Prelude

import qualified Data.Text as T

import Cardano.Node.Cli.Launcher
    ( NodeApi, nodeApiSocket )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Path
    ( Abs, Dir, File, Path, relfile, toFilePath, (</>) )
import System.IO
    ( IOMode (AppendMode), openFile )
import System.Process.Typed
    ( Process
    , proc
    , setStderr
    , setStdout
    , startProcess
    , stopProcess
    , useHandleClose
    )

newtype WalletInstance = WalletInstance (Process () () ())

data WalletApi = WalletApi
    { walletInstanceApiUrl :: Text
    , walletInstanceApiHost :: Text
    , walletInstanceApiPort :: Int
    }

data WalletProcessConfig = WalletProcessConfig
    { walletDir :: Path Abs Dir
    , walletNodeApi :: NodeApi
    , walletDatabase :: Path Abs Dir
    , walletListenHost :: Maybe Text
    , walletListenPort :: Maybe Int
    , walletByronGenesis :: Path Abs File
    }

start :: WalletProcessConfig -> IO (WalletInstance, WalletApi)
start WalletProcessConfig{..} = do
    let host = fromMaybe "localhost" walletListenHost
        port = fromMaybe 8090 walletListenPort
    let config =
            WalletApi
                { walletInstanceApiUrl =
                    "http://" <> host <> ":" <> T.pack (show port) <> "/v2"
                , walletInstanceApiHost = host
                , walletInstanceApiPort = port
                }
    let walletLog = walletDir </> [relfile|wallet.log|]
    handle <- openFile (toFilePath walletLog) AppendMode
    putStrLn $ "Writing wallet logs to " <> toFilePath walletLog
    process <-
        startProcess
            $ setStderr (useHandleClose handle)
            $ setStdout (useHandleClose handle)
            $ proc
                "cardano-wallet"
                [ "serve"
                , "--testnet"
                , toFilePath walletByronGenesis
                , "--node-socket"
                , toFilePath (nodeApiSocket walletNodeApi)
                , "--database"
                , toFilePath walletDatabase
                , "--listen-address"
                , T.unpack host
                , "--port"
                , show port
                , "--log-level"
                , "INFO"
                ]
    pure (WalletInstance process, config)

stop :: WalletInstance -> IO ()
stop (WalletInstance process) = stopProcess process
