{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Wallet
    ( start
    , stop
    , WalletInstance (..)
    , WalletApi (..)
    , WalletProcessConfig (..)
    ) where

import qualified Data.String as String

import Cardano.Wallet.Spec.Network.Node
    ( NodeApi, nodeApiSocket )
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
                    "http://" <> host <> ":" <> show port <> "/v2"
                , walletInstanceApiHost = host
                , walletInstanceApiPort = port
                }
    let walletLog = walletDir </> [relfile|wallet.log|]
    handle <- openFile (toFilePath walletLog) AppendMode
    putTextLn $ "Writing wallet logs to " <> toText (toFilePath walletLog)
    process <-
        startProcess
            $ setStderr (useHandleClose handle)
            $ setStdout (useHandleClose handle)
            $ shell
            $ String.unwords
                [ "cardano-wallet"
                , "serve"
                , "--testnet"
                , toFilePath walletByronGenesis
                , "--node-socket"
                , toFilePath (nodeApiSocket walletNodeApi)
                , "--database"
                , toFilePath walletDatabase
                , "--listen-address"
                , toString host
                , "--port"
                , show port
                , "--log-level"
                , "INFO"
                ]
    pure (WalletInstance process, config)

stop :: WalletInstance -> IO ()
stop (WalletInstance process) = stopProcess process
