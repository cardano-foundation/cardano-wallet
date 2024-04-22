{-# LANGUAGE DataKinds #-}
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
    ( NodeApi
    , nodeApiSocket
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import System.IO
    ( IOMode (AppendMode)
    , openFile
    )
import System.Path
    ( relFile
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

newtype WalletInstance = WalletInstance (Process () () ())

data WalletApi = WalletApi
    { walletInstanceApiUrl :: Text
    , walletInstanceApiHost :: Text
    , walletInstanceApiPort :: Int
    }

data WalletProcessConfig = WalletProcessConfig
    { walletDir :: DirOf "wallet"
    , walletNodeApi :: NodeApi
    , walletDatabase :: DirOf "db"
    , walletListenHost :: Maybe Text
    , walletListenPort :: Maybe Int
    , walletByronGenesisForTestnet :: Maybe (FileOf "byron-genesis-testnet")
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
    let walletLog = absDirOf walletDir </> relFile "wallet.log"
    handle <- openFile (toFilePath walletLog) AppendMode
    putStrLn $ "Writing wallet logs to " <> toFilePath walletLog
    process <-
        startProcess
            $ setStderr (useHandleClose handle)
            $ setStdout (useHandleClose handle)
            $ proc "cardano-wallet"
            $ concat
                [ [ "serve" ]
                , case walletByronGenesisForTestnet of
                    Nothing ->
                        [ "--mainnet" ]
                    Just walletByronGenesis ->
                        [ "--testnet", toFilePath
                            $ absFileOf walletByronGenesis
                        ]
                , [ "--node-socket"
                  , toFilePath (nodeApiSocket walletNodeApi)
                  , "--database"
                  , toFilePath $ absDirOf walletDatabase
                  , "--listen-address"
                  , T.unpack host
                  , "--port"
                  , show port
                  , "--log-level"
                  , "DEBUG"
                  ]
                ]
    pure (WalletInstance process, config)

stop :: WalletInstance -> IO ()
stop (WalletInstance process) = stopProcess process
