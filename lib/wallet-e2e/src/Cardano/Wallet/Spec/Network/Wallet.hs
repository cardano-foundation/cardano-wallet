module Cardano.Wallet.Spec.Network.Wallet
    ( start
    , stop
    , WalletInstance (..)
    , WalletApi (..)
    ) where

import System.Process.Typed
    ( Process, shell, startProcess, stopProcess )

newtype WalletInstance = WalletInstance (Process () () ())

newtype WalletApi = WalletApi {walletInstanceApiUrl :: Text}

start :: IO (WalletInstance, WalletApi)
start = do
    putTextLn "Starting wallet"
    let config =
            WalletApi
                { walletInstanceApiUrl = "http://localhost:8090/v2"
                }
    process <- startProcess $ shell "cardano-wallet version"
    pure (WalletInstance process, config)

stop :: WalletInstance -> IO ()
stop (WalletInstance process) = do
    putTextLn "Stopping wallet"
    stopProcess process
