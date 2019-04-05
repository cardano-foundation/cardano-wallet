{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module parses command line arguments for the wallet and executes
-- corresponding commands.
--
-- In essence, it's a proxy to the wallet server which is required for most
-- commands. Those Commands are turned into corresponding API calls, and
-- submitted to an up-and-running server. Some other commands do not require an
-- active server and can be ran "offline"

module Main where

import Prelude

import Cardano.CLI
    ( Network
    , Port (..)
    , getOptionalSensitiveValue
    , getRequiredSensitiveValue
    , parseArgWith
    )
import Cardano.Wallet
    ( mkWalletLayer )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( server )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..), Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..), WalletName )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Servant
    ( (:>), serve )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , command
    , docopt
    , exitWithUsage
    , isPresent
    , longOption
    , parseArgsOrExit
    )
import System.Environment
    ( getArgs )
import System.IO
    ( BufferMode (NoBuffering), hSetBuffering, stdout )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Wai.Handler.Warp as Warp


cli :: Docopt
cli = [docopt|Cardano Wallet CLI.

In essence, it's a proxy to the wallet server which is required for most
commands. Those Commands are turned into corresponding API calls, and
submitted to an up-and-running server. Some other commands do not require an
active server and can be ran "offline" (e.g. 'generate mnemonic')

    ⚠️  Options are positional (--a --b is not equivalent to --b --a) ! ⚠️

Usage:
  cardano-wallet server [--network=NETWORK] [--port=INT] [--bridge-port=INT]
  cardano-wallet generate mnemonic [--size=INT]
  cardano-wallet list address --wallet-id=STRING
  cardano-wallet list wallet
  cardano-wallet create wallet --name=STRING [--address-pool-gap=INT]
  cardano-wallet delete wallet --id=STRING
  cardano-wallet update wallet --id=STRING --name=STRING
  cardano-wallet -h | --help
  cardano-wallet --version

Options:
  --network <NETWORK>         mainnet, testnet or staging [default: mainnet]
  --port <INT>                port used for serving the wallet API [default: 8090]
  --bridge-port <INT>         port used for communicating with the http-bridge [default: 8080]
  --address-pool-gap <INT>    number of unused consecutive addresses to keep track of [default: 20]
  --size <INT>                number of mnemonic words to generate [default: 15]
|]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    getArgs >>= parseArgsOrExit cli >>= exec

{-------------------------------------------------------------------------------
                         Command and Argument Parsing
-------------------------------------------------------------------------------}

exec :: Arguments -> IO ()
exec args
    | args `isPresent` (longOption "help") = do
        exitWithUsage cli

    | args `isPresent` command "server" = do
        network <- args `parseArg` longOption "network"
        walletPort <- args `parseArg` longOption "port"
        bridgePort <- args `parseArg` longOption "bridge-port"
        execServer network walletPort bridgePort

    | args `isPresent` command "generate" && args `isPresent` command "mnemonic" = do
        n <- args `parseArg` longOption "size"
        execGenerateMnemonic n

    | args `isPresent` command "address" && args `isPresent` command "list" = do
        wid <- args `parseArg` longOption "wallet-id"
        print (wid :: WalletId)

    | args `isPresent` command "wallet" && args `isPresent` command "list" = do
        return ()

    | args `isPresent` command "wallet" && args `isPresent` command "create" = do
        poolGap <- args `parseArg` longOption "address-pool-gap"
        name <- args `parseArg` longOption "name"
        mnemonic <- getRequiredSensitiveValue
            (fromMnemonic @'[15,18,21,24] @"seed" . T.words)
            "Please enter a 15–24 word mnemonic sentence: "
        sndFactor <- getOptionalSensitiveValue
            (fromMnemonic @'[9,12] @"generation" . T.words)
            "Please enter a 9–12 word mnemonic second factor: \n\
            \(Enter a blank line if you do not wish to use a second factor.)"
        passphrase <- getRequiredSensitiveValue
            (fromText @(Passphrase "encryption"))
            "Please enter a passphrase: \n\
            \(Enter a blank line if you do not wish to use a passphrase.)"
        print
            ( poolGap :: AddressPoolGap
            , name :: WalletName
            , mnemonic
            , sndFactor
            , passphrase
            )

    | args `isPresent` command "wallet" && args `isPresent` command "update" = do
        wid <- args `parseArg` longOption "id"
        name <- args `parseArg` longOption "name"
        print (wid :: WalletId, name :: WalletName)

    | args `isPresent` command "wallet" && args `isPresent` command "delete" = do
        wid <- args `parseArg` longOption "id"
        print (wid :: WalletId)

    | otherwise =
        exitWithUsage cli
  where
    parseArg :: FromText a => Arguments -> Option -> IO a
    parseArg = parseArgWith cli

-- | Start a web-server to serve the wallet backend API on the given port.
execServer :: Network -> Port "wallet" -> Port "bridge" -> IO ()
execServer target (Port port) (Port bridgePort) = do
    db <- MVar.newDBLayer
    network <- HttpBridge.newNetworkLayer (toText target) bridgePort
    let wallet = mkWalletLayer db network
    Warp.runSettings settings (serve (Proxy @("v2" :> Api)) (server wallet))
  where
    settings = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setBeforeMainLoop (do
            TIO.putStrLn $ "Wallet backend server listening on: " <> toText port
        )

-- | Generate a random mnemonic of the given size 'n' (n = number of words),
-- and print it to stdout.
execGenerateMnemonic :: Int -> IO ()
execGenerateMnemonic n = do
    m <- case n of
        9  -> mnemonicToText @9 . entropyToMnemonic <$> genEntropy
        12 -> mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        15 -> mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        18 -> mnemonicToText @18 . entropyToMnemonic <$> genEntropy
        21 -> mnemonicToText @21 . entropyToMnemonic <$> genEntropy
        24 -> mnemonicToText @24 . entropyToMnemonic <$> genEntropy
        _  -> fail "Invalid mnemonic size. Expected one of: 9,12,15,18,21,24"
    TIO.putStrLn $ T.unwords m
