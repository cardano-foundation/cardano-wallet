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
    ( Port (..)
    , getOptionalSensitiveValue
    , getRequiredSensitiveValue
    , parseArgWith
    )
import Cardano.Wallet
    ( mkWalletLayer )
import Cardano.Wallet.Api
    ( Api, GetWallet, ListWallets, PostWallet )
import Control.Arrow
    ( second )

import Cardano.Wallet.Api.Server
    ( server )
import Cardano.Wallet.Api.Types
    ( ApiMnemonicT (..), ApiT (..), WalletPostData (..) )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..), Passphrase (..) )
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
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Servant
    ( (:<|>) (..), (:>), serve )
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM )
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
import qualified Cardano.Wallet.Transaction.HttpBridge as HttpBridge
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
  cardano-wallet server [--port=INT] [--bridge-port=INT]
  cardano-wallet generate mnemonic [--size=INT]
  cardano-wallet list wallets [--port=INT]
  cardano-wallet get wallet --wallet-id=STRING [--port=INT]
  cardano-wallet create wallet --name=STRING [--address-pool-gap=INT] [--port=INT]
  cardano-wallet delete wallet --id=STRING
  cardano-wallet -h | --help
  cardano-wallet --version

Options:
  --port <INT>                port used for serving the wallet API [default: 8090]
  --bridge-port <INT>         port used for communicating with the http-bridge [default: 8080]
  --address-pool-gap <INT>    number of unused consecutive addresses to keep track of [default: 20]
  --size <INT>                number of mnemonic words to generate [default: 15]
|]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    manager <- newManager defaultManagerSettings
    getArgs >>= parseArgsOrExit cli >>= exec manager

{-------------------------------------------------------------------------------
                         Command and Argument Parsing
-------------------------------------------------------------------------------}

exec :: Manager -> Arguments -> IO ()
exec manager args
    | args `isPresent` (longOption "help") = do
        exitWithUsage cli

    | args `isPresent` command "server" = do
        walletPort <- args `parseArg` longOption "port"
        bridgePort <- args `parseArg` longOption "bridge-port"
        execServer walletPort bridgePort

    | args `isPresent` command "generate" && args `isPresent` command "mnemonic" = do
        n <- args `parseArg` longOption "size"
        execGenerateMnemonic n

    | args `isPresent` command "wallets" && args `isPresent` command "list" = do
        runClient listWallets

    | args `isPresent` command "wallet" && args `isPresent` command "get" = do
        wId <- args `parseArg` longOption "wallet-id"
        runClient $ getWallet $ ApiT wId

    | args `isPresent` command "wallet" && args `isPresent` command "create" = do
        wName <- args `parseArg` longOption "name"
        wGap <- args `parseArg` longOption "address-pool-gap"
        wSeed <- getRequiredSensitiveValue
            (fromMnemonic @'[15,18,21,24] @"seed" . T.words)
            "Please enter a 15–24 word mnemonic sentence: "
        wSndFactor <- getOptionalSensitiveValue
            (fromMnemonic @'[9,12] @"generation" . T.words)
            "Please enter a 9–12 word mnemonic second factor: \n\
            \(Enter a blank line if you do not wish to use a second factor.)"
        (wPwd, _) <- getRequiredSensitiveValue
            (fromText @(Passphrase "encryption"))
            "Please enter a passphrase: "
        runClient $ postWallet $ WalletPostData
            (Just $ ApiT wGap)
            (ApiMnemonicT . second T.words $ wSeed)
            ((ApiMnemonicT . second T.words) <$> wSndFactor)
            (ApiT wName)
            (ApiT wPwd)

    | args `isPresent` command "wallet" && args `isPresent` command "update" = do
        wid <- args `parseArg` longOption "id"
        walletName <- args `parseArg` longOption "name"
        print (wid :: WalletId, walletName :: WalletName)

    | args `isPresent` command "wallet" && args `isPresent` command "delete" = do
        wid <- args `parseArg` longOption "id"
        print (wid :: WalletId)

    | otherwise =
        exitWithUsage cli
  where
    parseArg :: FromText a => Arguments -> Option -> IO a
    parseArg = parseArgWith cli

    listWallets :<|> getWallet :<|> postWallet =
        client (Proxy @("v2" :> ListWallets :<|> GetWallet :<|> PostWallet))

    runClient :: Show a => ClientM a -> IO ()
    runClient cmd = do
        port <- args `parseArg` longOption "port"
        let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
        res <- runClientM cmd env
        case res of
            Left err -> putStrLn $ "Error: " ++ show err
            Right wallet -> print wallet

-- | Start a web-server to serve the wallet backend API on the given port.
execServer :: Port "wallet" -> Port "bridge" -> IO ()
execServer (Port port) (Port bridgePort) = do
    db <- MVar.newDBLayer
    nw <- HttpBridge.newNetworkLayer bridgePort
    let tl = HttpBridge.newTransactionLayer
    let wallet = mkWalletLayer @_ @HttpBridge db nw tl
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
