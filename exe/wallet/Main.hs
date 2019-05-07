{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
    ( Port (..), getSensitiveLine, parseArgWith, putErrLn )
import Cardano.Environment
    ( network )
import Cardano.Wallet
    ( mkWalletLayer )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( server )
import Cardano.Wallet.Api.Types
    ( ApiMnemonicT (..), ApiT (..), WalletPostData (..), WalletPutData (..) )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..), Passphrase (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Control.Applicative
    ( many )
import Control.Arrow
    ( second )
import Control.Monad
    ( void )
import Data.Aeson
    ( ToJSON )
import Data.FileEmbed
    ( embedFile )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
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
import Servant.Client.Core
    ( ServantError (..), responseBody )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , argument
    , command
    , docopt
    , exitWithUsage
    , isPresent
    , longOption
    , parseArgsOrExit
    )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitFailure )
import System.IO
    ( BufferMode (NoBuffering), hSetBuffering, stdout )
import Text.Regex.Applicative
    ( anySym, few, match, string, sym )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge
import qualified Cardano.Wallet.Transaction.HttpBridge as HttpBridge
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
  cardano-wallet mnemonic generate [--size=INT]
  cardano-wallet wallet list [--port=INT]
  cardano-wallet wallet create [--port=INT] --name=STRING [--address-pool-gap=INT]
  cardano-wallet wallet get [--port=INT] <wallet-id>
  cardano-wallet wallet delete [--port=INT] <wallet-id>
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

    | args `isPresent` command "wallet" && args `isPresent` command "list" = do
        runClient listWallets

    | args `isPresent` command "wallet" && args `isPresent` command "get" = do
        wId <- args `parseArg` argument "wallet-id"
        runClient $ getWallet $ ApiT wId

    | args `isPresent` command "wallet" && args `isPresent` command "create" = do
        wName <- args `parseArg` longOption "name"
        wGap <- args `parseArg` longOption "address-pool-gap"
        wSeed <- do
            let prompt = "Please enter a 15–24 word mnemonic sentence: "
            let parser = fromMnemonic @'[15,18,21,24] @"seed" . T.words
            getSensitiveLine prompt (Just ' ') parser
        wSndFactor <- do
            let prompt =
                    "(Enter a blank line if you do not wish to use a second \
                    \factor.)\nPlease enter a 9–12 word mnemonic second factor: "
            let parser = optional (fromMnemonic @'[9,12] @"generation") . T.words
            getSensitiveLine prompt (Just ' ') parser <&> \case
                (Nothing, _) -> Nothing
                (Just a, t) -> Just (a, t)
        (wPwd, _) <- do
            let prompt = "Please enter a passphrase: "
            let parser = fromText @(Passphrase "encryption")
            getSensitiveLine prompt Nothing parser
        runClient $ postWallet $ WalletPostData
            (Just $ ApiT wGap)
            (ApiMnemonicT . second T.words $ wSeed)
            (ApiMnemonicT . second T.words <$> wSndFactor)
            (ApiT wName)
            (ApiT wPwd)

    | args `isPresent` command "wallet" && args `isPresent` command "update" = do
        wId <- args `parseArg` argument "wallet-id"
        wName <- args `parseArg` longOption "name"
        runClient $ putWallet (ApiT wId) $ WalletPutData
            (Just $ ApiT wName)

    | args `isPresent` command "wallet" && args `isPresent` command "delete" = do
        wId <- args `parseArg` argument "wallet-id"
        runClient $ void $ deleteWallet (ApiT wId)

    | args `isPresent` longOption "version" = do
        let cabal = B8.unpack $(embedFile "cardano-wallet.cabal")
        let re = few anySym
                *> string "version:" *> many (sym ' ') *> few anySym
                <* sym '\n' <* many anySym
        case match re cabal of
            Nothing -> do
                putErrLn "Couldn't find program version!"
                exitFailure
            Just version -> do
                TIO.putStrLn $ T.pack version

    | otherwise =
        exitWithUsage cli
  where
    parseArg :: FromText a => Arguments -> Option -> IO a
    parseArg = parseArgWith cli

    _ :<|> -- List Address
        ( deleteWallet
        :<|> getWallet
        :<|> listWallets
        :<|> postWallet
        :<|> putWallet
        :<|> _ -- Put Wallet Passphrase
        )
        :<|>
        ( _ -- Create Transaction
        )
        = client (Proxy @("v2" :> Api))

    runClient :: ToJSON a => ClientM a -> IO ()
    runClient cmd = do
        port <- args `parseArg` longOption "port"
        let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
        res <- runClientM cmd env
        case res of
            Left (FailureResponse r) ->
                putErrLn $ T.decodeUtf8 $ BL.toStrict $ responseBody r
            Left (ConnectionError t) ->
                putErrLn t
            Left e ->
                putErrLn $ T.pack $ show e
            Right a ->
                BL8.putStrLn $ Aeson.encodePretty a

-- | Start a web-server to serve the wallet backend API on the given port.
execServer :: Port "wallet" -> Port "bridge" -> IO ()
execServer (Port port) (Port bridgePort) = do
    network `seq` return  () -- Force evaluation of ENV[network]
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

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Make an existing parser optional. Returns 'Right Nothing' if the input is
-- empty, without running the parser.
optional
    :: (Monoid m, Eq m)
    => (m -> Either e a)
    -> (m -> Either e (Maybe a))
optional parse = \case
    m | m == mempty -> Right Nothing
    m  -> Just <$> parse m
