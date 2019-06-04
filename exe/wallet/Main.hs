{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- In essence, it's a proxy to the wallet server, which is required for most
-- commands. Commands are turned into corresponding API calls, and submitted
-- to an up-and-running server. Some commands do not require an active server
-- and can be run "offline".

module Main where

import Prelude hiding
    ( getLine )

import Cardano.CLI
    ( getLine
    , getSensitiveLine
    , help
    , parseAllArgsWith
    , parseArgWith
    , putErrLn
    , setUtf8Encoding
    )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Types
    ( ApiMnemonicT (..)
    , ApiT (..)
    , PostTransactionData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Network
    ( defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..), KeyToAddress, Passphrase (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress, EncodeAddress )
import Control.Applicative
    ( many )
import Control.Arrow
    ( second )
import Control.Monad
    ( when )
import Data.Aeson
    ( (.:) )
import Data.FileEmbed
    ( embedFile )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Servant
    ( (:<|>) (..), (:>) )
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
    , shortOption
    )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitFailure )
import System.IO
    ( BufferMode (NoBuffering), hSetBuffering, stderr, stdout )
import Text.Regex.Applicative
    ( anySym, few, match, string, sym )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Network.Wai.Handler.Warp as Warp


cli :: Docopt
cli = [docopt|Cardano Wallet CLI.

The CLI is a proxy to the wallet server, which is required for most
commands. Commands are turned into corresponding API calls, and submitted
to an up-and-running server. Some commands do not require an active server
and can be run "offline". (e.g. 'generate mnemonic')

    ⚠️  Options are positional (--a --b is not equivalent to --b --a) ! ⚠️

Usage:
  cardano-wallet server [--network=STRING] [--port=INT] [--bridge-port=INT]
  cardano-wallet mnemonic generate [--size=INT]
  cardano-wallet wallet list [--port=INT]
  cardano-wallet wallet create [--port=INT] <name> [--address-pool-gap=INT]
  cardano-wallet wallet get [--port=INT] <wallet-id>
  cardano-wallet wallet update [--port=INT] <wallet-id> --name=STRING
  cardano-wallet wallet delete [--port=INT] <wallet-id>
  cardano-wallet transaction create [--port=INT] <wallet-id> --payment=PAYMENT...
  cardano-wallet address list [--port=INT] <wallet-id>
  cardano-wallet -h | --help
  cardano-wallet --version

Options:
  --port <INT>                port used for serving the wallet API [default: 8090]
  --bridge-port <INT>         port used for communicating with the http-bridge [default: 8080]
  --address-pool-gap <INT>    number of unused consecutive addresses to keep track of [default: 20]
  --size <INT>                number of mnemonic words to generate [default: 15]
  --payment <PAYMENT>         address to send to and amount to send separated by @: '<amount>@<address>'
  --network <STRING>          testnet, staging, or mainnet [default: testnet]

Examples:
  # Create a transaction and send 22 lovelace from wallet-id to specified addres
  cardano-wallet transaction create \
    2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D
|]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    setUtf8Encoding
    manager <- newManager defaultManagerSettings
    getArgs >>= parseArgsOrExit cli >>= exec' manager

{-------------------------------------------------------------------------------
                         Command and Argument Parsing
-------------------------------------------------------------------------------}


exec' :: Manager -> Arguments -> IO ()
exec' manager args = args `parseArg` longOption "network" >>= \case
    Testnet -> exec (execHttpBridge @'Testnet args) manager args
    Staging -> exec (execHttpBridge @'Staging args) manager args
    Mainnet -> exec (execHttpBridge @'Mainnet args) manager args

exec
    :: forall t.
        ( DecodeAddress t
        , EncodeAddress t
        , KeyToAddress t
        )
    => (Proxy t -> IO ()) -- | Action to launch the wallet server
    -> Manager
    -> Arguments
    -> IO ()
exec execServer manager args
    | args `isPresent` (longOption "help") = help cli
    | args `isPresent` (shortOption 'h') = help cli

    | args `isPresent` command "server" = do
        execServer Proxy

    | args `isPresent` command "generate" &&
      args `isPresent` command "mnemonic" = do
        n <- args `parseArg` longOption "size"
        execGenerateMnemonic n

    | args `isPresent` command "wallet" &&
      args `isPresent` command "list" = do
        runClient Aeson.encodePretty listWallets

    | args `isPresent` command "wallet" &&
      args `isPresent` command "get" = do
        wId <- args `parseArg` argument "wallet-id"
        runClient Aeson.encodePretty $ getWallet $ ApiT wId

    | args `isPresent` command "wallet" &&
      args `isPresent` command "create" = do
        wName <- args `parseArg` argument "name"
        wGap <- args `parseArg` longOption "address-pool-gap"
        wSeed <- do
            let prompt = "Please enter a 15–24 word mnemonic sentence: "
            let parser = fromMnemonic @'[15,18,21,24] @"seed" . T.words
            getLine prompt parser
        wSndFactor <- do
            let prompt =
                    "(Enter a blank line if you do not wish to use a second \
                    \factor.)\nPlease enter a 9–12 word mnemonic second factor: "
            let parser = optional (fromMnemonic @'[9,12] @"generation") . T.words
            getLine prompt parser <&> \case
                (Nothing, _) -> Nothing
                (Just a, t) -> Just (a, t)
        wPwd <- getPassphraseWithConfirm
        runClient Aeson.encodePretty $ postWallet $ WalletPostData
            (Just $ ApiT wGap)
            (ApiMnemonicT . second T.words $ wSeed)
            (ApiMnemonicT . second T.words <$> wSndFactor)
            (ApiT wName)
            (ApiT wPwd)

    | args `isPresent` command "wallet" &&
      args `isPresent` command "update" = do
        wId <- args `parseArg` argument "wallet-id"
        wName <- args `parseArg` longOption "name"
        runClient Aeson.encodePretty $ putWallet (ApiT wId) $ WalletPutData
            (Just $ ApiT wName)

    | args `isPresent` command "wallet" &&
      args `isPresent` command "delete" = do
        wId <- args `parseArg` argument "wallet-id"
        runClient (const "") $ deleteWallet (ApiT wId)

    | args `isPresent` command "transaction" &&
      args `isPresent` command "create" = do
        wId <- args `parseArg` argument "wallet-id"
        ts <- args `parseAllArgs` longOption "payment"
        wPwd <- getPassphrase
        runClient Aeson.encodePretty $ createTransaction (ApiT wId) $
            PostTransactionData
                ts
                (ApiT wPwd)

    | args `isPresent` command "address" &&
      args `isPresent` command "list" = do
        wId <- args `parseArg` argument "wallet-id"
        runClient Aeson.encodePretty $ listAddresses (ApiT wId) Nothing

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
    getPassphrase :: IO (Passphrase "encryption")
    getPassphrase = do
        let prompt = "Please enter a passphrase: "
        let parser = fromText @(Passphrase "encryption")
        fst <$> getSensitiveLine prompt parser
    getPassphraseWithConfirm :: IO (Passphrase "encryption")
    getPassphraseWithConfirm = do
        wPwd <- getPassphrase
        (wPwd', _) <- do
            let prompt = "Enter the passphrase a second time: "
            let parser = fromText @(Passphrase "encryption")
            getSensitiveLine prompt parser
        when (wPwd /= wPwd') $ do
            putErrLn "Passphrases don't match."
            exitFailure
        pure wPwd

    listAddresses :<|> -- List Address
        ( deleteWallet
        :<|> getWallet
        :<|> listWallets
        :<|> postWallet
        :<|> putWallet
        :<|> _ -- Put Wallet Passphrase
        )
        :<|> createTransaction
        = client (Proxy @("v2" :> Api t))

    -- | 'runClient' requires a type-application to carry a particular
    -- namespace and adjust error messages accordingly. For instances, when
    -- running commands from the 'cardano-wallet wallet' namespace, one should
    -- do:
    --
    -- @
    -- runClient @Wallet ...
    -- @
    runClient
        :: forall a. ()
        => (a -> BL.ByteString)
        -> ClientM a
        -> IO ()
    runClient encode cmd = do
        port <- args `parseArg` longOption "port"
        let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
        res <- runClientM cmd env
        case res of
            Right a -> do
                TIO.hPutStrLn stderr "Ok."
                BL8.putStrLn (encode a)
            Left e -> do
                let msg = case e of
                        FailureResponse r -> fromMaybe
                            (T.decodeUtf8 $ BL.toStrict $ responseBody r)
                            (decodeError $ responseBody r)
                        ConnectionError t ->
                            t
                        _ ->
                            T.pack $ show e
                putErrLn msg

-- | Start a web-server to serve the wallet backend API on the given port.
execHttpBridge
    :: forall n. (KeyToAddress (HttpBridge n), KnownNetwork n)
    => Arguments -> Proxy (HttpBridge n) -> IO ()
execHttpBridge args _ = do
    (walletPort :: Int)
        <- args `parseArg` longOption "port"
    (bridgePort :: Int)
        <- args `parseArg` longOption "bridge-port"
    db <- MVar.newDBLayer
    nw <- HttpBridge.newNetworkLayer @n bridgePort
    waitForConnection nw defaultRetryPolicy
    let tl = HttpBridge.newTransactionLayer @n
    wallet <- newWalletLayer @_ @(HttpBridge n) db nw tl
    let settings = Warp.defaultSettings
            & Warp.setPort walletPort
            & Warp.setBeforeMainLoop (TIO.hPutStrLn stderr $
                "Wallet backend server listening on: " <> toText walletPort
            )
    Server.start settings wallet

-- | Generate a random mnemonic of the given size 'n' (n = number of words),
-- and print it to stdout.
execGenerateMnemonic :: Text -> IO ()
execGenerateMnemonic n = do
    m <- case n of
        "9"  -> mnemonicToText @9 . entropyToMnemonic <$> genEntropy
        "12" -> mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        "15" -> mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        "18" -> mnemonicToText @18 . entropyToMnemonic <$> genEntropy
        "21" -> mnemonicToText @21 . entropyToMnemonic <$> genEntropy
        "24" -> mnemonicToText @24 . entropyToMnemonic <$> genEntropy
        _  -> do
            putErrLn "Invalid mnemonic size. Expected one of: 9,12,15,18,21,24"
            exitFailure
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

-- | Decode API error messages and extract the corresponding message.
decodeError
    :: BL.ByteString
    -> Maybe Text
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj

parseArg :: FromText a => Arguments -> Option -> IO a
parseArg = parseArgWith cli

parseAllArgs :: FromText a => Arguments -> Option -> IO (NE.NonEmpty a)
parseAllArgs = parseAllArgsWith cli
