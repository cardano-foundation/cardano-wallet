{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Cardano.BM.Configuration.Model
    ( setMinSeverity )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
import Cardano.CLI
    ( Port
    , getLine
    , getOptionValue
    , getSensitiveLine
    , help
    , parseAllArgsWith
    , parseArgWith
    , putErrLn
    , setUtf8Encoding
    )
import Cardano.Launcher
    ( Command (Command)
    , ProcessHasExited (ProcessHasExited)
    , StdStream (..)
    , installSignalHandlers
    , launch
    )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiMnemonicT (..)
    , ApiT (..)
    , PostTransactionData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, block0 )
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
import Control.Arrow
    ( second )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race_ )
import Control.Monad
    ( when )
import Data.Aeson
    ( (.:) )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Version
    ( showVersion )
import Fmt
    ( blockListF, fmt, nameF )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Paths_cardano_wallet
    ( version )
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
    , getArg
    , isPresent
    , longOption
    , parseArgsOrExit
    , shortOption
    )
import System.Directory
    ( createDirectory, doesDirectoryExist )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitFailure, exitSuccess, exitWith )
import System.FilePath
    ( (</>) )
import System.IO
    ( BufferMode (NoBuffering), hSetBuffering, stderr, stdout )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
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
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--bridge-port=INT] [--state-dir=DIR] [--min-log-severity=SEVERITY]
  cardano-wallet serve [--network=STRING] [(--port=INT | --random-port)] [--bridge-port=INT] [--database=FILE] [--min-log-severity=SEVERITY]
  cardano-wallet mnemonic generate [--size=INT]
  cardano-wallet wallet list [--port=INT]
  cardano-wallet wallet create [--port=INT] <name> [--address-pool-gap=INT]
  cardano-wallet wallet get [--port=INT] <wallet-id>
  cardano-wallet wallet update [--port=INT] <wallet-id> --name=STRING
  cardano-wallet wallet delete [--port=INT] <wallet-id>
  cardano-wallet transaction create [--port=INT] <wallet-id> --payment=PAYMENT...
  cardano-wallet address list [--port=INT] [--state=STRING] <wallet-id>
  cardano-wallet -h | --help
  cardano-wallet --version

Options:
  --address-pool-gap <INT>        number of unused consecutive addresses to keep track of [default: 20]
  --bridge-port <INT>             port used for communicating with the http-bridge [default: 8080]
  --database <FILE>               use this file for storing wallet state
  --min-log-severity <SEVERITY>   minimum log severity level, in ascending order: {debug, info, notice, warning, error, critical, alert, emergency} [default: debug]
  --network <STRING>              testnet or mainnet [default: testnet]
  --payment <PAYMENT>             address to send to and amount to send separated by @: '<amount>@<address>'
  --port <INT>                    port used for serving the wallet API [default: 8090]
  --random-port                   serve wallet API on any available port (conflicts with --port)
  --size <INT>                    number of mnemonic words to generate [default: 15]
  --state <STRING>                address state: either used or unused
  --state-dir <DIR>               write wallet state (blockchain and database) to this directory

Examples:
  # Launch and monitor a wallet server and its associated chain producer
  cardano-wallet launch --network mainnet --random-port --state-dir .state-dir

  # Start only a wallet server and connect it to an already existing chain producer
  cardano-wallet serve --bridge-port 8080

  # Create a transaction and send 22 lovelace from wallet-id to specified address
  cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D
|]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    setUtf8Encoding
    manager <- newManager defaultManagerSettings
    args <- getArgs >>= parseArgsOrExit cli
    args `parseArg` longOption "network" >>= \case
        Testnet -> exec (execHttpBridge @'Testnet args) manager args
        Mainnet -> exec (execHttpBridge @'Mainnet args) manager args

{-------------------------------------------------------------------------------
                                  Logging
-------------------------------------------------------------------------------}

initTracer :: Severity -> Text -> IO (Trace IO Text)
initTracer minSeverity cmd = do
    c <- defaultConfigStdout
    setMinSeverity c minSeverity
    setupTrace (Right c) "cardano-wallet" >>= appendName cmd

{-------------------------------------------------------------------------------
                         Command and Argument Parsing
-------------------------------------------------------------------------------}

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
exec execServe manager args
    | args `isPresent` (longOption "help") = help cli
    | args `isPresent` (shortOption 'h') = help cli

    | args `isPresent` command "serve" = do
        execServe Proxy

    | args `isPresent` command "launch" = do
        -- NOTE: 'fromJust' is safe because the network value has a default.
        let network = fromJust $ args `getArg` longOption "network"
        let stateDir = args `getArg` longOption "state-dir"
        bridgePort <- args `parseArg` longOption "bridge-port"
        listen <- parseWalletListen args
        minLogSeverity <- getOptionValue <$>
            args `parseArg` longOption "min-log-severity"
        tracer <- initTracer minLogSeverity "launch"
        execLaunch tracer network stateDir bridgePort listen

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
                    \factor.)\n\
                    \Please enter a 9–12 word mnemonic second factor: "
            let parser =
                    optional (fromMnemonic @'[9,12] @"generation") . T.words
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
        runClient (const "") (deleteWallet (ApiT wId))

    | args `isPresent` command "transaction" &&
      args `isPresent` command "create" = do
        wId <- args `parseArg` argument "wallet-id"
        ts <- args `parseAllArgs` longOption "payment"
        res <- sendRequest $ getWallet $ ApiT wId
        if (isRight res) then do
            wPwd <- getPassphrase
            runClient Aeson.encodePretty $ createTransaction (ApiT wId) $
                PostTransactionData
                    ts
                    (ApiT wPwd)
        else
            handleResponse Aeson.encodePretty res

    | args `isPresent` command "address" &&
      args `isPresent` command "list" = do
        wId <- args `parseArg` argument "wallet-id"
        maybeState <- args `parseOptionalArg` longOption "state"
        runClient Aeson.encodePretty
            (listAddresses (ApiT wId) (ApiT <$> maybeState))

    | args `isPresent` longOption "version" = do
        putStrLn (showVersion version)
        exitSuccess

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

    listAddresses :<|>
        ( deleteWallet
        :<|> getWallet
        :<|> listWallets
        :<|> postWallet
        :<|> putWallet
        :<|> _ -- Put Wallet Passphrase
        )
        :<|> createTransaction
        = client (Proxy @("v2" :> Api t))

    runClient
        :: forall a. ()
        => (a -> BL.ByteString)
        -> ClientM a
        -> IO ()
    runClient encode cmd = do
        res <- sendRequest cmd
        handleResponse encode res

    sendRequest
        :: forall a. ()
        => ClientM a
        -> IO (Either ServantError a)
    sendRequest cmd = do
        port <- args `parseArg` longOption "port"
        let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
        runClientM cmd env

    handleResponse
        :: forall a. ()
        => (a -> BL.ByteString)
        -> Either ServantError a
        -> IO ()
    handleResponse encode res = do
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
                exitFailure

-- | Start a web-server to serve the wallet backend API on the given port.
execHttpBridge
    :: forall n. (KeyToAddress (HttpBridge n), KnownNetwork n)
    => Arguments -> Proxy (HttpBridge n) -> IO ()
execHttpBridge args _ = do
    minLogSeverity <- getOptionValue <$>
        args `parseArg` longOption "min-log-severity"
    tracer <- initTracer minLogSeverity "serve"
    walletListen <- parseWalletListen args
    (bridgePort :: Int)
        <- args `parseArg` longOption "bridge-port"
    let dbFile = args `getArg` longOption "database"
    (_, db) <- Sqlite.newDBLayer dbFile
    nw <- HttpBridge.newNetworkLayer @n bridgePort
    waitForConnection nw defaultRetryPolicy
    let tl = HttpBridge.newTransactionLayer @n
    wallet <- newWalletLayer @_ @(HttpBridge n) tracer block0 db nw tl
    Server.withListeningSocket walletListen $ \(port, socket) -> do
        tracerIPC <- appendName "DaedalusIPC" tracer
        tracerApi <- appendName "api" tracer
        let beforeMainLoop = logInfo tracer $
                "Wallet backend server listening on: " <> toText port
        let settings = Warp.defaultSettings
                & setBeforeMainLoop beforeMainLoop
        let ipcServer = daedalusIPC tracerIPC port
        let apiServer = Server.start settings tracerApi socket wallet
        race_ ipcServer apiServer

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

-- | Execute the 'launch' command. This differs from the 'serve' command as it
-- takes care of also starting a node backend (http-bridge) in two separate
-- processes and monitor both processes: if one terminates, then the other one
-- is cancelled.
execLaunch
    :: Trace IO Text
    -> String
    -> Maybe FilePath
    -> Port "Node"
    -> Listen
    -> IO ()
execLaunch tracer network stateDir bridgePort listen = do
    installSignalHandlers
    maybe (pure ()) (setupStateDir tracer) stateDir
    let commands = [ httpBridgeCmd, walletCmd ]
    logInfo tracer $ fmt $ nameF "launch" $ blockListF commands
    (ProcessHasExited pName code) <- launch commands
    logAlert tracer $ T.pack pName <> " exited with code " <> T.pack (show code)
    exitWith code
  where
    -- | Launch a sub-process starting the http-bridge with the given options
    httpBridgeCmd :: Command
    httpBridgeCmd =
        Command "cardano-http-bridge" args (return ()) Inherit
      where
        args = mconcat
            [ [ "start" ]
            , [ "--port", showT bridgePort ]
            , [ "--template", network ]
            , maybe [] (\d -> ["--networks-dir", d]) stateDir
            ]

    -- | Launch a sub-process starting the wallet server with the given options
    walletCmd :: Command
    walletCmd =
        Command "cardano-wallet" args (threadDelay oneSecond) Inherit
      where
        oneSecond = 1000000
        args = mconcat
            [ [ "serve" ]
            , [ "--network", if network == "local" then "testnet" else network ]
            , case listen of
                ListenOnRandomPort -> ["--random-port"]
                ListenOnPort port  -> ["--port", showT port]
            , [ "--bridge-port", showT bridgePort ]
            , maybe [] (\d -> ["--database", d </> "wallet.db"]) stateDir
            ]

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Show a data-type through its 'ToText' instance
showT :: ToText a => a -> String
showT = T.unpack . toText

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

parseOptionalArg :: FromText a => Arguments -> Option -> IO (Maybe a)
parseOptionalArg args option
    | args `isPresent` option = Just <$> parseArg args option
    | otherwise = pure Nothing

parseAllArgs :: FromText a => Arguments -> Option -> IO (NE.NonEmpty a)
parseAllArgs = parseAllArgsWith cli

-- | Parse and convert the `--port` or `--random-port` option into a 'Listen'
-- data-type.
parseWalletListen :: Arguments -> IO Listen
parseWalletListen args = do
    let useRandomPort = args `isPresent` longOption "random-port"
    walletPort <- args `parseArg` longOption "port"
    pure $ case (useRandomPort, walletPort) of
        (True, _) -> ListenOnRandomPort
        (False, port) -> ListenOnPort port

-- | Initialize a state directory to store blockchain data such as blocks or
-- the wallet database.
setupStateDir :: Trace IO Text -> FilePath -> IO ()
setupStateDir tracer dir = doesDirectoryExist dir >>= \case
    True -> logInfo tracer $ "Using state directory: " <> T.pack dir
    False -> do
        logInfo tracer $ "Creating state directory: " <> T.pack dir
        createDirectory dir
