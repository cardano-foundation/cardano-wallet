{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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

import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
import Cardano.CLI
    ( Port (..)
    , Verbosity (..)
    , decodeError
    , getLine
    , getSensitiveLine
    , help
    , initTracer
    , minSeverityFromArgs
    , optional
    , parseAllArgsWith
    , parseArgWith
    , putErrLn
    , setUtf8Encoding
    , showT
    , verbosityFromArgs
    , verbosityToArgs
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
    ( HttpBridge, byronFeePolicy )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Network
    ( getBlock )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..), KeyToAddress, Passphrase (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( Block (..), DecodeAddress, EncodeAddress, Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Arrow
    ( second )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race_ )
import Control.Monad
    ( when )
import Data.Coerce
    ( coerce )
import Data.Either
    ( fromRight, isRight )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.List
    ( sort )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Fmt
    ( blockListF, fmt, nameF )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
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
    , exitWithUsage
    , getArg
    , isPresent
    , longOption
    , parseArgsOrExit
    , shortOption
    )
import System.Console.Docopt.NoTH
    ( parseUsage )
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
import Text.Heredoc
    ( here )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Compatibility as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Environment as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Cardano.Wallet.Jormungandr.Environment as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Network.Wai.Handler.Warp as Warp

cliHttpBridge :: Docopt
cliHttpBridge = makeCli
    cliCommandsHttpBridge
    cliOptionsHttpBridge
    cliExamplesHttpBridge

cliJormungandr :: Docopt
cliJormungandr = makeCli
    cliCommandsJormungandr
    cliOptionsJormungandr
    cliExamplesJormungandr

makeCli :: String -> String -> String -> Docopt
makeCli cliCommandsSpecific cliOptionsSpecific cliExamplesSpecific =
    fromRight (error "Unable to construct CLI.") $
        parseUsage usageString
  where
    usageString = mempty
        <> cliHeader
        <> "\nUsage:\n"
        <> cliCommands
        <> "\nOptions:\n"
        <> cliOptions
        <> "\nExamples:\n"
        <> cliExamples
    cliCommands = unlines $ filter (not . null) $ lines $ mempty
        <> cliCommandsSpecific
        <> cliCommandsGeneric
    cliOptions = unlines $ filter (not . null) $ sort $ lines $ mempty
        <> cliOptionsSpecific
        <> cliOptionsGeneric
    cliExamples = unlines $ filter (not . null) $ lines $ mempty
        <> cliExamplesSpecific
        <> cliExamplesGeneric

cliHeader :: String
cliHeader = [here|Cardano Wallet CLI.

The CLI is a proxy to the wallet server, which is required for most
commands. Commands are turned into corresponding API calls, and submitted
to an up-and-running server. Some commands do not require an active server
and can be run "offline". (e.g. 'generate mnemonic')

    ⚠️  Options are positional (--a --b is not equivalent to --b --a) ! ⚠️
|]

cliCommandsGeneric :: String
cliCommandsGeneric = [here|
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
|]

cliCommandsHttpBridge :: String
cliCommandsHttpBridge = [here|
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--state-dir=DIR] [(--quiet | --verbose )]
  cardano-wallet serve  [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--database=FILE] [(--quiet | --verbose )]
|]

cliCommandsJormungandr :: String
cliCommandsJormungandr = [here|
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--state-dir=DIR] [(--quiet | --verbose )] --genesis-hash=HASH --genesis-block=PATH --node-config=PATH --node-secret=PATH
  cardano-wallet serve  [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--database=FILE] [(--quiet | --verbose )] --genesis-hash=HASH
|]

cliOptionsGeneric :: String
cliOptionsGeneric = [here|
  --address-pool-gap <INT>  number of unused consecutive addresses to keep track of [default: 20]
  --database <FILE>         use this file for storing wallet state
  --network <STRING>        testnet or mainnet [default: testnet]
  --payment <PAYMENT>       address to send to and amount to send separated by @: '<amount>@<address>'
  --port <INT>              port used for serving the wallet API [default: 8090]
  --quiet                   suppress all log output apart from errors
  --random-port             serve wallet API on any available port (conflicts with --port)
  --size <INT>              number of mnemonic words to generate [default: 15]
  --state <STRING>          address state: either used or unused
  --state-dir <DIR>         write wallet state (blockchain and database) to this directory
  --verbose                 display debugging information in the log output
|]

cliOptionsHttpBridge :: String
cliOptionsHttpBridge = [here|
  --backend-port <INT>      port used for communicating with the HTTP bridge [default: 8080]
|]

cliOptionsJormungandr :: String
cliOptionsJormungandr = [here|
  --backend-port <INT>      port used for communicating with the backend node [default: 8081]
  --genesis-block <FILE>    path to genesis block
  --genesis-hash <HASH>     hash of genesis block
  --node-config <FILE>      path to node configuration (general)
  --node-secret <FILE>      path to node configuration (secret)
|]

cliExamplesGeneric :: String
cliExamplesGeneric = [here|
  # Create a transaction and send 22 lovelace from wallet-id to specified address
  cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D
|]

cliExamplesHttpBridge :: String
cliExamplesHttpBridge = [here|
  # Launch and monitor a wallet server and its associated chain producer
  cardano-wallet launch --network mainnet --random-port --state-dir .state-dir

  # Start only a wallet server and connect it to an already existing chain producer
  cardano-wallet serve --backend-port 8080
|]

cliExamplesJormungandr :: String
cliExamplesJormungandr = [here|
  # Launch and monitor a wallet server and its associated chain producer
  cardano-wallet launch \
    --genesis-hash=dba597bee5f0987efbf56f6bd7f44c38158a7770d0cb28a26b5eca40613a7ebd \
    --genesis-block=block0.bin \
    --node-config config.yaml \
    --backend-port 8081 \
    --node-secret secret.yaml

  # Start only a wallet server and connect it to an already existing chain producer
  cardano-wallet serve --backend-port 8081
|]

data Environment = Environment
    { args
        :: Arguments
    , cli
        :: Docopt
    , argPresent
        :: Option -> Bool
    , parseAllArgs
        :: forall a . FromText a => Option -> IO (NE.NonEmpty a)
    , parseArg
        :: forall a . FromText a => Option -> IO a
    , parseOptionalArg
        :: forall a . FromText a => Option -> IO (Maybe a)
    }

main :: IO ()
main = run withHttpBridge cliHttpBridge

run :: (Manager -> Environment -> IO ()) -> Docopt -> IO ()
run runWith cliDefinition = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    setUtf8Encoding
    manager <- newManager defaultManagerSettings
    arguments <- getArgs >>= parseArgsOrExit cliDefinition
    runWith manager $ Environment
        { args = arguments
        , cli = cliDefinition
        , argPresent = (arguments `isPresent`)
        , parseAllArgs = parseAllArgsWith cliDefinition arguments
        , parseArg = parseArgWith cliDefinition arguments
        , parseOptionalArg = \o ->
            if arguments `isPresent` o
            then Just <$> parseArgWith cliDefinition arguments o
            else pure Nothing
        }

withHttpBridge :: Manager -> Environment -> IO ()
withHttpBridge manager env@Environment {..} =
    parseArg (longOption "network") >>= \case
        HttpBridge.Testnet ->
            exec env manager
                (execServeHttpBridge @'HttpBridge.Testnet env)
                (execLaunchHttpBridge env)
        HttpBridge.Mainnet ->
            exec env manager
                (execServeHttpBridge @'HttpBridge.Mainnet env)
                (execLaunchHttpBridge env)

withJormungandr :: Manager -> Environment -> IO ()
withJormungandr manager env@Environment {..} =
    parseArg (longOption "network") >>= \case
        Jormungandr.Testnet ->
            exec env manager
                (execServeJormungandr @'Jormungandr.Testnet env)
                (execLaunchJormungandr env)
        Jormungandr.Mainnet ->
            exec env manager
                (execServeJormungandr @'Jormungandr.Mainnet env)
                (execLaunchJormungandr env)

{-------------------------------------------------------------------------------
                         Command and Argument Parsing
-------------------------------------------------------------------------------}

exec
    :: forall t.
        ( DecodeAddress t
        , EncodeAddress t
        , KeyToAddress t
        )
    => Environment
    -> Manager
    -> (Proxy t -> IO ()) -- | execServe
    -> (IO ())            -- | execLaunch
    -> IO ()
exec Environment {..} manager execServe execLaunch
    | argPresent (longOption "help") = help cli
    | argPresent (shortOption 'h') = help cli

    | argPresent $ command "serve" = do
        execServe Proxy

    | argPresent $ command "launch" = do
        execLaunch

    | argPresent (command "generate") &&
      argPresent (command "mnemonic") = do
        n <- parseArg $ longOption "size"
        execGenerateMnemonic n

    | argPresent (command "wallet") &&
      argPresent (command "list") = do
        runClient Aeson.encodePretty listWallets

    | argPresent (command "wallet") &&
      argPresent (command "get") = do
        wId <- parseArg $ argument "wallet-id"
        runClient Aeson.encodePretty $ getWallet $ ApiT wId

    | argPresent (command "wallet") &&
      argPresent (command "create") = do
        wName <- parseArg $ argument "name"
        wGap <- parseArg $ longOption "address-pool-gap"
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

    | argPresent (command "wallet") &&
      argPresent (command "update") = do
        wId <- parseArg $ argument "wallet-id"
        wName <- parseArg $ longOption "name"
        runClient Aeson.encodePretty $ putWallet (ApiT wId) $ WalletPutData
            (Just $ ApiT wName)

    | argPresent (command "wallet") &&
      argPresent (command "delete") = do
        wId <- parseArg $ argument "wallet-id"
        runClient (const "") (deleteWallet (ApiT wId))

    | argPresent (command "transaction") &&
      argPresent (command "create") = do
        wId <- parseArg $ argument "wallet-id"
        ts <- parseAllArgs $ longOption "payment"
        res <- sendRequest $ getWallet $ ApiT wId
        if (isRight res) then do
            wPwd <- getPassphrase
            runClient Aeson.encodePretty $ createTransaction (ApiT wId) $
                PostTransactionData
                    ts
                    (ApiT wPwd)
        else
            handleResponse Aeson.encodePretty res

    | argPresent (command "address") &&
      argPresent (command "list") = do
        wId <- parseArg $ argument "wallet-id"
        maybeState <- parseOptionalArg $ longOption "state"
        runClient Aeson.encodePretty
            (listAddresses (ApiT wId) (ApiT <$> maybeState))

    | argPresent $ longOption "version" = do
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
        port <- getPort <$> parseArg (longOption "port")
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

{-------------------------------------------------------------------------------
                                Launching
-------------------------------------------------------------------------------}

execLaunchHttpBridge :: Environment -> IO ()
execLaunchHttpBridge env@Environment {..} = do
    -- NOTE: 'fromJust' is safe because the network value has a default.
    let network = fromJust $ args `getArg` longOption "network"
    let stateDir = args `getArg` longOption "state-dir"
    let verbosity = verbosityFromArgs args
    backendPort <- parseArg $ longOption "backend-port"
    listen <- parseWalletListen env
    tracer <- initTracer (minSeverityFromArgs args) "launch"
    execLaunchCommands tracer stateDir
        [ commandHttpBridge
              backendPort stateDir network verbosity
        , commandWalletServe
              listen backendPort stateDir network verbosity Nothing
        ]

execLaunchJormungandr :: Environment -> IO ()
execLaunchJormungandr env@Environment {..} = do
    -- NOTE: 'fromJust' is safe because the network value has a default.
    let network = fromJust $ args `getArg` longOption "network"
    let stateDir = args `getArg` longOption "state-dir"
    let verbosity = verbosityFromArgs args
    genesisBlockPath <- parseArg $ longOption "genesis-block"
    genesisHash <- Just <$> parseArg (longOption "genesis-hash")
    listen <- parseWalletListen env
    nodeConfigPath <- parseArg $ longOption "node-config"
    backendPort <- parseArg $ longOption "backend-port"
    nodeSecretPath <- parseArg $ longOption "node-secret"
    tracer <- initTracer (minSeverityFromArgs args) "launch"
    execLaunchCommands tracer stateDir
        [ commandJormungandr
            genesisBlockPath nodeConfigPath nodeSecretPath
        , commandWalletServe
            listen backendPort stateDir network verbosity genesisHash
        ]

-- | Execute 'launch' commands. This differs from the 'serve' command as it
-- takes care of also starting a node backend in two separate processes and
-- monitors both processes: if one terminates, then the other one is cancelled.
execLaunchCommands :: Trace IO Text -> Maybe FilePath -> [Command] -> IO ()
execLaunchCommands tracer stateDir commands = do
    installSignalHandlers
    maybe (pure ()) (setupStateDir tracer) stateDir
    logInfo tracer $ fmt $ nameF "launch" $ blockListF commands
    (ProcessHasExited pName code) <- launch commands
    logAlert tracer $ T.pack pName <> " exited with code " <> T.pack (show code)
    exitWith code

commandHttpBridge
    :: Port "Node" -> Maybe FilePath -> String -> Verbosity -> Command
commandHttpBridge backendPort stateDir network verbosity =
    Command "cardano-http-bridge" args (return ()) Inherit
  where
    args = mconcat
        [ [ "start" ]
        , [ "--port", showT backendPort ]
        , [ "--template", network ]
        , maybe [] (\d -> ["--networks-dir", d]) stateDir
        , verbosityToArgs verbosity
        ]

commandJormungandr :: FilePath -> FilePath -> FilePath -> Command
commandJormungandr genesisBlockPath nodeConfigPath nodeSecretPath =
    Command "jormungandr" args (return ()) Inherit
  where
    args = mconcat
      [ [ "--genesis-block", genesisBlockPath ]
      , [ "--config", nodeConfigPath ]
      , [ "--secret", nodeSecretPath ]
      ]

commandWalletServe
    :: Listen -> Port "Node" -> Maybe FilePath -> String -> Verbosity
    -> Maybe (Hash "Genesis") -> Command
commandWalletServe listen backendPort stateDir network verbosity mGenesisHash =
    Command "cardano-wallet" args (threadDelay oneSecond) Inherit
  where
    oneSecond = 1000000
    args = mconcat
        [ [ "serve" ]
        , [ "--network", if network == "local" then "testnet" else network ]
        , case listen of
            ListenOnRandomPort -> ["--random-port"]
            ListenOnPort port  -> ["--port", showT port]
        , [ "--backend-port", showT backendPort ]
        , maybe [] (\d -> ["--database", d </> "wallet.db"]) stateDir
        , verbosityToArgs verbosity
        , case mGenesisHash of
            Just genesisHash -> [ "--genesis-hash", showT genesisHash ]
            Nothing -> []
        ]

{-------------------------------------------------------------------------------
                                 Serving
-------------------------------------------------------------------------------}

execServeHttpBridge
    :: forall n. (KeyToAddress (HttpBridge n), HttpBridge.KnownNetwork n)
    => Environment -> Proxy (HttpBridge n) -> IO ()
execServeHttpBridge env@Environment {..} _ = do
    tracer <- initTracer (minSeverityFromArgs args) "serve"
    logInfo tracer $ "Wallet backend server starting. "
        <> "Version "
        <> T.pack (showVersion version)
    walletListen <- parseWalletListen env
    backendPort <- parseArg $ longOption "backend-port"
    let dbFile = args `getArg` longOption "database"
    tracerDB <- appendName "DBLayer" tracer
    (_, db) <- Sqlite.newDBLayer tracerDB dbFile
    nw <- HttpBridge.newNetworkLayer @n (getPort backendPort)
    waitForConnection nw defaultRetryPolicy
    let tl = HttpBridge.newTransactionLayer @n
    wallet <- newWalletLayer tracer HttpBridge.block0 byronFeePolicy db nw tl
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

-- | Start a web-server to serve the wallet backend API on the given port.
execServeJormungandr
    :: forall n . (KeyToAddress (Jormungandr n), Jormungandr.KnownNetwork n)
    => Environment -> Proxy (Jormungandr n) -> IO ()
execServeJormungandr env@Environment {..} _ = do
    tracer <- initTracer (minSeverityFromArgs args) "serve"
    logInfo tracer $ "Wallet backend server starting. "
        <> "Version "
        <> T.pack (showVersion version)
    walletListen <- parseWalletListen env
    backendPort <- parseArg $ longOption "backend-port"
    block0H <- parseArg $ longOption "genesis-hash"
    let dbFile = args `getArg` longOption "database"
    tracerDB <- appendName "DBLayer" tracer
    (_, db) <- Sqlite.newDBLayer tracerDB dbFile
    (nl, block0, feePolicy) <- newNetworkLayer
        (BaseUrl Http "localhost" (getPort backendPort) "/api") block0H
    waitForConnection nl defaultRetryPolicy
    let tl = Jormungandr.newTransactionLayer @n block0H
    wallet <- newWalletLayer tracer block0 feePolicy db nl tl
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
  where
    newNetworkLayer
        :: BaseUrl
        -> Hash "Genesis"
        -> IO (NetworkLayer (Jormungandr n) IO, Block Tx, FeePolicy)
    newNetworkLayer url block0H = do
        mgr <- newManager defaultManagerSettings
        let jormungandr = Jormungandr.mkJormungandrLayer mgr url
        let nl = Jormungandr.mkNetworkLayer jormungandr
        waitForConnection nl defaultRetryPolicy
        block0 <- unsafeRunExceptT $ getBlock jormungandr (coerce block0H)
        feePolicy <- unsafeRunExceptT $
            Jormungandr.getInitialFeePolicy jormungandr (coerce block0H)
        return (nl, block0, feePolicy)

{-------------------------------------------------------------------------------
                                 Mnemonics
-------------------------------------------------------------------------------}

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

-- | Parse and convert the `--port` or `--random-port` option into a 'Listen'
-- data-type.
parseWalletListen :: Environment -> IO Listen
parseWalletListen Environment {..} = do
    let useRandomPort = argPresent $ longOption "random-port"
    walletPort <- parseArg $ longOption "port"
    pure $ case (useRandomPort, walletPort) of
        (True, _) -> ListenOnRandomPort
        (False, port) -> ListenOnPort (getPort port)

-- | Initialize a state directory to store blockchain data such as blocks or
-- the wallet database.
setupStateDir :: Trace IO Text -> FilePath -> IO ()
setupStateDir tracer dir = doesDirectoryExist dir >>= \case
    True -> logInfo tracer $ "Using state directory: " <> T.pack dir
    False -> do
        logInfo tracer $ "Creating state directory: " <> T.pack dir
        createDirectory dir
