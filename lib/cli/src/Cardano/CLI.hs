{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Shared types and helpers for CLI parsing

module Cardano.CLI
    (
    -- * CLI Execution
      cli
    , execParser

    -- * Commands
    , cmdMnemonic
    , cmdWallet
    , cmdTransaction
    , cmdAddress
    , cmdVersion
    , execLaunch

    -- * Option & Argument Parsers
    , optionT
    , argumentT
    , databaseOption
    , listenOption
    , nodePortOption
    , stateDirOption
    , verbosityOption

    -- * Types
    , Port (..)

    -- * Logging
    , Verbosity (..)
    , initTracer
    , verbosityToArgs
    , verbosityToMinSeverity

    -- * Unicode Terminal Helpers
    , setUtf8Encoding

    -- * ANSI Terminal Helpers
    , putErrLn
    , hPutErrLn

    -- * Working with Sensitive Data
    , getLine
    , hGetLine
    , getSensitiveLine
    , hGetSensitiveLine

    -- * Helpers
    , decodeError
    ) where

import Prelude hiding
    ( getLine )

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
import Cardano.Launcher
    ( Command
    , ProcessHasExited (..)
    , installSignalHandlers
    , launch
    , setupStateDir
    )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( AddressAmount
    , ApiAddress
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    , PostTransactionData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..), Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, defaultAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( AddressState, DecodeAddress, EncodeAddress, WalletId, WalletName )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Applicative
    ( optional, some, (<|>) )
import Control.Arrow
    ( first, left, second )
import Control.Exception
    ( bracket )
import Control.Monad
    ( unless, void, when )
import Data.Aeson
    ( (.:) )
import Data.Bifunctor
    ( bimap )
import Data.Functor
    ( (<$), (<&>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..), showT )
import Data.Text.Read
    ( decimal )
import Fmt
    ( Buildable, blockListF, fmt, nameF, pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Options.Applicative
    ( ArgumentFields
    , CommandFields
    , Mod
    , OptionFields
    , Parser
    , ParserInfo
    , argument
    , command
    , eitherReader
    , execParser
    , flag'
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , showDefaultWith
    , subparser
    , value
    )
import Servant
    ( (:<|>) (..), (:>) )
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM )
import Servant.Client.Core
    ( ServantError (..), responseBody )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hCursorBackward
    , hSetSGR
    )
import System.Exit
    ( exitFailure, exitSuccess, exitWith )
import System.IO
    ( BufferMode (..)
    , Handle
    , hGetBuffering
    , hGetChar
    , hGetEcho
    , hIsTerminalDevice
    , hPutChar
    , hSetBuffering
    , hSetEcho
    , hSetEncoding
    , stderr
    , stdin
    , stdout
    , utf8
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

{-------------------------------------------------------------------------------
                                   CLI
-------------------------------------------------------------------------------}

-- | Construct a CLI from a list of a commands
--
-- >>> join $ execParser $ cli $ cmdA <> cmdB <> cmdC
--
cli :: Mod CommandFields a -> ParserInfo a
cli cmds = info (helper <*> subparser cmds) $ mempty
    <> progDesc "Cardano Wallet Command-Line Interface (CLI)"
    <> header (mconcat
        [ "The CLI is a proxy to the wallet server, which is required for most "
        , "commands. Commands are turned into corresponding API calls, and "
        , "submitted to an up-and-running server. Some commands do not require "
        , "an active server and can be run offline (e.g. 'mnemonic generate')."
        ])

{-------------------------------------------------------------------------------
                            Commands - 'mnemonic'

  cardano-wallet mnemonic generate [--size=INT]
-------------------------------------------------------------------------------}

-- | cardano-wallet mnemonic
cmdMnemonic :: Mod CommandFields (IO ())
cmdMnemonic = command "mnemonic" $ info (helper <*> cmds) mempty
  where
    cmds = subparser $ mempty
        <> cmdMnemonicGenerate

-- | Arguments for 'mnemonic generate' command
newtype MnemonicGenerateArgs = MnemonicGenerateArgs
    { _size :: String -- See 'sizeOption' note.
    }

-- | cardano-wallet mnemonic generate [--size=INT]
cmdMnemonicGenerate :: Mod CommandFields (IO ())
cmdMnemonicGenerate = command "generate" $ info (helper <*> cmd) $ mempty
    <> progDesc "Generate English BIP-0039 compatible mnemonic words."
  where
    cmd = exec . MnemonicGenerateArgs <$> sizeOption
    exec (MnemonicGenerateArgs n) = do
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
                            Commands - 'wallet'

  cardano-wallet wallet list [--port=INT]
  cardano-wallet wallet create [--port=INT] <name> [--address-pool-gap=INT]
  cardano-wallet wallet get [--port=INT] <wallet-id>
  cardano-wallet wallet update [--port=INT] <wallet-id> --name=STRING
  cardano-wallet wallet delete [--port=INT] <wallet-id>
-------------------------------------------------------------------------------}

-- | cardano-wallet wallet
cmdWallet
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWallet = command "wallet" $ info (helper <*> cmds) mempty
  where
    cmds = subparser $ mempty
        <> cmdWalletList @t
        <> cmdWalletCreate @t
        <> cmdWalletGet @t
        <> cmdWalletUpdate @t
        <> cmdWalletDelete @t

-- | Arguments for 'wallet list' command
newtype WalletListArgs = WalletListArgs
    { _port :: Port "Wallet"
    }

-- | cardano-wallet wallet list [--port=INT]
cmdWalletList
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletList = command "list" $ info (helper <*> cmd) $ mempty
    <> progDesc "List all known wallets."
  where
    cmd = fmap exec $ WalletListArgs <$> portOption
    exec (WalletListArgs wPort) = do
        runClient wPort Aeson.encodePretty $ listWallets (walletClient @t)

-- | Arguments for 'wallet create' command
data WalletCreateArgs = WalletCreateArgs
    { _port :: Port "Wallet"
    , _name :: WalletName
    , _gap :: AddressPoolGap
    }

-- | cardano-wallet wallet create [--port=INT] <name> [--address-pool-gap=INT]
cmdWalletCreate
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletCreate = command "create" $ info (helper <*> cmd) $ mempty
    <> progDesc "Create a new wallet using a sequential address scheme."
  where
    cmd = fmap exec $ WalletCreateArgs
        <$> portOption
        <*> walletNameArgument
        <*> poolGapOption
    exec (WalletCreateArgs wPort wName wGap) = do
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
                    optionalE (fromMnemonic @'[9,12] @"generation") . T.words
            getLine prompt parser <&> \case
                (Nothing, _) -> Nothing
                (Just a, t) -> Just (a, t)
        wPwd <- getPassphraseWithConfirm
        runClient wPort Aeson.encodePretty $ postWallet (walletClient @t) $
            WalletPostData
                (Just $ ApiT wGap)
                (ApiMnemonicT . second T.words $ wSeed)
                (ApiMnemonicT . second T.words <$> wSndFactor)
                (ApiT wName)
                (ApiT wPwd)

-- | Arguments for 'wallet get' command
data WalletGetArgs = WalletGetArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    }

-- | cardano-wallet wallet get [--port=INT] <wallet-id>
cmdWalletGet
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletGet = command "get" $ info (helper <*> cmd) $ mempty
    <> progDesc "Fetch the wallet with specified id."
  where
    cmd = fmap exec $ WalletGetArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletGetArgs wPort wId) = do
        runClient wPort Aeson.encodePretty $ getWallet (walletClient @t) $
            ApiT wId

-- | Arguments for 'wallet update' command
data WalletUpdateArgs = WalletUpdateArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _name :: WalletName
    }

-- | cardano-wallet wallet update [--port=INT] <wallet-id> --name=STRING
cmdWalletUpdate
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletUpdate = command "update" $ info (helper <*> cmd) $ mempty
    <> progDesc "Update metadata of a wallet with specified id."
  where
    cmd = fmap exec $ WalletUpdateArgs
        <$> portOption
        <*> walletIdArgument
        <*> walletNameOption
    exec (WalletUpdateArgs wPort wId wName) = do
        runClient wPort Aeson.encodePretty $ putWallet (walletClient @t)
            (ApiT wId)
            (WalletPutData $ Just (ApiT wName))

-- | Arguments for 'wallet delete' command
data WalletDeleteArgs = WalletDeleteArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    }

-- | cardano-wallet wallet delete [--port=INT] <wallet-id>
cmdWalletDelete
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletDelete = command "delete" $ info (helper <*> cmd) $ mempty
    <> progDesc "Deletes wallet with specified wallet id."
  where
    cmd = fmap exec $ WalletDeleteArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletDeleteArgs wPort wId) = do
        runClient wPort (const "") $ deleteWallet (walletClient @t) $
            ApiT wId

{-------------------------------------------------------------------------------
                            Commands - 'transaction'

  cardano-wallet transaction create [--port=INT] <wallet-id> --payment=PAYMENT...
-------------------------------------------------------------------------------}

-- | cardano-wallet transaction
cmdTransaction
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransaction = command "transaction" $ info (helper <*> cmds) mempty
  where
    cmds = subparser $ mempty
        <> cmdTransactionCreate @t

-- | Arguments for 'transaction create' command
data TransactionCreateArgs t = TransactionCreateArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _payments :: NonEmpty (AddressAmount t)
    }

-- | cardano-wallet wallet list [--port=INT]
cmdTransactionCreate
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransactionCreate = command "create" $ info (helper <*> cmd) $ mempty
    <> progDesc "Create and submit a new transaction."
  where
    cmd = fmap exec $ TransactionCreateArgs
        <$> portOption
        <*> walletIdArgument
        <*> fmap NE.fromList (some paymentOption)
    exec (TransactionCreateArgs wPort wId wPayments) = do
        res <- sendRequest wPort $ getWallet (walletClient @t) $ ApiT wId
        case res of
            Right _ -> do
                wPwd <- getPassphrase
                runClient wPort Aeson.encodePretty $ postTransaction
                    (walletClient @t)
                    (ApiT wId)
                    (PostTransactionData wPayments (ApiT wPwd))
            Left _ ->
                handleResponse Aeson.encodePretty res

{-------------------------------------------------------------------------------
                            Commands - 'address'

  cardano-wallet address list [--port=INT] [--state=STRING] <wallet-id>
-------------------------------------------------------------------------------}

-- | cardano-wallet address
cmdAddress
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdAddress = command "address" $ info (helper <*> cmds) mempty
  where
    cmds = subparser $ mempty
        <> cmdAddressList @t

-- | Arguments for 'address list' command
data AddressListArgs = AddressListArgs
    { _port :: Port "Wallet"
    , _state :: Maybe AddressState
    , _id :: WalletId
    }

-- | cardano-wallet address list [--port=INT] [--state=STRING] <wallet-id>
cmdAddressList
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdAddressList = command "list" $ info (helper <*> cmd) $ mempty
    <> progDesc "List all known addresses of a given wallet."
  where
    cmd = fmap exec $ AddressListArgs
        <$> portOption
        <*> optional addressStateOption
        <*> walletIdArgument
    exec (AddressListArgs wPort wState wId) = do
        runClient wPort Aeson.encodePretty $ listAddresses (walletClient @t)
            (ApiT wId)
            (ApiT <$> wState)

{-------------------------------------------------------------------------------
                            Commands - 'version'

  cardano-wallet version
-------------------------------------------------------------------------------}

-- | cardano-wallet version
cmdVersion :: Mod CommandFields (IO ())
cmdVersion = command "version" $ info cmd $ mempty
    <> progDesc "Show the program's version."
  where
    cmd = pure exec
    exec = do
        putStrLn (showVersion version)
        exitSuccess

{-------------------------------------------------------------------------------
                            Commands - 'launch'

  cardano-wallet launch ...
-------------------------------------------------------------------------------}

-- | Execute 'launch' commands. This differs from the 'serve' command as it
-- takes care of also starting a node backend in two separate processes and
-- monitors both processes: if one terminates, then the other one is cancelled.
execLaunch
    :: Verbosity
    -> Maybe FilePath
    -> [Command]
    -> IO ()
execLaunch verbosity stateDir commands = do
    installSignalHandlers
    (_, tracer) <- initTracer (verbosityToMinSeverity verbosity) "launch"
    maybe (pure ()) (setupStateDir $ logInfo tracer) stateDir
    logInfo tracer $ fmt $ nameF "launch" $ blockListF commands
    (ProcessHasExited pName code) <- launch commands
    logAlert tracer $ T.pack pName <> " exited with code " <> T.pack (show code)
    exitWith code

{-------------------------------------------------------------------------------
                              Options & Arguments
-------------------------------------------------------------------------------}

-- | --state=STRING
addressStateOption :: Parser AddressState
addressStateOption = optionT $ mempty
    <> long "state"
    <> metavar "STRING"
    <> help "only addresses with the given state: either 'used' or 'unused'."

-- | --database=FILEPATH
databaseOption :: Parser FilePath
databaseOption = optionT $ mempty
    <> long "database"
    <> metavar "FILEPATH"
    <> help "use this file for storing wallet state. Run in-memory otherwise."

-- | [--random-port|--port=INT]
listenOption :: Parser Listen
listenOption =
    (ListenOnRandomPort <$ randomPortOption)
    <|>
    (ListenOnPort . getPort <$> portOption)

-- | [--random-port]
randomPortOption :: Parser Bool
randomPortOption = flag' False $ mempty
    <> long "random-port"
    <> help "serve wallet API on any available port (conflicts with --port)"

-- | [--node-port=INT], default: 8080
nodePortOption :: Parser (Port "Node")
nodePortOption = optionT $ mempty
    <> long "node-port"
    <> metavar "INT"
    <> help "port used for communicating with the target node."
    <> value (Port 8080)
    <> showDefaultWith showT

-- | --payment=PAYMENT
paymentOption :: DecodeAddress t => Parser (AddressAmount t)
paymentOption = optionT $ mempty
    <> long "payment"
    <> metavar "PAYMENT"
    <> help
        "address to send to and amount to send separated by @\
        \, e.g. '<amount>@<address>'"

-- | [--address-pool-gap=INT], default: 20
poolGapOption :: Parser AddressPoolGap
poolGapOption = optionT $ mempty
    <> long "address-pool-gap"
    <> metavar "INT"
    <> help "number of unused consecutive addresses to keep track of."
    <> value defaultAddressPoolGap
    <> showDefaultWith showT

-- | [--port=INT], default: 8090
portOption :: Parser (Port "Wallet")
portOption = optionT $ mempty
    <> long "port"
    <> metavar "INT"
    <> help "port used for serving the wallet API."
    <> value (Port 8090)
    <> showDefaultWith showT

-- | [--size=INT], default: 15
--
-- NOTE: We keep the size in 'String' to ease error reporting in the command
-- handler. This allows to avoid errors that are too Haskell-ish like:
--
--    Int is an integer number between -9223372036854775808 and 9223372036854775807.
--
-- and report something more appropriate to the user.
sizeOption :: Parser String
sizeOption = optionT $ mempty
    <> long "size"
    <> metavar "INT"
    <> help "number of mnemonic words to generate."
    <> value "15"
    <> showDefaultWith show

-- | --state-dir=FILEPATH
stateDirOption :: Parser FilePath
stateDirOption = optionT $ mempty
    <> long "state-dir"
    <> metavar "DIR"
    <> help "write wallet state (blockchain and database) to this directory"

-- | [(--quiet|--verbose)]
verbosityOption :: Parser Verbosity
verbosityOption = (Quiet <$ quiet) <|> (Verbose <$ verbose) <|> (pure Default)
  where
    quiet = flag' False $ mempty
        <> long "quiet"
        <> help "suppress all log output apart from errors"
    verbose = flag' False $ mempty
        <> long "verbose"
        <> help "display debugging information in the log output"

-- | --name=STRING
walletNameOption :: Parser WalletName
walletNameOption = optionT $ mempty
    <> long "name"
    <> metavar "STRING"
    <> help "name of the wallet."

-- | <wallet-id=WALLET_ID>
walletIdArgument :: Parser WalletId
walletIdArgument = argumentT $ mempty
    <> metavar "WALLET_ID"

-- | <name=STRING>
walletNameArgument :: Parser WalletName
walletNameArgument = argumentT $ mempty
    <> metavar "STRING"

-- | Helper for writing an option 'Parser' using a 'FromText' instance.
optionT :: FromText a => Mod OptionFields a -> Parser a
optionT = option (eitherReader fromTextS)

-- | Helper for writing an argument 'Parser' using a 'FromText' instance.
argumentT :: FromText a => Mod ArgumentFields a -> Parser a
argumentT = argument (eitherReader fromTextS)

-- | Like 'fromText', but stringly-typed.
fromTextS :: FromText a => String -> Either String a
fromTextS = left getTextDecodingError . fromText . T.pack

{-------------------------------------------------------------------------------
                              Server Interaction
-------------------------------------------------------------------------------}

data WalletClient t = WalletClient
    { listAddresses
        :: ApiT WalletId
        -> Maybe (ApiT AddressState)
        -> ClientM [ApiAddress t]
    , deleteWallet
        :: ApiT WalletId
        -> ClientM ()
    , getWallet
        :: ApiT WalletId
        -> ClientM ApiWallet
    , listWallets
        :: ClientM [ApiWallet]
    , postWallet
        :: WalletPostData
        -> ClientM ApiWallet
    , putWallet
        :: ApiT WalletId
        -> WalletPutData
        -> ClientM ApiWallet
    , postTransaction
        :: ApiT WalletId
        -> PostTransactionData t
        -> ClientM (ApiTransaction t)
    }

walletClient :: forall t. (DecodeAddress t, EncodeAddress t) => WalletClient t
walletClient =
    let
        addresses :<|> wallets :<|> transactions =
            client (Proxy @("v2" :> Api t))

        _listAddresses =
            addresses

        _deleteWallet
            :<|> _getWallet
            :<|> _listWallets
            :<|> _postWallet
            :<|> _putWallet
            :<|> _ -- Put Wallet Passphrase
            = wallets

        _postTransaction =
            transactions
    in
        WalletClient
            { listAddresses = _listAddresses
            , deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , postTransaction = _postTransaction
            }

runClient
    :: forall a. ()
    => Port "Wallet"
    -> (a -> BL.ByteString)
    -> ClientM a
    -> IO ()
runClient p encode cmd = do
    res <- sendRequest p cmd
    handleResponse encode res

sendRequest
    :: forall a. ()
    => Port "Wallet"
    -> ClientM a
    -> IO (Either ServantError a)
sendRequest (Port p) cmd = do
    manager <- newManager defaultManagerSettings
    let env = mkClientEnv manager (BaseUrl Http "localhost" p "")
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
                                Extra Types
-------------------------------------------------------------------------------}

-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port { getPort :: Int }
    deriving stock (Eq, Show, Generic)
    deriving newtype (Enum, Ord)

-- NOTE
-- TCP port ranges from [[-1;65535]] \ {0}
-- However, ports in [[-1; 1023]] \ {0} are well-known ports reserved
-- and only "bindable" through root privileges.
--
-- Ports above 49151 are also reserved for ephemeral connections. This
-- leaves us with a valid range of [[1024;49151]] for TCP ports.
instance Bounded (Port tag) where
    minBound = Port 1024
    maxBound = Port 49151

instance FromText (Port tag) where
    fromText t = do
        (p, unconsumed) <- bimap (const err) (first Port) (decimal t)
        unless (T.null unconsumed && p >= minBound && p <= maxBound) $ Left err
        return p
      where
        err = TextDecodingError
            $ "expected a TCP port number between "
            <> show (getPort minBound)
            <> " and "
            <> show (getPort maxBound)

instance ToText (Port tag) where
    toText (Port p) = toText p

{-------------------------------------------------------------------------------
                                  Logging
-------------------------------------------------------------------------------}

-- | Controls how much information to include in log output.
data Verbosity
    = Default
        -- ^ The default level of verbosity.
    | Quiet
        -- ^ Include less information in the log output.
    | Verbose
        -- ^ Include more information in the log output.
    deriving (Eq, Show)

-- | Convert a given 'Verbosity' level into a list of command line arguments
--   that can be passed through to a sub-process.
verbosityToArgs :: Verbosity -> [String]
verbosityToArgs = \case
    Default -> []
    Quiet   -> ["--quiet"]
    Verbose -> ["--verbose"]

-- | Map a given 'Verbosity' level onto a 'Severity' level.
verbosityToMinSeverity :: Verbosity -> Severity
verbosityToMinSeverity = \case
    Default -> Info
    Quiet   -> Error
    Verbose -> Debug

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer :: Severity -> Text -> IO (CM.Configuration, Trace IO Text)
initTracer minSeverity cmd = do
    c <- defaultConfigStdout
    CM.setMinSeverity c minSeverity
    CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
    tr <- appendName cmd =<< setupTrace (Right c) "cardano-wallet"
    pure (c, tr)

{-------------------------------------------------------------------------------
                            Unicode Terminal Helpers
-------------------------------------------------------------------------------}

-- | Override the system output encoding setting. This is needed because the CLI
-- prints UTF-8 characters regardless of the @LANG@ environment variable.
setUtf8Encoding :: IO ()
setUtf8Encoding = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

{-------------------------------------------------------------------------------
                            ANSI Terminal Helpers
-------------------------------------------------------------------------------}

-- | Print an error message in red
hPutErrLn :: Handle -> Text -> IO ()
hPutErrLn h msg = withSGR h (SetColor Foreground Vivid Red) $ do
    TIO.hPutStrLn h msg

-- | Like 'hPutErrLn' but with provided default 'Handle'
putErrLn :: Text -> IO ()
putErrLn = hPutErrLn stderr

{-------------------------------------------------------------------------------
                         Processing of Sensitive Data
-------------------------------------------------------------------------------}

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

-- | Prompt user and parse the input. Re-prompt on invalid inputs.
hGetLine
    :: Buildable e
    => (Handle, Handle)
    -> Text
    -> (Text -> Either e a)
    -> IO (a, Text)
hGetLine (hstdin, hstderr) prompt fromT = do
    TIO.hPutStr hstderr prompt
    txt <- TIO.hGetLine hstdin
    case fromT txt of
        Right a ->
            return (a, txt)
        Left e -> do
            hPutErrLn hstderr (pretty e)
            hGetLine (hstdin, hstderr) prompt fromT

-- | Like 'hGetLine' but with default handles
getLine
    :: Buildable e
    => Text
    -> (Text -> Either e a)
    -> IO (a, Text)
getLine = hGetLine (stdin, stderr)

-- | Gather user inputs until a newline is met, hiding what's typed with a
-- placeholder character.
hGetSensitiveLine
    :: Buildable e
    => (Handle, Handle)
    -> Text
    -> (Text -> Either e a)
    -> IO (a, Text)
hGetSensitiveLine (hstdin, hstderr) prompt fromT =
    withBuffering hstderr NoBuffering $
    withBuffering hstdin NoBuffering $
    withEcho hstdin False $ do
        TIO.hPutStr hstderr prompt
        txt <- getLineProtected '*'
        case fromT txt of
            Right a ->
                return (a, txt)
            Left e -> do
                hPutErrLn hstderr (pretty e)
                hGetSensitiveLine (hstdin, hstderr) prompt fromT
  where
    getLineProtected :: Char -> IO Text
    getLineProtected placeholder =
        getLineProtected' mempty
      where
        backspace = toEnum 127
        getLineProtected' line = do
            hGetChar hstdin >>= \case
                '\n' -> do
                    hPutChar hstderr '\n'
                    return line
                c | c == backspace ->
                    if T.null line
                        then getLineProtected' line
                        else do
                            hCursorBackward hstderr  1
                            hPutChar hstderr ' '
                            hCursorBackward hstderr 1
                            getLineProtected' (T.init line)
                c -> do
                    hPutChar hstderr placeholder
                    getLineProtected' (line <> T.singleton c)

-- | Like 'hGetSensitiveLine' but with default handles
getSensitiveLine
    :: Buildable e
    => Text
    -- ^ A message to prompt the user
    -> (Text -> Either e a)
    -- ^ An explicit parser from 'Text'
    -> IO (a, Text)
getSensitiveLine = hGetSensitiveLine (stdin, stderr)

{-------------------------------------------------------------------------------
                                Internals
-------------------------------------------------------------------------------}

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h buffering action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetBuffering h <* hSetBuffering h buffering)
    aLast = hSetBuffering h
    aBetween = const action

withEcho :: Handle -> Bool -> IO a -> IO a
withEcho h echo action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetEcho h <* hSetEcho h echo)
    aLast = hSetEcho h
    aBetween = const action

withSGR :: Handle -> SGR -> IO a -> IO a
withSGR h sgr action = hIsTerminalDevice h >>= \case
    True -> bracket aFirst aLast aBetween
    False -> action
  where
    aFirst = ([] <$ hSetSGR h [sgr])
    aLast = hSetSGR h
    aBetween = const action

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Decode API error messages and extract the corresponding message.
decodeError
    :: BL.ByteString
    -> Maybe Text
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj

-- | Make a parser optional
optionalE
    :: (Monoid m, Eq m)
    => (m -> Either e a)
    -> (m -> Either e (Maybe a))
optionalE parse = \case
    m | m == mempty -> Right Nothing
    m  -> Just <$> parse m
