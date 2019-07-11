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
    , runCli

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
    , Iso8601Time (..)
    , Service
    , MnemonicSize (..)
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
    , requireFilePath
    , resolveHomeDir
    , waitForService
    ) where

import Prelude hiding
    ( getLine )

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
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
    , ApiFee
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , Passphrase (..)
    , PassphraseMaxLength
    , PassphraseMinLength
    )
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
    ( Exception, bracket, catch )
import Control.Monad
    ( join, unless, void, when )
import Data.Aeson
    ( (.:) )
import Data.Bifunctor
    ( bimap )
import Data.Functor
    ( (<$), (<&>) )
import Data.List.Extra
    ( enumerate )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..), showT )
import Data.Text.Read
    ( decimal )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.ISO8601
    ( formatISO8601, parseISO8601 )
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
    , customExecParser
    , eitherReader
    , flag'
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , prefs
    , progDesc
    , showDefaultWith
    , showHelpOnEmpty
    , subparser
    , value
    )
import Servant
    ( (:<|>) (..), (:>), NoContent )
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
import System.Directory
    ( doesFileExist, getHomeDirectory )
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
-- >>> runCli $ cli $ cmdA <> cmdB <> cmdC
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

-- | Runs a specific command parser using appropriate preferences
runCli :: ParserInfo (IO ()) -> IO ()
runCli = join . customExecParser preferences
  where
    preferences = prefs showHelpOnEmpty

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
    { _size :: MnemonicSize
    }

-- | cardano-wallet mnemonic generate [--size=INT]
cmdMnemonicGenerate :: Mod CommandFields (IO ())
cmdMnemonicGenerate = command "generate" $ info (helper <*> cmd) $ mempty
    <> progDesc "Generate English BIP-0039 compatible mnemonic words."
  where
    cmd = exec . MnemonicGenerateArgs <$> sizeOption
    exec (MnemonicGenerateArgs n) = do
        m <- case n of
            MS_9  -> mnemonicToText @9  . entropyToMnemonic <$> genEntropy
            MS_12 -> mnemonicToText @12 . entropyToMnemonic <$> genEntropy
            MS_15 -> mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            MS_18 -> mnemonicToText @18 . entropyToMnemonic <$> genEntropy
            MS_21 -> mnemonicToText @21 . entropyToMnemonic <$> genEntropy
            MS_24 -> mnemonicToText @24 . entropyToMnemonic <$> genEntropy
        TIO.putStrLn $ T.unwords m

{-------------------------------------------------------------------------------
                            Commands - 'wallet'

  cardano-wallet wallet list [--port=INT]
  cardano-wallet wallet create [--port=INT] <name> [--address-pool-gap=INT]
  cardano-wallet wallet get [--port=INT] <wallet-id>
  cardano-wallet wallet update name [--port=INT] <wallet-id> STRING
  cardano-wallet wallet update passphrase [--port=INT] <wallet-id>
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
        wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
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

cmdWalletUpdate
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletUpdate = command "update" $ info (helper <*> cmds) $ mempty
    <> progDesc "Update a wallet."
  where
    cmds = subparser $ mempty
        <> cmdWalletUpdateName @t
        <> cmdWalletUpdatePassphrase @t

-- | Arguments for 'wallet update name' command
data WalletUpdateNameArgs = WalletUpdateNameArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _name :: WalletName
    }

-- | cardano-wallet wallet update name [--port=INT] <wallet-id> STRING
cmdWalletUpdateName
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletUpdateName = command "name" $ info (helper <*> cmd) $ mempty
    <> progDesc "Update a wallet's name."
  where
    cmd = fmap exec $ WalletUpdateNameArgs
        <$> portOption
        <*> walletIdArgument
        <*> walletNameArgument
    exec (WalletUpdateNameArgs wPort wId wName) = do
        runClient wPort Aeson.encodePretty $ putWallet (walletClient @t)
            (ApiT wId)
            (WalletPutData $ Just (ApiT wName))

-- | Arguments for 'wallet update passphrase' command
data WalletUpdatePassphraseArgs = WalletUpdatePassphraseArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    }

-- | cardano-wallet wallet update passphrase [--port=INT] <wallet-id>
cmdWalletUpdatePassphrase
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletUpdatePassphrase = command "passphrase" $ info (helper <*> cmd) $
    progDesc "Update a wallet's passphrase."
  where
    cmd = fmap exec $ WalletUpdatePassphraseArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletUpdatePassphraseArgs wPort wId) = do
        res <- sendRequest wPort $ getWallet (walletClient @t) $ ApiT wId
        case res of
            Right _ -> do
                wPassphraseOld <- getPassphrase
                    "Please enter your current passphrase: "
                wPassphraseNew <- getPassphraseWithConfirm
                    "Please enter a new passphrase: "
                runClient wPort (const mempty) $
                    putWalletPassphrase (walletClient @t) (ApiT wId) $
                        WalletPutPassphraseData
                            (ApiT wPassphraseOld)
                            (ApiT wPassphraseNew)
            Left _ ->
                handleResponse Aeson.encodePretty res

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
  cardano-wallet transaction fees [--port=INT] <wallet-id> --payment=PAYMENT...
-------------------------------------------------------------------------------}

-- | cardano-wallet transaction
cmdTransaction
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransaction = command "transaction" $ info (helper <*> cmds) mempty
  where
    cmds = subparser $ mempty
        <> cmdTransactionCreate @t
        <> cmdTransactionFees @t

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
                wPwd <- getPassphrase "Please enter your passphrase: "
                runClient wPort Aeson.encodePretty $ postTransaction
                    (walletClient @t)
                    (ApiT wId)
                    (PostTransactionData wPayments (ApiT wPwd))
            Left _ ->
                handleResponse Aeson.encodePretty res

cmdTransactionFees
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransactionFees = command "fees" $ info (helper <*> cmd) $ mempty
    <> progDesc "Estimate fees for a transaction."
  where
    cmd = fmap exec $ TransactionCreateArgs
        <$> portOption
        <*> walletIdArgument
        <*> fmap NE.fromList (some paymentOption)
    exec (TransactionCreateArgs wPort wId wPayments) = do
        res <- sendRequest wPort $ getWallet (walletClient @t) $ ApiT wId
        case res of
            Right _ -> do
                runClient wPort Aeson.encodePretty $ postTransactionFee
                    (walletClient @t)
                    (ApiT wId)
                    (PostTransactionFeeData wPayments)
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
    -> FilePath
    -> (Trace IO Text -> FilePath -> IO ())
    -> [Command]
    -> IO ()
execLaunch verbosity stateDir withStateDir commands = do
    installSignalHandlers
    (_, _, tracer) <- initTracer (verbosityToMinSeverity verbosity) "launch"
    setupStateDir (logInfo tracer) (withStateDir tracer) stateDir
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
sizeOption :: Parser MnemonicSize
sizeOption = optionT $ mempty
    <> long "size"
    <> metavar "INT"
    <> help "number of mnemonic words to generate."
    <> value MS_15
    <> showDefaultWith showT

-- | --state-dir=FILEPATH, default: ~/.cardano-wallet
stateDirOption :: Parser FilePath
stateDirOption = optionT $ mempty
    <> long "state-dir"
    <> metavar "DIR"
    <> help "write wallet state (blockchain and database) to this directory"
    <> value "$HOME/.cardano-wallet"
    <> showDefaultWith show

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
    , putWalletPassphrase
        :: ApiT WalletId
        -> WalletPutPassphraseData
        -> ClientM NoContent
    , postTransaction
        :: ApiT WalletId
        -> PostTransactionData t
        -> ClientM (ApiTransaction t)
    , postTransactionFee
        :: ApiT WalletId
        -> PostTransactionFeeData t
        -> ClientM ApiFee
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
            :<|> _putWalletPassphrase
            = wallets

        _postTransaction
            :<|> _listTransactions
            :<|> _postTransactionFee
            = transactions
    in
        WalletClient
            { listAddresses = _listAddresses
            , deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletPassphrase = _putWalletPassphrase
            , postTransaction = _postTransaction
            , postTransactionFee = _postTransactionFee
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

-- | Defines a point in time that can be formatted as and parsed from an
--   ISO 8601-compliant string.
--
newtype Iso8601Time = Iso8601Time
    { getIso8601Time :: UTCTime
    } deriving (Eq, Ord, Show)

instance ToText Iso8601Time where
    toText = T.pack . formatISO8601 . getIso8601Time

instance FromText Iso8601Time where
    fromText = maybe (Left err) (Right . Iso8601Time) . parseISO8601 . T.unpack
      where
        err = TextDecodingError
            "Unable to parse time argument. Expecting ISO 8601 format."

-- | Represents the number of words in a mnemonic sentence.
--
-- Only valid sizes are representable by this type.
--
data MnemonicSize
    = MS_9 | MS_12 | MS_15 | MS_18 | MS_21 | MS_24
    deriving (Bounded, Enum, Eq, Generic, Show)

instance ToText MnemonicSize where
    toText = T.pack . drop 3 .  show

instance FromText MnemonicSize where
    fromText t = case lookup t sizeMap of
        Just ms -> pure ms
        Nothing -> Left $ TextDecodingError $ mempty
            <> "Invalid mnemonic size. Expected one of: "
            <> T.unpack (T.intercalate ", " sizeTexts)
            <> "."
      where
        sizes = enumerate
        sizeMap = sizeTexts `zip` sizes
        sizeTexts = toText <$> sizes

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

-- | Wrapper type around 'Text' to make its semantic more explicit
newtype Service = Service Text deriving newtype IsString

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
initTracer
    :: Severity
    -> Text
    -> IO (CM.Configuration, Switchboard Text, Trace IO Text)
initTracer minSeverity cmd = do
    c <- defaultConfigStdout
    CM.setMinSeverity c minSeverity
    CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
    (tr0, sb) <- setupTrace_ c "cardano-wallet"
    tr <- appendName cmd tr0
    pure (c, sb, tr)

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

getPassphrase
    :: forall a . (PassphraseMinLength a, PassphraseMaxLength a)
    => Text
    -> IO (Passphrase a)
getPassphrase prompt = do
    let parser = fromText @(Passphrase a)
    fst <$> getSensitiveLine prompt parser

getPassphraseWithConfirm
    :: forall a . (PassphraseMinLength a, PassphraseMaxLength a)
    => Text
    -> IO (Passphrase a)
getPassphraseWithConfirm prompt = do
    wPwd <- getPassphrase prompt
    (wPwd', _) <- do
        let promptRepeat = "Enter the passphrase a second time: "
        let parser = fromText @(Passphrase a)
        getSensitiveLine promptRepeat parser
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

-- | Resolve '~' or '$HOME' in a 'FilePath' to their actual system value
resolveHomeDir :: FilePath -> IO FilePath
resolveHomeDir dir = do
    homeDir <- T.pack <$> getHomeDirectory
    return
        $ T.unpack
        $ T.replace "$HOME" homeDir
        $ T.replace "~" homeDir
        $ T.pack dir

-- | Wait for a service to become available on a given TCP port. Exit on failure
-- with a proper error message.
waitForService
    :: forall e. (Exception e)
    => Service
        -- ^ Name of the service
    -> (Switchboard Text, Trace IO Text)
        -- ^ A 'Trace' for logging
    -> Port "Node"
        -- ^ Underlying TCP port
    -> IO ()
        -- ^ Service we're waiting after.
    -> IO ()
waitForService (Service service) (sb, tracer) port action = do
    logInfo tracer $ mconcat
        [ "Waiting for "
        , service
        , " to be ready on tcp/"
        , T.pack (showT port)
        ]
    let handler (_ :: e) = do
            logAlert tracer $ mconcat
                [ "Waited too long for "
                , service
                , " to become available. Giving up!"
                ]
            shutdown sb
            putErrLn $ mconcat
                [ "Hint (1): If you're launching the wallet server on your own,"
                , " double-check that ", service
                , " is up-and-running and listening on the same port given to"
                , " '--node-port' (i.e. tcp/", T.pack (showT port), ")."
                ]
            putErrLn $ mconcat
                [ "Hint (2): Should you be starting from scratch, make"
                , " sure to have a good-enough network connection to"
                , " synchronize the first blocks in a timely manner."
                ]
            exitFailure
    action `catch` handler

-- | Look whether a particular filepath is correctly resolved on the filesystem.
-- This makes for a better user experience when passing wrong filepaths via
-- options or arguments, especially when they get forwarded to other services.
requireFilePath :: FilePath -> IO ()
requireFilePath path = doesFileExist path >>= \case
    True -> return ()
    False -> do
        putErrLn $ "I couldn't find any file at the given location: " <> pathT
        exitFailure
  where
    pathT = T.pack path

-- | Make a parser optional
optionalE
    :: (Monoid m, Eq m)
    => (m -> Either e a)
    -> (m -> Either e (Maybe a))
optionalE parse = \case
    m | m == mempty -> Right Nothing
    m  -> Just <$> parse m
