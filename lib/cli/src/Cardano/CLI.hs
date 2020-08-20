{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
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
    , cmdWalletCreate
    , cmdByronWalletCreate
    , cmdTransaction
    , cmdAddress
    , cmdStakePool
    , cmdNetwork
    , cmdVersion
    , cmdKey

    -- * Option & Argument Parsers
    , optionT
    , argumentT
    , databaseOption
    , hostPreferenceOption
    , listenOption
    , nodePortOption
    , nodePortMaybeOption
    , shutdownHandlerFlag
    , stateDirOption
    , syncToleranceOption
    , tlsOption
    , smashURLOption

    -- * Option parsers for configuring tracing
    , LoggingOptions (..)
    , helperTracing
    , loggingOptions
    , loggingSeverities
    , parseLoggingSeverity
    , loggingSeverityOrOffReader
    , loggingSeverityReader

    -- * Types
    , Service
    , TxId
    , Port (..)

    -- * Logging
    , withLogging

    -- * ANSI Terminal Helpers
    , putErrLn
    , hPutErrLn
    , enableWindowsANSI

    -- * Working with Sensitive Data
    , getLine
    , hGetLine
    , getSensitiveLine
    , hGetSensitiveLine

    -- * Helpers
    , decodeError
    , requireFilePath
    , getDataDir
    , setupDirectory
    , waitForService
    , WaitForServiceLog (..)
    ) where

import Prelude hiding
    ( getLine )

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
import Cardano.BM.Trace
    ( Trace, appendName, logDebug )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..), SomeMnemonic (..) )
import Cardano.Wallet.Api.Client
    ( AddressClient (..)
    , NetworkClient (..)
    , StakePoolClient (..)
    , TransactionClient (..)
    , WalletClient (..)
    )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), TlsConfiguration (..) )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount
    , AllowedMnemonics
    , ApiAccountPublicKey
    , ApiByronWallet
    , ApiMnemonicT (..)
    , ApiPostRandomAddressData (..)
    , ApiT (..)
    , ApiTxId (ApiTxId)
    , ApiWallet
    , ByronWalletPostData (..)
    , ByronWalletStyle (..)
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , SomeByronWalletPostData (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , fmtAllowedWords
    )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Passphrase (..)
    , PassphraseMaxLength
    , PassphraseMinLength
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, defaultAddressPoolGap )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( AddressState, Coin (..), Hash, SortOrder, WalletId, WalletName )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Version
    ( gitRevision, showFullVersion, version )
import Control.Applicative
    ( optional, some, (<|>) )
import Control.Arrow
    ( first, left )
import Control.Exception
    ( bracket, catch )
import Control.Monad
    ( join, unless, void, when, (>=>) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON (..), (.:), (.=) )
import Data.Bifunctor
    ( bimap )
import Data.Char
    ( toLower )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..), showT )
import Data.Text.Read
    ( decimal )
import Data.Void
    ( Void )
import Fmt
    ( Buildable, pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutNone
    )
import Network.URI
    ( URI (..), parseAbsoluteURI )
import Options.Applicative
    ( ArgumentFields
    , CommandFields
    , Mod
    , OptionFields
    , ParseError (InfoMsg)
    , Parser
    , ParserInfo
    , abortOption
    , argument
    , auto
    , command
    , customExecParser
    , eitherReader
    , flag
    , flag'
    , header
    , help
    , helpDoc
    , helper
    , hidden
    , info
    , long
    , metavar
    , option
    , prefs
    , progDesc
    , showDefaultWith
    , showHelpOnEmpty
    , str
    , strOption
    , subparser
    , switch
    , value
    )
import Options.Applicative.Help.Pretty
    ( string, vsep )
import Options.Applicative.Types
    ( ReadM (..), readerAsk )
import Servant.Client.Core
    ( ClientError (..), responseBody )
import Servant.Client.Streaming
    ( BaseUrl (..), ClientM, Scheme (..), mkClientEnv, withClientM )
import Servant.Types.SourceT
    ( runSourceT )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hCursorBackward
    , hSetSGR
    , hSupportsANSIWithoutEmulation
    )
import System.Directory
    ( XdgDirectory (..)
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getXdgDirectory
    )
import System.Exit
    ( exitFailure, exitSuccess )
import System.FilePath
    ( (</>) )
import System.Info
    ( os )
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
    , stderr
    , stdin
    , stdout
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Command.Key as Key
import qualified Command.RecoveryPhrase as RecoveryPhrase
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bi
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
        , "an active server and can be run offline (e.g. 'recovery-phrase generate')."
        ])

-- | Runs a specific command parser using appropriate preferences
runCli :: ParserInfo (IO ()) -> IO ()
runCli = join . customExecParser preferences
  where
    preferences = prefs showHelpOnEmpty

{-------------------------------------------------------------------------------
                            Commands - 'key'
-------------------------------------------------------------------------------}

cmdKey :: Mod CommandFields (IO ())
cmdKey = Key.mod Key.run

{-------------------------------------------------------------------------------
                            Commands - 'recovery-phrase'
-------------------------------------------------------------------------------}

cmdMnemonic :: Mod CommandFields (IO ())
cmdMnemonic = RecoveryPhrase.mod RecoveryPhrase.run

{-------------------------------------------------------------------------------
                            Commands - 'wallet'
-------------------------------------------------------------------------------}

type CmdWalletCreate wallet = WalletClient wallet -> Mod CommandFields (IO ())

cmdWallet
    :: ToJSON wallet
    => CmdWalletCreate wallet
    -> WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWallet cmdCreate mkClient =
    command "wallet" $ info (helper <*> cmds) $ mempty
        <> progDesc "About wallets"
  where
    cmds = subparser $ mempty
        <> cmdWalletList mkClient
        <> cmdCreate mkClient
        <> cmdWalletGet mkClient
        <> cmdWalletUpdate mkClient
        <> cmdWalletDelete mkClient
        <> cmdWalletGetUtxoStatistics mkClient

-- | Arguments for 'wallet list' command
newtype WalletListArgs = WalletListArgs
    { _port :: Port "Wallet"
    }

cmdWalletList
    :: ToJSON wallet
    => WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletList mkClient =
    command "list" $ info (helper <*> cmd) $ mempty
        <> progDesc "List all known wallets."
  where
    cmd = fmap exec $ WalletListArgs
        <$> portOption
    exec (WalletListArgs wPort) = do
        runClient wPort (pure . Aeson.encodePretty) $ listWallets mkClient

cmdWalletCreate
    :: WalletClient ApiWallet
    -> Mod CommandFields (IO ())
cmdWalletCreate mkClient =
    command "create" $ info (helper <*> cmds) $ mempty
        <> progDesc "Create a new wallet."
  where
    cmds = subparser $ mempty
        <> cmdWalletCreateFromMnemonic mkClient
        <> cmdWalletCreateFromPublicKey mkClient

cmdByronWalletCreate
    :: WalletClient ApiByronWallet
    -> Mod CommandFields (IO ())
cmdByronWalletCreate mkClient =
    command "create" $ info (helper <*> cmds) $ mempty
        <> progDesc "Create a new Byron wallet."
  where
    cmds = subparser $ mempty
        <> cmdByronWalletCreateFromMnemonic mkClient

data ByronWalletCreateFromMnemonicArgs = ByronWalletCreateFromMnemonicArgs
    { _port :: Port "Wallet"
    , _name :: WalletName
    , _style :: ByronWalletStyle
    }

cmdByronWalletCreateFromMnemonic
    :: WalletClient ApiByronWallet
    -> Mod CommandFields (IO ())
cmdByronWalletCreateFromMnemonic mkClient =
    command "from-recovery-phrase" $ info (helper <*> cmd) $ mempty
        <> progDesc "Create a new wallet using a recovery phrase."
  where
    cmd = fmap exec $ ByronWalletCreateFromMnemonicArgs
        <$> portOption
        <*> walletNameArgument
        <*> walletStyleOption Icarus [Random,Icarus,Trezor,Ledger]
    exec (ByronWalletCreateFromMnemonicArgs wPort wName wStyle) = case wStyle of
        Random -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = mkSomeMnemonic @(AllowedMnemonics 'Random) . T.words
                fst <$> getLine @SomeMnemonic (T.pack prompt) (left show . parser)
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort (pure . Aeson.encodePretty) $ postWallet mkClient $
                RandomWalletFromMnemonic $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

        Icarus -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = mkSomeMnemonic @(AllowedMnemonics 'Icarus) . T.words
                fst <$> getLine @SomeMnemonic (T.pack prompt) (left show . parser)
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort (pure . Aeson.encodePretty) $ postWallet mkClient $
                SomeIcarusWallet $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

        Trezor -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = mkSomeMnemonic @(AllowedMnemonics 'Trezor) . T.words
                fst <$> getLine @SomeMnemonic (T.pack prompt) (left show . parser)
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort (pure . Aeson.encodePretty) $ postWallet mkClient $
                SomeTrezorWallet $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

        Ledger -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = mkSomeMnemonic @(AllowedMnemonics 'Ledger) . T.words
                fst <$> getLine @SomeMnemonic (T.pack prompt) (left show . parser)
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort (pure . Aeson.encodePretty) $ postWallet mkClient $
                SomeLedgerWallet $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

-- | Arguments for 'wallet create' command
data WalletCreateArgs = WalletCreateArgs
    { _port :: Port "Wallet"
    , _name :: WalletName
    , _gap :: AddressPoolGap
    }

cmdWalletCreateFromMnemonic
    :: WalletClient ApiWallet
    -> Mod CommandFields (IO ())
cmdWalletCreateFromMnemonic mkClient =
    command "from-recovery-phrase" $ info (helper <*> cmd) $ mempty
        <> progDesc "Create a new wallet using a recovery phrase."
  where
    cmd = fmap exec $ WalletCreateArgs
        <$> portOption
        <*> walletNameArgument
        <*> poolGapOption
    exec (WalletCreateArgs wPort wName wGap) = do
        wSeed <- do
            let prompt = "Please enter a 15–24 word recovery phrase: "
            let parser = mkSomeMnemonic @'[15,18,21,24] . T.words
            fst <$> getLine @SomeMnemonic prompt (left show . parser)
        wSndFactor <- do
            let prompt =
                    "(Enter a blank line if you do not wish to use a second " <>
                    "factor.)\n" <>
                    "Please enter a 9–12 word second factor: "
            let parser =
                    optionalE (mkSomeMnemonic @'[9,12]) . T.words
            fst <$> getLine @(Maybe SomeMnemonic) prompt (left show . parser)
        wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
        runClient wPort (pure . Aeson.encodePretty) $ postWallet mkClient $
            WalletOrAccountPostData $ Left $ WalletPostData
                (Just $ ApiT wGap)
                (ApiMnemonicT wSeed)
                (ApiMnemonicT <$> wSndFactor)
                (ApiT wName)
                (ApiT wPwd)

-- | Arguments for 'wallet create from-public-key' command
data WalletCreateFromPublicKeyArgs = WalletCreateFromPublicKeyArgs
    { _port :: Port "Wallet"
    , _name :: WalletName
    , _gap :: AddressPoolGap
    , _key :: ApiAccountPublicKey
    }

cmdWalletCreateFromPublicKey
    :: WalletClient ApiWallet
    -> Mod CommandFields (IO ())
cmdWalletCreateFromPublicKey mkClient =
    command "from-public-key" $ info (helper <*> cmd) $ mempty
    <> progDesc "Create a wallet using a public account key."
  where
    cmd = fmap exec $ WalletCreateFromPublicKeyArgs
        <$> portOption
        <*> walletNameArgument
        <*> poolGapOption
        <*> accPubKeyArgument
    exec (WalletCreateFromPublicKeyArgs wPort wName wGap wAccPubKey) =
        runClient wPort (pure . Aeson.encodePretty) $ postWallet mkClient $
            WalletOrAccountPostData $ Right $ AccountPostData
                (ApiT wName)
                wAccPubKey
                (Just $ ApiT wGap)

-- | Arguments for 'wallet get' command
data WalletGetArgs = WalletGetArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    }

cmdWalletGet
    :: ToJSON wallet
    => WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletGet mkClient =
    command "get" $ info (helper <*> cmd) $ mempty
        <> progDesc "Fetch the wallet with specified id."
  where
    cmd = fmap exec $ WalletGetArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletGetArgs wPort wId) = do
        runClient wPort (pure . Aeson.encodePretty) $ getWallet mkClient $
            ApiT wId

cmdWalletUpdate
    :: ToJSON wallet
    => WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletUpdate mkClient =
    command "update" $ info (helper <*> cmds) $ mempty
        <> progDesc "Update a wallet."
  where
    cmds = subparser $ mempty
        <> cmdWalletUpdateName mkClient
        <> cmdWalletUpdatePassphrase mkClient

-- | Arguments for 'wallet update name' command
data WalletUpdateNameArgs = WalletUpdateNameArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _name :: WalletName
    }

cmdWalletUpdateName
    :: ToJSON wallet
    => WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletUpdateName mkClient =
    command "name" $ info (helper <*> cmd) $ mempty
        <> progDesc "Update a wallet's name."
  where
    cmd = fmap exec $ WalletUpdateNameArgs
        <$> portOption
        <*> walletIdArgument
        <*> walletNameArgument
    exec (WalletUpdateNameArgs wPort wId wName) = do
        runClient wPort (pure . Aeson.encodePretty) $ putWallet mkClient
            (ApiT wId)
            (WalletPutData $ Just (ApiT wName))

-- | Arguments for 'wallet update passphrase' command
data WalletUpdatePassphraseArgs = WalletUpdatePassphraseArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    }

cmdWalletUpdatePassphrase
    :: ToJSON wallet
    => WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletUpdatePassphrase mkClient =
    command "passphrase" $ info (helper <*> cmd) $ mempty
        <> progDesc "Update a wallet's passphrase."
  where
    cmd = fmap exec $ WalletUpdatePassphraseArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletUpdatePassphraseArgs wPort wId) = do
        sendRequest wPort (getWallet mkClient (ApiT wId)) $ \case
            Right _ -> do
                wPassphraseOld <- getPassphrase
                    "Please enter your current passphrase: "
                wPassphraseNew <- getPassphraseWithConfirm
                    "Please enter a new passphrase: "
                runClient wPort (const mempty) $
                    putWalletPassphrase mkClient (ApiT wId) $
                        WalletPutPassphraseData
                            (ApiT wPassphraseOld)
                            (ApiT wPassphraseNew)
            res@(Left _) ->
                handleResponse (pure . Aeson.encodePretty) res

-- | Arguments for 'wallet delete' command
data WalletDeleteArgs = WalletDeleteArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    }

cmdWalletDelete
    :: WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletDelete mkClient =
    command "delete" $ info (helper <*> cmd) $ mempty
        <> progDesc "Deletes wallet with specified wallet id."
  where
    cmd = fmap exec $ WalletDeleteArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletDeleteArgs wPort wId) = do
        runClient wPort (pure . const "") $ deleteWallet mkClient $
            ApiT wId

cmdWalletGetUtxoStatistics
    :: ToJSON wallet
    => WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWalletGetUtxoStatistics mkClient =
    command "utxo" $ info (helper <*> cmd) $ mempty
        <> progDesc "Get UTxO statistics for the wallet with specified id."
  where
    cmd = fmap exec $ WalletGetArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletGetArgs wPort wId) = do
        sendRequest wPort (getWallet mkClient (ApiT wId)) $ \case
            Right _ -> do
                runClient wPort (pure . Aeson.encodePretty) $
                    getWalletUtxoStatistics mkClient (ApiT wId)
            res@(Left _) ->
                handleResponse (pure . Aeson.encodePretty) res

{-------------------------------------------------------------------------------
                            Commands - 'transaction'
-------------------------------------------------------------------------------}

-- | cardano-wallet transaction
cmdTransaction
    :: ToJSON wallet
    => TransactionClient
    -> WalletClient wallet
    -> Mod CommandFields (IO ())
cmdTransaction mkTxClient mkWalletClient =
    command "transaction" $ info (helper <*> cmds) $ mempty
        <> progDesc "About transactions"
  where
    cmds = subparser $ mempty
        <> cmdTransactionCreate mkTxClient mkWalletClient
        <> cmdTransactionFees mkTxClient mkWalletClient
        <> cmdTransactionList mkTxClient
        <> cmdTransactionSubmit mkTxClient
        <> cmdTransactionForget mkTxClient
        <> cmdTransactionGet mkTxClient

-- | Arguments for 'transaction create' command
data TransactionCreateArgs t = TransactionCreateArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _payments :: NonEmpty Text
    }

cmdTransactionCreate
    :: ToJSON wallet
    => TransactionClient
    -> WalletClient wallet
    -> Mod CommandFields (IO ())
cmdTransactionCreate mkTxClient mkWalletClient =
    command "create" $ info (helper <*> cmd) $ mempty
        <> progDesc "Create and submit a new transaction."
  where
    cmd = fmap exec $ TransactionCreateArgs
        <$> portOption
        <*> walletIdArgument
        <*> fmap NE.fromList (some paymentOption)
    exec (TransactionCreateArgs wPort wId wAddressAmounts) = do
        wPayments <- either (fail . getTextDecodingError) pure $
            traverse (fromText @(AddressAmount Text)) wAddressAmounts
        sendRequest wPort (getWallet mkWalletClient (ApiT wId)) $ \case
            Right _ -> do
                wPwd <- getPassphrase @"raw" "Please enter your passphrase: "
                runClient wPort (pure . Aeson.encodePretty) $ postTransaction
                    mkTxClient
                    (ApiT wId)
                    (Aeson.object
                        [ "payments" .= wPayments
                        , "passphrase" .= ApiT wPwd
                        ]
                    )
            res@(Left _) ->
                handleResponse (pure . Aeson.encodePretty) res

cmdTransactionFees
    :: ToJSON wallet
    => TransactionClient
    -> WalletClient wallet
    -> Mod CommandFields (IO ())
cmdTransactionFees mkTxClient mkWalletClient =
    command "fees" $ info (helper <*> cmd) $ mempty
        <> progDesc "Estimate fees for a transaction."
  where
    cmd = fmap exec $ TransactionCreateArgs
        <$> portOption
        <*> walletIdArgument
        <*> fmap NE.fromList (some paymentOption)
    exec (TransactionCreateArgs wPort wId wAddressAmounts) = do
        wPayments <- either (fail . getTextDecodingError) pure $
            traverse (fromText @(AddressAmount Text)) wAddressAmounts
        sendRequest wPort (getWallet mkWalletClient (ApiT wId)) $ \case
            Right _ -> do
                runClient wPort (pure . Aeson.encodePretty) $ postTransactionFee
                    mkTxClient
                    (ApiT wId)
                    (Aeson.object [ "payments" .= wPayments ])
            res@(Left _) ->
                handleResponse (pure . Aeson.encodePretty) res

-- | Arguments for 'transaction list' command.
data TransactionListArgs = TransactionListArgs
    { _port :: Port "Wallet"
    , _walletId :: WalletId
    , _timeRangeStart :: Maybe Iso8601Time
    , _timeRangeEnd :: Maybe Iso8601Time
    , _sortOrder :: Maybe SortOrder
    }

cmdTransactionList
    :: TransactionClient
    -> Mod CommandFields (IO ())
cmdTransactionList mkTxClient =
    command "list" $ info (helper <*> cmd) $ mempty
        <> progDesc "List the transactions associated with a wallet."
  where
    cmd = fmap exec $ TransactionListArgs
        <$> portOption
        <*> walletIdArgument
        <*> optional timeRangeStartOption
        <*> optional timeRangeEndOption
        <*> optional sortOrderOption
    exec (TransactionListArgs wPort wId mTimeRangeStart mTimeRangeEnd mOrder) =
        runClient wPort (pure . Aeson.encodePretty) $ listTransactions
            mkTxClient
            (ApiT wId)
            mTimeRangeStart
            mTimeRangeEnd
            (ApiT <$> mOrder)

-- | Arguments for 'transaction submit' command
data TransactionSubmitArgs = TransactionSubmitArgs
    { _port :: Port "Wallet"
    , _payload :: PostExternalTransactionData
    }

cmdTransactionSubmit
    :: TransactionClient
    -> Mod CommandFields (IO ())
cmdTransactionSubmit mkTxClient =
    command "submit" $ info (helper <*> cmd) $ mempty
        <> progDesc "Submit an externally-signed transaction."
  where
    cmd = fmap exec $ TransactionSubmitArgs
        <$> portOption
        <*> transactionSubmitPayloadArgument
    exec (TransactionSubmitArgs wPort wPayload) = do
        runClient wPort (pure . Aeson.encodePretty) $
            postExternalTransaction mkTxClient wPayload

-- | Arguments for 'transaction forget' command
data TransactionForgetArgs = TransactionForgetArgs
    { _port :: Port "Wallet"
    , _wid :: WalletId
    , _txid :: TxId
    }

cmdTransactionForget
    :: TransactionClient
    -> Mod CommandFields (IO ())
cmdTransactionForget mkClient =
    command "forget" $ info (helper <*> cmd) $ mempty
        <> progDesc "Forget a pending transaction with specified id."
  where
    cmd = fmap exec $ TransactionForgetArgs
        <$> portOption
        <*> walletIdArgument
        <*> transactionIdArgument
    exec (TransactionForgetArgs wPort wId txId) = do
        runClient wPort (const mempty) $ deleteTransaction mkClient
            (ApiT wId)
            (ApiTxId $ ApiT $ getTxId txId)

-- | Arguments for 'transaction get' command
data TransactionGetArgs = TransactionGetArgs
    { _port :: Port "Wallet"
    , _wid :: WalletId
    , _txid :: TxId
    }

cmdTransactionGet
    :: TransactionClient
    -> Mod CommandFields (IO ())
cmdTransactionGet mkClient =
    command "get" $ info (helper <*> cmd) $ mempty
        <> progDesc "Get a transaction with specified id."
  where
    cmd = fmap exec $ TransactionGetArgs
        <$> portOption
        <*> walletIdArgument
        <*> transactionIdArgument
    exec (TransactionGetArgs wPort wId txId) = do
        runClient wPort (pure . Aeson.encodePretty) $ getTransaction mkClient
            (ApiT wId)
            (ApiTxId $ ApiT $ getTxId txId)

{-------------------------------------------------------------------------------
                            Commands - 'address'
-------------------------------------------------------------------------------}

cmdAddress
    :: AddressClient
    -> Mod CommandFields (IO ())
cmdAddress mkClient =
    command "address" $ info (helper <*> cmds) $ mempty
        <> progDesc "About addresses"
  where
    cmds = subparser $ mempty
        <> cmdAddressList mkClient
        <> cmdAddressCreate mkClient
        <> cmdAddressImport mkClient

-- | Arguments for 'address list' command
data AddressListArgs = AddressListArgs
    { _port :: Port "Wallet"
    , _state :: Maybe AddressState
    , _id :: WalletId
    }

cmdAddressList
    :: AddressClient
    -> Mod CommandFields (IO ())
cmdAddressList mkClient =
    command "list" $ info (helper <*> cmd) $ mempty
        <> progDesc "List all known addresses of a given wallet."
  where
    cmd = fmap exec $ AddressListArgs
        <$> portOption
        <*> optional addressStateOption
        <*> walletIdArgument
    exec (AddressListArgs wPort wState wId) = do
        let runStream = fmap Aeson.encodePretty . unsafeRunExceptT . runSourceT
        runClient wPort runStream $ listAddresses mkClient
            (ApiT wId)
            (ApiT <$> wState)

-- | Arguments for 'address create' command
data AddressCreateArgs = AddressCreateArgs
    { _port :: Port "Wallet"
    , _addressIndex :: Maybe (Index 'Hardened 'AddressK)
    , _id :: WalletId
    }

cmdAddressCreate
    :: AddressClient
    -> Mod CommandFields (IO ())
cmdAddressCreate mkClient =
    command "create" $ info (helper <*> cmd) $ mempty
        <> progDesc "Create a new random address. Only available for random wallets. \
            \The address index is optional, give none to let the wallet generate \
            \a random one."
  where
    cmd = fmap exec $ AddressCreateArgs
        <$> portOption
        <*> optional addressIndexOption
        <*> walletIdArgument
    exec (AddressCreateArgs wPort wIx wId) = do
        pwd <- getPassphrase "Please enter your passphrase: "
        runClient wPort (pure . Aeson.encodePretty) $ postRandomAddress mkClient
            (ApiT wId)
            (ApiPostRandomAddressData (ApiT pwd) (ApiT <$> wIx))

-- | Arguments for 'address import' command
data AddressImportArgs = AddressImportArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _addr :: Text
    }

cmdAddressImport
    :: AddressClient
    -> Mod CommandFields (IO ())
cmdAddressImport mkClient =
    command "import" $ info (helper <*> cmd) $ mempty
        <> progDesc "Import a random address generated elsewhere. Only available \
            \for random wallets. The address must belong to the target wallet."
  where
    cmd = fmap exec $ AddressImportArgs
        <$> portOption
        <*> walletIdArgument
        <*> addressIdArgument
    exec (AddressImportArgs wPort wId addr) = do
        runClient wPort (pure . const "") $ putRandomAddress mkClient (ApiT wId) addr

{-------------------------------------------------------------------------------
                            Commands - 'version'
-------------------------------------------------------------------------------}

cmdVersion :: Mod CommandFields (IO ())
cmdVersion = command "version" $ info cmd $ mempty
    <> progDesc "Show the program's version."
  where
    cmd = pure exec
    exec = do
        putStrLn $ showFullVersion version gitRevision
        exitSuccess

{-------------------------------------------------------------------------------
                            Commands - 'stake-pool'
-------------------------------------------------------------------------------}

cmdStakePool
    :: ToJSON apiPool
    => StakePoolClient apiPool
    -> Mod CommandFields (IO ())
cmdStakePool mkClient =
    command "stake-pool" $ info (helper <*> cmds) $ mempty
        <> progDesc "About stake pools"
  where
    cmds = subparser $ mempty
        <> cmdStakePoolList mkClient

-- | Arguments for 'stake-pool list' command
data StakePoolListArgs = StakePoolListArgs
    { _port :: Port "Wallet"
    , _stake :: Maybe Coin
    }

cmdStakePoolList
    :: ToJSON apiPool
    => StakePoolClient apiPool
    -> Mod CommandFields (IO ())
cmdStakePoolList mkClient =
    command "list" $ info (helper <*> cmd) $ mempty
        <> progDesc "List all known stake pools."
  where
    cmd = fmap exec $ StakePoolListArgs
        <$> portOption <*> stakeOption
    exec (StakePoolListArgs wPort stake) = do
        runClient wPort (pure . Aeson.encodePretty) $ listPools mkClient (ApiT <$> stake)

{-------------------------------------------------------------------------------
                            Commands - 'network'
-------------------------------------------------------------------------------}

cmdNetwork
    :: NetworkClient
    -> Mod CommandFields (IO ())
cmdNetwork mkClient =
    command "network" $ info (helper <*> cmds) $ mempty
        <> progDesc "About the network"
  where
    cmds = subparser $ mempty
        <> cmdNetworkInformation mkClient
        <> cmdNetworkParameters mkClient
        <> cmdNetworkClock mkClient

-- | Arguments for 'network information' command
newtype NetworkInformationArgs = NetworkInformationArgs
    { _port :: Port "Wallet"
    }

cmdNetworkInformation
    :: NetworkClient
    -> Mod CommandFields (IO ())
cmdNetworkInformation mkClient =
    command "information" $ info (helper <*> cmd) $ mempty
        <> progDesc "View network information."
  where
    cmd = fmap exec $ NetworkInformationArgs
        <$> portOption
    exec (NetworkInformationArgs wPort) = do
        runClient wPort (pure . Aeson.encodePretty) (networkInformation mkClient)

-- | Arguments for 'network parameters' command
newtype NetworkParametersArgs = NetworkParametersArgs
    { _port :: Port "Wallet"
    }

cmdNetworkParameters
    :: NetworkClient
    -> Mod CommandFields (IO ())
cmdNetworkParameters mkClient =
    command "parameters" $ info (helper <*> cmd) $ mempty
        <> progDesc "View network parameters for the current epoch."
  where
    cmd = fmap exec $ NetworkParametersArgs
        <$> portOption
    exec (NetworkParametersArgs wPort) = do
        runClient wPort (pure . Aeson.encodePretty) $ networkParameters mkClient

-- | Arguments for 'network clock' command
data NetworkClockArgs = NetworkClockArgs
    { _port :: Port "Wallet"
    , _forceNtpCheck :: Bool
    }

cmdNetworkClock
    :: NetworkClient
    -> Mod CommandFields (IO ())
cmdNetworkClock mkClient =
    command "clock" $ info (helper <*> cmd) $ mempty
        <> progDesc "View NTP offset."
  where
    cmd = fmap exec $ NetworkClockArgs
        <$> portOption
        <*> forceNtpCheckOption
    exec (NetworkClockArgs wPort forceNtpCheck) = do
        runClient wPort (pure . Aeson.encodePretty) $ networkClock mkClient forceNtpCheck

{-------------------------------------------------------------------------------
                            Commands - 'launch'
-------------------------------------------------------------------------------}

-- | Initialize a directory to store data such as blocks or the wallet databases
setupDirectory :: (Text -> IO ()) -> FilePath -> IO ()
setupDirectory logT dir = do
    exists <- doesFileExist dir
    when exists $ do
        putErrLn $ mconcat
                [ T.pack dir <> " must be a directory, but it is"
                , " a file. Exiting."
                ]
        exitFailure
    doesDirectoryExist dir >>= \case
        True -> logT $ "Using directory: " <> T.pack dir
        False -> do
            logT $ "Creating directory: " <> T.pack dir
            let createParentIfMissing = True
            createDirectoryIfMissing createParentIfMissing dir

{-------------------------------------------------------------------------------
                              Options & Arguments
-------------------------------------------------------------------------------}

-- | --state=STRING
addressStateOption :: Parser AddressState
addressStateOption = optionT $ mempty
    <> long "state"
    <> metavar "STRING"
    <> help "only addresses with the given state: either 'used' or 'unused'."

-- | --database=DIR
databaseOption :: Parser FilePath
databaseOption = optionT $ mempty
    <> long "database"
    <> metavar "DIR"
    <> help "use this directory for storing wallets. Run in-memory otherwise."

-- | [--listen-address=HOSTSPEC], default: 127.0.0.1
hostPreferenceOption :: Parser HostPreference
hostPreferenceOption = option str $ mempty
    <> long "listen-address"
    <> metavar "HOST"
    <> help
        ("Specification of which host to the bind API server to. " <>
         "Can be an IPv[46] address, hostname, or '*'.")
    <> value "127.0.0.1"
    <> showDefaultWith (const "127.0.0.1")

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
nodePortOption = optionT $ optionNodePort <> value (Port 8080)

-- | [--node-port=INT], default: use any available port
nodePortMaybeOption :: Parser (Maybe (Port "Node"))
nodePortMaybeOption = optional $ optionT optionNodePort

optionNodePort :: Mod OptionFields (Port "Node")
optionNodePort = mempty
    <> long "node-port"
    <> metavar "INT"
    <> help "port used for communicating with the target node."
    <> showDefaultWith showT

-- | --payment=PAYMENT
paymentOption :: Parser Text
paymentOption = optionT $ mempty
    <> long "payment"
    <> metavar "PAYMENT"
    <> help
        ("address to send to and amount to send separated by @" <>
        ", e.g. '<amount>@<address>'")

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

-- | [--shutdown-handler]
shutdownHandlerFlag :: Parser Bool
shutdownHandlerFlag = switch
    (  long "shutdown-handler"
    <> help "Enable the clean shutdown handler (exits when stdin is closed)" )

-- | --state-dir=DIR, default: ~/.cardano-wallet/$backend/$network
stateDirOption :: FilePath -> Parser (Maybe FilePath)
stateDirOption backendDir = optional $ strOption $ mempty
    <> long "state-dir"
    <> metavar "DIR"
    <> help (mconcat
        [ "write wallet state (blockchain and database) to this directory"
        , " (default: ", defaultDir, ")"
        ])
  where
    defaultDir = backendDir </> "NETWORK"

-- | --sync-tolerance=DURATION, default: 300s
syncToleranceOption :: Parser SyncTolerance
syncToleranceOption = optionT $ mempty
    <> long "sync-tolerance"
    <> metavar "DURATION"
    <> help (mconcat
        [ "time duration within which we consider being synced with the "
        , "network. Expressed in seconds with a trailing 's'."
        ])
    <> value fiveMinutes
    <> showDefaultWith showT
  where
    fiveMinutes = SyncTolerance (5*60)

-- | [--start=TIME]
timeRangeStartOption :: Parser Iso8601Time
timeRangeStartOption = optionT $ mempty
    <> long "start"
    <> metavar "TIME"
    <> help (mconcat
        [ "start time (ISO 8601 date-and-time format:"
        , " basic or extended, e.g. 2012-09-25T10:15:00Z)."
        ])
    <> showDefaultWith showT

-- | [--end=TIME]
timeRangeEndOption :: Parser Iso8601Time
timeRangeEndOption = optionT $ mempty
    <> long "end"
    <> metavar "TIME"
    <> help (mconcat
        [ "end time (ISO 8601 date-and-time format:"
        , " basic or extended, e.g. 2016-11-21T10:15:00Z)."
        ])
    <> showDefaultWith showT

-- | [--order=ORDER]
sortOrderOption :: Parser SortOrder
sortOrderOption = optionT $ mempty
    <> long "order"
    <> metavar "ORDER"
    <> help "specifies a sort order, either 'ascending' or 'descending'."
    <> showDefaultWith showT

-- | [--force-ntp-check]
forceNtpCheckOption :: Parser Bool
forceNtpCheckOption = flag False True $ mempty
    <> long "force-ntp-check"
    <> help "When set, will block and force an NTP check with the server. \
            \Otherwise, uses an available cached result."

-- | The lower-case names of all 'Severity' values.
loggingSeverities :: [(String, Severity)]
loggingSeverities = [(toLower <$> show s, s) | s <- [minBound .. maxBound]]

parseLoggingSeverity :: String -> Either String Severity
parseLoggingSeverity arg =
    case lookup (map toLower arg) loggingSeverities of
        Just sev -> pure sev
        Nothing -> fail $ "unknown logging severity: " ++ arg

loggingSeverityReader :: ReadM Severity
loggingSeverityReader = eitherReader parseLoggingSeverity

loggingSeverityOrOffReader :: ReadM (Maybe Severity)
loggingSeverityOrOffReader = do
    arg <- readerAsk
    case map toLower arg of
        "off" -> pure Nothing
        _ -> Just <$> loggingSeverityReader

-- | [--wallet-style=WALLET_STYLE]
--
-- Note that we in the future might replace the type @ByronWalletStyle@ with
-- another type, to include Jormungandr keys.
walletStyleOption
    :: ByronWalletStyle
        -- ^ Default style
    -> [ByronWalletStyle]
        -- ^ Accepted styles
    -> Parser ByronWalletStyle
walletStyleOption defaultStyle accepted = option (eitherReader fromTextS)
    ( long "wallet-style"
    <> metavar "WALLET_STYLE"
    <> helpDoc (Just (vsep typeOptions))
    <> value defaultStyle
    )
  where
    typeOptions = string <$>
        ( "Any of the following (default: " <> T.unpack (toText defaultStyle) <> ")"
        ) : map prettyStyle accepted

    prettyStyle s =
        "  " ++ T.unpack (toText s) ++ " (" ++ fmtAllowedWords s ++ ")"

addressIndexOption
    :: FromText (Index derivation level)
    => Parser (Index derivation level)
addressIndexOption = optionT $ mempty
    <> long "address-index"
    <> metavar "INDEX"
    <> help "A derivation index for the address"

tlsOption
    :: Parser TlsConfiguration
tlsOption = TlsConfiguration
    <$> tlsCaCertOption
    <*> tlsSvCertOption
    <*> tlsSvKeyOption
  where
    tlsCaCertOption = optionT $ mempty
        <> long "tls-ca-cert"
        <> metavar "FILE"
        <> help "A x.509 Certificate Authority (CA) certificate."

    tlsSvCertOption = optionT $ mempty
        <> long "tls-sv-cert"
        <> metavar "FILE"
        <> help "A x.509 Server (SV) certificate."

    tlsSvKeyOption = optionT $ mempty
        <> long "tls-sv-key"
        <> metavar "FILE"
        <> help "The RSA Server key which signed the x.509 server certificate."

smashURLOption
    :: Parser URI
smashURLOption = option (eitherReader reader) $ mempty
    <> long "smash-url"
    <> metavar "URL"
    <> help "Optional HTTP(S) address of a \
            \Stakepool Metadata Aggregation Server."
  where
    reader :: String -> Either String URI
    reader = maybe (Left err) Right . (parseAbsoluteURI >=> isHttp)

    isHttp uri
        | uriScheme uri `elem` ["http:", "https:"] = Just uri
        | otherwise = Nothing

    err :: String
    err =
        "Invalid URI. Make sure it is a well-formed \
        \ absolute http or https URI."

-- | <wallet-id=WALLET_ID>
walletIdArgument :: Parser WalletId
walletIdArgument = argumentT $ mempty
    <> metavar "WALLET_ID"

-- | <stake=STAKE>
stakeOption :: Parser (Maybe Coin)
stakeOption = optional $ optionT $ mempty
    <> long "stake"
    <> metavar "STAKE"
    <> help ("The stake you intend to delegate, which affects the rewards and "
            <> "the ranking of pools.")

-- | <transaction-id=TX_ID>
transactionIdArgument :: Parser TxId
transactionIdArgument = argumentT $ mempty
    <> metavar "TRANSACTION_ID"

-- | <name=STRING>
walletNameArgument :: Parser WalletName
walletNameArgument = argumentT $ mempty
    <> metavar "STRING"

-- | <public-key=ACCOUNT_PUBLIC_KEY>
accPubKeyArgument :: Parser ApiAccountPublicKey
accPubKeyArgument = argumentT $ mempty
    <> metavar "ACCOUNT_PUBLIC_KEY"
    <> help "64-byte (128-character) hex-encoded public account key."

-- | <payload=BINARY_BLOB>
transactionSubmitPayloadArgument :: Parser PostExternalTransactionData
transactionSubmitPayloadArgument = argumentT $ mempty
    <> metavar "BINARY_BLOB"
    <> help "hex-encoded binary blob of externally-signed transaction."

-- | <address=ADDRESS>
addressIdArgument :: Parser Text
addressIdArgument = argumentT $ mempty
    <> metavar "ADDRESS"

-- | Helper for writing an option 'Parser' using a 'FromText' instance.
optionT :: FromText a => Mod OptionFields a -> Parser a
optionT = option (eitherReader fromTextS)

-- | Helper for writing an argument 'Parser' using a 'FromText' instance.
argumentT :: FromText a => Mod ArgumentFields a -> Parser a
argumentT = argument (eitherReader fromTextS)

-- | Like 'fromText', but stringly-typed.
fromTextS :: FromText a => String -> Either String a
fromTextS = left getTextDecodingError . fromText . T.pack

runClient
    :: forall a. ()
    => Port "Wallet"
    -> (a -> IO BL.ByteString)
    -> ClientM a
    -> IO ()
runClient p encode cmd = do
    sendRequest p cmd (handleResponse encode)

sendRequest
    :: forall a. ()
    => Port "Wallet"
    -> ClientM a
    -> (Either ClientError a -> IO ())
    -> IO ()
sendRequest (Port p) cmd handle = do
    manager <- newManager $ defaultManagerSettings
        { managerResponseTimeout = responseTimeoutNone }
    let env = mkClientEnv manager (BaseUrl Http "localhost" p "")
    withClientM cmd env handle

handleResponse
    :: forall a. ()
    => (a -> IO BL.ByteString)
    -> Either ClientError a
    -> IO ()
handleResponse encode res = do
    case res of
        Right a -> do
            TIO.hPutStrLn stderr "Ok."
            BL8.putStrLn =<< encode a
        Left e -> do
            let msg = case e of
                    FailureResponse _ r -> fromMaybe
                        (T.decodeUtf8 $ BL.toStrict $ responseBody r)
                        (decodeError $ responseBody r)
                    _ ->
                        T.pack $ show e
            putErrLn msg
            exitFailure

{-------------------------------------------------------------------------------
                                Extra Types
-------------------------------------------------------------------------------}

-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port { getPort :: Int }
    deriving stock (Eq, Generic)
    deriving newtype (Enum, Ord, Show)

-- NOTE
-- TCP port ranges from [[-1;65535]] \ {0}
-- However, ports in [[-1; 1023]] \ {0} are well-known ports reserved
-- and only "bindable" through root privileges.
instance Bounded (Port tag) where
    minBound = Port 1024
    maxBound = Port 65535

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
newtype Service = Service Text deriving newtype (IsString, Show, Eq)

newtype TxId = TxId { getTxId :: Hash "Tx" }
    deriving (Eq, Show)

instance FromText TxId where
    fromText = Bi.first (const err) . fmap TxId . fromText
      where
        err = TextDecodingError
            "A transaction ID should be a hex-encoded string of 64 characters."

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

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer
    :: Maybe FilePath
    -> Severity
    -> IO (Switchboard Text, (CM.Configuration, Trace IO Text))
initTracer configFile minSeverity = do
    let defaultConfig = do
            c <- defaultConfigStdout
            CM.setMinSeverity c minSeverity
            CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
            pure c
    cfg <- maybe defaultConfig CM.setup configFile
    (tr, sb) <- setupTrace_ cfg "cardano-wallet"
    pure (sb, (cfg, tr))

-- | Run an action with logging available and configured. When the action is
-- finished (normally or otherwise), log messages are flushed.
withLogging
    :: Maybe FilePath
    -- ^ Configuration file - uses default otherwise.
    -> Severity
    -- ^ Minimum severity level to log
    -> ((CM.Configuration, Trace IO Text) -> IO a)
    -- ^ The action to run with logging configured.
    -> IO a
withLogging configFile minSeverity action = bracket before after (action . snd)
  where
    before = initTracer configFile minSeverity
    after (sb, (_, tr)) = do
        logDebug (appendName "main" tr) "Logging shutdown."
        shutdown sb

data LoggingOptions tracers = LoggingOptions
    { loggingMinSeverity :: Severity
    , loggingTracers :: tracers
    , loggingTracersDoc :: Maybe Void
    } deriving (Show, Eq)

loggingOptions :: Parser tracers -> Parser (LoggingOptions tracers)
loggingOptions tracers = LoggingOptions
    <$> minSev
    <*> tracers
    <*> tracersDoc
  where
    -- Note: If the global log level is Info then there will be no Debug-level
    --   messages whatsoever.
    --   If the global log level is Debug then there will be Debug, Info, and
    --   higher-severity messages.
    --   So the default global log level is Debug.
    minSev = option loggingSeverityReader $ mempty
        <> long "log-level"
        <> value Debug
        <> metavar "SEVERITY"
        <> help "Global minimum severity for a message to be logged. \
            \Individual tracers severities still need to be configured \
            \independently. Defaults to \"DEBUG\"."
        <> hidden
    tracersDoc = optional $ option auto $ mempty
        <> long "trace-NAME"
        <> metavar "SEVERITY"
        <> help "Individual component severity for 'NAME'. See --help-tracing \
            \for details and available tracers."

-- | A hidden "helper" option which always fails, but shows info about the
-- logging options.
helperTracing :: [(String, String)] -> Parser (a -> a)
helperTracing tracerDescriptions = abortOption (InfoMsg helpTxt) $ mempty
    <> long "help-tracing"
    <> help "Show help for tracing options"
    <> hidden
  where
    helpTxt = helperTracingText tracerDescriptions

helperTracingText :: [(String, String)] -> String
helperTracingText tracerDescriptions = unlines $
    [ "Additional tracing options:"
    , ""
    , "  --log-level SEVERITY     Global minimum severity for a message to be logged."
    , "                           Defaults to \"DEBUG\"."
    , ""
    , "  --trace-NAME=off         Disable logging on the given tracer."
    , "  --trace-NAME=SEVERITY    Minimum severity for a message to be logged, or"
    , "                           \"off\" to disable the tracer. Note that component"
    , "                           traces still abide by the global log-level. For"
    , "                           example, if the global log level is \"INFO\", then"
    , "                           there will be no \"DEBUG\" messages whatsoever."
    , "                           Defaults to \"INFO\"."
    , ""
    , "The possible log levels (lowest to highest) are:"
    , "  " ++ unwords (map fst loggingSeverities)
    , ""
    , "The possible tracers are:"
    ] ++ [ pretty_ tracerName desc | (tracerName, desc) <- tracerDescriptions]
  where
    maxLength = maximum $ map (length . fst) tracerDescriptions
    pretty_ tracerName desc =
        "  " ++ padRight maxLength ' ' tracerName ++ "  " ++ desc
      where
        padRight n c cs = take n $ cs ++ replicate n c

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

-- | The IOHK logging framework prints out ANSI colour codes with its messages.
-- On Windows 10 and above it's possible to enable processing of these colour
-- codes. The 'hSupportsANSIWithoutEmulation' function does this as a side
-- effect. On older versions of Windows, special treatment is required (see:
-- 'System.Console.ANSI'). In this case, this function will achieve nothing, and
-- the ANSI control characters will be printed in grey (too bad).
enableWindowsANSI :: IO ()
enableWindowsANSI = do
    void $ hSupportsANSIWithoutEmulation stdout
    void $ hSupportsANSIWithoutEmulation stderr

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
    :: Text
    -> (Text -> Either String a)
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

-- | Find the user data directory for a given node network backend.
getDataDir
    :: String -- ^ The network backend name.
    -> IO FilePath
getDataDir backendDir = do
    -- On Linux/MacOS, use the XDG data directory.
    -- On Windows, use the Local AppData (XdgCache) rather than one from the
    -- Roaming profile because we don't want to (potentially) transmit the
    -- wallet database to a network share.
    let dir = if os /= "windows" then XdgData else XdgCache
    dataDir <- getXdgDirectory dir "cardano-wallet"
    return $ dataDir </> backendDir

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

{-------------------------------------------------------------------------------
                        Polling for service availability
-------------------------------------------------------------------------------}

-- | Wait for a service to become available on a given TCP port. Exit on failure
-- with a proper error message.
waitForService
    :: Service
        -- ^ Name of the service
    -> Tracer IO WaitForServiceLog
        -- ^ A 'Trace' for logging
    -> Port "node"
        -- ^ TCP Port of the service
    -> IO ()
        -- ^ Service we're waiting after.
    -> IO ()
waitForService service tracer port action = do
    let handler (ErrNetworkInvalid net) = do
            traceWith tracer $ MsgServiceErrNetworkInvalid net
            exitFailure
        handler _ = do
            traceWith tracer $ MsgServiceTimedOut service
            exitFailure

    traceWith tracer $ MsgServiceWaiting service port
    action `catch` handler
    traceWith tracer $ MsgServiceReady service

-- | Log messages from 'waitForService'
data WaitForServiceLog
    = MsgServiceWaiting Service (Port "node")
    | MsgServiceReady Service
    | MsgServiceTimedOut Service
    | MsgServiceErrNetworkInvalid Text
    deriving (Show, Eq)

instance ToText WaitForServiceLog where
    toText = \case
        MsgServiceWaiting (Service service) port -> mconcat
            [ "Waiting for "
            , service
            , " to be ready on tcp/"
            , T.pack (showT port)
            ]
        MsgServiceReady (Service service) ->
            service <> " is ready."
        MsgServiceTimedOut (Service service) -> mconcat
             [ "Waited too long for "
             , service
             , " to become available. Giving up!"
             ]
        MsgServiceErrNetworkInvalid net -> mconcat
            [ "The node backend is not running on the \"", net, "\" "
            , "network. Please start the wallet server and the node "
            , "backend on the same network. Exiting now."
            ]

instance HasPrivacyAnnotation WaitForServiceLog
instance HasSeverityAnnotation WaitForServiceLog where
    getSeverityAnnotation = \case
        MsgServiceWaiting _ _ -> Info
        MsgServiceReady _ -> Info
        MsgServiceTimedOut _ -> Info
        MsgServiceErrNetworkInvalid _ -> Info
