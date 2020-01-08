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
    , cmdTransaction
    , cmdAddress
    , cmdStakePool
    , cmdNetwork
    , cmdVersion

    -- * Option & Argument Parsers
    , optionT
    , argumentT
    , databaseOption
    , hostPreferenceOption
    , listenOption
    , nodePortOption
    , nodePortMaybeOption
    , stateDirOption
    , verbosityOption
    , syncToleranceOption

    -- * Types
    , Service
    , TxId
    , MnemonicSize (..)
    , Port (..)

    -- * Logging
    , Verbosity (..)
    , withLogging
    , verbosityToArgs
    , verbosityToMinSeverity

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
    , waitForService
    , setupDirectory
    , failWith
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
    ( Trace, logAlert, logDebug, logInfo )
import Cardano.Wallet.Api.Client
    ( WalletClient (..), walletClient )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..) )
import Cardano.Wallet.Api.Types
    ( AddressAmount
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiTxId (..)
    , DecodeAddress
    , EncodeAddress
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , Passphrase (..)
    , PassphraseMaxLength
    , PassphraseMinLength
    , WalletKey (..)
    , deriveRewardAccount
    , unXPrv
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, defaultAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( AddressState, Hash, SortOrder, SyncTolerance (..), WalletId, WalletName )
import Cardano.Wallet.Version
    ( gitRevision, showFullVersion, version )
import Control.Applicative
    ( optional, some, (<|>) )
import Control.Arrow
    ( first, left, second )
import Control.Exception
    ( bracket, catch )
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
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..), showT )
import Data.Text.Read
    ( decimal )
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
    , footer
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
    , str
    , strOption
    , subparser
    , value
    )
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM )
import Servant.Client.Core
    ( ServantError (..), responseBody )
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
    ( ExitCode (..), exitFailure, exitSuccess, exitWith )
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
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
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
-------------------------------------------------------------------------------}

cmdMnemonic :: Mod CommandFields (IO ())
cmdMnemonic = command "mnemonic" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage mnemonic phrases."
  where
    cmds = subparser $ mempty
        <> cmdMnemonicGenerate
        <> cmdMnemonicRewardCredentials

-- | Arguments for 'mnemonic generate' command
newtype MnemonicGenerateArgs = MnemonicGenerateArgs
    { _size :: MnemonicSize
    }

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

cmdMnemonicRewardCredentials :: Mod CommandFields (IO ())
cmdMnemonicRewardCredentials =
    command "reward-credentials" $ info (helper <*> cmd) $ mempty
        <> progDesc "Derive reward account private key from a given mnemonic."
        <> footer "!!! Only for the Incentivized Testnet !!!"
  where
    cmd = pure exec
    exec = do
        wSeed <- fst <$> do
            let prompt = "Please enter your 15–24 word mnemonic sentence: "
            let parser = fromMnemonic @'[15,18,21,24] @"seed" . T.words
            getLine prompt parser
        wSndFactor <- maybe mempty fst <$> do
            let prompt =
                    "(Enter a blank line if you didn't use a second factor.)\n"
                    <> "Please enter your 9–12 word mnemonic second factor: "
            let parser =
                    optionalE (fromMnemonic @'[9,12] @"generation") . T.words
            getLine prompt parser <&> \case
                (Nothing, _) -> Nothing
                (Just a, t) -> Just (a, t)

        let rootXPrv = Shelley.generateKeyFromSeed (wSeed, wSndFactor) mempty
        let rewardAccountXPrv = deriveRewardAccount mempty rootXPrv

        let hrp = Bech32.unsafeHumanReadablePartFromText "ed25519e_sk"
        let dp = Bech32.dataPartFromBytes
                $ BS.take 64
                $ unXPrv
                $ getRawKey
                rewardAccountXPrv
        TIO.putStrLn $ mconcat
            [ "\nHere's your reward account private key:\n\n"
            , "    ", Bech32.encodeLenient hrp dp
            , "\n\nKeep it safe!"
            ]

{-------------------------------------------------------------------------------
                            Commands - 'wallet'
-------------------------------------------------------------------------------}

cmdWallet
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWallet = command "wallet" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage wallets."
  where
    cmds = subparser $ mempty
        <> cmdWalletList @t
        <> cmdWalletCreate @t
        <> cmdWalletGet @t
        <> cmdWalletUpdate @t
        <> cmdWalletDelete @t
        <> cmdWalletGetUtxoStatistics @t

-- | Arguments for 'wallet list' command
newtype WalletListArgs = WalletListArgs
    { _port :: Port "Wallet"
    }

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
                    "(Enter a blank line if you do not wish to use a second " <>
                    "factor.)\n" <>
                    "Please enter a 9–12 word mnemonic second factor: "
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


cmdWalletGetUtxoStatistics
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdWalletGetUtxoStatistics = command "utxo" $ info (helper <*> cmd) $ mempty
    <> progDesc "Get UTxO statistics for the wallet with specified id."
  where
    cmd = fmap exec $ WalletGetArgs
        <$> portOption
        <*> walletIdArgument
    exec (WalletGetArgs wPort wId) = do
        res <- sendRequest wPort $ getWallet (walletClient @t) $ ApiT wId
        case res of
            Right _ -> do
                runClient wPort Aeson.encodePretty $
                    getWalletUtxoStatistics (walletClient @t) (ApiT wId)
            Left _ ->
                handleResponse Aeson.encodePretty res

{-------------------------------------------------------------------------------
                            Commands - 'transaction'
-------------------------------------------------------------------------------}

-- | cardano-wallet transaction
cmdTransaction
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransaction = command "transaction" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage transactions."
  where
    cmds = subparser $ mempty
        <> cmdTransactionCreate @t
        <> cmdTransactionFees @t
        <> cmdTransactionList @t
        <> cmdTransactionSubmit @t
        <> cmdTransactionForget @t

-- | Arguments for 'transaction create' command
data TransactionCreateArgs t = TransactionCreateArgs
    { _port :: Port "Wallet"
    , _id :: WalletId
    , _payments :: NonEmpty (AddressAmount t)
    }

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

-- | Arguments for 'transaction list' command.
data TransactionListArgs = TransactionListArgs
    { _port :: Port "Wallet"
    , _walletId :: WalletId
    , _timeRangeStart :: Maybe Iso8601Time
    , _timeRangeEnd :: Maybe Iso8601Time
    , _sortOrder :: Maybe SortOrder
    }

cmdTransactionList
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransactionList = command "list" $ info (helper <*> cmd) $ mempty
    <> progDesc "List the transactions associated with a wallet."
  where
    cmd = fmap exec $ TransactionListArgs
        <$> portOption
        <*> walletIdArgument
        <*> optional timeRangeStartOption
        <*> optional timeRangeEndOption
        <*> optional sortOrderOption
    exec (TransactionListArgs wPort wId mTimeRangeStart mTimeRangeEnd mOrder) =
        runClient wPort Aeson.encodePretty $ listTransactions
            (walletClient @t)
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
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransactionSubmit = command "submit" $ info (helper <*> cmd) $ mempty
    <> progDesc "Submit an externally-signed transaction."
  where
    cmd = fmap exec $ TransactionSubmitArgs
        <$> portOption
        <*> transactionSubmitPayloadArgument
    exec (TransactionSubmitArgs wPort wPayload) = do
        runClient wPort Aeson.encodePretty $
            postExternalTransaction (walletClient @t) wPayload

-- | Arguments for 'transaction forget' command
data TransactionForgetArgs = TransactionForgetArgs
    { _port :: Port "Wallet"
    , _wid :: WalletId
    , _txid :: TxId
    }

cmdTransactionForget
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdTransactionForget = command "forget" $ info (helper <*> cmd) $ mempty
    <> progDesc "Forget a pending transaction with specified id."
  where
    cmd = fmap exec $ TransactionForgetArgs
        <$> portOption
        <*> walletIdArgument
        <*> transactionIdArgument
    exec (TransactionForgetArgs wPort wId txId) = do
        runClient wPort (const mempty) $ deleteTransaction (walletClient @t)
            (ApiT wId)
            (ApiTxId $ ApiT $ getTxId txId)

{-------------------------------------------------------------------------------
                            Commands - 'address'
-------------------------------------------------------------------------------}

cmdAddress
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdAddress = command "address" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage addresses."
  where
    cmds = subparser $ mempty
        <> cmdAddressList @t

-- | Arguments for 'address list' command
data AddressListArgs = AddressListArgs
    { _port :: Port "Wallet"
    , _state :: Maybe AddressState
    , _id :: WalletId
    }

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
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdStakePool = command "stake-pool" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage stake pools."
  where
    cmds = subparser $ mempty
        <> cmdStakePoolList @t

-- | Arguments for 'stake-pool list' command
newtype StakePoolListArgs = StakePoolListArgs
    { _port :: Port "Wallet"
    }

cmdStakePoolList
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdStakePoolList = command "list" $ info (helper <*> cmd) $ mempty
    <> progDesc "List all known stake pools."
  where
    cmd = fmap exec $ StakePoolListArgs <$> portOption
    exec (StakePoolListArgs wPort) = do
        runClient wPort Aeson.encodePretty $ listPools (walletClient @t)

{-------------------------------------------------------------------------------
                            Commands - 'network'
-------------------------------------------------------------------------------}

cmdNetwork
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdNetwork = command "network" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage network."
  where
    cmds = subparser $ mempty
        <> cmdNetworkInformation @t

-- | Arguments for 'network information' command
newtype NetworkInformationArgs = NetworkInformationArgs
    { _port :: Port "Wallet"
    }

cmdNetworkInformation
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Mod CommandFields (IO ())
cmdNetworkInformation = command "information" $ info (helper <*> cmd) $ mempty
    <> progDesc "View network information."
  where
    cmd = fmap exec $ NetworkInformationArgs <$> portOption
    exec (NetworkInformationArgs wPort) = do
        runClient wPort Aeson.encodePretty $ networkInformation (walletClient @t)

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
paymentOption :: DecodeAddress t => Parser (AddressAmount t)
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

-- | [--size=INT], default: 15
sizeOption :: Parser MnemonicSize
sizeOption = optionT $ mempty
    <> long "size"
    <> metavar "INT"
    <> help "number of mnemonic words to generate."
    <> value MS_15
    <> showDefaultWith showT

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

-- | <transaction-id=TX_ID>
transactionIdArgument :: Parser TxId
transactionIdArgument = argumentT $ mempty
    <> metavar "TRANSACTION_ID"

-- | <name=STRING>
walletNameArgument :: Parser WalletName
walletNameArgument = argumentT $ mempty
    <> metavar "STRING"

-- | <payload=BINARY_BLOB>
transactionSubmitPayloadArgument :: Parser PostExternalTransactionData
transactionSubmitPayloadArgument = argumentT $ mempty
    <> metavar "BINARY_BLOB"
    <> help "hex-encoded binary blob of externally-signed transaction."

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
    manager <- newManager $ defaultManagerSettings
        { managerResponseTimeout = responseTimeoutNone }
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
newtype Service = Service Text deriving newtype IsString

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
        logDebug tr "Logging shutdown."
        shutdown sb

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

-- | Wait for a service to become available on a given TCP port. Exit on failure
-- with a proper error message.
waitForService
    :: Service
        -- ^ Name of the service
    -> Trace IO Text
        -- ^ A 'Trace' for logging
    -> Port "node"
        -- ^ TCP Port of the service
    -> IO ()
        -- ^ Service we're waiting after.
    -> IO ()
waitForService (Service service) tracer port action = do
    let handler (ErrNetworkInvalid net) = do
            exitWith =<< failWith tracer (mconcat
                [ "The node backend is not running on the \"", net, "\" "
                , "network. Please start the wallet server and the node "
                , "backend on the same network. Exiting now."
                ])

        handler _ = do
            exitWith =<< failWith tracer (mconcat
                [ "Waited too long for "
                , service
                , " to become available. Giving up!"
                ])
    logInfo tracer $ mconcat
        [ "Waiting for "
        , service
        , " to be ready on tcp/"
        , T.pack (showT port)
        ]
    action `catch` handler
    logInfo tracer $ service <> " is ready."

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

-- Terminate the applicatio and make sure to flush logs before exiting.
failWith
    :: Trace IO Text
        -- ^ A 'Trace' for logging
    -> Text
        -- ^ A message to before exiting
    -> IO ExitCode
failWith tr msg = do
    logAlert tr msg
    pure (ExitFailure 1)

-- | Make a parser optional
optionalE
    :: (Monoid m, Eq m)
    => (m -> Either e a)
    -> (m -> Either e (Maybe a))
optionalE parse = \case
    m | m == mempty -> Right Nothing
    m  -> Just <$> parse m
