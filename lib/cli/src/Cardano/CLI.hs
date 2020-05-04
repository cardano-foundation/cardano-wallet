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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
    , cmdMnemonicByron
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

    -- * Option parsers for configuring tracing
    , LoggingOptions (..)
    , helperTracing
    , loggingOptions
    , loggingSeverities
    , loggingSeverityOrOffReader
    , loggingSeverityReader

    -- * Types
    , Service
    , TxId
    , MnemonicSize (..)
    , Port (..)
    , CliKeyScheme (..)
    , DerivationIndex (..)
    , DerivationPath (..)
    , XPrvOrXPub (..)
    , KeyEncoding (..)

    , newCliKeyScheme
    , firstHardenedIndex
    , fullKeyEncodingDescription
    , decodeKey
    , detectEncoding
    , decodeAnyKey
    , encodeKey
    , deriveChildKey
    , toPublic

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
    ( SomeMnemonic (..), entropyToMnemonic, genEntropy, mnemonicToText )
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
    , ApiEpochNumber
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
    , ErrUnXPrvStripPub (..)
    , ErrXPrvFromStrippedPubXPrv (..)
    , FromMnemonic (..)
    , FromMnemonicError (..)
    , Index (..)
    , NatVals (..)
    , Passphrase (..)
    , PassphraseMaxLength
    , PassphraseMinLength
    , WalletKey (..)
    , XPrv
    , XPub
    , deriveRewardAccount
    , hex
    , unXPrv
    , unXPrvStripPubCheckRoundtrip
    , xPrvFromStrippedPubXPrvCheckRoundtrip
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, defaultAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState, Hash, SortOrder, SyncTolerance (..), WalletId, WalletName )
import Cardano.Wallet.Version
    ( gitRevision, showFullVersion, version )
import Codec.Binary.Bech32
    ( CharPosition (..), DecodingError (..) )
import Control.Applicative
    ( optional, some, (<|>) )
import Control.Arrow
    ( first, left )
import Control.Exception
    ( bracket, catch )
import Control.Monad
    ( foldM, join, unless, void, when, (>=>) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON (..), (.:), (.=) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.Either
    ( isRight )
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
import Data.Void
    ( Void )
import Data.Word
    ( Word32 )
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
    , footer
    , footerDoc
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
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM )
import Servant.Client.Core
    ( ClientError (..), responseBody )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hCursorBackward
    , hSetSGR
    , hSupportsANSIWithoutEmulation
    , setSGRCode
    )
import System.Directory
    ( XdgDirectory (..)
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getXdgDirectory
    )
import System.Environment
    ( getProgName )
import System.Exit
    ( die, exitFailure, exitSuccess )
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
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Api.Types as API
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
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
                            Commands - 'HD Derivation'
-------------------------------------------------------------------------------}

{-# NOINLINE progName #-}
progName :: String
progName = unsafePerformIO getProgName

cmdKey :: Mod CommandFields (IO ())
cmdKey = command "key" $ info (helper <*> cmds) $ mempty
    <> progDesc "Derive and manipulate keys."
    <> footerDoc (Just $ string $ mconcat
        [ "Keys are read from standard input for convenient chaining of commands."
        , "\n\n"
        , "Bech32- and hexadecimal encodings are supported."
        , "\n\n"
        , "Example:\n"
        , "$ "
        , progName
        , " key root --wallet-style icarus --encoding bech32 -- express theme celery coral permit ... \\\n"
        , "    | "
        , progName
        , " key public\n"
        , "xpub1k365denpkmqhj9zj6qpax..."
        ])
  where
    cmds = subparser $ mempty
        <> cmdKeyRoot
        <> cmdKeyChild
        <> cmdKeyPublic
        <> cmdKeyInspect

-- | Record encapsulating @mnemonicToRootKey@  — /without/ any type
-- parameters related to scheme.
--
-- This means that we can have a value for byron, a value for icarus, both with
-- the same type @CliKeyScheme@.
--
-- @CliKeyScheme@ is on the other hand parameterized over @key@ and @m@.
data CliKeyScheme key m = CliKeyScheme
    { allowedWordLengths :: [Int]
    , mnemonicToRootKey :: [Text] -> m key
    }

data XPrvOrXPub = AXPrv XPrv | AXPub XPub

eitherToIO :: Either String a -> IO a
eitherToIO = either die return

data KeyEncoding = Hex | Bech32
    deriving (Show, Eq, Enum, Bounded)

detectEncoding :: Text -> Either String KeyEncoding
detectEncoding x
    | "xprv1" `T.isPrefixOf` x = pure Bech32
    | "xpub1" `T.isPrefixOf` x = pure Bech32
    | isHex x                  = pure Hex
    | otherwise                = Left fullKeyEncodingDescription
  where
    isHex = isRight . decodeHex

fullKeyEncodingDescription :: String
fullKeyEncodingDescription = mconcat
    [ "Invalid key. Expected one of the following:\n"
    , "- 96 bytes long hex (extended private key)\n"
    , "- 64 bytes long hex (extended public key)\n"
    , "- xprv1... (bech32-encoded extended private key)\n"
    , "- xpub1... (bech32-encoded extended public key)"
    ]

xprvFromBytes :: ByteString -> Either String XPrvOrXPub
xprvFromBytes = fmap AXPrv . left showErr . xPrvFromStrippedPubXPrvCheckRoundtrip
  where
    showErr (ErrInputLengthMismatch expected actual) = mconcat
        [ "Expected extended private key to be "
        , show expected
        , " bytes but got "
        , show actual
        , " bytes."
        ]
    showErr (ErrCannotRoundtripToSameBytes) = mconcat
        [ "That extended private key looks weird. "
        , "Is it encrypted? Or is it an old Byron key?"
        ]

xpubFromBytes :: ByteString -> Either String XPrvOrXPub
xpubFromBytes = fmap AXPub . CC.xpub

xprvOrXPubFromBytes :: ByteString -> Either String XPrvOrXPub
xprvOrXPubFromBytes bs =
    case BS.length bs of
        96 -> xprvFromBytes bs
        64 -> xpubFromBytes bs
        n -> Left . mconcat $
            [ "Expected key to be 96 bytes in the case of a private key"
            , " and, 64 bytes for public keys. This key is "
            , show n
            , " bytes."
            ]

xprvHrp :: Bech32.HumanReadablePart
xprvHrp = [Bech32.humanReadablePart|xprv|]

xpubHrp :: Bech32.HumanReadablePart
xpubHrp = [Bech32.humanReadablePart|xpub|]

decodeAnyKey :: Text -> Either String (XPrvOrXPub, KeyEncoding)
decodeAnyKey rawKey = do
    enc <- detectEncoding rawKey
    k <- decodeKey enc rawKey
    return (k, enc)

decodeKey :: KeyEncoding -> Text -> Either String XPrvOrXPub
decodeKey = \case
    Bech32 -> decodeBech32
    Hex -> decodeHex >=> xprvOrXPubFromBytes
  where
    decodeBech32 :: Text -> Either String XPrvOrXPub
    decodeBech32 t = do
        (hrp, dp) <- left bech32Err $ Bech32.decodeLenient t
        bytes <- maybe (Left dpErr) Right $ Bech32.dataPartToBytes dp
        case hrp of
            h | h == xpubHrp -> xpubFromBytes bytes
            h | h == xprvHrp -> xprvFromBytes bytes
            _ -> Left "unrecognized Bech32 Human Readable Part"
      where
        dpErr = "Internal error: Unable to extract bytes from bech32 data part"

        bech32Err  = ("Bech32 error: " <>) . \case
            StringToDecodeTooLong -> "string is too long"
            StringToDecodeTooShort -> "string is too short"
            StringToDecodeHasMixedCase -> "string has mixed case"
            StringToDecodeMissingSeparatorChar -> "string has no separator char"
            StringToDecodeContainsInvalidChars [] -> invalidCharsMsg
            StringToDecodeContainsInvalidChars ixs -> invalidCharsMsg <> ":\n"
                    <> markCharsRedAtIndices (map unCharPos ixs) (T.unpack t)
        invalidCharsMsg = "Invalid character(s) in string"
        unCharPos (CharPosition x) = x

markCharsRedAtIndices :: Integral i => [i] -> String -> String
markCharsRedAtIndices ixs txt = go 0 ixs txt ++ def
  where
    go _c [] [] = mempty
    go c (i:is) (s:ss)
        | c == i    = red ++ s:def ++ go (c + 1) is ss
        | otherwise = s : go (c + 1) (i:is) ss
    go _ [] ss = ss
    go _ _ [] = [] -- NOTE: Really an error case.

    red = setSGRCode [SetColor Foreground Vivid Red]
    def = setSGRCode [Reset]

decodeHex :: Text -> Either String ByteString
decodeHex txt = fromHex $ T.encodeUtf8 . T.strip $ txt
 where
   fromHex = left (const "Invalid hex.")
       . convertFromBase Base16

encodeKey :: KeyEncoding -> XPrvOrXPub -> Either String Text
encodeKey enc key = case enc of
    Hex -> T.decodeUtf8 . hex <$> bytes
    Bech32 -> bech32 hrp <$> bytes
  where
    bech32 h = Bech32.encodeLenient h . Bech32.dataPartFromBytes

    hrp = case key of
        AXPrv _ -> xprvHrp
        AXPub _ -> xpubHrp

    bytes :: Either String ByteString
    bytes = case key of
        AXPrv xprv -> left showErr . unXPrvStripPubCheckRoundtrip $ xprv
        AXPub xpub -> return . CC.unXPub $ xpub
      where
        -- NOTE: This error should never happen from using the CLI.
        showErr ErrCannotRoundtripToSameXPrv =
            "Internal error: Failed to safely encode an extended private key"

newtype DerivationPath = DerivationPath [DerivationIndex]
    deriving (Show, Eq)

instance FromText DerivationPath where
    fromText x = DerivationPath <$> mapM fromText (T.splitOn "/" x)

instance ToText DerivationPath where
    toText (DerivationPath xs) = T.intercalate "/" $ map toText xs

newtype DerivationIndex = DerivationIndex { unDerivationIndex :: Word32 }
    deriving (Show, Eq)
    deriving newtype (Bounded, Enum, Ord)

firstHardenedIndex :: Word32
firstHardenedIndex = getIndex $ minBound @(Index 'Hardened 'AddressK)

instance FromText DerivationIndex where
    fromText "" = Left $ TextDecodingError
        "An empty string is not a derivation index!"
    fromText x = do
       -- NOTE: T.takeEnd will not throw, but may return "".
       if T.takeEnd 1 x == "H"
       then do
           let x' = T.dropEnd 1 x
           parseHardenedIndex x'
       else parseSoftIndex x
      where
        parseWord = left (const err) .
            fmap fromIntegral . fromText @Int
          where
            err = TextDecodingError $
                "\"" ++ T.unpack x ++ "\" is not a number."

        mkDerivationIndex :: Int -> Either TextDecodingError DerivationIndex
        mkDerivationIndex =
            fmap (DerivationIndex . toEnum . fromEnum) . indexInv

        parseSoftIndex txt = do
            num <- parseWord txt >>= softIndexInv
            mkDerivationIndex num

        parseHardenedIndex txt = do
            num <- parseWord txt
            let i = num + fromIntegral firstHardenedIndex
            mkDerivationIndex i

        softIndexInv a = do
            idx <- mkDerivationIndex a
            if a >= fromIntegral firstHardenedIndex
            then Left . TextDecodingError $ mconcat
                [ show a
                , " is too high to be a soft derivation index. "
                , "Please use \"H\" to denote hardened indexes. "
                , "Did you mean \""
                , T.unpack $ toText idx
                , "\"?"
                ]
            else Right a

        indexInv a =
            if a > fromIntegral (maxBound @Word32)
            then Left . TextDecodingError $ mconcat
                [ show a
                , " is too high to be a derivation index."
                ]
            else Right a

instance ToText DerivationIndex where
    toText (DerivationIndex i) = do
        T.pack $
            if i >= firstHardenedIndex
            then show (i - firstHardenedIndex) ++ "H"
            else show i

newCliKeyScheme :: ByronWalletStyle -> CliKeyScheme XPrvOrXPub (Either String)
newCliKeyScheme = \case
    Random ->
        -- NOTE
        -- This cannot happen because 'Random' style is filtered out by the
        -- option parser already.
        error "newCliKeyScheme: unsupported wallet type"

    Icarus ->
        let
            proxy = Proxy @'API.Icarus
        in
            CliKeyScheme
                (apiAllowedLengths proxy)
                (fmap (AXPrv . icarusKeyFromSeed) . seedFromMnemonic proxy)
    Trezor ->
        let
            proxy = Proxy @'API.Trezor
        in
            CliKeyScheme
                (apiAllowedLengths proxy)
                (fmap (AXPrv . icarusKeyFromSeed) . seedFromMnemonic proxy)
    Ledger ->
        let
            proxy = Proxy @'API.Ledger
        in
            CliKeyScheme
                (apiAllowedLengths proxy)
                (fmap (AXPrv . ledgerKeyFromSeed) . seedFromMnemonic proxy)
  where
    seedFromMnemonic
        :: forall (s :: ByronWalletStyle).
            (FromMnemonic (AllowedMnemonics s))
        => Proxy s
        -> [Text]
        -> Either String SomeMnemonic
    seedFromMnemonic _ =
        left getFromMnemonicError . fromMnemonic @(AllowedMnemonics s)

    apiAllowedLengths
        :: forall (s :: ByronWalletStyle). ( NatVals (AllowedMnemonics s))
        => Proxy s
        -> [Int]
    apiAllowedLengths _ =
         (map fromIntegral (natVals $ Proxy @(AllowedMnemonics s)))

    icarusKeyFromSeed :: SomeMnemonic -> XPrv
    icarusKeyFromSeed = Icarus.getKey . flip Icarus.generateKeyFromSeed pass

    ledgerKeyFromSeed :: SomeMnemonic -> XPrv
    ledgerKeyFromSeed = Icarus.getKey
        . flip Icarus.generateKeyFromHardwareLedger pass

    -- We don't use passwords to encrypt the keys here.
    pass = mempty


toPublic :: XPrvOrXPub -> Either String XPrvOrXPub
toPublic (AXPrv xprv) = return . AXPub . CC.toXPub $ xprv
toPublic (AXPub _) = Left "Input is already a public key."

deriveChildKey
    :: XPrvOrXPub
    -> DerivationIndex
    -> Either String XPrvOrXPub
deriveChildKey (AXPrv k) i =
    return . AXPrv
        $ CC.deriveXPrv
            CC.DerivationScheme2
            (mempty :: Passphrase "Encryption")
            k
            (unDerivationIndex i)
deriveChildKey (AXPub k) i =
    maybe (Left err) (return . AXPub)
        $ CC.deriveXPub
            CC.DerivationScheme2
            k
            (unDerivationIndex i)
  where
    err = mconcat
        [ T.unpack (toText i)
        , " is a hardened index. Public key derivation is only possible for"
        , " soft indices. \nIf the index is correct, please use the"
        , " corresponding private key as input."
        ]

inspect :: XPrvOrXPub -> Text
inspect (AXPrv key) =
    let
        bytes = CC.unXPrv key
        (xprv, rest) = BS.splitAt 64 bytes
        (_pub, cc) = BS.splitAt 32 rest
        encodeToHex = T.pack . B8.unpack . hex
    in
        mconcat
        [ "extended private key: "
        , encodeToHex xprv
        , "\n"
        , "chain code: "
        , encodeToHex cc
        ]
inspect (AXPub key) =
    let bytes = CC.unXPub key
        (xpub, cc) = BS.splitAt 32 bytes
        encodeToHex = T.pack . B8.unpack . hex
    in
        mconcat
            [ "extended public key: "
            , encodeToHex xpub
            , "\n"
            , "chain code: "
            , encodeToHex cc
            ]

data KeyRootArgs = KeyRootArgs
    { _walletStyle :: ByronWalletStyle
    , _mnemonicWords :: [Text]
    , _keyEncoding :: KeyEncoding
    }

cmdKeyRoot :: Mod CommandFields (IO ())
cmdKeyRoot =
    command "root" $ info (helper <*> cmd) $ mempty
        <> progDesc "Extract root extended private key from a mnemonic sentence."
  where
    cmd = fmap exec $ KeyRootArgs
        <$> walletStyleOption Icarus [Icarus, Trezor, Ledger]
        <*> mnemonicWordsArgument
        <*> keyEncodingOption
    exec (KeyRootArgs keyType ws enc) = do
        eitherToIO (mnemonicToRootKey scheme ws >>= encode) >>= TIO.putStrLn
      where
        encode = encodeKey enc
        scheme = newCliKeyScheme keyType

newtype KeyChildArgs = KeyChildArgs
    { _path :: DerivationPath
    }

cmdKeyChild :: Mod CommandFields (IO ())
cmdKeyChild =
    command "child" $ info (helper <*> cmd) $ mempty
        <> progDesc "Derive child keys."
        <> footerDoc
            (Just $ string "The parent key is read from standard input.")
  where
    cmd = fmap exec $
        KeyChildArgs <$> pathOption

    exec (KeyChildArgs (DerivationPath path)) = do
        TIO.getLine
            >>= eitherToIO . action
            >>= TIO.putStrLn
      where
        action :: Text -> Either String Text
        action = decodeAnyKey
            >=> \(k, enc) -> foldM deriveChildKey k path
            >>= encodeKey enc

cmdKeyPublic :: Mod CommandFields (IO ())
cmdKeyPublic =
    command "public" $ info (helper <*> cmd) $ mempty
        <> progDesc "Extract the public key from a private key."
        <> footerDoc
            (Just $ string "The private key is read from standard input.")
  where
    cmd = pure exec

    exec =
        TIO.getLine
            >>= eitherToIO . action
            >>= TIO.putStrLn
      where
        action :: Text -> Either String Text
        action = decodeAnyKey >=> \(k, enc) -> toPublic k >>= encodeKey enc

cmdKeyInspect :: Mod CommandFields (IO ())
cmdKeyInspect =
    command "inspect" $ info (helper <*> cmd) $ mempty
        <> progDesc "Show information about a key."
        <> footerDoc
            (Just $ string "The key is read from standard input.")
  where
    cmd = pure exec

    exec = do
        TIO.getLine
            >>= eitherToIO . action
            >>= TIO.putStrLn
      where
        action :: Text -> Either String Text
        action = fmap (\(k, _) -> inspect k) . decodeAnyKey

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

cmdMnemonicByron :: Mod CommandFields (IO ())
cmdMnemonicByron = command "mnemonic" $ info (helper <*> cmds) $ mempty
    <> progDesc "Manage mnemonic phrases."
  where
    cmds = subparser $ mempty
        <> cmdMnemonicGenerate

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
        wSeed <- do
            let prompt = "Please enter your 15–24 word mnemonic sentence: "
            let parser = fromMnemonic @'[15,18,21,24] . T.words
            fst <$> getLine prompt parser
        wSndFactor <- do
            let prompt =
                    "(Enter a blank line if you didn't use a second factor.)\n"
                    <> "Please enter your 9–12 word mnemonic second factor: "
            let parser = optionalE $ fromMnemonic @'[9,12] . T.words
            fst <$> getLine prompt parser

        let rootXPrv = Shelley.generateKeyFromSeed (wSeed, wSndFactor) mempty
        let rewardAccountXPrv = deriveRewardAccount mempty rootXPrv

        let hrp = [Bech32.humanReadablePart|ed25519e_sk|]
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

type CmdWalletCreate wallet = WalletClient wallet -> Mod CommandFields (IO ())

cmdWallet
    :: ToJSON wallet
    => CmdWalletCreate wallet
    -> WalletClient wallet
    -> Mod CommandFields (IO ())
cmdWallet cmdCreate mkClient =
    command "wallet" $ info (helper <*> cmds) $ mempty
        <> progDesc "Manage wallets."
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
        runClient wPort Aeson.encodePretty $ listWallets mkClient

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
    command "from-mnemonic" $ info (helper <*> cmd) $ mempty
        <> progDesc "Create a new wallet using a mnemonic."
  where
    cmd = fmap exec $ ByronWalletCreateFromMnemonicArgs
        <$> portOption
        <*> walletNameArgument
        <*> walletStyleOption Icarus [Random,Icarus,Trezor,Ledger]
    exec (ByronWalletCreateFromMnemonicArgs wPort wName wStyle) = case wStyle of
        Random -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = fromMnemonic @(AllowedMnemonics 'Random) . T.words
                fst <$> getLine (T.pack prompt) parser
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort Aeson.encodePretty $ postWallet mkClient $
                RandomWalletFromMnemonic $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

        Icarus -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = fromMnemonic @(AllowedMnemonics 'Icarus) . T.words
                fst <$> getLine (T.pack prompt) parser
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort Aeson.encodePretty $ postWallet mkClient $
                SomeIcarusWallet $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

        Trezor -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = fromMnemonic @(AllowedMnemonics 'Trezor) . T.words
                fst <$> getLine (T.pack prompt) parser
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort Aeson.encodePretty $ postWallet mkClient $
                SomeTrezorWallet $ ByronWalletPostData
                    (ApiMnemonicT wSeed)
                    (ApiT wName)
                    (ApiT wPwd)

        Ledger -> do
            wSeed <- do
                let prompt = "Please enter " ++ fmtAllowedWords wStyle ++ " : "
                let parser = fromMnemonic @(AllowedMnemonics 'Ledger) . T.words
                fst <$> getLine (T.pack prompt) parser
            wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
            runClient wPort Aeson.encodePretty $ postWallet mkClient $
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
    command "from-mnemonic" $ info (helper <*> cmd) $ mempty
        <> progDesc "Create a new wallet using a mnemonic."
  where
    cmd = fmap exec $ WalletCreateArgs
        <$> portOption
        <*> walletNameArgument
        <*> poolGapOption
    exec (WalletCreateArgs wPort wName wGap) = do
        wSeed <- do
            let prompt = "Please enter a 15–24 word mnemonic sentence: "
            let parser = fromMnemonic @'[15,18,21,24] . T.words
            fst <$> getLine prompt parser
        wSndFactor <- do
            let prompt =
                    "(Enter a blank line if you do not wish to use a second " <>
                    "factor.)\n" <>
                    "Please enter a 9–12 word mnemonic second factor: "
            let parser =
                    optionalE (fromMnemonic @'[9,12]) . T.words
            fst <$> getLine prompt parser
        wPwd <- getPassphraseWithConfirm "Please enter a passphrase: "
        runClient wPort Aeson.encodePretty $ postWallet mkClient $
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
        runClient wPort Aeson.encodePretty $ postWallet mkClient $
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
        runClient wPort Aeson.encodePretty $ getWallet mkClient $
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
        runClient wPort Aeson.encodePretty $ putWallet mkClient
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
        res <- sendRequest wPort $ getWallet mkClient $ ApiT wId
        case res of
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
            Left _ ->
                handleResponse Aeson.encodePretty res

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
        runClient wPort (const "") $ deleteWallet mkClient $
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
        res <- sendRequest wPort $ getWallet mkClient $ ApiT wId
        case res of
            Right _ -> do
                runClient wPort Aeson.encodePretty $
                    getWalletUtxoStatistics mkClient (ApiT wId)
            Left _ ->
                handleResponse Aeson.encodePretty res

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
        <> progDesc "Manage transactions."
  where
    cmds = subparser $ mempty
        <> cmdTransactionCreate mkTxClient mkWalletClient
        <> cmdTransactionFees mkTxClient mkWalletClient
        <> cmdTransactionList mkTxClient
        <> cmdTransactionSubmit mkTxClient
        <> cmdTransactionForget mkTxClient

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
        res <- sendRequest wPort $ getWallet mkWalletClient $ ApiT wId
        case res of
            Right _ -> do
                wPwd <- getPassphrase @"raw" "Please enter your passphrase: "
                runClient wPort Aeson.encodePretty $ postTransaction
                    mkTxClient
                    (ApiT wId)
                    (Aeson.object
                        [ "payments" .= wPayments
                        , "passphrase" .= ApiT wPwd
                        ]
                    )
            Left _ ->
                handleResponse Aeson.encodePretty res

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
        res <- sendRequest wPort $ getWallet mkWalletClient $ ApiT wId
        case res of
            Right _ -> do
                runClient wPort Aeson.encodePretty $ postTransactionFee
                    mkTxClient
                    (ApiT wId)
                    (Aeson.object [ "payments" .= wPayments ])
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
        runClient wPort Aeson.encodePretty $ listTransactions
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
        runClient wPort Aeson.encodePretty $
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

{-------------------------------------------------------------------------------
                            Commands - 'address'
-------------------------------------------------------------------------------}

cmdAddress
    :: AddressClient
    -> Mod CommandFields (IO ())
cmdAddress mkClient =
    command "address" $ info (helper <*> cmds) $ mempty
        <> progDesc "Manage addresses."
  where
    cmds = subparser $ mempty
        <> cmdAddressList mkClient
        <> cmdAddressCreate mkClient

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
        runClient wPort Aeson.encodePretty $ listAddresses mkClient
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
        runClient wPort Aeson.encodePretty $ postRandomAddress mkClient
            (ApiT wId)
            (ApiPostRandomAddressData (ApiT pwd) (ApiT <$> wIx))

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
    :: StakePoolClient
    -> Mod CommandFields (IO ())
cmdStakePool mkClient =
    command "stake-pool" $ info (helper <*> cmds) $ mempty
        <> progDesc "Manage stake pools."
  where
    cmds = subparser $ mempty
        <> cmdStakePoolList mkClient

-- | Arguments for 'stake-pool list' command
newtype StakePoolListArgs = StakePoolListArgs
    { _port :: Port "Wallet"
    }

cmdStakePoolList
    :: StakePoolClient
    -> Mod CommandFields (IO ())
cmdStakePoolList mkClient =
    command "list" $ info (helper <*> cmd) $ mempty
        <> progDesc "List all known stake pools."
  where
    cmd = fmap exec $ StakePoolListArgs
        <$> portOption
    exec (StakePoolListArgs wPort) = do
        runClient wPort Aeson.encodePretty $ listPools mkClient

{-------------------------------------------------------------------------------
                            Commands - 'network'
-------------------------------------------------------------------------------}

cmdNetwork
    :: NetworkClient
    -> Mod CommandFields (IO ())
cmdNetwork mkClient =
    command "network" $ info (helper <*> cmds) $ mempty
        <> progDesc "Manage network."
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
        runClient wPort Aeson.encodePretty (networkInformation mkClient)

-- | Arguments for 'network parameters' command
data NetworkParametersArgs = NetworkParametersArgs
    { _port :: Port "Wallet"
    , _epoch :: ApiEpochNumber
    }

cmdNetworkParameters
    :: NetworkClient
    -> Mod CommandFields (IO ())
cmdNetworkParameters mkClient =
    command "parameters" $ info (helper <*> cmd) $ mempty
        <> progDesc "View network parameters."
  where
    cmd = fmap exec $ NetworkParametersArgs
        <$> portOption
        <*> epochArgument
    exec (NetworkParametersArgs wPort epoch) = do
        runClient wPort Aeson.encodePretty $ networkParameters mkClient epoch

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
        runClient wPort Aeson.encodePretty $ networkClock mkClient forceNtpCheck

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

-- | [--size=INT], default: 15
sizeOption :: Parser MnemonicSize
sizeOption = optionT $ mempty
    <> long "size"
    <> metavar "INT"
    <> help "number of mnemonic words to generate."
    <> value MS_15
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

loggingSeverities :: [(String, Severity)]
loggingSeverities =
    [ ("debug", Debug)
    , ("info", Info)
    , ("notice", Notice)
    , ("warning", Warning)
    , ("error", Error)
    , ("critical", Critical)
    , ("alert", Alert)
    , ("emergency", Emergency)
    ]

loggingSeverityReader :: ReadM Severity
loggingSeverityReader = do
    arg <- readerAsk
    case lookup (map toLower arg) loggingSeverities of
        Just sev -> pure sev
        Nothing -> fail $ "unknown logging severity: " ++ arg

loggingSeverityOrOffReader :: ReadM (Maybe Severity)
loggingSeverityOrOffReader = do
    arg <- readerAsk
    case map toLower arg of
        "off" -> pure Nothing
        _ -> Just <$> loggingSeverityReader

-- | [--wallet-style=WALLET_STYLE]
--
-- Note that we in the future might replace the type @ByronWalletStyle@ with
-- another type, to include Shelley keys.
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

instance FromText KeyEncoding where
    fromText "bech32" = return Bech32
    fromText "hex" = return Hex
    fromText _ = Left $ TextDecodingError "Invalid key encoding option."

keyEncodingOption :: Parser KeyEncoding
keyEncodingOption = option (eitherReader fromTextS) $
    mempty
    <> long "encoding"
    <> metavar "KEY-ENCODING"
    <> value Hex

pathOption :: Parser DerivationPath
pathOption = option (eitherReader fromTextS) $
    mempty
    <> long "path"
    <> metavar "DER-PATH"
    <> help "Derivation path e.g. 44H/1815H/0H/0"

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

-- | <wallet-id=WALLET_ID>
walletIdArgument :: Parser WalletId
walletIdArgument = argumentT $ mempty
    <> metavar "WALLET_ID"

-- | <epoch=EPOCH_NUMBER>
epochArgument :: Parser ApiEpochNumber
epochArgument = argumentT $ mempty
    <> metavar "EPOCH_NUMBER"
    <> help "epoch number parameter or 'latest'"

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

-- | <mnemonic-words=MNEMONIC_WORD...>
mnemonicWordsArgument :: Parser [Text]
mnemonicWordsArgument = some (argument str (metavar "MNEMONIC_WORD..."))

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
    -> IO (Either ClientError a)
sendRequest (Port p) cmd = do
    manager <- newManager $ defaultManagerSettings
        { managerResponseTimeout = responseTimeoutNone }
    let env = mkClientEnv manager (BaseUrl Http "localhost" p "")
    runClientM cmd env

handleResponse
    :: forall a. ()
    => (a -> BL.ByteString)
    -> Either ClientError a
    -> IO ()
handleResponse encode res = do
    case res of
        Right a -> do
            TIO.hPutStrLn stderr "Ok."
            BL8.putStrLn (encode a)
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
