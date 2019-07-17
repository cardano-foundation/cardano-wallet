{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLISpec
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Iso8601Time (..)
    , MnemonicSize (..)
    , Port (..)
    , cli
    , cmdAddress
    , cmdMnemonic
    , cmdTransaction
    , cmdWallet
    , hGetLine
    , hGetSensitiveLine
    )
import Cardano.Crypto.Wallet
    ( unXPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), getKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..), DecodeAddress (..), EncodeAddress (..) )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( mapM_ )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.Either
    ( isLeft, isRight )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), toText )
import Options.Applicative
    ( ParserResult (..), columns, execParserPure, prefs, renderFailure )
import System.IO
    ( Handle, IOMode (..), hClose, openFile )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Large (..)
    , arbitraryBoundedEnum
    , checkCoverage
    , counterexample
    , cover
    , genericShrink
    , property
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Instances.Time
    ()
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Specification / Usage Overview" $ do
        let parser = cli $ mempty
                <> cmdMnemonic
                <> cmdWallet @DummyTarget
                <> cmdTransaction @DummyTarget
                <> cmdAddress @DummyTarget

        let defaultPrefs = prefs (mempty <> columns 65)

        let expectationFailure = flip counterexample False

        let shouldShowUsage args expected = it (unwords args) $
                case execParserPure defaultPrefs parser args of
                    Success _ -> expectationFailure
                        "expected parser to show usage but it has succeeded"
                    CompletionInvoked _ -> expectationFailure
                        "expected parser to show usage but it offered completion"
                    Failure failure -> property $
                        let (usage, _) = renderFailure failure mempty
                        in counterexample usage $ expected === lines usage

        ["--help"] `shouldShowUsage`
            [ "The CLI is a proxy to the wallet server, which is required for"
            , "most commands. Commands are turned into corresponding API calls,"
            , "and submitted to an up-and-running server. Some commands do not"
            , "require an active server and can be run offline (e.g. 'mnemonic"
            , "generate')."
            , ""
            , "Usage:  COMMAND"
            , "  Cardano Wallet Command-Line Interface (CLI)"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  mnemonic                 "
            , "  wallet                   "
            , "  transaction              "
            , "  address                  "
            ]

        ["mnemonic", "--help"] `shouldShowUsage`
            [ "Usage:  mnemonic COMMAND"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  generate                 Generate English BIP-0039 compatible"
            , "                           mnemonic words."
            ]

        ["mnemonic", "generate", "--help"] `shouldShowUsage`
            [ "Usage:  mnemonic generate [--size INT]"
            , "  Generate English BIP-0039 compatible mnemonic words."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --size INT               number of mnemonic words to"
            , "                           generate. (default: 15)"
            ]

        ["wallet", "--help"] `shouldShowUsage`
            [ "Usage:  wallet COMMAND"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  list                     List all known wallets."
            , "  create                   Create a new wallet using a sequential"
            , "                           address scheme."
            , "  get                      Fetch the wallet with specified id."
            , "  update                   Update a wallet."
            , "  delete                   Deletes wallet with specified wallet"
            , "                           id."
            ]

        ["wallet", "list", "--help"] `shouldShowUsage`
            [ "Usage:  wallet list [--port INT]"
            , "  List all known wallets."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["wallet", "create", "--help"] `shouldShowUsage`
            [ "Usage:  wallet create [--port INT] STRING"
            , "                      [--address-pool-gap INT]"
            , "  Create a new wallet using a sequential address scheme."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --address-pool-gap INT   number of unused consecutive addresses"
            , "                           to keep track of. (default: 20)"
            ]

        ["wallet", "get", "--help"] `shouldShowUsage`
            [ "Usage:  wallet get [--port INT] WALLET_ID"
            , "  Fetch the wallet with specified id."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["wallet", "update", "--help"] `shouldShowUsage`
            [ "Usage:  wallet update COMMAND"
            , "  Update a wallet."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  name                     Update a wallet's name."
            , "  passphrase               Update a wallet's passphrase."
            ]

        ["wallet", "delete", "--help"] `shouldShowUsage`
            [ "Usage:  wallet delete [--port INT] WALLET_ID"
            , "  Deletes wallet with specified wallet id."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["transaction", "--help"] `shouldShowUsage`
            [ "Usage:  transaction COMMAND"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  create                   Create and submit a new transaction."
            , "  fees                     Estimate fees for a transaction."
            ]

        ["transaction", "create", "--help"] `shouldShowUsage`
            [ "Usage:  transaction create [--port INT] WALLET_ID"
            , "                           --payment PAYMENT"
            , "  Create and submit a new transaction."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --payment PAYMENT        address to send to and amount to send"
            , "                           separated by @, e.g."
            , "                           '<amount>@<address>'"
            ]

        ["transaction", "fees", "--help"] `shouldShowUsage`
            [ "Usage:  transaction fees [--port INT] WALLET_ID --payment PAYMENT"
            , "  Estimate fees for a transaction."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --payment PAYMENT        address to send to and amount to send"
            , "                           separated by @, e.g."
            , "                           '<amount>@<address>'"
            ]

        ["address", "--help"] `shouldShowUsage`
            [ "Usage:  address COMMAND"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  list                     List all known addresses of a given"
            , "                           wallet."
            ]

        ["address", "list", "--help"] `shouldShowUsage`
            [ "Usage:  address list [--port INT] [--state STRING] WALLET_ID"
            , "  List all known addresses of a given wallet."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --state STRING           only addresses with the given state:"
            , "                           either 'used' or 'unused'."
            ]

    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @Iso8601Time
        textRoundtrip $ Proxy @(Port "test")
        textRoundtrip $ Proxy @MnemonicSize

    describe "ISO 8601 encoding of dates and times" $ do

        describe "Can decode valid ISO 8601 strings" $ do

            describe "Basic format" $ do
                describe "UTC+0 timezone (Z)" $ do
                    canDecodeValidIso8601Time "20080915T155300Z"
                    canDecodeValidIso8601Time "20080915T155300.1Z"
                    canDecodeValidIso8601Time "20080915T155300.12Z"
                describe "UTC+0 timezone (manually specified)" $ do
                    canDecodeValidIso8601Time "20080915T155300+0000"
                    canDecodeValidIso8601Time "20080915T155300.1+0000"
                    canDecodeValidIso8601Time "20080915T155300.12+0000"
                describe "UTC+8 timezone (manually specified)" $ do
                    canDecodeValidIso8601Time "20080915T155300+0800"
                    canDecodeValidIso8601Time "20080915T155300.1+0800"
                    canDecodeValidIso8601Time "20080915T155300.12+0800"
                describe "UTC-8 timezone (manually specified)" $ do
                    canDecodeValidIso8601Time "20080915T155300-0800"
                    canDecodeValidIso8601Time "20080915T155300.1-0800"
                    canDecodeValidIso8601Time "20080915T155300.12-0800"

            describe "Extended format" $ do
                describe "UTC+0 timezone (Z)" $ do
                    canDecodeValidIso8601Time "2008-09-15T15:53:00Z"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.1Z"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.12Z"
                describe "UTC+0 timezone (manually specified)" $ do
                    canDecodeValidIso8601Time "2008-09-15T15:53:00+00:00"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.1+00:00"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.12+00:00"
                describe "UTC+8 timezone (manually specified)" $ do
                    canDecodeValidIso8601Time "2008-09-15T15:53:00+08:00"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.1+08:00"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.12+08:00"
                describe "UTC-8 timezone (manually specified)" $ do
                    canDecodeValidIso8601Time "2008-09-15T15:53:00-08:00"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.1-08:00"
                    canDecodeValidIso8601Time "2008-09-15T15:53:00.12-08:00"

        describe "Cannot decode invalid ISO 8601 strings" $ do

            describe "Strings that are not time values" $ do
                cannotDecodeInvalidIso8601Time ""
                cannotDecodeInvalidIso8601Time "w"
                cannotDecodeInvalidIso8601Time "wibble"

            describe "Basic format" $ do
                describe "Dates without times" $ do
                    cannotDecodeInvalidIso8601Time "2008"
                    cannotDecodeInvalidIso8601Time "200809"
                    cannotDecodeInvalidIso8601Time "20080915"
                describe "Missing timezones" $ do
                    cannotDecodeInvalidIso8601Time "20080915T155300"
                    cannotDecodeInvalidIso8601Time "20080915T155300.1"
                    cannotDecodeInvalidIso8601Time "20080915T155300.12"
                describe "Invalid timezone characters" $ do
                    cannotDecodeInvalidIso8601Time "20080915T155300A"
                    cannotDecodeInvalidIso8601Time "20080915T155300.1A"
                    cannotDecodeInvalidIso8601Time "20080915T155300.12A"
                describe "Invalid date-time separators" $ do
                    cannotDecodeInvalidIso8601Time "20080915S155300Z"
                    cannotDecodeInvalidIso8601Time "20080915S155300.1Z"
                    cannotDecodeInvalidIso8601Time "20080915S155300.12Z"
                describe "Missing date-time separators" $ do
                    cannotDecodeInvalidIso8601Time "20080915155300Z"
                    cannotDecodeInvalidIso8601Time "20080915155300.1Z"
                    cannotDecodeInvalidIso8601Time "20080915155300.12Z"

            describe "Extended format" $ do
                describe "Dates without times" $ do
                    cannotDecodeInvalidIso8601Time "2008"
                    cannotDecodeInvalidIso8601Time "2008-09"
                    cannotDecodeInvalidIso8601Time "2008-09-15"
                describe "Missing timezones" $ do
                    cannotDecodeInvalidIso8601Time "2008-09-15T15:53:00"
                    cannotDecodeInvalidIso8601Time "2008-09-15T15:53:00.1"
                    cannotDecodeInvalidIso8601Time "2008-09-15T15:53:00.12"
                describe "Invalid timezone characters" $ do
                    cannotDecodeInvalidIso8601Time "2008-09-15T15:53:00A"
                    cannotDecodeInvalidIso8601Time "2008-09-15T15:53:00.1A"
                    cannotDecodeInvalidIso8601Time "2008-09-15T15:53:00.12A"
                describe "Invalid date-time separators" $ do
                    cannotDecodeInvalidIso8601Time "2008-09-15S15:53:00Z"
                    cannotDecodeInvalidIso8601Time "2008-09-15S15:53:00.1Z"
                    cannotDecodeInvalidIso8601Time "2008-09-15S15:53:00.12Z"
                describe "Missing date-time separators" $ do
                    cannotDecodeInvalidIso8601Time "2008-09-1515:53:00Z"
                    cannotDecodeInvalidIso8601Time "2008-09-1515:53:00.1Z"
                    cannotDecodeInvalidIso8601Time "2008-09-1515:53:00.12Z"

        describe "Equivalent times are decoded equivalently" $ do

            describe "Times with the same date" $ do
                ensureIso8601TimesEquivalent
                    "2008-08-08T12:00:00+01:00"
                    "2008-08-08T11:00:00Z"
                ensureIso8601TimesEquivalent
                    "2008-08-08T12:00:00+08:00"
                    "2008-08-08T04:00:00Z"
                ensureIso8601TimesEquivalent
                    "2008-08-08T12:00:00-01:00"
                    "2008-08-08T13:00:00Z"
                ensureIso8601TimesEquivalent
                    "2008-08-08T12:00:00-08:00"
                    "2008-08-08T20:00:00Z"

            describe "Times with different dates" $ do
                ensureIso8601TimesEquivalent
                    "2008-08-08T00:00:00+01:00"
                    "2008-08-07T23:00:00Z"
                ensureIso8601TimesEquivalent
                    "2008-08-08T00:00:00+08:00"
                    "2008-08-07T16:00:00Z"
                ensureIso8601TimesEquivalent
                    "2008-08-08T23:00:00-01:00"
                    "2008-08-09T00:00:00Z"
                ensureIso8601TimesEquivalent
                    "2008-08-08T23:00:00-08:00"
                    "2008-08-09T07:00:00Z"

    describe "Port decoding from text" $ do
        let err = TextDecodingError
                $ "expected a TCP port number between "
                <> show (getPort minBound)
                <> " and "
                <> show (getPort maxBound)

        it "decode valid numbers to TCP Port, fail otherwise" $ checkCoverage $
            \(Large p) ->
                let
                    result :: Either TextDecodingError (Port "")
                    result = fromText (toText p)
                in
                        if p >= getPort minBound && p <= getPort maxBound
                            then cover 3 True "Right" $ result === Right (Port p)
                            else cover 90 True "Left" $ result === Left err

        mapM_ (\p -> it (T.unpack p) $ fromText @(Port "") p === Left err)
            [ "not-a-int"
            , "14.42"
            , ""
            , "[]"
            , "0x1337"
            , "0"
            ]

    describe "getLine" $ do
        it "Normal usage" $ test hGetLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "warrior toilet word\n"
            , expectedStdout = "Prompt: "
            , expectedResult = "warrior toilet word" :: Text
            }

        it "Parser with failure" $ test hGetLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "patate\n14\n"
            , expectedStdout =
                "Prompt: Int is an \
                \integer number between "
                <> T.pack (show $ minBound @Int)
                <> " and "
                <> T.pack (show $ maxBound @Int)
                <> ".\nPrompt: "
            , expectedResult = 14 :: Int
            }

    describe "getSensitiveLine" $ do
        it "Normal usage" $ test hGetSensitiveLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "password\n"
            , expectedStdout = "Prompt: ********\n"
            , expectedResult = "password" :: Text
            }

        it "Parser with failure" $ test hGetSensitiveLine $ GetLineTest
            { prompt = "Prompt: "
            , input = "patate\n14\n"
            , expectedStdout =
                "Prompt: ******\nInt is an \
                \integer number between "
                <> T.pack (show $ minBound @Int)
                <> " and "
                <> T.pack (show $ maxBound @Int)
                <> ".\nPrompt: **\n"
            , expectedResult = 14 :: Int
            }

        it "With backspaces" $ test hGetSensitiveLine $ GetLineTest
            { prompt = "Prompt: "
            , input = backspace <> "patate" <> backspace <> backspace <> "14\n"
            , expectedStdout = "Prompt: ******\ESC[1D \ESC[1D\ESC[1D \ESC[1D**\n"
            , expectedResult = "pata14" :: Text
            }
  where
    backspace :: Text
    backspace = T.singleton (toEnum 127)

{-------------------------------------------------------------------------------
                                hGetSensitiveLine
-------------------------------------------------------------------------------}

data GetLineTest a = GetLineTest
    { prompt :: Text
    , input :: Text
    , expectedStdout :: Text
    , expectedResult :: a
    }

test
    :: (FromText a, Show a, Eq a)
    =>  (  (Handle, Handle)
        -> Text
        -> (Text -> Either TextDecodingError a)
        -> IO (a, Text)
        )
    -> GetLineTest a
    -> IO ()
test fn (GetLineTest prompt_ input_ output expected) = do
    -- Setup
    let fstdin = "/tmp/cardano-wallet-cli-stdin"
    let fstdout = "/tmp/cardano-wallet-cli-stdout"
    TIO.writeFile fstdin input_ *> writeFile fstdout mempty
    stdin <- openFile fstdin ReadWriteMode
    stdout <- openFile fstdout ReadWriteMode

    -- Action
    mvar <- newEmptyMVar
    let action = fn (stdin, stdout) prompt_ fromText
    _ <- forkFinally action (handler mvar)
    res <- takeMVar mvar
    hClose stdin *> hClose stdout
    content <- TIO.readFile fstdout

    -- Expectations
    (fst <$> res) `shouldBe` Just expected
    content `shouldBe` output
  where
    handler mvar = \case
        Left _ ->
            putMVar mvar Nothing
        Right a ->
            putMVar mvar (Just a)

{-------------------------------------------------------------------------------
                               Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary Iso8601Time where
    arbitrary = Iso8601Time <$> arbitrary
    shrink (Iso8601Time t) = Iso8601Time <$> shrink t

instance Arbitrary MnemonicSize where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary (Port "test") where
    arbitrary = arbitraryBoundedEnum
    shrink p
        | p == minBound = []
        | otherwise = [pred p]

{-------------------------------------------------------------------------------
                               Helper Functions
-------------------------------------------------------------------------------}

-- | Checks that the specified 'Text' CAN be decoded as an 'Iso8601Time'
--   value using the 'FromText` instance.
--
canDecodeValidIso8601Time :: Text -> Spec
canDecodeValidIso8601Time text =
    it ("Can decode as ISO 8601 time: " <> T.unpack text) $ property $ do
        let result = fromText @Iso8601Time text
        -- Internally, 'Iso8601Time' values are always stored canonically as
        -- times in the UTC+0 timezone. Any original timezone information is
        -- lost. So we check that a roundtrip text conversion can be applied
        -- to the result of parsing the original input, rather than to the
        -- original input itself.
        (result `shouldSatisfy` isRight)
            .&&.
            ((fromText . toText =<< result) `shouldBe` result)

-- | Checks that the specified 'Text' CANNOT be decoded as an 'Iso8601Time'
--   value using the 'FromText' instance.
--
cannotDecodeInvalidIso8601Time :: Text -> Spec
cannotDecodeInvalidIso8601Time text =
    it ("Cannot decode as ISO 8601 time: " <> T.unpack text) $ property $ do
        fromText @Iso8601Time text `shouldSatisfy` isLeft

-- | Checks that the specified "Text' values can both be decoded as valid
--   'Iso8601Time' values, and that the resultant values are equal.
--
ensureIso8601TimesEquivalent :: Text -> Text -> Spec
ensureIso8601TimesEquivalent t1 t2 = it title $ property $
    (r1 `shouldBe` r2)
    .&&. (r1 `shouldSatisfy` isRight)
    .&&. (r2 `shouldSatisfy` isRight)

  where
    r1 = fromText @Iso8601Time t1
    r2 = fromText @Iso8601Time t2
    title = mempty
            <> "Equivalent ISO 8601 times are decoded equivalently: "
            <> show (t1, t2)

{-------------------------------------------------------------------------------
                                 Dummy Target
-------------------------------------------------------------------------------}

data DummyTarget

instance KeyToAddress DummyTarget where
    keyToAddress = Address . unXPub . getKey

instance EncodeAddress DummyTarget where
    encodeAddress _ = T.decodeUtf8 . convertToBase Base16 . unAddress

instance DecodeAddress DummyTarget where
    decodeAddress _ = bimap decodingError Address
        . convertFromBase Base16
        . T.encodeUtf8
      where
        decodingError _ = TextDecodingError
            "Unable to decode Address: expected Base16 encoding"
