{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLISpec
    ( spec
    ) where

import Prelude hiding
    ( (.) )

import Cardano.CLI
    ( CliKeyScheme (..)
    , Codec
    , DerivationIndex (..)
    , DerivationPath (..)
    , MnemonicSize (..)
    , Port (..)
    , TxId
    , XPrvOrXPub (..)
    , anyKeyCodec
    , cli
    , cmdAddress
    , cmdKey
    , cmdMnemonic
    , cmdNetwork
    , cmdStakePool
    , cmdTransaction
    , cmdWallet
    , cmdWalletCreate
    , firstHardenedIndex
    , fullKeyEncodingDescription
    , hGetLine
    , hGetSensitiveLine
    , inverse
    , keyBech32Codec
    , keyByteStringCodec
    , mapKey
    , newCliKeyScheme
    )
import Cardano.Startup
    ( setUtf8EncodingHandles )
import Cardano.Wallet.Api.Client
    ( addressClient
    , networkClient
    , stakePoolClient
    , transactionClient
    , walletClient
    )
import Cardano.Wallet.Api.Types
    ( ByronWalletStyle (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( XPrv, unXPrv )
import Cardano.Wallet.Primitive.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , entropyToMnemonic
    , mnemonicToText
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy )
import Control.Arrow
    ( left )
import Control.Category
    ( (.) )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( SomeException, try )
import Control.Monad
    ( mapM_ )
import Data.Either
    ( isLeft )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), toText )
import GHC.TypeLits
    ( natVal )
import Options.Applicative
    ( ParserResult (..), columns, execParserPure, prefs, renderFailure )
import System.Exit
    ( ExitCode (..) )
import System.FilePath
    ( (</>) )
import System.IO
    ( Handle, IOMode (..), hClose, openFile, stderr )
import System.IO.Silently
    ( capture_, hCapture_ )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( HasCallStack
    , Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldStartWith
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Large (..)
    , NonEmptyList (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , counterexample
    , cover
    , expectFailure
    , forAll
    , genericShrink
    , oneof
    , property
    , suchThat
    , vector
    , (.&&.)
    , (===)
    )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do

    let defaultPrefs = prefs (mempty <> columns 65)

    let parser = cli $ mempty
            <> cmdMnemonic
            <> cmdWallet cmdWalletCreate walletClient
            <> cmdTransaction transactionClient walletClient
            <> cmdAddress addressClient
            <> cmdStakePool stakePoolClient
            <> cmdNetwork networkClient
            <> cmdKey

    let shouldStdOut args expected = it (unwords args) $ do
            setUtf8EncodingHandles
            case execParserPure defaultPrefs parser args of
                Success x -> capture_ x >>= (`shouldBe` expected)
                CompletionInvoked _ -> expectationFailure
                    "expected parser to show usage but it offered completion"
                Failure failure ->
                    expectationFailure $ "parser failed with: " ++ show failure
    let expectStdErr args expectation = it (unwords args) $ do
            setUtf8EncodingHandles
            case execParserPure defaultPrefs parser args of
                Success x ->
                    hCapture_ [stderr] (try @SomeException x) >>= (expectation)
                CompletionInvoked _ -> expectationFailure
                    "expected parser to show usage but it offered completion"
                Failure failure -> do
                    let (str, code) = renderFailure failure ""
                    code `shouldBe` (ExitFailure 1)
                    expectation str
    describe "Specification / Usage Overview" $ do

        let expectationFailure' = flip counterexample False
        let shouldShowUsage args expected = it (unwords args) $
                case execParserPure defaultPrefs parser args of
                    Success _ -> expectationFailure'
                        "expected parser to show usage but it has succeeded"
                    CompletionInvoked _ -> expectationFailure'
                        "expected parser to show usage but it offered completion"
                    Failure failure -> property $
                        let (usage, _) = renderFailure failure mempty
                            msg = "*** Expected:\n" ++ (unlines expected)
                                ++ "*** but actual usage is:\n" ++ usage
                        in counterexample msg $ expected === lines usage

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
            , "  mnemonic                 Manage mnemonic phrases."
            , "  wallet                   Manage wallets."
            , "  transaction              Manage transactions."
            , "  address                  Manage addresses."
            , "  stake-pool               Manage stake pools."
            , "  network                  Manage network."
            , "  key                      Derive and manipulate keys."
            ]

        ["mnemonic", "--help"] `shouldShowUsage`
            [ "Usage:  mnemonic COMMAND"
            , "  Manage mnemonic phrases."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  generate                 Generate English BIP-0039 compatible"
            , "                           mnemonic words."
            , "  reward-credentials       Derive reward account private key from"
            , "                           a given mnemonic."
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

        ["mnemonic", "reward-credentials", "--help"] `shouldShowUsage`
            [ "Usage:  mnemonic reward-credentials "
            , "  Derive reward account private key from a given mnemonic."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "!!! Only for the Incentivized Testnet !!!"
            ]

        ["key", "--help"] `shouldShowUsage`
            [ "Keys can be passed as arguments or read as standard input. Both bech32- and hexadecimal encodings are supported."
            , ""
            , "For instance:"
            , "$ cardano-wallet-byron key root --wallet-style icarus --encoding bech32 -- express theme celery coral permit ... \\"
            , "    | cardano-wallet-byron key public"
            , "xpub1k365denpkmqhj9zj6qpax..."
            , ""
            , "Usage:  key COMMAND"
            , "  Derive and manipulate keys."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  root                     Extract root extended private key from"
            , "                           a mnemonic sentence."
            , "  child                    Derive child keys."
            , "  public                   Extract public key from a private key."
            , "  inspect                  Show information about a key."
            ]

        ["wallet", "--help"] `shouldShowUsage`
            [ "Usage:  wallet COMMAND"
            , "  Manage wallets."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  list                     List all known wallets."
            , "  create                   Create a new wallet."
            , "  get                      Fetch the wallet with specified id."
            , "  update                   Update a wallet."
            , "  delete                   Deletes wallet with specified wallet"
            , "                           id."
            , "  utxo                     Get UTxO statistics for the wallet"
            , "                           with specified id."
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

        ["wallet", "create", "from-mnemonic", "--help"] `shouldShowUsage`
            [ "Usage:  wallet create from-mnemonic [--port INT] STRING"
            , "                                    [--address-pool-gap INT]"
            , "  Create a new wallet using a mnemonic."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --address-pool-gap INT   number of unused consecutive addresses"
            , "                           to keep track of. (default: 20)"
            ]

        ["wallet", "create", "from-public-key", "--help"] `shouldShowUsage`
            [ "Usage:  wallet create from-public-key [--port INT] STRING"
            , "                                      [--address-pool-gap INT]"
            , "                                      ACCOUNT_PUBLIC_KEY"
            , "  Create a wallet using a public account key."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --address-pool-gap INT   number of unused consecutive addresses"
            , "                           to keep track of. (default: 20)"
            , "  ACCOUNT_PUBLIC_KEY       64-byte (128-character) hex-encoded"
            , "                           public account key."
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

        ["wallet", "utxo", "--help"] `shouldShowUsage`
            [ "Usage:  wallet utxo [--port INT] WALLET_ID"
            , "  Get UTxO statistics for the wallet with specified id."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["transaction", "--help"] `shouldShowUsage`
            [ "Usage:  transaction COMMAND"
            , "  Manage transactions."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  create                   Create and submit a new transaction."
            , "  fees                     Estimate fees for a transaction."
            , "  list                     List the transactions associated with"
            , "                           a wallet."
            , "  submit                   Submit an externally-signed"
            , "                           transaction."
            , "  forget                   Forget a pending transaction with"
            , "                           specified id."
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

        ["transaction", "list", "--help"] `shouldShowUsage`
            [ "Usage:  transaction list [--port INT] WALLET_ID [--start TIME]"
            , "                         [--end TIME] [--order ORDER]"
            , "  List the transactions associated with a wallet."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --start TIME             start time (ISO 8601 date-and-time"
            , "                           format: basic or extended, e.g."
            , "                           2012-09-25T10:15:00Z)."
            , "  --end TIME               end time (ISO 8601 date-and-time"
            , "                           format: basic or extended, e.g."
            , "                           2016-11-21T10:15:00Z)."
            , "  --order ORDER            specifies a sort order, either"
            , "                           'ascending' or 'descending'."
            ]

        ["transaction", "submit", "--help"] `shouldShowUsage`
            [ "Usage:  transaction submit [--port INT] BINARY_BLOB"
            , "  Submit an externally-signed transaction."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  BINARY_BLOB              hex-encoded binary blob of"
            , "                           externally-signed transaction."
            ]

        ["transaction", "forget", "--help"] `shouldShowUsage`
            [ "Usage:  transaction forget [--port INT] WALLET_ID TRANSACTION_ID"
            , "  Forget a pending transaction with specified id."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["address", "--help"] `shouldShowUsage`
            [ "Usage:  address COMMAND"
            , "  Manage addresses."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  list                     List all known addresses of a given"
            , "                           wallet."
            , "  create                   Create a new random address. Only"
            , "                           available for random wallets. The"
            , "                           address index is optional, give none"
            , "                           to let the wallet generate a random"
            , "                           one."
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

        ["address", "create", "--help"] `shouldShowUsage`
            [ "Usage:  address create [--port INT] [--address-index INDEX]"
            , "                       WALLET_ID"
            , "  Create a new random address. Only available for random wallets."
            , "  The address index is optional, give none to let the wallet"
            , "  generate a random one."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --address-index INDEX    A derivation index for the address"
            ]

        ["stake-pool", "list", "--help"] `shouldShowUsage`
            [ "Usage:  stake-pool list [--port INT]"
            , "  List all known stake pools."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["network", "--help"] `shouldShowUsage`
            [ "Usage:  network COMMAND"
            , "  Manage network."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  information              View network information."
            , "  parameters               View network parameters."
            , "  clock                    View NTP offset."
            ]

        ["network", "information", "--help"] `shouldShowUsage`
            [ "Usage:  network information [--port INT]"
            , "  View network information."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["network", "parameters", "--help"] `shouldShowUsage`
            [ "Usage:  network parameters [--port INT] EPOCH_NUMBER"
            , "  View network parameters."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  EPOCH_NUMBER             epoch number parameter or 'latest'"
            ]

        ["network", "clock", "--help"] `shouldShowUsage`
            [ "Usage:  network clock [--port INT] [--force-ntp-check]"
            , "  View NTP offset."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --force-ntp-check        When set, will block and force an NTP"
            , "                           check with the server. Otherwise, uses"
            , "                           an available cached result."
            ]

        ["key", "root", "--help"] `shouldShowUsage`
            [ "Usage:  key root [--wallet-style WALLET_STYLE] MNEMONIC_WORD..."
            , "                 [--encoding KEY-ENCODING]"
            , "  Extract root extended private key from a mnemonic sentence."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --wallet-style WALLET_STYLE"
            , "                           Any of the following (default: icarus)"
            , "                             icarus (15 mnemonic words)"
            , "                             trezor (12, 15, 18, 21 or 24 mnemonic words)"
            , "                             ledger (12, 15, 18, 21 or 24 mnemonic words)"
            ]

        ["key", "child", "--help"] `shouldShowUsage`
            [ "Usage:  key child --path DER-PATH [XPRV]"
            , "  Derive child keys."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --path DER-PATH          Derivation path e.g. 44H/1815H/0H/0"
            ]

        ["key", "public", "--help"] `shouldShowUsage`
            [ "Usage:  key public [XPRV]"
            , "  Extract public key from a private key."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            ]

        ["key", "inspect", "--help"] `shouldShowUsage`
            [ "Usage:  key inspect [XPRV]"
            , "  Show information about a key."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            ]

    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @(Port "test")
        textRoundtrip $ Proxy @MnemonicSize
        textRoundtrip $ Proxy @DerivationIndex
        textRoundtrip $ Proxy @DerivationPath

    describe "DerivationPath/Index fromText goldens" $ do
        fromTextGolden @DerivationIndex "" "should fail" $
            Left "An empty string is not a derivation index!"

        fromTextGolden @DerivationIndex "0'" "should fail" $
            Left "\"0'\" is not a number."

        fromTextGolden @DerivationIndex "4294967295H" "should fail" $
            Left "6442450943 is too high to be a derivation index."

        fromTextGolden @DerivationPath "" "should fail" $
            Left "An empty string is not a derivation index!"

        let firstHardenedIx = 0x80000000
        fromTextGolden @DerivationPath "44H/0H/0" "" $
            Right . DerivationPath . map DerivationIndex $
                [firstHardenedIx + 44, firstHardenedIx + 0, 0]

        fromTextGolden
            @DerivationPath "2147483692" "hardened index without ' notation" $
                Left "2147483692 is too high to be a soft derivation index. \
                     \Please use \"H\" to denote hardened indexes. Did you \
                     \mean \"44H\"?"

        fromTextGolden @DerivationPath "44H/0H/0/" "should fail (trailing /)" $
            Left "An empty string is not a derivation index!"

        fromTextGolden @DerivationPath "öH/0H/0/" "should fail" $
            Left "\"öH\" is not a number."

        fromTextGolden @DerivationPath "0x80000000/0H/0/" "should fail" $
            Left "\"0x80000000\" is not a number."

    describe "Transaction ID decoding from text" $ do

        it "Should produce a user-friendly error message on failing to \
            \decode a transaction ID." $ do

            let err = TextDecodingError
                    "A transaction ID should be a hex-encoded string of 64 \
                    \characters."

            fromText @TxId "not-a-transaction-id" `shouldBe` Left err

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

    let mw15 = words "message mask aunt wheel ten maze between tomato slow \
                     \analyst ladder such report capital produce"
    let mw12 = words "broccoli side goddess shaft alarm victory sheriff \
                     \combine birth deny train outdoor"
    describe "key root" $ do
        (["key", "root", "--wallet-style", "icarus"] ++ mw15) `shouldStdOut`
            "00aa5f5f364980f4ac6295fd0fbf65643390d6bb1cf76536c2ebb02713c8ba50d8\
            \903bee774b7bf8678ea0d6fded6d876db3b42bef687640cc514eb73f767537a8c7\
            \54f89bc9cc83533eab257d7c94625c95f0d749710428f5aa2404eeb6499b\n"
        (["key", "root", "--wallet-style", "trezor"] ++ mw15) `shouldStdOut`
            "00aa5f5f364980f4ac6295fd0fbf65643390d6bb1cf76536c2ebb02713c8ba50d8\
            \903bee774b7bf8678ea0d6fded6d876db3b42bef687640cc514eb73f767537a8c7\
            \54f89bc9cc83533eab257d7c94625c95f0d749710428f5aa2404eeb6499b\n"
        (["key", "root", "--wallet-style", "ledger"] ++ mw15) `shouldStdOut`
            "003a914372e711b910a75b87e98695929b6960bd5380cfd766b572ea844ea14080\
            \9eb7ad13f798d06ce550a9f6c48dd2151db4593e67dbd2821d75378c7350f1366b\
            \85e0be9cdec2213af2084d462cc11e85c215e0f003acbeb996567e371502\n"

    describe "key root (negative tests)" $ do
        (["key", "root", "--wallet-style", "icarus"] ++ mw12) `expectStdErr`
            (`shouldBe` "Invalid number of words: 15 words are expected.\n")

        (["key", "root", "--wallet-style", "icarus"]) `expectStdErr`
            (`shouldStartWith` "Missing: MNEMONIC_WORD...")

        let shrug = "¯\\_(ツ)_/¯"
        (["key", "root", "--wallet-style", "icarus"] ++ (replicate 15 shrug))
            `expectStdErr` (`shouldBe`
            "Found an unknown word not present in the pre-defined dictionary. \
            \The full dictionary is available here:\
            \ https://github.com/input-output-hk/cardano-wallet/tree/master/spe\
            \cifications/mnemonic/english.txt\n")

    describe "key child" $ do
        let rootXPrv = "588102383ed9ecc5c44e1bfa18d1cf8ef19a7cf806a20bb4cbbe4e5\
                       \11666cf48d6fd7bec908e4c6ced5f0c4f0798b1b619d6b61e611049\
                       \2b5ebb430f570488f074a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fb\
                       \ed4ad474371095c74db3d9c743a\n"
        ["key", "child", "--path", "1852H/1815H/0H/0/0", rootXPrv]
            `shouldStdOut`
            "5073cbc3e3f85b0099c67ed5b0344bfc0f15861ef05f41cde2a797352f66cf48ab\
            \59c46d040abb4b3e0623bb151362233e75cf1f923b6d5964780ebbcf3a2d7a3d90\
            \78e802011f1580465c80e7040f1e4d8e24f978d23f01c1d2cf18fcf741a7\n"

        -- This key does not have the "tweak" that newer keys are expected
        -- to have.
        let byronKey =
              "464f3a1316a3849a1ca49a7e3a8b9ab35379598ac4fbcd0ba2bc3a165185150a\
              \5c56ebf6d6d39fd6c070731a44133ebb083c42b949046d79aac48b7a1f52787c\
              \a5078d2194b78ccb6116d64f4d5a3fad3cd41e4748c20fc589d87a0e69583357"
        let encryptedKey =
              "9d41c6c66a0aaac73b31bfbf2522c63eea4e16e7df63ccf43e012b20a4606cbb\
              \e99a00cfed56e9516bc947f327a73e0849882a32a682932c51b42156055abb0b\
              \5d3661deb9064f2d0e03fe85d68070b2fe33b4916059658e28ac7f7f91ca4b12"

        describe "bryon keys fail" $ do
            ["key", "child", "--path", "0", byronKey]
                `expectStdErr` (`shouldBe` "That extended private key looks \
                       \weird. Is it encrypted? Or is it an old Byron key?\n")
        describe "encrypted keys fail" $ do
            ["key", "child", "--path", "0", encryptedKey]
                `expectStdErr` (`shouldBe` "That extended private key looks \
                       \weird. Is it encrypted? Or is it an old Byron key?\n")
        describe "fails when key is not 96 bytes" $ do
            ["key", "child", "--path", "0", "5073"]
                `expectStdErr` (`shouldBe` "Expected key to be 96 bytes in the\
                    \ case of a private key and, 64 bytes for public keys. This\
                    \ key is 2 bytes.\n")

        let pub1 =
              "20997b093a426804de5120fa2b6d2184a605274b364201ddc9f79307eae8dfed\
              \74a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad474371095c74db3d9c743a"

        let pub2 =
              "708792807c1e3959626cd0ab78b1db5ed1544a97f3addbb01457de56ee5a450f\
              \e932040a815c8994e44b7075ec61bdfc66e77671c59d14c76f3b33e8c534b15f"

        describe "public key" $ do
            ["key", "child", "--path", "0", pub1]
                `shouldStdOut` (pub2 <> "\n")

            ["key", "child", "--path", "0H", pub1]
                `expectStdErr` (`shouldBe` "0H is a hardened index. Public key \
                    \derivation is only possible for soft indices. \nIf the\
                    \ index is correct, please use the corresponding private \
                    \key as input.\n")


    describe "key public" $ do
        let prv1 = "588102383ed9ecc5c44e1bfa18d1cf8ef19a7cf806a20bb4cbbe4e51166\
                   \6cf48d6fd7bec908e4c6ced5f0c4f0798b1b619d6b61e6110492b5ebb43\
                   \0f570488f074a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad4743710\
                   \95c74db3d9c743a"
        let pub1 = "20997b093a426804de5120fa2b6d2184a605274b364201ddc9f79307eae\
                   \8dfed74a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad474371095c74\
                   \db3d9c743a"

        -- Verified manually with jcli.
        ["key", "public", prv1] `shouldStdOut` (pub1 ++ "\n")

        describe "fails when input is a public key" $ do
            let err1 = "Input is already a public key."
            ["key", "public", pub1] `expectStdErr` (`shouldBe` (err1 ++ "\n"))

    describe "detection of key encoding (bech32 vs hex)" $ do
        let prv1 = "xprv18rppm7hzl6m68c5zqh5nxp4sa8d6wmazz6g4p4spt67k3ck3kf0u2u\
                   \3pgaexmgpafmpwhjyks546rk5fxw9ma685zw8sy6erhtn3s9lkq2jy27ejz\
                   \gca990aa67mvauxnnajh26hdxprzjry7q26tx0qvy6zgldk"
        let pub1 = "xpub1hqslt9dm5stzpphhfjah5mk3s27psdt2yal8t5rz9e8dm3r7zpalvq\
                   \4yg4anyy33622lmm4akemcd88m9w44w6vzx9yxfuq45kv7qcgfe72yg"

        ["key", "public", prv1] `shouldStdOut` (pub1 ++ "\n")


        ["key", "public", "xp"] `expectStdErr` (`shouldBe`
            (fullKeyEncodingDescription ++ "\n"))

        ["key", "public", "xprv118rppm"] `expectStdErr` (`shouldBe`
            ("StringToDecodeTooShort\n"))

        let hexErr = "Expected key to be 96 bytes in the case of a private key \
                     \and, 64 bytes for public keys. This key is 2 bytes.\n"

        ["key", "public", "0000"] `expectStdErr` (`shouldBe` hexErr)


    describe "key inspect" $ do
        let xprv = "588102383ed9ecc5c44e1bfa18d1cf8ef19a7cf806a20bb4cbbe4e51166\
                   \6cf48d6fd7bec908e4c6ced5f0c4f0798b1b619d6b61e6110492b5ebb43\
                   \0f570488f0"
        let cc = "74a9fc9a22f0a61b2ab9b1f1a990e3f8\
                 \dd6fbed4ad474371095c74db3d9c743a"
        ["key", "inspect", xprv ++ cc] `shouldStdOut`
            mconcat [ "extended private key: "
                , xprv
                , "\n"
                , "chain code: "
                , cc
                , "\n"
                ]

        let xpub =
              "20997b093a426804de5120fa2b6d2184a605274b364201ddc9f79307eae8dfed"
        let cc2 =
              "74a9fc9a22f0a61b2ab9b1f1a990e3f8dd6fbed4ad474371095c74db3d9c743a"
        ["key", "inspect", xpub ++ cc2] `shouldStdOut`
            mconcat [ "extended public key: "
                , xpub
                , "\n"
                , "chain code: "
                , cc2
                , "\n"
                ]

    describe "CliKeyScheme" $ do
        it "all allowedWordLengths are supported"
            $ property prop_allowedWordLengthsAllWork

        it "derive . toPublic == toPublic . derive (for soft indices)"
            $ property prop_publicKeyDerivation

        it "scheme == scheme (reflexivity)" $ property $ \s ->
            propCliKeySchemeEquality
                (newCliKeyScheme s)
                (newCliKeyScheme s)

        it "scheme == mapKey (fromBS . toBS) scheme" $ property
            $ propCliKeySchemeCodecRoundTrip keyByteStringCodec

        it "scheme == mapKey (fromAny . toAny) scheme" $ property
            $ propCliKeySchemeCodecRoundTrip (anyKeyCodec . keyByteStringCodec )

        it "scheme == mapKey (fromBech32 . toBech32) scheme" $ property
            $ propCliKeySchemeCodecRoundTrip (keyBech32Codec . keyByteStringCodec )

        it "ledger /= icarus" $ do
            expectFailure $ propCliKeySchemeEquality
                (newCliKeyScheme Ledger)
                (newCliKeyScheme Icarus)

  where
    backspace :: Text
    backspace = T.singleton (toEnum 127)

-- | For soft indices, public key derivation should be "equivalent" to private
-- key derivation.
--
-- I.e. The following diagram should commute:
--
-- @
--                       toPublic
--
--              xprv +-----------------> xpub
--                +                       +
--                |                       |
--                |                       |
-- deriveChildKey |                       | deriveChildKey
--                |                       |
--                |                       |
--                v                       v
--           child xprv +-----------> child xpub
--
--                        toPublic
-- @
--
prop_publicKeyDerivation
    :: ByronWalletStyle
    -> DerivationIndex
    -> XPrv
    -> Property
prop_publicKeyDerivation style i key = do
    if isSoftIndex i
    then (public k >>= derive) === (derive k >>= toPublic s)
    else property $ isLeft (public k >>= derive)
  where
    derive = flip (deriveChildKey s) i
    public = toPublic s
    k = AXPrv key

    isSoftIndex = (< (DerivationIndex firstHardenedIndex))

    s :: CliKeyScheme XPrvOrXPub (Either String)
    s = newCliKeyScheme style

prop_allowedWordLengthsAllWork :: ByronWalletStyle -> Property
prop_allowedWordLengthsAllWork style = do
    (forAll (genAllowedMnemonic s) propCanRetrieveRootKey)
  where
    s :: CliKeyScheme XPrvOrXPub (Either String)
    s = newCliKeyScheme style

    propCanRetrieveRootKey :: [Text] -> Property
    propCanRetrieveRootKey mw = case mnemonicToRootKey s mw of
        Right _ -> property True
        Left e -> counterexample
            (show (length mw) ++ " words, failed with: " ++ e)
            (property False)

propCliKeySchemeCodecRoundTrip
    :: Codec (Either String) XPrvOrXPub a
    -> ByronWalletStyle
    -> Property
propCliKeySchemeCodecRoundTrip c style =
            propCliKeySchemeEquality
                scheme
                (mapKey (inverse c) . mapKey c $ scheme)
  where
    scheme = newCliKeyScheme style

propCliKeySchemeEquality
    :: CliKeyScheme XPrvOrXPub (Either String)
    -> CliKeyScheme XPrvOrXPub (Either String)
    -> Property
propCliKeySchemeEquality s1 s2 = do
    (forAll (genAllowedMnemonic s1) propSameMnem)
    .&&.
    (forAll (genKey s1) propSameChild)
    .&&.
    (forAll (genKey s1) propSameToPublic)
    .&&.
    (forAll (genKey s1) propSameInspect)
    .&&.
    (allowedWordLengths s1) === (allowedWordLengths s2)
  where
    propSameMnem :: [Text] -> Property
    propSameMnem mw = (mnemonicToRootKey s1 mw) === (mnemonicToRootKey s2 mw)

    propSameChild :: XPrvOrXPub -> DerivationIndex -> Property
    propSameChild k i = (deriveChildKey s1 k i) === (deriveChildKey s2 k i)

    propSameToPublic :: XPrvOrXPub -> Property
    propSameToPublic k = (toPublic s1 k) === (toPublic s2 k)

    propSameInspect :: XPrvOrXPub -> Property
    propSameInspect k = (inspect s1 k) === (inspect s2 k)

    unsafe = either (error . show) id

    genRootKey :: CliKeyScheme XPrvOrXPub (Either String) -> Gen XPrvOrXPub
    genRootKey s = unsafe
        . mnemonicToRootKey s
        <$> genAllowedMnemonic s

    genKey s = oneof [genRootKey s, (unsafe . toPublic s) <$> genRootKey s]

genAllowedMnemonic :: CliKeyScheme key m -> Gen [Text]
genAllowedMnemonic s = oneof (map genMnemonicOfSize $ allowedWordLengths s)

genMnemonicOfSize :: Int -> Gen [Text]
genMnemonicOfSize = \case
    12 -> mnemonicToText <$> genMnemonic @12
    15 -> mnemonicToText <$> genMnemonic @15
    18 -> mnemonicToText <$> genMnemonic @18
    21 -> mnemonicToText <$> genMnemonic @21
    24 -> mnemonicToText <$> genMnemonic @24
    n  -> error $ "when this test was written, " ++ show n ++
            " was not a valid length of a mnemonic"

instance Show XPrv where
    show = show . unXPrv

instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

deriving instance Eq XPrvOrXPub
deriving instance Show XPrvOrXPub

instance Arbitrary XPrv where
    arbitrary = do
        mw <- genMnemonicOfSize 15
        let AXPrv rootKey = either (error . show) id $ mnemonicToRootKey s mw
        return rootKey
      where
        s = newCliKeyScheme Icarus

genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

fromTextGolden
    :: (HasCallStack, FromText a, Show a, Eq a)
    => Text
    -> String
    -> Either String a
    -> Spec
fromTextGolden str desc expected =
    it (show str ++ " " ++ desc) $
        fromText str `shouldBe` (left TextDecodingError expected)

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
test fn (GetLineTest prompt_ input_ output expected) =
    withSystemTempDirectory "cardano-wallet-cli" $ \dir -> do
        -- Setup
        let fstdin = dir </> "stdin"
        let fstdout = dir </> "stdout"
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

instance Arbitrary MnemonicSize where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary ByronWalletStyle where
    shrink = genericShrink
    arbitrary = arbitraryBoundedEnum `suchThat` (/= Random)

instance Arbitrary (Port "test") where
    arbitrary = arbitraryBoundedEnum
    shrink p
        | p == minBound = []
        | otherwise = [pred p]

instance Arbitrary DerivationIndex where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary DerivationPath where
    arbitrary = DerivationPath . getNonEmpty <$> arbitrary
