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
    , DerivationIndex (..)
    , DerivationPath (..)
    , KeyEncoding (..)
    , MnemonicSize (..)
    , Port (..)
    , TxId
    , XPrvOrXPub (..)
    , cli
    , cmdAddress
    , cmdKey
    , cmdMnemonic
    , cmdNetwork
    , cmdStakePool
    , cmdTransaction
    , cmdWallet
    , cmdWalletCreate
    , decodeAnyKey
    , deriveChildKey
    , encodeKey
    , firstHardenedIndex
    , fullKeyEncodingDescription
    , hGetLine
    , hGetSensitiveLine
    , newCliKeyScheme
    , toPublic
    )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , entropyToMnemonic
    , mnemonicToText
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
    ( ParserInfo
    , ParserPrefs
    , ParserResult (..)
    , columns
    , execParserPure
    , prefs
    , renderFailure
    )
import System.Environment
    ( getProgName )
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
    ( Expectation
    , HasCallStack
    , HasCallStack
    , Spec
    , describe
    , expectationFailure
    , it
    , runIO
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
    , forAll
    , genericShrink
    , oneof
    , property
    , suchThat
    , vector
    , (===)
    )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Specification / Usage Overview" $ do

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

        name <- runIO getProgName
        ["key", "--help"] `shouldShowUsage`
            [ "Usage:  key COMMAND"
            , "  Derive and manipulate keys."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  root                     Extract root extended private key from"
            , "                           a mnemonic sentence."
            , "  child                    Derive child keys."
            , "  public                   Extract the public key from a private"
            , "                           key."
            , "  inspect                  Show information about a key."
            , ""
            , "Keys are read from standard input for convenient chaining of commands."
            , ""
            , "Bech32- and hexadecimal encodings are supported."
            , ""
            , "Example:"
            , "$ " ++ name ++ " key root --wallet-style icarus --encoding bech32 -- express theme celery coral permit ... \\"
            , "    | " ++ name ++ " key public"
            , "xpub1k365denpkmqhj9zj6qpax..."
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
            [ "Usage:  key child --path DER-PATH"
            , "  Derive child keys."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --path DER-PATH          Derivation path e.g. 44H/1815H/0H/0"
            , ""
            , "The parent key is read from standard input."
            ]

        ["key", "public", "--help"] `shouldShowUsage`
            [ "Usage:  key public "
            , "  Extract the public key from a private key."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "The private key is read from standard input."
            ]

        ["key", "inspect", "--help"] `shouldShowUsage`
            [ "Usage:  key inspect "
            , "  Show information about a key."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "The key is read from standard input."
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


    describe "Key encoding" $ do
        let xprv1Bech32 =
                "xprv18rppm7hzl6m68c5zqh5nxp4sa8d6wmazz6g4p4spt67k3ck3kf0u2u3pg\
                \aexmgpafmpwhjyks546rk5fxw9ma685zw8sy6erhtn3s9lkq2jy27ejzgca990\
                \aa67mvauxnnajh26hdxprzjry7q26tx0qvy6zgldk"

        let xprv1Bech32FooHrp =
                "foo18rppm7hzl6m68c5zqh5nxp4sa8d6wmazz6g4p4spt67k3ck3kf0u2u3pga\
                \exmgpafmpwhjyks546rk5fxw9ma685zw8sy6erhtn3s9lkq2jy27ejzgca990a\
                \a67mvauxnnajh26hdxprzjry7q26tx0qvy9csa56"

        -- This is a /public/ key with a "xprv" hrp
        let notAXPrv = "xprv1hqslt9dm5stzpphhfjah5mk3s27psdt2yal8t5rz9e8dm3r7zp\
                       \alvq4yg4anyy33622lmm4akemcd88m9w44w6vzx9yxfuq45kv7qcg0j\
                       \8juy"
        -- This is a /private/ key with a "xpub" hrp
        let notAXPub = "xpub18rppm7hzl6m68c5zqh5nxp4sa8d6wmazz6g4p4spt67k3ck3kf\
                       \0u2u3pgaexmgpafmpwhjyks546rk5fxw9ma685zw8sy6erhtn3s9lkq\
                       \2jy27ejzgca990aa67mvauxnnajh26hdxprzjry7q26tx0qvyhat9w6"
        describe "Detection of key encoding" $ do
            describe "hex" $ do
                decodeKeyGoldenErr "000000" "too short"
                    "Expected key to be 96 bytes in the case of a private key \
                    \and, 64 bytes for public keys. This key is 3 bytes."

            describe "bech32" $ do
                decodeKeyGoldenErr "xprv1hs" "xprv1hs"
                    "Bech32 error: string is too short"
                decodeKeyGoldenErr "xpub1hs" "xpubhs"
                    "Bech32 error: string is too short"
                decodeKeyGoldenErr (xprv1Bech32 <> "ö") "trailing \"ö\" char" $
                    "Bech32 error: Invalid character(s) in string:\n"
                    <> T.unpack xprv1Bech32 <> "\ESC[91m\246\ESC[0m\ESC[0m"
                decodeKeyGoldenErr (xprv1Bech32 <> "n") "trailing \"n\" char"
                    "Bech32 error: Invalid character(s) in string"

                -- We should /perhaps/ return a bech32 specific error here
                decodeKeyGoldenErr xprv1Bech32FooHrp "wrong hrp"
                    fullKeyEncodingDescription

        describe "Key encoding: Bech32 goldens" $ do
            it "xpub pretending to be xprv fails" $ do
                decodeAnyKey notAXPrv
                    `shouldBe`
                    (Left "Expected extended private key to be 96 bytes \
                          \but got 64 bytes.")

            -- TODO: Make the error message match the previous case.
            -- This one currently comes from cardano-crypto.
            it "xprv pretending to be xpub fails" $ do
                decodeAnyKey notAXPub
                    `shouldBe`
                    (Left "error: xprv needs to be 64 bytes: \
                          \got 96 bytes")

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

    describe "CLI Key derivation properties" $ do
        it "all allowedWordLengths are supported"
            $ property prop_allowedWordLengthsAllWork

        it "derive . toPublic == toPublic . derive (for soft indices)"
            $ property prop_publicKeyDerivation

        describe "Key Encoding Roundtrips" $ do
            let decodeAny = fmap fst . decodeAnyKey
            it "decodeAnyKey . encodeKey Hex == pure" $ property $ \k -> do
                (encodeKey Hex k >>= decodeAny) === Right k

            it "decodeAnyKey . encodeKey Bech32 == pure" $ property $ \k -> do
                (encodeKey Bech32 k >>= decodeAny) === Right k

            it "encodeBack . decodeAnyKey . encodeKey enc === encodeKey enc" $
                property $ \k enc -> do
                let f x = decodeAnyKey x >>= \(k', enc') -> encodeKey enc' k'
                (encodeKey enc k >>= f) === (encodeKey enc k)

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
    :: DerivationIndex
    -> XPrv
    -> Property
prop_publicKeyDerivation i key = do
    if isSoftIndex i
    then (toPublic k >>= derive) === (derive k >>= toPublic)
    else property $ isLeft (toPublic k >>= derive)
  where
    derive = flip deriveChildKey i
    k = AXPrv key

    isSoftIndex = (< (DerivationIndex firstHardenedIndex))

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

instance Arbitrary XPrvOrXPub where
    arbitrary = oneof [ AXPrv <$> arbitrary, AXPub . CC.toXPub <$> arbitrary]

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

{-------------------------------------------------------------------------------
                               Test Helpers
-------------------------------------------------------------------------------}

fromTextGolden
    :: (HasCallStack, FromText a, Show a, Eq a)
    => Text
    -> String
    -> Either String a
    -> Spec
fromTextGolden str desc expected =
    it (show str ++ " " ++ desc) $
        fromText str `shouldBe` (left TextDecodingError expected)


decodeKeyGoldenErr
    :: (HasCallStack)
    => Text
    -> String
    -> String
    -> Spec
decodeKeyGoldenErr str desc err =
    it (show str ++ " " ++ desc) $
        decodeAnyKey str `shouldBe` (Left err)


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

defaultPrefs :: ParserPrefs
defaultPrefs = prefs (mempty <> columns 65)

parser :: ParserInfo (IO ())
parser = cli $ mempty
    <> cmdMnemonic
    <> cmdWallet cmdWalletCreate walletClient
    <> cmdTransaction transactionClient walletClient
    <> cmdAddress addressClient
    <> cmdStakePool stakePoolClient
    <> cmdNetwork networkClient
    <> cmdKey

shouldStdOut :: HasCallStack => [String] -> String -> Spec
shouldStdOut args expected = it (unwords args) $ do
    setUtf8EncodingHandles
    case execParserPure defaultPrefs parser args of
        Success x -> capture_ x >>= (`shouldBe` expected)
        CompletionInvoked _ -> expectationFailure
            "expected parser to show usage but it offered completion"
        Failure failure ->
            expectationFailure $ "parser failed with: " ++ show failure

expectStdErr :: HasCallStack => [String] -> (String -> Expectation) -> Spec
expectStdErr args expectation = it (unwords args) $ do
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

shouldShowUsage :: HasCallStack => [String] -> [String] -> Spec
shouldShowUsage args expected = it (unwords args) $
    case execParserPure defaultPrefs parser args of
        Success _ -> expectationFailure
            "expected parser to show usage but it has succeeded"
        CompletionInvoked _ -> expectationFailure
            "expected parser to show usage but it offered completion"
        Failure failure -> do
            let (usage, _) = renderFailure failure mempty
            (lines usage) `shouldBe` expected


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

instance Arbitrary KeyEncoding where
    arbitrary = arbitraryBoundedEnum
