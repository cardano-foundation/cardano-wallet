{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLISpec
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..)
    , TxId
    , cli
    , cmdAddress
    , cmdKey
    , cmdMnemonic
    , cmdNetwork
    , cmdStakePool
    , cmdTransaction
    , cmdWallet
    , cmdWalletCreate
    , hGetLine
    , hGetSensitiveLine
    , metadataOption
    , poolMetadataSourceOption
    )
import Cardano.Wallet.Api.Client
    ( addressClient
    , networkClient
    , stakePoolClient
    , transactionClient
    , walletClient
    )
import Cardano.Wallet.Api.Types
    ( ApiT (..), ApiTxMetadata (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolMetadataSource, TxMetadata (..), TxMetadataValue (..) )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( mapM_ )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), toText )
import Options.Applicative
    ( ParserInfo
    , ParserPrefs
    , ParserResult (..)
    , columns
    , execParserPure
    , info
    , prefs
    , renderFailure
    )
import System.Environment
    ( getProgName )
import System.FilePath
    ( (</>) )
import System.IO
    ( Handle, IOMode (..), hClose, openFile )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Hspec
    ( HasCallStack
    , Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Large (..)
    , arbitraryBoundedEnum
    , checkCoverage
    , cover
    , (===)
    )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Specification / Usage Overview" $ do

        ["--help"] `shouldShowUsage`
            [ "The CLI is a proxy to the wallet server, which is required for"
            , "most commands. Commands are turned into corresponding API calls,"
            , "and submitted to an up-and-running server. Some commands do not"
            , "require an active server and can be run offline (e.g."
            , "'recovery-phrase generate')."
            , ""
            , "Usage:  COMMAND"
            , "  Cardano Wallet Command-Line Interface (CLI)"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  recovery-phrase          About recovery phrases"
            , "  key                      About public/private keys"
            , "  wallet                   About wallets"
            , "  transaction              About transactions"
            , "  address                  About addresses"
            , "  stake-pool               About stake pools"
            , "  network                  About the network"
            ]

        ["recovery-phrase", "--help"] `shouldShowUsage`
            [ "Usage:  recovery-phrase COMMAND"
            , "  About recovery phrases"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  generate                 Generate an English recovery phrase"
            ]

        ["recovery-phrase", "generate", "--help"] `shouldShowUsage`
            [ "Usage:  recovery-phrase generate [--size INT]"
            , "  Generate an English recovery phrase"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --size INT               Number of mnemonic words to generate."
            , "                           Must be a multiple of 3. (default: 24)"
            ]

        ["wallet", "--help"] `shouldShowUsage`
            [ "Usage:  wallet COMMAND"
            , "  About wallets"
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

        ["wallet", "create", "from-recovery-phrase", "--help"] `shouldShowUsage`
            [ "Usage:  wallet create from-recovery-phrase [--port INT] STRING"
            , "                                           [--address-pool-gap INT]"
            , "  Create a new wallet using a recovery phrase."
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
            , "  About transactions"
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
            , "  get                      Get a transaction with specified id."
            ]

        ["transaction", "create", "--help"] `shouldShowUsage`
            [ "Usage:  transaction create [--port INT] WALLET_ID"
            , "                           --payment PAYMENT [--metadata JSON]"
            , "  Create and submit a new transaction."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --payment PAYMENT        address to send to and amount to send"
            , "                           separated by @, e.g."
            , "                           '<amount>@<address>'"
            , "  --metadata JSON          Application-specific transaction"
            , "                           metadata as a JSON object. The value"
            , "                           must match the schema defined in the"
            , "                           cardano-wallet OpenAPI specification."
            ]

        ["transaction", "fees", "--help"] `shouldShowUsage`
            [ "Usage:  transaction fees [--port INT] WALLET_ID --payment PAYMENT"
            , "                         [--metadata JSON]"
            , "  Estimate fees for a transaction."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --payment PAYMENT        address to send to and amount to send"
            , "                           separated by @, e.g."
            , "                           '<amount>@<address>'"
            , "  --metadata JSON          Application-specific transaction"
            , "                           metadata as a JSON object. The value"
            , "                           must match the schema defined in the"
            , "                           cardano-wallet OpenAPI specification."
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

        ["transaction", "get", "--help"] `shouldShowUsage`
            [ "Usage:  transaction get [--port INT] WALLET_ID TRANSACTION_ID"
            , "  Get a transaction with specified id."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["address", "--help"] `shouldShowUsage`
            [ "Usage:  address COMMAND"
            , "  About addresses"
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
            , "  import                   Import a random address generated"
            , "                           elsewhere. Only available for random"
            , "                           wallets. The address must belong to"
            , "                           the target wallet."
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

        ["address", "import", "--help"] `shouldShowUsage`
            [ "Usage:  address import [--port INT] WALLET_ID ADDRESS"
            , "  Import a random address generated elsewhere. Only available for"
            , "  random wallets. The address must belong to the target wallet."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            ]

        ["stake-pool", "list", "--help"] `shouldShowUsage`
            [ "Usage:  stake-pool list [--port INT] [--stake STAKE]"
            , "  List all known stake pools."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
            , "  --stake STAKE            The stake you intend to delegate,"
            , "                           which affects the rewards and the"
            , "                           ranking of pools."
            ]

        ["network", "--help"] `shouldShowUsage`
            [ "Usage:  network COMMAND"
            , "  About the network"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  information              View network information."
            , "  parameters               View network parameters for the"
            , "                           current epoch."
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
            [ "Usage:  network parameters [--port INT]"
            , "  View network parameters for the current epoch."
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  --port INT               port used for serving the wallet"
            , "                           API. (default: 8090)"
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

        ["key", "--help"] `shouldShowUsage`
            [ "Usage:  key COMMAND"
            , "  About public/private keys"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "Available commands:"
            , "  from-recovery-phrase     Convert a recovery phrase to an"
            , "                           extended private key"
            , "  child                    Derive child keys from a parent"
            , "                           public/private key"
            , "  public                   Get the public counterpart of a"
            , "                           private key"
            , "  inspect                  Show information about a key"
            , "  hash                     Get the hash of a public key"
            , ""
            , "Example:"
            , "  \ESC[1m$ "<>progName<>" recovery-phrase generate --size 15 \\\ESC[0m"
            , "    \ESC[1m| "<>progName<>" key from-recovery-phrase Shelley > root.prv\ESC[0m"
            , "  "
            , "  \ESC[1m$ cat root.prv \\\ESC[0m"
            , "    \ESC[1m| "<>progName<>" key child 1852H/1815H/0H \\\ESC[0m"
            , "    \ESC[1m| tee acct.prv \\\ESC[0m"
            , "    \ESC[1m| "<>progName<>" key public > acct.pub\ESC[0m"
            , "  "
            , "  \ESC[1m$ "<>progName<>" key inspect <<< $(cat acct.prv)\ESC[0m"
            , "  {"
            , "      \"key_type\": \"private\","
            , "      \"chain_code\": \"67bef6f80df02c7452e20e76ffb4bb57cae8aac2adf042b21a6b19e4f7b1f511\","
            , "      \"extended_key\": \"90ead3efad7aacac242705ede323665387f49ed847bed025eb333708ccf6aa54403482a867daeb18f38c57d6cddd7e6fd6aed4a3209f7425a3d1c5d9987a9c5f\""
            , "  }"
            , "  "
            , "  \ESC[1m$ "<>progName<>" key inspect <<< $(cat acct.pub)\ESC[0m"
            , "  {"
            , "      \"key_type\": \"public\","
            , "      \"chain_code\": \"67bef6f80df02c7452e20e76ffb4bb57cae8aac2adf042b21a6b19e4f7b1f511\","
            , "      \"extended_key\": \"d306350ee88f51fb710252e27f0c40006c58e994761b383e02d400e2be59b3cc\""
            , "  }"
            ]

        ["key", "from-recovery-phrase", "--help"] `shouldShowUsage`
            [ "Usage:  key from-recovery-phrase STYLE"
            , "  Convert a recovery phrase to an extended private key"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  STYLE                    Byron | Icarus | Shelley"
            , ""
            , "The recovery phrase is read from stdin."
            , ""
            , "Example:"
            , "  \ESC[1m$ "<>progName<>" recovery-phrase generate \\\ESC[0m"
            , "  \ESC[1m| "<>progName<>" key from-recovery-phrase Icarus\ESC[0m"
            ]

        ["key", "child", "--help"] `shouldShowUsage`
            [ "Usage:  key child DERIVATION-PATH"
            , "  Derive child keys from a parent public/private key"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , "  DERIVATION-PATH          Slash-separated derivation path."
            , "                           Hardened indexes are marked with a 'H'"
            , "                           (e.g. 1852H/1815H/0H/0)."
            , ""
            , "The parent key is read from stdin."
            ]

        ["key", "public", "--help"] `shouldShowUsage`
            [ "Usage:  key public (--without-chain-code | --with-chain-code)"
            , "  Get the public counterpart of a private key"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "The private key is read from stdin.To get extended public key pass '--with-chain-code'.To get public key pass '--without-chain-code'."
            ]

        ["key", "inspect", "--help"] `shouldShowUsage`
            [ "Usage:  key inspect "
            , "  Show information about a key"
            , ""
            , "Available options:"
            , "  -h,--help                Show this help text"
            , ""
            , "The parent key is read from stdin."
            ]

    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @(Port "test")

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

    describe "Pool metadata fetching option" $ do
        let parse arg = execParserPure defaultPrefs
                (info poolMetadataSourceOption mempty)
                ["--pool-metadata-fetching", arg]
        let ok arg (Success url) = Right url == fromText @PoolMetadataSource
                (T.pack arg)
            ok _ _ = False
        let err _ (Failure _) = True
            err _ _ = False
        mapM_
            (\(desc, arg, tst) -> it desc (parse arg `shouldSatisfy` tst arg))
            [ ( "http", "http://localhost", ok )
            , ( "https", "https://iohkdev.io", ok )
            , ( "not http(s)", "gopher://iohk.io", err )
            , ( "relative", "/home/user", err )
            ]

    describe "Tx Metadata JSON option" $ do
        let parse arg = execParserPure defaultPrefs
                (info metadataOption mempty) ["--metadata", arg]
        let md = ApiT (TxMetadata (Map.singleton 42 (TxMetaText "hi")))
        let ok ex (Success res) = ex == getApiTxMetadata res
            ok _ _ = False
        let err (Failure _) = True
            err _ = False
        mapM_
            (\(desc, arg, tst) -> it desc (parse arg `shouldSatisfy` tst))
            [ ("valid", "{ \"42\": { \"string\": \"hi\" } }", ok (Just md))
            , ("malformed", "testing", err)
            , ("malformed trailling", "{ \"0\": { \"string\": \"\" } } arstneio", err)
            , ("invalid", "{ \"json\": true }", err)
            , ("null 1", "{ \"0\": null }", err)
            , ("null 2", "null", ok Nothing)
            , ("null 3", "{ }", ok (Just (ApiT mempty)))
            ]

  where
    backspace :: Text
    backspace = T.singleton (toEnum 127)

{-------------------------------------------------------------------------------
                               Test Helpers
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

defaultPrefs :: ParserPrefs
defaultPrefs = prefs (mempty <> columns 65)

parser :: ParserInfo (IO ())
parser = cli $ mempty
    <> cmdMnemonic
    <> cmdKey
    <> cmdWallet cmdWalletCreate walletClient
    <> cmdTransaction transactionClient walletClient
    <> cmdAddress addressClient
    <> cmdStakePool (stakePoolClient @()) -- Type of pool not important here.
    <> cmdNetwork networkClient

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

-- | Get program name to avoid hard-coding it in documentation excerpt.
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}

{-------------------------------------------------------------------------------
                               Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Port "test") where
    arbitrary = arbitraryBoundedEnum
    shrink p
        | p == minBound = []
        | otherwise = [pred p]
