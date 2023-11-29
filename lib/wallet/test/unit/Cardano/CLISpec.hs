{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    , cmdNetwork
    , cmdStakePool
    , cmdTransaction
    , cmdWallet
    , cmdWalletCreate
    , hGetLine
    , hGetSensitiveLine
    , metadataOption
    , poolMetadataSourceOption
    , timeToLiveOption
    )
import Cardano.Wallet.Api.Client
    ( addressClient
    , networkClient
    , stakePoolClient
    , transactionClient
    , walletClient
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( detailedMetadata
    , noSchemaMetadata
    )
import Cardano.Wallet.Primitive.Types
    ( PoolMetadataSource
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata (..)
    , TxMetadataValue (..)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , toText
    )
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
    ( getProgName
    )
import System.FilePath
    ( (</>)
    )
import System.IO
    ( Handle
    , IOMode (..)
    , hClose
    , openFile
    )
import Test.Hspec
    ( HasCallStack
    , Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.Goldens
    ( Settings (..)
    , textGolden
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
    ( textRoundtrip
    )
import Test.Utils.Paths
    ( getTestData
    )
import UnliftIO.Concurrent
    ( forkFinally
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Specification / Usage Overview" $ do

        -- IDEA: We could traverse the optparse-applicative structure
        -- to get this list automatically.
        let goldenDir = $(getTestData) </> "Cardano" </> "CLISpec"
        mapM_ (usageGolden goldenDir)
            [ ["--help"]
            , ["wallet", "--help"]
            , ["wallet", "list", "--help"]
            , ["wallet", "create", "from-recovery-phrase", "--help"]
            , ["wallet", "create", "from-public-key", "--help"]
            , ["wallet", "get", "--help"]
            , ["wallet", "update", "--help"]
            , ["wallet", "delete", "--help"]
            , ["wallet", "utxo", "--help"]
            , ["transaction", "--help"]
            , ["transaction", "create", "--help"]
            , ["transaction", "fees", "--help"]
            , ["transaction", "list", "--help"]
            , ["transaction", "submit", "--help"]
            , ["transaction", "forget", "--help"]
            , ["transaction", "get", "--help"]
            , ["address", "--help"]
            , ["address", "list", "--help"]
            , ["address", "create", "--help"]
            , ["address", "import", "--help"]
            , ["stake-pool", "list", "--help"]
            , ["network", "--help"]
            , ["network", "information", "--help"]
            , ["network", "parameters", "--help"]
            , ["network", "clock", "--help"]
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
        let md = detailedMetadata
                (TxMetadata (Map.singleton 42 (TxMetaText "hi")))
        let ok ex (Success res) = ex ==  res
            ok _ _ = False
        let err (Failure _) = True
            err _ = False
        mapM_
            (\(desc, arg, tst) -> it desc (parse arg `shouldSatisfy` tst))
            [ ("valid", "{ \"42\": { \"string\": \"hi\" } }", ok (Just md))
            , ("malformed", "testing", err)
            , ("malformed trailing", "{ \"0\": { \"string\": \"\" } } arstneio", err)
            , ("invalid", "{ \"json\": true }", err)
            , ("null 1", "{ \"0\": null }", err)
            , ("null 2", "null", ok Nothing)
            , ("null 3", "{ }", ok (Just (detailedMetadata mempty)))
            ]
    describe "Tx No-Schema Metadata JSON option" $ do
        let parse arg = execParserPure
                defaultPrefs (info metadataOption mempty) ["--metadata", arg]
        let md = noSchemaMetadata
                (TxMetadata (Map.singleton 42 (TxMetaText "hi")))
        let ok ex (Success res) = ex == res
            ok _ _ = False
        let err (Failure _) = True
            err _ = False
        mapM_
            (\(desc, arg, tst) -> it desc (parse arg `shouldSatisfy` tst))
            [ ("valid", "{ \"42\": \"hi\"    }", ok $ Just md)
            , ("malformed", "testing", err)
            , ("malformed trailing", "{ \"0\": \"\"    } arstneio", err)
            , ("invalid", "{ \"json\": true }", err)
            , ("null 1", "{ \"0\": null }", err)
            , ("null 2", "null", ok Nothing)
            -- this is the default parsing success:
            , ("null 3", "{ }", ok $ Just $ detailedMetadata mempty)

            ]

    describe "Tx TTL option" $ do
        let parse arg = execParserPure defaultPrefs
                (info timeToLiveOption mempty) ["--ttl", arg]
        let ok ex (Success res) = Just (Quantity ex) == res
            ok _ _ = False
        let err (Failure _) = True
            err _ = False
        mapM_
            (\(desc, arg, tst) -> it desc (parse arg `shouldSatisfy` tst))
            [ ("valid integer", "1s", ok 1)
            , ("valid zero", "0s", ok 0)
            , ("invalid negative", "-1s", err)
            , ("invalid fractional", "1.5s", err)
            , ("malformed trailing", "1ss", err)
            , ("malformed suffix", "1", err)
            , ("malformed empty", "", err)
            , ("malformed emptyish", "s", err)
            , ("malformed leading", "a1s", err)
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
    <> cmdWallet cmdWalletCreate walletClient
    <> cmdTransaction transactionClient walletClient
    <> cmdAddress addressClient
    <> cmdStakePool stakePoolClient
    <> cmdNetwork networkClient

usageGolden :: HasCallStack => FilePath -> [String] -> Spec
usageGolden dir args = it (unwords args) $ do
    exe <- T.pack <$> getProgName
    case execParserPure defaultPrefs parser args of
        Success _ -> expectationFailure
            "expected parser to show usage but it has succeeded"
        CompletionInvoked _ -> expectationFailure
            "expected parser to show usage but it offered completion"
        Failure failure -> do
            let (usage, _) = renderFailure failure mempty
            let settings = Settings
                    { goldenDirectory = dir
                    , postProcess = T.replace exe "cardano-wallet" . (<> "\n")
                    }
            textGolden settings (unwords args) (T.pack usage)

{-------------------------------------------------------------------------------
                               Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Port "test") where
    arbitrary = arbitraryBoundedEnum
    shrink p
        | p == minBound = []
        | otherwise = [pred p]
