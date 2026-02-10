module FlakyTests.HspecParserSpec (spec) where

import Prelude

import qualified Data.ByteString as BS
import Data.Text
    ( Text
    )
import qualified Data.Text.Encoding as T
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import FlakyTests.HspecParser
    ( extractFailures
    , extractSummary
    , stripAnsi
    , stripTimestamps
    )
import FlakyTests.Types
    ( TestFailure (..)
    )

readGolden :: IO Text
readGolden = do
    bs <- BS.readFile "test/golden/conway-integration.log"
    pure (T.decodeUtf8 bs)

spec :: Spec
spec = describe "HspecParser" $ do
    describe "stripTimestamps" $ do
        it "removes GHA timestamp prefixes" $ do
            let input =
                    "2026-02-10T14:40:35.4702189Z hello world"
            stripTimestamps input `shouldBe` "hello world\n"

        it "leaves non-timestamped lines alone" $ do
            let input = "no timestamp here"
            stripTimestamps input `shouldBe` "no timestamp here\n"

    describe "stripAnsi" $ do
        it "removes ANSI color codes" $ do
            let input = "\ESC[38;5;1mFAILED\ESC[0m"
            stripAnsi input `shouldBe` "FAILED"

        it "handles text without ANSI codes" $ do
            stripAnsi "plain text" `shouldBe` "plain text"

    describe "extractSummary (golden)" $ do
        it "extracts correct example and failure counts" $ do
            golden <- readGolden
            extractSummary golden `shouldBe` Just (8, 3)

    describe "extractFailures (golden)" $ do
        it "extracts all 3 failed test names" $ do
            golden <- readGolden
            let failures = extractFailures golden
            length failures `shouldBe` 3

        it "extracts correct test paths" $ do
            golden <- readGolden
            let failures = extractFailures golden
                paths = map testPath failures
            paths
                `shouldBe` [ "API Specifications, SHELLEY_WALLETS,"
                                <> " WALLETS_UPDATE_PASS_01a"
                                <> " - passphraseLastUpdate gets updated"
                           , "API Specifications, TRANSACTIONS,"
                                <> " TRANS_ESTIMATE_01"
                                <> " - Can estimate fees"
                           , "API Specifications, STAKE_POOLS,"
                                <> " POOLS_JOIN_01 - Can join a pool"
                           ]

    describe "extractSummary (inline)" $ do
        it "parses summary without timestamps" $ do
            let input = "  42 examples, 5 failures"
            extractSummary input `shouldBe` Just (42, 5)

        it "returns Nothing for non-summary text" $ do
            extractSummary "no summary here" `shouldBe` Nothing
