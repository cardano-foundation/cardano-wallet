{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Benchmarks.HistorySpec
    ( spec
    )
where

import Cardano.Wallet.Benchmarks.Collect
    ( Result (..)
    )
import Cardano.Wallet.Benchmarks.History
    ( HarmonizedHistory (..)
    , IndexedSemantic (..)
    , harmonizeHistory
    , historyFromResults
    , parseHistory
    , parseResults
    , renderHarmonizedHistoryCsv
    )
import Data.Csv
    ( encodeByName
    )
import Data.Time
    ( fromGregorian
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Prelude

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Set as Set

sampleCsv :: BL8.ByteString
sampleCsv =
    BL8.unlines
        [ "semantic,value,units,iterations"
        , "restore wallet,3.5,s,1"
        , "list wallets,120.0,ms,10"
        ]

spec :: Spec
spec = do
    describe "parseResults" $ do
        it "parses benchmark CSV into indexed results"
            $ case parseResults sampleCsv of
                Left e -> fail e
                Right rs@((ix0, r0) : _) -> do
                    length rs `shouldBe` 2
                    index ix0 `shouldBe` 0
                    resultValue r0 `shouldBe` 3.5
                Right [] -> fail "expected non-empty results"

        it "rejects malformed CSV" $ do
            let bad = "not,a,valid,csv\nfoo"
            parseResults bad `shouldSatisfy` isLeft'

    describe "historyFromResults" $ do
        it "builds history keyed by day"
            $ case parseResults sampleCsv of
                Left e -> fail e
                Right rs -> do
                    let day = fromGregorian 2025 6 15
                        h = historyFromResults day rs
                        entries = MMap.assocs h
                    length entries `shouldBe` 2

    describe "round-trip through harmonize and render" $ do
        it "produces CSV with expected structure"
            $ case parseResults sampleCsv of
                Left e -> fail e
                Right rs -> do
                    let day1 = fromGregorian 2025 6 15
                        day2 = fromGregorian 2025 6 16
                        h =
                            historyFromResults day1 rs
                                <> historyFromResults
                                    day2
                                    rs
                    case harmonizeHistory h of
                        Left _ ->
                            fail "harmonization failed"
                        Right hh -> do
                            days hh
                                `shouldBe` Set.fromList
                                    [day1, day2]
                            let (hdr, rows) =
                                    renderHarmonizedHistoryCsv
                                        hh
                                csv =
                                    encodeByName hdr rows
                            csv
                                `shouldSatisfy` ( not
                                                    . BL8.null
                                                )

    describe "parseHistory round-trip" $ do
        it "round-trips through render and parse"
            $ case parseResults sampleCsv of
                Left e -> fail e
                Right rs -> do
                    let day = fromGregorian 2025 6 15
                        h = historyFromResults day rs
                    case harmonizeHistory h of
                        Left _ ->
                            fail "harmonization failed"
                        Right hh -> do
                            let csv =
                                    uncurry encodeByName
                                        $ renderHarmonizedHistoryCsv
                                            hh
                            case parseHistory csv of
                                Left e' -> fail e'
                                Right h' ->
                                    length (MMap.assocs h')
                                        `shouldBe` 2

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False
