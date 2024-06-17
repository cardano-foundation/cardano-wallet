module Cardano.Wallet.Benchmark.Memory.PmapSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Benchmark.Memory.Pmap
    ( Line (..)
    , lineParser
    , pmapParser
    )
import Control.Monad
    ( replicateM
    )
import Data.Attoparsec.ByteString.Char8
    ( parseOnly
    )
import Paths_cardano_wallet_blackbox_benchmarks
    ( getDataFileName
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "pmap parser" $ do
        it "can parse a line"
            $ parseOnly
                lineParser
                "0000000000400000  50668K r-x-- cardano-wallet\n"
            `shouldBe` Right (Line 0x400000 50668 "r-x--" "cardano-wallet")
        it "can parse 2 lines" $ do
            let input =
                    "0000000000400000  50668K r-x-- cardano-wallet\n\
                    \000000f000600000  50669K -wxp- cardano-wallet2\n"
            parseOnly (replicateM 2 lineParser) input
                `shouldBe` Right
                    [ Line 0x400000 50668 "r-x--" "cardano-wallet"
                    , Line 0xf000600000 50669 "-wxp-" "cardano-wallet2"
                    ]
        it "can parse a topline and lines of pmap output" $ do
            input <-
                getDataFileName "data/hoogle-pmap.txt"
                    >>= B8.readFile
            case parseOnly pmapParser input of
                Left e -> fail e
                Right _ -> pure ()

-- 0000000000400000  50668K r-x-- cardano-wallet
