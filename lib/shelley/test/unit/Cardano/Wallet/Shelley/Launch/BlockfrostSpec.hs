module Cardano.Wallet.Shelley.Launch.BlockfrostSpec
    ( spec
    ) where

import Prelude

import qualified Blockfrost.Client.Types as Blockfrost
import qualified Data.Text as T

import Blockfrost.Env
    ( Env (Testnet) )
import Cardano.Wallet.Shelley.Launch.Blockfrost
    ( readToken, tokenFileOption )
import Options.Applicative
    ( ParserResult (CompletionInvoked, Failure, Success)
    , defaultPrefs
    , execParserPure
    , fullDesc
    , info
    )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldReturn )
import UnliftIO
    ( withSystemTempFile )
import UnliftIO.IO
    ( hClose )

spec :: Spec
spec = describe "Blockfrost CLI options" $
    it "tokenFileOption" $ withSystemTempFile "blockfrost.token" $ \fp h -> do
        let parserInfo = info tokenFileOption fullDesc
            args = ["--blockfrost-token-file", fp]
            net = "testnet"
            projectId = "jlUej4vcMt3nKPRAiNpLUEeKBIEPqgH2"
        case execParserPure defaultPrefs parserInfo args of
            Failure pf -> expectationFailure $ show pf
            CompletionInvoked cr -> expectationFailure $ show cr
            Success tf -> do
                hClose h *> writeFile fp (net <> projectId)
                readToken tf `shouldReturn`
                    Blockfrost.Project Testnet (T.pack projectId)
