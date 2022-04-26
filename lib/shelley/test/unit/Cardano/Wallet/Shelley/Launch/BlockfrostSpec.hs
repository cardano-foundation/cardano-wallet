module Cardano.Wallet.Shelley.Launch.BlockfrostSpec
    ( spec
    ) where

import Prelude

import qualified Blockfrost.Client.Types as Blockfrost
import qualified Data.Text as T

import Blockfrost.Env
    ( Env (Testnet) )
import Cardano.Wallet.Shelley.Launch
    ( Mode (Light, Normal), modeOption )
import Cardano.Wallet.Shelley.Launch.Blockfrost
    ( TokenException (..)
    , TokenFile (TokenFile)
    , TokenFileException (TokenFileException)
    , readToken
    )
import Options.Applicative
    ( ParserFailure (execFailure)
    , ParserResult (CompletionInvoked, Failure, Success)
    , defaultPrefs
    , execParserPure
    , fullDesc
    , info
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldReturn
    , shouldStartWith
    , shouldThrow
    )
import Test.Utils.Platform
    ( isWindows )
import UnliftIO
    ( withSystemTempFile )
import UnliftIO.IO
    ( hClose )

spec :: Spec
spec = describe "Blockfrost CLI options" $ do
    it "modeOption --node-socket" $ do
        let parserInfo = info modeOption fullDesc
            args = ["--node-socket", mockSocketOrPipe]
        case execParserPure defaultPrefs parserInfo args of
            Failure pf -> expectationFailure $ show pf
            CompletionInvoked cr -> expectationFailure $ show cr
            Success (Light _) -> expectationFailure "Normal mode expected"
            Success (Normal _conn) -> pure ()

    it "modeOption --light" $ withSystemTempFile "blockfrost.token" $ \f h -> do
        let parserInfo = info modeOption fullDesc
            args = ["--light", "--blockfrost-token-file", f]
            net = "testnet"
            projectId = "jlUej4vcMt3nKPRAiNpLUEeKBIEPqgH2"
        case execParserPure defaultPrefs parserInfo args of
            Failure pf -> expectationFailure $ show pf
            CompletionInvoked cr -> expectationFailure $ show cr
            Success (Normal _conn) -> expectationFailure "Light mode expected"
            Success (Light tf) -> do
                hClose h *> writeFile f (net <> projectId)
                readToken tf `shouldReturn`
                    Blockfrost.Project Testnet (T.pack projectId)

    it "modeOption requires --light flag" $ do
        let parserInfo = info modeOption fullDesc
            args = ["--blockfrost-token-file", mockSocketOrPipe]
        case execParserPure defaultPrefs parserInfo args of
            Failure pf | (help, _code, _int) <- execFailure pf "" ->
                show help `shouldStartWith` "Missing: --light"
            result -> expectationFailure $ show result

    it "readToken throws in case of a non-existing token file" $ do
        readToken (TokenFile "non-existing-file")
            `shouldThrow` \(TokenFileException _) -> True

    it "readToken throws in case of an empty token file" $
        withSystemTempFile "blockfrost.token" $ \f h -> do
            hClose h
            readToken (TokenFile f) `shouldThrow` \(EmptyToken _) -> True

    it "readToken throws in case of an invalid token file content" $
        withSystemTempFile "blockfrost.token" $ \f h -> do
            hClose h *> writeFile f "invalid"
            readToken (TokenFile f) `shouldThrow` \(InvalidToken _) -> True

mockSocketOrPipe :: String
mockSocketOrPipe = if isWindows then "\\\\.\\pipe\\test" else "/tmp/pipe"
