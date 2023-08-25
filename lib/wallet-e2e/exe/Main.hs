module Main where

import qualified Cardano.Wallet.Spec.Data.TestNetwork as TestNetwork
import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( effectsSpec, walletSpec )
import Cardano.Wallet.Spec.Data.TestNetwork
    ( TestNetwork )
import Main.Utf8
    ( withUtf8 )
import Options.Applicative
    ( Parser
    , eitherReader
    , execParser
    , fullDesc
    , help
    , helpDoc
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    )
import Path
    ( Dir, SomeBase (..), parseSomeDir )
import Path.IO
    ( AnyPath (makeAbsolute) )
import Prettyprinter
    ( annotate, vsep, (<+>) )
import Prettyprinter.Render.Terminal
    ( underlined )
import Test.Syd
    ( sydTestWith )

data Options = Options
    { stateDir :: SomeBase Dir
    , testNetwork :: TestNetwork
    }

main :: IO ()
main = withUtf8 do
    Options{stateDir, testNetwork} <-
        execParser
            $ info
                (parser <**> helper)
                (fullDesc <> progDesc "E2E Wallet test suite")
    stateDirectory <-
        case stateDir of
            Abs absDir -> pure absDir
            Rel relDir -> makeAbsolute relDir
    sydTestWith SydTest.defaultSettings{SydTest.settingRetries = 1} do
        effectsSpec
        walletSpec stateDirectory testNetwork

parser :: Parser Options
parser =
    Options
        <$> option
            (eitherReader (first show . parseSomeDir))
            ( long "state-dir"
                <> short 's'
                <> metavar "STATE_DIR"
                <> help
                    "Absolute or relative directory path to save \
                    \node and wallet state"
            )
        <*> option
            (eitherReader (TestNetwork.fromText . toText))
            ( long "network"
                <> short 'n'
                <> metavar "NETWORK"
                <> helpDoc (Just networkDoc)
            )
  where
    enum = annotate underlined
    networkDoc =
        vsep
            [ "Which network to run the tests against:"
            , enum "local"
                <+> "- Automatically starts a local test cluster."
            , enum "preprod"
                <+> "- Automatically starts a preprod node and wallet."
            , enum "manual"
                <+> "- Relies on a node and wallet started manually."
            ]
