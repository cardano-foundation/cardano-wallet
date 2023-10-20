module Main where

import qualified Options.Applicative as OptParse
import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    , effectsSpec
    , walletSpec
    )
import Main.Utf8
    ( withUtf8
    )
import Options.Applicative
    ( Parser
    , eitherReader
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    )
import Path
    ( Dir
    , SomeBase (..)
    , parseSomeDir
    )
import Path.IO
    ( AnyPath (makeAbsolute)
    )
import Test.Syd
    ( sydTestWith
    )

main :: IO ()
main = withUtf8 do
    testNetworkOptions <-
        execParser
            $ info
                (parser <**> helper)
                (fullDesc <> progDesc "E2E Wallet test suite")
    testNetwork <- testNetworkOptionsToConfig testNetworkOptions
    sydTestWith SydTest.defaultSettings{SydTest.settingRetries = 1} do
        effectsSpec
        walletSpec testNetwork

--------------------------------------------------------------------------------
-- Command line options --------------------------------------------------------

data TestNetworkOptions
    = TestNetworkOptionManual
    | TestNetworkOptionLocal !(SomeBase Dir)
    | TestNetworkOptionPreprod !(SomeBase Dir) !(SomeBase Dir)

testNetworkOptionsToConfig :: TestNetworkOptions -> IO TestNetworkConfig
testNetworkOptionsToConfig = \case
    TestNetworkOptionManual ->
        pure TestNetworkManual
    TestNetworkOptionLocal stateDir -> do
        absStateDir <- makeDirAbsolute stateDir
        pure (TestNetworkLocal absStateDir)
    TestNetworkOptionPreprod stateDir nodeConfigsDir -> do
        absStateDir <- makeDirAbsolute stateDir
        absNodeConfigsDir <- makeDirAbsolute nodeConfigsDir
        pure (TestNetworkPreprod absStateDir absNodeConfigsDir)
  where
    makeDirAbsolute = \case
        Abs absDir -> pure absDir
        Rel relDir -> makeAbsolute relDir

parser :: Parser TestNetworkOptions
parser = OptParse.subparser $ cmdManual <> cmdLocal <> cmdPreprod
  where
    cmdManual =
        OptParse.command
            "manual"
            ( OptParse.info
                (pure TestNetworkOptionManual)
                ( OptParse.progDesc
                    "Relies on a node and wallet started manually."
                )
            )
    cmdLocal =
        OptParse.command
            "local"
            ( OptParse.info
                (TestNetworkOptionLocal <$> stateDirOption)
                (OptParse.progDesc "Automatically starts a local test cluster.")
            )
    cmdPreprod =
        OptParse.command
            "preprod"
            ( OptParse.info
                ( TestNetworkOptionPreprod
                    <$> stateDirOption
                    <*> option
                        (eitherReader (first show . parseSomeDir))
                        ( long "node-configs-dir"
                            <> short 'c'
                            <> metavar "NODE_CONFIGS_DIR"
                            <> help
                                "Absolute or relative directory path \
                                \ to a directory with node configs"
                        )
                )
                ( OptParse.progDesc
                    "Automatically starts a preprod node and wallet."
                )
            )
    stateDirOption =
        option
            (eitherReader (first show . parseSomeDir))
            ( long "state-dir"
                <> short 's'
                <> metavar "STATE_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to save node and wallet state"
            )
