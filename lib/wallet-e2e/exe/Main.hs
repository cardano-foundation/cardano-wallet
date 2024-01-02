module Main where

import qualified Options.Applicative as OptParse
import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    , effectsSpec
    , walletSpec
    )
import Data.Tagged
    ( Tagged (..)
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
    | TestNetworkOptionLocal
        (Tagged "state" (SomeBase Dir))
        (Tagged "config" (SomeBase Dir))
    | TestNetworkOptionPreprod
        (Tagged "state" (SomeBase Dir))
        (Tagged "config" (SomeBase Dir))

testNetworkOptionsToConfig :: TestNetworkOptions -> IO TestNetworkConfig
testNetworkOptionsToConfig = \case
    TestNetworkOptionManual ->
        pure TestNetworkManual
    TestNetworkOptionLocal stateDir nodeConfigsDir -> do
        absStateDir <- traverse makeDirAbsolute stateDir
        absNodeConfigsDir <- traverse makeDirAbsolute nodeConfigsDir
        pure (TestNetworkLocal absStateDir absNodeConfigsDir)
    TestNetworkOptionPreprod stateDir nodeConfigsDir -> do
        absStateDir <- traverse makeDirAbsolute stateDir
        absNodeConfigsDir <- traverse makeDirAbsolute nodeConfigsDir
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
                ( TestNetworkOptionLocal
                    <$> stateDirOption
                    <*> nodeConfigsDirOption
                )
                (OptParse.progDesc "Automatically starts a local test cluster.")
            )
    cmdPreprod =
        OptParse.command
            "preprod"
            ( OptParse.info
                ( TestNetworkOptionPreprod
                    <$> stateDirOption
                    <*> nodeConfigsDirOption
                )
                ( OptParse.progDesc
                    "Automatically starts a preprod node and wallet."
                )
            )
    stateDirOption :: Parser (Tagged "state" (SomeBase Dir)) =
        option
            (eitherReader (bimap show Tagged . parseSomeDir))
            ( long "state-dir"
                <> short 's'
                <> metavar "STATE_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to save node and wallet state"
            )
    nodeConfigsDirOption :: Parser (Tagged "config" (SomeBase Dir)) =
        option
            (eitherReader (bimap show Tagged . parseSomeDir))
            ( long "node-configs-dir"
                <> short 'c'
                <> metavar "NODE_CONFIGS_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to a directory with node configs"
            )
