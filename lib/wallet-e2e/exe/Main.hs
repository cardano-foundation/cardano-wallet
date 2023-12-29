module Main where

import qualified Options.Applicative as OptParse
import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    , effectsSpec
    , walletSpec
    )
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration (..)
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
    ( Abs
    , Dir
    , Path
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
    TestOptions{..} :: TestOptions <-
        execParser
            $ info
                (parserTestOptions <**> helper)
                (fullDesc <> progDesc "E2E Wallet test suite")
    testNetwork <- testNetworkOptionsToConfig testNetworkOptions
    traceConfiguration <- do
        absTraceDir <-
            traverse makeDirAbsolute
                $ testGlobalOptionsTraceOutput testGlobalOptions
        pure $ TraceConfiguration absTraceDir
    sydTestWith SydTest.defaultSettings{SydTest.settingRetries = 1} do
        effectsSpec
        walletSpec traceConfiguration testNetwork

parserTestOptions :: Parser TestOptions
parserTestOptions = TestOptions <$> parserNetworkOptions <*> parserGlobalOptions

--------------------------------------------------------------------------------
-- Command line options --------------------------------------------------------

data TestOptions = TestOptions
    { testNetworkOptions :: TestNetworkOptions
    , testGlobalOptions :: TestGlobalOptions
    }

newtype TestGlobalOptions = TestGlobalOptions
    { testGlobalOptionsTraceOutput :: Tagged "tracing-dir" (SomeBase Dir)
    }

parserGlobalOptions :: Parser TestGlobalOptions
parserGlobalOptions = TestGlobalOptions <$> traceOutputOption
  where
    traceOutputOption :: Parser (Tagged "tracing-dir" (SomeBase Dir)) =
        option
            (eitherReader (bimap show Tagged . parseSomeDir))
            ( long "tracing-dir"
                <> short 't'
                <> metavar "TRACE_OUTPUT_DIR"
                <> help
                    "Absolute or relative directory path to save trace output"
            )

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

makeDirAbsolute :: SomeBase Dir -> IO (Path Abs Dir)
makeDirAbsolute = \case
    Abs absDir -> pure absDir
    Rel relDir -> makeAbsolute relDir

parserNetworkOptions :: Parser TestNetworkOptions
parserNetworkOptions = OptParse.subparser $ cmdManual <> cmdLocal <> cmdPreprod
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
