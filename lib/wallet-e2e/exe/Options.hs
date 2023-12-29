module Options where

import qualified Options.Applicative as OptParse

import Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    )
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration (..)
    )
import Cardano.Wallet.Spec.Lib.Paths
    ( SomeDirOf
    , makeDirAbsolute
    )
import Data.Tagged
    ( Tagged (..)
    )
import Options.Applicative
    ( Parser
    , eitherReader
    , help
    , long
    , metavar
    , option
    , short
    )
import Path
    ( parseSomeDir
    )

withTestOptions :: (TestNetworkConfig -> TraceConfiguration -> IO b) -> IO b
withTestOptions action = do
    TestOptions{..} :: TestOptions <-
        OptParse.execParser
            $ OptParse.info
                (parserTestOptions OptParse.<**> OptParse.helper)
                (OptParse.fullDesc <> OptParse.progDesc "E2E Wallet test suite")
    testNetwork <- testNetworkOptionsToConfig testNetworkOptions
    traceConfiguration <- do
        absTraceDir <-
            makeDirAbsolute
                $ testGlobalOptionsTraceOutput testGlobalOptions
        pure $ TraceConfiguration absTraceDir
    action testNetwork traceConfiguration

parserTestOptions :: Parser TestOptions
parserTestOptions = TestOptions <$> parserNetworkOptions <*> parserGlobalOptions

data TestOptions = TestOptions
    { testNetworkOptions :: TestNetworkOptions
    , testGlobalOptions :: TestGlobalOptions
    }

newtype TestGlobalOptions = TestGlobalOptions
    { testGlobalOptionsTraceOutput :: SomeDirOf "tracing-dir"
    }

parserGlobalOptions :: Parser TestGlobalOptions
parserGlobalOptions = TestGlobalOptions <$> traceOutputOption
  where
    traceOutputOption :: Parser (SomeDirOf "tracing-dir") =
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
    | TestNetworkOptionLocal (SomeDirOf "state") (SomeDirOf "config")
    | TestNetworkOptionPreprod (SomeDirOf "state") (SomeDirOf "config")

testNetworkOptionsToConfig :: TestNetworkOptions -> IO TestNetworkConfig
testNetworkOptionsToConfig = \case
    TestNetworkOptionManual ->
        pure TestNetworkManual
    TestNetworkOptionLocal stateDir nodeConfigsDir -> do
        absStateDir <- makeDirAbsolute stateDir
        absNodeConfigsDir <- makeDirAbsolute nodeConfigsDir
        pure (TestNetworkLocal absStateDir absNodeConfigsDir)
    TestNetworkOptionPreprod stateDir nodeConfigsDir -> do
        absStateDir <- makeDirAbsolute stateDir
        absNodeConfigsDir <- makeDirAbsolute nodeConfigsDir
        pure (TestNetworkPreprod absStateDir absNodeConfigsDir)

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
    stateDirOption :: Parser (SomeDirOf "state") =
        option
            (eitherReader (bimap show Tagged . parseSomeDir))
            ( long "state-dir"
                <> short 's'
                <> metavar "STATE_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to save node and wallet state"
            )
    nodeConfigsDirOption :: Parser (SomeDirOf "config") =
        option
            (eitherReader (bimap show Tagged . parseSomeDir))
            ( long "node-configs-dir"
                <> short 'c'
                <> metavar "NODE_CONFIGS_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to a directory with node configs"
            )
