module Cardano.Wallet.Spec.Options where

import Cardano.Wallet.Launch.Cluster.FileOf
    ( Absolutizer (..)
    , DirOf (..)
    , newAbsolutizer
    )
import Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    )
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration (..)
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
import System.Path
    ( Abs
    , parse
    )

import qualified Options.Applicative as OptParse
import qualified System.Path.PartClass as Path

withTestOptions :: (TestNetworkConfig -> TraceConfiguration -> IO b) -> IO b
withTestOptions action = do
    absolutizer <- newAbsolutizer
    TestOptions{..} :: TestOptions <-
        OptParse.execParser
            $ OptParse.info
                (parserTestOptions absolutizer OptParse.<**> OptParse.helper)
                (OptParse.fullDesc <> OptParse.progDesc "E2E Wallet test suite")
    traceConfiguration <- do
        let absTraceDir = testGlobalOptionsTraceOutput testGlobalOptions
        pure $ TraceConfiguration absTraceDir
    action testNetworkConfig traceConfiguration

parserTestOptions :: Absolutizer -> Parser TestOptions
parserTestOptions absolutizer =
    TestOptions
        <$> parserNetworkOptions absolutizer
        <*> parserGlobalOptions absolutizer

data TestOptions = TestOptions
    { testNetworkConfig :: TestNetworkConfig
    , testGlobalOptions :: TestGlobalOptions
    }

newtype TestGlobalOptions = TestGlobalOptions
    { testGlobalOptionsTraceOutput :: DirOf "tracing-dir"
    }

parserGlobalOptions :: Absolutizer -> Parser TestGlobalOptions
parserGlobalOptions absolutizer = TestGlobalOptions <$> traceOutputOption
  where
    traceOutputOption :: Parser (DirOf "tracing-dir") =
        option
            (eitherReader (bimap show DirOf . parseAbs absolutizer))
            ( long "tracing-dir"
                <> short 't'
                <> metavar "TRACE_OUTPUT_DIR"
                <> help
                    "Absolute or relative directory path to save trace output"
            )

parseAbs :: Path.FileDir t => Absolutizer -> String -> Either String (Abs t)
parseAbs (Absolutizer absolutizer) str = do
    dir <- parse str
    pure $ absolutizer dir

parserNetworkOptions
    :: Absolutizer
    -> Parser TestNetworkConfig
parserNetworkOptions absolutizer =
    OptParse.subparser $ cmdManual <> cmdLocal <> cmdPreprod
  where
    cmdManual =
        OptParse.command
            "manual"
            ( OptParse.info
                (pure TestNetworkManual)
                ( OptParse.progDesc
                    "Relies on a node and wallet started manually."
                )
            )
    cmdLocal =
        OptParse.command
            "local"
            ( OptParse.info
                ( TestNetworkLocal
                    <$> stateDirOption
                    <*> nodeConfigsDirOption
                )
                (OptParse.progDesc "Automatically starts a local test cluster.")
            )
    cmdPreprod =
        OptParse.command
            "preprod"
            ( OptParse.info
                ( TestNetworkPreprod
                    <$> stateDirOption
                    <*> nodeConfigsDirOption
                )
                ( OptParse.progDesc
                    "Automatically starts a preprod node and wallet."
                )
            )
    stateDirOption :: Parser (DirOf "state") =
        option
            (eitherReader (bimap show DirOf . parseAbs absolutizer))
            ( long "state-dir"
                <> short 's'
                <> metavar "STATE_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to save node and wallet state"
            )
    nodeConfigsDirOption :: Parser (DirOf "config") =
        option
            (eitherReader (bimap show DirOf . parseAbs absolutizer))
            ( long "node-configs-dir"
                <> short 'c'
                <> metavar "NODE_CONFIGS_DIR"
                <> help
                    "Absolute or relative directory path \
                    \ to a directory with node configs"
            )
