{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , LogFileConfig (..)
    , bracketTracer'
    , logFileConfigFromEnv
    , nodeMinSeverityFromEnv
    , walletMinSeverityFromEnv
    , testMinSeverityFromEnv
    , testLogDirFromEnv
    , minSeverityFromEnv
    , setLoggingName
    )
where

import Prelude

import Cardano.BM.Extra
    ( BracketLog
    , bracketTracer
    )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation
    , HasSeverityAnnotation (..)
    , Severity (..)
    , Tracer
    , contramap
    )
import Cardano.Launcher
    ( LauncherLog
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra
    , clusterEraToString
    )
import Control.Monad
    ( liftM2
    )
import Data.Char
    ( toLower
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import System.Directory
    ( makeAbsolute
    )
import System.Environment.Extended
    ( lookupEnvNonEmpty
    )
import System.Exit
    ( ExitCode (..)
    , die
    )
import System.FilePath
    ( (</>)
    )
import System.FilePath.Posix
    ( (<.>)
    )
import System.IO.Temp.Extra
    ( TempDirLog
    )

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

data ClusterLog
    = -- | How many pools
      MsgRegisteringStakePools Int
    | MsgStartingCluster FilePath
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgRegisteringPoolMetadataInSMASH String String
    | MsgRegisteringPoolMetadata String String
    | MsgTempDir TempDirLog
    | MsgBracket Text BracketLog
    | MsgCLIStatus Text ExitCode BL8.ByteString BL8.ByteString
    | MsgCLIRetry Text
    | MsgCLIRetryResult Text Int BL8.ByteString
    | MsgSocketIsReady CardanoNodeConn
    | MsgStakeDistribution String ExitCode BL8.ByteString BL8.ByteString
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    | MsgHardFork ClusterEra
    deriving stock (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgRegisteringPoolMetadata url hash ->
            T.pack
                $ unwords
                    [ "Hosting metadata for pool using url"
                    , url
                    , "with hash"
                    , hash
                    ]
        MsgRegisteringPoolMetadataInSMASH pool hash ->
            T.pack
                $ unwords
                    [ "Registering metadata for pool"
                    , pool
                    , "with SMASH with the metadata hash"
                    , hash
                    ]
        MsgRegisteringStakePools n ->
            mconcat
                [ T.pack (show n)
                , " stake pools are being registered on chain... "
                ]
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for "
                <> T.pack fp
                <> " at "
                <> T.pack baseUrl
        MsgTempDir msg -> toText msg
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> msg
            ExitFailure code ->
                "Failed "
                    <> msg
                    <> " with exit code "
                    <> T.pack (show code)
                    <> ":\n"
                    <> indent out
                    <> "\n"
                    <> indent err
        MsgCLIRetry msg -> msg
        MsgCLIRetryResult msg code err ->
            "Failed "
                <> msg
                <> " with exit code "
                <> T.pack (show code)
                <> ":\n"
                <> indent err
        MsgSocketIsReady conn ->
            toText conn <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for "
                    <> T.pack name
                    <> ":\n"
                    <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status "
                    <> T.pack (show code)
                    <> ":\n"
                    <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli" : args)
        MsgHardFork era ->
            "Hard fork to " <> T.pack (clusterEraToString era)
      where
        indent =
            T.unlines
                . map ("  " <>)
                . T.lines
                . T.decodeUtf8With T.lenientDecode
                . BL8.toStrict

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgStartingCluster _ -> Info
        MsgRegisteringStakePools _ -> Info
        MsgLauncher _ _ -> Info
        MsgStartedStaticServer _ _ -> Info
        MsgTempDir msg -> getSeverityAnnotation msg
        MsgBracket _ _ -> Debug
        MsgCLIStatus _ ExitSuccess _ _ -> Info
        MsgCLIStatus _ (ExitFailure _) _ _ -> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _ -> Info
        MsgStakeDistribution _ (ExitFailure _) _ _ -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug
        MsgRegisteringPoolMetadataInSMASH{} -> Info
        MsgRegisteringPoolMetadata{} -> Info
        MsgHardFork _ -> Info

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)

data LogFileConfig = LogFileConfig
    { minSeverityTerminal :: Severity
    -- ^ Minimum logging severity
    , extraLogDir :: Maybe FilePath
    -- ^ Optional additional output to log file
    , minSeverityFile :: Severity
    -- ^ Minimum logging severity for 'extraLogFile'
    }
    deriving stock (Show)

logFileConfigFromEnv
    :: Maybe String
    -- ^ Optional extra subdir for TESTS_LOGDIR. E.g. @Just "alonzo"@ and
    -- @Just "mary"@ to keep them separate.
    -> IO LogFileConfig
logFileConfigFromEnv subdir =
    LogFileConfig
        <$> nodeMinSeverityFromEnv
        <*> testLogDirFromEnv subdir
        <*> pure Info

-- | The lower-case names of all 'Severity' values.
loggingSeverities :: [(String, Severity)]
loggingSeverities = [(toLower <$> show s, s) | s <- [minBound .. maxBound]]

parseLoggingSeverity :: String -> Either String Severity
parseLoggingSeverity arg =
    case lookup (map toLower arg) loggingSeverities of
        Just sev -> pure sev
        Nothing -> Left $ "unknown logging severity: " ++ arg

minSeverityFromEnv :: Severity -> String -> IO Severity
minSeverityFromEnv def var =
    lookupEnvNonEmpty var >>= \case
        Nothing -> pure def
        Just arg -> either die pure (parseLoggingSeverity arg)

-- Allow configuring @cardano-node@ log level with the
-- @CARDANO_NODE_TRACING_MIN_SEVERITY@ environment variable.
nodeMinSeverityFromEnv :: IO Severity
nodeMinSeverityFromEnv =
    minSeverityFromEnv Info "CARDANO_NODE_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @CARDANO_WALLET_TRACING_MIN_SEVERITY@ environment variable.
walletMinSeverityFromEnv :: IO Severity
walletMinSeverityFromEnv =
    minSeverityFromEnv Warning "CARDANO_WALLET_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @TESTS_TRACING_MIN_SEVERITY@ environment variable.
testMinSeverityFromEnv :: IO Severity
testMinSeverityFromEnv =
    minSeverityFromEnv Notice "TESTS_TRACING_MIN_SEVERITY"

-- | Directory for extra logging. Buildkite will set this environment variable
-- and upload logs in it automatically.
testLogDirFromEnv :: Maybe String -> IO (Maybe FilePath)
testLogDirFromEnv msubdir = do
    rel <- lookupEnvNonEmpty "TESTS_LOGDIR"
    makeAbsolute `traverse` case msubdir of
        Just subdir -> liftM2 (</>) rel (Just subdir)
        Nothing -> rel

setLoggingName :: String -> LogFileConfig -> LogFileConfig
setLoggingName name cfg = cfg{extraLogDir = filename <$> extraLogDir cfg}
  where
    filename = (</> (name <.> "log"))
