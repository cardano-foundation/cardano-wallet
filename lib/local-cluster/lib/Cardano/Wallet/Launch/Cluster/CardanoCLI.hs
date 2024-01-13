{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cliConfigNode
    , cliConfigBase
    , cli
    , cliLine
    , cliRetry
    )
where

import Prelude

import Cardano.BM.Tracing
    ( Tracer
    , traceWith
    )
import Cardano.Launcher
    ( ProcessHasExited (..)
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , nodeSocketFile
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (MsgCLI, MsgCLIRetry, MsgCLIRetryResult, MsgCLIStatus)
    )
import Control.Exception
    ( throwIO
    )
import Control.Monad
    ( void
    , (>=>)
    )
import Control.Retry
    ( constantDelay
    , limitRetriesByCumulativeDelay
    , retrying
    )
import Data.Text
    ( Text
    )
import System.Environment
    ( getEnvironment
    )
import System.Process.Typed
    ( ExitCode (..)
    , ProcessConfig
    , proc
    , readProcess
    , setEnv
    , setEnvInherit
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

-- | Make a 'ProcessConfig' for running @cardano-cli@. The program must be on
-- the @PATH@, as normal. Sets @CARDANO_NODE_SOCKET_PATH@ for the subprocess, if
-- a 'CardanoNodeConn' is provided.
cliConfigBase
    :: Tracer IO ClusterLog
    -- ^ for logging the command
    -> Maybe CardanoNodeConn
    -- ^ optional cardano-node socket path
    -> [String]
    -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigBase tr conn args = do
    traceWith tr (MsgCLI args)
    env <- getEnvironment
    let mkEnv c = ("CARDANO_NODE_SOCKET_PATH", nodeSocketFile c) : env
    let cliEnv :: ProcessConfig stdin stdout stderr
                -> ProcessConfig stdin stdout stderr
        cliEnv = maybe setEnvInherit (setEnv . mkEnv) conn
    pure $ cliEnv $ proc "cardano-cli" args

cliConfigNode
    :: Tracer IO ClusterLog
    -- ^ for logging the command
    -> CardanoNodeConn
    -- ^ cardano-node socket path
    -> [String]
    -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigNode tr conn = cliConfigBase tr (Just conn)

cliConfig
    :: Tracer IO ClusterLog
    -- ^ for logging the command
    -> [String]
    -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfig tr = cliConfigBase tr Nothing

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO ()
cli tr = cliConfig tr >=> void . readProcessStdoutOrFail

cliLine :: Tracer IO ClusterLog -> [String] -> IO String
cliLine tr =
    cliConfig tr
        >=> fmap (BL8.unpack . getFirstLine) . readProcessStdoutOrFail

readProcessStdoutOrFail :: ProcessConfig () () () -> IO BL.ByteString
readProcessStdoutOrFail processConfig = do
    (st, out, err) <- readProcess processConfig
    case st of
        ExitSuccess -> pure out
        ExitFailure _ ->
            throwIO
                $ userError
                $ mconcat
                    [ "command failed: "
                    , BL8.unpack err
                    ]

getFirstLine :: BL8.ByteString -> BL8.ByteString
getFirstLine = BL8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@.
cliRetry
    :: Tracer IO ClusterLog
    -> Text
    -- ^ message to print before running command
    -> ProcessConfig () a b
    -> IO ()
cliRetry tr msg processConfig = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            throwIO
                $ ProcessHasExited
                    ("cardano-cli failed: " <> BL8.unpack err)
                    st
  where
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcess processConfig
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail :: (ExitCode, b, c) -> IO Bool
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000
