{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , traceClusterLog
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
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
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
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
    :: Maybe CardanoNodeConn
    -- ^ optional cardano-node socket path
    -> [String]
    -- ^ command-line arguments
    -> ClusterM (ProcessConfig () () ())
cliConfigBase conn args = do
    traceClusterLog (MsgCLI args)
    env <- liftIO getEnvironment
    let mkEnv c = ("CARDANO_NODE_SOCKET_PATH", nodeSocketFile c) : env
    let cliEnv
            :: ProcessConfig stdin stdout stderr
            -> ProcessConfig stdin stdout stderr
        cliEnv = maybe setEnvInherit (setEnv . mkEnv) conn
    pure
        $ cliEnv
        $ proc "cardano-cli" args

cliConfigNode
    :: CardanoNodeConn
    -- ^ cardano-node socket path
    -> [String]
    -- ^ command-line arguments
    -> ClusterM (ProcessConfig () () ())
cliConfigNode conn = cliConfigBase (Just conn)

cliConfig
    :: [String]
    -- ^ command-line arguments
    -> ClusterM (ProcessConfig () () ())
cliConfig = cliConfigBase Nothing

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: [String] -> ClusterM ()
cli = cliConfig >=> void . liftIO . readProcessStdoutOrFail

cliLine :: [String] -> ClusterM String
cliLine =
    cliConfig
        >=> fmap (BL8.unpack . getFirstLine) . liftIO . readProcessStdoutOrFail

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
    :: Text
    -- ^ message to print before running command
    -> ProcessConfig () a b
    -> ClusterM ()
cliRetry msg processConfig = do
    Config{..} <- ask
    (st, out, err) <- liftIO $ retrying pol (const isFail) (const $ cmd cfgTracer)
    traceClusterLog $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure ()
        ExitFailure _ -> liftIO $
            throwIO
                $ ProcessHasExited
                    ("cardano-cli failed: " <> BL8.unpack err)
                    st
  where
    cmd :: Tracer IO ClusterLog -> IO (ExitCode, BL8.ByteString, BL8.ByteString)
    cmd tr = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcess processConfig
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail :: (ExitCode, b, c) -> IO Bool
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000
