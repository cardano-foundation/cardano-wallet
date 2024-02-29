{-# LANGUAGE TupleSections #-}
-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch @cardano-node@.

module Cardano.Launcher.Node
    ( -- * Startup
      withCardanoNode
    , CardanoNodeConfig (..)
    , NodePort (..)

    -- * cardano-node Snockets
    , CardanoNodeConn
    , cardanoNodeConn
    , nodeSocketFile
    , isWindows

    -- * Helpers
    , nodeSocketPath
    ) where

import Prelude

import Cardano.Launcher
    ( LauncherLog
    , ProcessHasExited
    , ProcessRun (..)
    , StdStream (..)
    , withBackendCreateProcess
    )
import Cardano.Launcher.Logging
    ( traceHandle
    )
import Control.Monad
    ( (<=<)
    )
import Control.Tracer
    ( Tracer (..)
    , debugTracer
    , stdoutTracer
    )
import Data.Bifunctor
    ( first
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.List
    ( isPrefixOf
    )
import Data.Maybe
    ( fromMaybe
    , maybeToList
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import System.Environment
    ( getEnvironment
    )
import System.FilePath
    ( isValid
    , takeFileName
    , (</>)
    )
import System.Info
    ( os
    )
import System.IO
    ( Handle
    )
import UnliftIO
    ( async
    , link
    )
import UnliftIO.Process
    ( CreateProcess (..)
    , proc
    )

import qualified Data.Text as T

-- | Parameters for connecting to the node.
newtype CardanoNodeConn = CardanoNodeConn FilePath
    deriving (Show, Eq)

-- | Gets the socket filename or pipe name from 'CardanoNodeConn'. Whether it's
-- a unix socket or named pipe depends on the value of 'isWindows'.
nodeSocketFile :: CardanoNodeConn -> FilePath
nodeSocketFile (CardanoNodeConn name) = name

-- | Produces a 'CardanoNodeConn' if the socket path or pipe name (depending on
-- 'isWindows') is valid.
cardanoNodeConn :: FilePath -> Either String CardanoNodeConn
cardanoNodeConn name
    | isWindows = if isValidWindowsPipeName name
        then Right $ CardanoNodeConn name
        else Left "Invalid pipe name."
    | otherwise = if isValid name
        then Right $ CardanoNodeConn name
        else Left "Invalid file path."

isWindows :: Bool
isWindows = os == "mingw32"

isValidWindowsPipeName :: FilePath -> Bool
isValidWindowsPipeName name = slashPipe `isPrefixOf` name
    && isValid (drop (length slashPipe) name)
  where
    slashPipe = "\\\\.\\pipe\\"

instance ToText CardanoNodeConn where
    toText = T.pack . nodeSocketFile

instance FromText CardanoNodeConn where
    fromText = first TextDecodingError . cardanoNodeConn . T.unpack

newtype NodePort = NodePort { unNodePort :: Int }
    deriving (Show, Eq)

-- | A subset of the @cardano-node@ CLI parameters, used for starting the
-- backend.
data CardanoNodeConfig = CardanoNodeConfig
    { nodeDir             :: FilePath
    , nodeConfigFile      :: FilePath
    , nodeTopologyFile    :: FilePath
    , nodeDatabaseDir     :: FilePath
    , nodeDlgCertFile     :: Maybe FilePath
    , nodeSignKeyFile     :: Maybe FilePath
    , nodeOpCertFile      :: Maybe FilePath
    , nodeKesKeyFile      :: Maybe FilePath
    , nodeVrfKeyFile      :: Maybe FilePath
    , nodePort            :: Maybe NodePort
    , nodeLoggingHostname :: Maybe String
    , nodeExecutable      :: Maybe FilePath
    , nodeOutputFile      :: Maybe FilePath
    } deriving (Show, Eq)

traceProcessHandles :: Tracer IO Text -> Maybe Handle -> IO ()
traceProcessHandles _ Nothing = pure ()
traceProcessHandles r (Just h) = link <=< async $ traceHandle r h

defaultOutTracer :: Maybe (Tracer IO Text) -> Tracer IO Text
defaultOutTracer Nothing = T.unpack >$< stdoutTracer
defaultOutTracer (Just tr) = tr

defaultErrTracer :: Maybe (Tracer IO Text) -> Tracer IO Text
defaultErrTracer Nothing = T.unpack >$< debugTracer
defaultErrTracer (Just tr) = tr

-- | Spawns a @cardano-node@ process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Tracer IO LauncherLog
    -- ^ Trace for subprocess control logging
    -> Maybe (Tracer IO Text)
    -- ^ Trace for cardano-node stdout
    -> Maybe (Tracer IO Text)
    -- ^ Trace for cardano-node stderr
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO (Either ProcessHasExited a)
withCardanoNode tr nodeOutTr nodeErrTr cfg action = do
    let socketPath = nodeSocketPath (nodeDir cfg)
    cp <- cardanoNodeProcess cfg socketPath
    withBackendCreateProcess tr cp
        $ ProcessRun
        $ \_ mout merr _ -> do
            traceProcessHandles (defaultOutTracer nodeOutTr) mout
            traceProcessHandles (defaultErrTracer nodeErrTr) merr
            action $ CardanoNodeConn socketPath

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess
    :: CardanoNodeConfig
    -> FilePath
    -> IO CreateProcess
cardanoNodeProcess cfg socketPath = do
    myEnv <- getEnvironment
    let env' = ("CARDANO_NODE_LOGGING_HOSTNAME",) <$> nodeLoggingHostname cfg
    pure
        $ (proc (fromMaybe "cardano-node" $ nodeExecutable cfg) args)
            { env = Just $ maybeToList env' ++ myEnv
            , cwd = Just $ nodeDir cfg
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
  where
    args =
        [ "run"
        , "--config"
        , nodeConfigFile cfg
        , "--topology"
        , nodeTopologyFile cfg
        , "--database-path"
        , nodeDatabaseDir cfg
        , "--socket-path"
        , socketPath
        ]
            ++ opt "--port" (show . unNodePort <$> nodePort cfg)
            ++ opt "--byron-signing-key" (nodeSignKeyFile cfg)
            ++ opt "--byron-delegation-certificate" (nodeDlgCertFile cfg)
            ++ opt "--shelley-operational-certificate" (nodeOpCertFile cfg)
            ++ opt "--shelley-kes-key" (nodeKesKeyFile cfg)
            ++ opt "--shelley-vrf-key" (nodeVrfKeyFile cfg)
            ++ ["+RTS", "-N4", "-RTS"]

    opt _ Nothing = []
    opt arg (Just val) = [arg, val]

-- | Generate a 'FilePath' for the @cardano-node@ domain socket/named pipe.
nodeSocketPath
    :: FilePath -- ^ @cardano-node@ state directory
    -> FilePath -- ^ UNIX socket file path or Windows named pipe name
nodeSocketPath dir
    | os == "mingw32" = "\\\\.\\pipe\\" ++ takeFileName dir
    | otherwise = dir </> "node.socket"
