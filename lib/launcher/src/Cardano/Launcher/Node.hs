{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
    , MaybeK (..)
    , IsMaybe (..)
    , maybeFromMaybeK
    , NodePort (..)

      -- * cardano-node Snockets
    , CardanoNodeConn
    , cardanoNodeConn
    , nodeSocketFile
    , isWindows

      -- * Helpers
    , nodeSocketPath
    , mkWindowsPipeName
    ) where

import Cardano.Launcher
    ( IfToSendSigINT (..)
    , LauncherLog
    , ProcessHandles (..)
    , StdStream (..)
    , TimeoutInSecs (..)
    , withBackendCreateProcess
    )
import Control.Monad
    ( unless
    )
import Control.Tracer
    ( Tracer (..)
    )
import Data.Bifunctor
    ( first
    )
import Data.List
    ( isPrefixOf
    , nub
    )
import Data.Maybe
    ( fromMaybe
    , maybeToList
    )
import Data.MaybeK
    ( IsMaybe (..)
    , MaybeK (..)
    , maybeFromMaybeK
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
    )
import System.IO
    ( Handle
    , IOMode (..)
    , hFlush
    , withFile
    )
import System.Info
    ( os
    )
import UnliftIO.Async
    ( link
    , withAsync
    )
import UnliftIO.Process
    ( CreateProcess (..)
    , proc
    )
import Prelude

import qualified Data.ByteString as BS
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
    | isWindows =
        if isValidWindowsPipeName name
            then Right $ CardanoNodeConn name
            else Left "Invalid pipe name."
    | otherwise =
        if isValid name
            then Right $ CardanoNodeConn name
            else Left "Invalid file path."

isWindows :: Bool
isWindows = os == "mingw32"

isValidWindowsPipeName :: FilePath -> Bool
isValidWindowsPipeName name =
    slashPipe `isPrefixOf` name
        && isValid (drop (length slashPipe) name)
  where
    slashPipe = "\\\\.\\pipe\\"

instance ToText CardanoNodeConn where
    toText = T.pack . nodeSocketFile

instance FromText CardanoNodeConn where
    fromText = first TextDecodingError . cardanoNodeConn . T.unpack

newtype NodePort = NodePort {unNodePort :: Int}
    deriving (Show, Eq)

-- | A subset of the @cardano-node@ CLI parameters, used for starting the
-- backend.
data CardanoNodeConfig d = CardanoNodeConfig
    { nodeDir :: FilePath
    , nodeConfigFile :: FilePath
    , nodeTopologyFile :: FilePath
    , nodeDatabaseDir :: FilePath
    , nodeDlgCertFile :: Maybe FilePath
    , nodeSignKeyFile :: Maybe FilePath
    , nodeOpCertFile :: Maybe FilePath
    , nodeKesKeyFile :: Maybe FilePath
    , nodeVrfKeyFile :: Maybe FilePath
    , nodePort :: Maybe NodePort
    , nodeLoggingHostname :: Maybe String
    , nodeExecutable :: Maybe FilePath
    , nodeOutputFiles :: [FilePath]
    , nodeSocketPathFile :: MaybeK d FilePath
    }
    deriving (Show, Eq)

-- | Spawns a @cardano-node@ process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Tracer IO LauncherLog
    -- ^ Trace for subprocess control logging
    -> CardanoNodeConfig d
    -- ^ Configuration for the node
    -> (MaybeK d CardanoNodeConn -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO a
withCardanoNode tr cfg action = do
    let socketPath = nodeSocketPathFile cfg
    let run output useHandles = do
            cp <- cardanoNodeProcess cfg output
            withBackendCreateProcess tr cp (TimeoutInSecs 4) SendSigINT
                $ \handles ->
                    useHandles handles $ action $ fmap CardanoNodeConn socketPath
    case nub $ nodeOutputFiles cfg of
        [] -> run Inherit useDirectly
        [file] ->
            withFile file AppendMode $ \h -> run (UseHandle h) useDirectly
        files ->
            withFiles files $ \handles ->
                run CreatePipe $ \processHandles continuation ->
                    case outputHandle processHandles of
                        Nothing -> continuation
                        Just source ->
                            withAsync (copyToFiles source handles) $ \copier -> do
                                link copier
                                continuation

useDirectly :: ProcessHandles -> IO a -> IO a
useDirectly _ action = action

withFiles :: [FilePath] -> ([Handle] -> IO a) -> IO a
withFiles [] action = action []
withFiles (file : files) action =
    withFile file AppendMode $ \handle ->
        withFiles files $ action . (handle :)

copyToFiles :: Handle -> [Handle] -> IO ()
copyToFiles source targets = do
    chunk <- BS.hGetSome source 65536
    unless (BS.null chunk) $ do
        mapM_ (\target -> BS.hPut target chunk >> hFlush target) targets
        copyToFiles source targets

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess
    :: CardanoNodeConfig d
    -> StdStream
    -> IO CreateProcess
cardanoNodeProcess cfg output = do
    myEnv <- getEnvironment
    let env' = ("CARDANO_NODE_LOGGING_HOSTNAME",) <$> nodeLoggingHostname cfg
    pure
        $ (proc (fromMaybe "cardano-node" $ nodeExecutable cfg) args)
            { env = Just $ maybeToList env' ++ myEnv
            , cwd = Just $ nodeDir cfg
            , std_out = output
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
        ]
            <> maybe
                []
                (\p -> ["--socket-path", p])
                (maybeFromMaybeK $ nodeSocketPathFile cfg)
            <> opt "--port" (show . unNodePort <$> nodePort cfg)
            ++ opt "--byron-signing-key" (nodeSignKeyFile cfg)
            ++ opt "--byron-delegation-certificate" (nodeDlgCertFile cfg)
            ++ opt "--shelley-operational-certificate" (nodeOpCertFile cfg)
            ++ opt "--shelley-kes-key" (nodeKesKeyFile cfg)
            ++ opt "--shelley-vrf-key" (nodeVrfKeyFile cfg)
            ++ ["+RTS", "-N", "-RTS"]

    opt _ Nothing = []
    opt arg (Just val) = [arg, val]

-- | Generate a 'FilePath' for the @cardano-node@ domain socket/named pipe.
-- On Windows, the path is the filename at the end of the path
-- prefixed with @"\\\\.\\pipe\\"@.
nodeSocketPath
    :: FilePath
    -- ^ unix path
    -> FilePath
    -- ^ UNIX socket file path or Windows named pipe name
nodeSocketPath name
    | os == "mingw32" = mkWindowsPipeName $ takeFileName name
    | otherwise = name

mkWindowsPipeName :: FilePath -> FilePath
mkWindowsPipeName name = "\\\\.\\pipe\\" ++ name
