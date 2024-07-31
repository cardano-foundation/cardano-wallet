{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.Application.Server
    ( Listen (..)
    , walletListenFromEnv
    , start
    , withListeningSocket
    , ListenError (..)
    -- * Re-exported for convenience
    , HostPreference
    ) where

import Prelude

import Cardano.Wallet.Api.Http.Server.Tls
    ( TlsConfiguration (..)
    , requireClientAuth
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( IsServerError (..)
    )
import Control.Exception
    ( IOException
    , bracket
    , tryJust
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer
    )
import Data.Functor
    ( (<&>)
    )
import Data.List
    ( isInfixOf
    )
import Data.Streaming.Network
    ( HostPreference
    , bindPortTCP
    , bindRandomPortTCP
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Network.Socket
    ( Socket
    , close
    )
import Network.Wai.Handler.Warp
    ( Port
    )
import Network.Wai.Middleware.Logging
    ( ApiLog
    , newApiLoggerSettings
    , obfuscateKeys
    , withApiLogger
    )
import Network.Wai.Middleware.ServerError
    ( handleRawError
    )
import Servant.Server
    ( Application
    )
import System.Exit
    ( die
    )
import System.IO.Error
    ( ioeGetErrorType
    , isAlreadyInUseError
    , isDoesNotExistError
    , isPermissionError
    , isUserError
    )

import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

-- | Allow configuring which port the wallet server listen to in an integration
-- setup. Crashes if the variable is not a number.
walletListenFromEnv
    :: Show e
    => (String -> IO (Maybe (Either e Port)))
    -> IO Listen
walletListenFromEnv envFromText =
    envFromText "CARDANO_WALLET_PORT" >>= \case
        Nothing -> pure ListenOnRandomPort
        Just (Right port) -> pure $ ListenOnPort port
        Just (Left e) -> die $ show e

-- | How the server should listen for incoming requests.
data Listen
    = -- | Listen on given TCP port
      ListenOnPort Port
    | -- | Listen on an unused TCP port, selected at random
      ListenOnRandomPort
    deriving (Show, Eq)

runSocket :: Socket -> Warp.Settings -> Maybe TlsConfiguration -> Application -> IO ()
runSocket socket settings = \case
    Nothing -> Warp.runSettingsSocket settings socket
    Just tls -> Warp.runTLSSocket (requireClientAuth tls) settings socket

-- | Start the application server, using the given settings and a bound socket.
start
    :: Warp.Settings
    -> Tracer IO ApiLog
    -> Maybe TlsConfiguration
    -> Socket
    -> Application
    -> IO ()
start settings tr tlsConfig socket application = do
    logSettings <- newApiLoggerSettings <&> obfuscateKeys (const sensitive)
    runSocket socket settings tlsConfig
        $ handleRawError (curry toServerError)
        $ withApiLogger
            tr
            logSettings
            application
  where
    sensitive :: [Text]
    sensitive =
        [ "passphrase"
        , "old_passphrase"
        , "new_passphrase"
        , "mnemonic_sentence"
        , "mnemonic_second_factor"
        ]

-- | Run an action with a TCP socket bound to a port specified by the `Listen`
-- parameter.
withListeningSocket
    :: HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ Whether to listen on a given port, or random port.
    -> (Either ListenError (Port, Socket) -> IO a)
    -- ^ Action to run with listening socket.
    -> IO a
withListeningSocket hostPreference portOpt = bracket acquire release
  where
    acquire = tryJust handleErr bindAndListen
    -- Note: These Data.Streaming.Network functions also listen on the socket,
    -- even though their name just says "bind".
    bindAndListen = case portOpt of
        ListenOnPort port -> (port,) <$> bindPortTCP port hostPreference
        ListenOnRandomPort -> bindRandomPortTCP hostPreference
    release (Right (_, socket)) = liftIO $ close socket
    release (Left _) = pure ()
    handleErr = ioToListenError hostPreference portOpt

data ListenError
    = ListenErrorAddressAlreadyInUse (Maybe Port)
    | ListenErrorOperationNotPermitted
    | ListenErrorHostDoesNotExist HostPreference
    | ListenErrorInvalidAddress HostPreference
    deriving (Show, Eq)

instance ToText ListenError where
    toText = \case
        ListenErrorHostDoesNotExist host ->
            mempty
                <> "Can't listen on "
                <> T.pack (show host)
                <> ". It does not exist."
        ListenErrorInvalidAddress host ->
            mempty
                <> "Can't listen on "
                <> T.pack (show host)
                <> ". Invalid address."
        ListenErrorAddressAlreadyInUse mPort ->
            mempty
                <> "The API server listen port "
                <> maybe "(unknown)" (T.pack . show) mPort
                <> " is already in use."
        ListenErrorOperationNotPermitted ->
            mempty
                <> "Cannot listen on the given port. "
                <> "The operation is not permitted."

ioToListenError :: HostPreference -> Listen -> IOException -> Maybe ListenError
ioToListenError hostPreference portOpt e
    -- A socket is already listening on that address and port
    | isAlreadyInUseError e =
        Just (ListenErrorAddressAlreadyInUse (listenPort portOpt))
    -- Usually caused by trying to listen on a privileged port
    | isPermissionError e =
        Just ListenErrorOperationNotPermitted
    -- Bad hostname -- Linux and Darwin
    | isDoesNotExistError e =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Bad hostname -- Windows
    -- WSAHOST_NOT_FOUND, WSATRY_AGAIN, or bind: WSAEOPNOTSUPP
    | isUserError e && any hasDescription ["11001", "11002", "10045"] =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Address is valid, but can't be used for listening -- Linux
    | show (ioeGetErrorType e) == "invalid argument" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Darwin
    | show (ioeGetErrorType e) == "unsupported operation" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Windows
    | isOtherError e && any hasDescription ["WSAEINVAL", "WSAEADDRNOTAVAIL"] =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Listening on an unavailable or privileged port -- Windows
    | isOtherError e && hasDescription "WSAEACCESS" =
        Just (ListenErrorAddressAlreadyInUse (listenPort portOpt))
    | otherwise =
        Nothing
  where
    listenPort (ListenOnPort port) = Just port
    listenPort ListenOnRandomPort = Nothing

    isOtherError ex = show (ioeGetErrorType ex) == "failed"
    hasDescription text = text `isInfixOf` show e
