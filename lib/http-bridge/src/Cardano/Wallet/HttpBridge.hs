{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module provides the main wallet server function for the
-- @cardano-http-bridge@ backend.

module Cardano.Wallet.HttpBridge
    ( serveWallet
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.CLI
    ( failWith )
import Cardano.Launcher
    ( ProcessHasExited (..), installSignalHandlers )
import Cardano.Pool.Metrics
    ( StakePoolLayer (..) )
import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..), ListenError (..) )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.DB.Sqlite
    ( PersistState )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.HttpBridge.Network
    ( ErrStartup (..), HttpBridgeBackend (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Network.Ports
    ( PortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress, PersistKey )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Block )
import Control.Concurrent.Async
    ( race_ )
import Control.DeepSeq
    ( NFData )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import System.Exit
    ( ExitCode (..) )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-http-bridge@ main function.
serveWallet
    :: forall t n a. (t ~ HttpBridge n)
    =>  ( KeyToAddress t RndKey
        , KeyToAddress t SeqKey
        , KnownNetwork n
        )
    => (CM.Configuration, Trace IO Text)
    -- ^ Logging config.
    -> Maybe FilePath
    -- ^ Database file.
    -> Listen
    -- ^ HTTP API Server port.
    -> HttpBridgeBackend
    -- ^ Whether and how to launch or use the node backend.
    -> Maybe
        (  PortNumber
        -> PortNumber
        -> NetworkLayer IO t (Block Tx)
        -> IO a
        )
    -- ^ Optional operation to run under the wallet server (e.g. test suite).
    -> IO ExitCode
serveWallet (cfg, tr) databaseDir listen bridge mAction = do
    installSignalHandlers tr
    logInfo tr "Wallet backend server starting..."
    logInfo tr $ "Node is Http-Bridge on " <> toText (networkVal @n)
    HttpBridge.withNetworkLayer @n tr bridge $ \case
        Right (bridgePort, nl) -> do
            wlRnd <- newApiLayer nl
            wlSeq <- newApiLayer nl
            let mkCallback action apiPort =
                    action (fromIntegral apiPort) bridgePort nl
            withServer wlRnd wlSeq (mkCallback <$> mAction)
        Left e -> handleNetworkStartupError e
  where
    withServer
        :: ApiLayer (RndState t) t RndKey
        -> ApiLayer (SeqState t) t SeqKey
        -> Maybe (Int -> IO a)
        -> IO ExitCode
    withServer apiRnd apiSeq action = do
        Server.withListeningSocket "127.0.0.1" listen $ \case
            Right (port, socket) -> do
                  let tracerIPC = appendName "daedalus-ipc" tr
                  let tracerApi = appendName "api" tr
                  let beforeMainLoop = logInfo tr $
                          "Wallet backend server listening on: " <> toText port
                  let settings = Warp.defaultSettings
                          & setBeforeMainLoop beforeMainLoop
                  let ipcServer = daedalusIPC tracerIPC port
                  let apiServer = Server.start
                        settings tracerApi socket apiRnd apiSeq dummyPool
                  let withAction = maybe id (\cb -> race_ (cb port)) action
                  withAction $ race_ ipcServer apiServer
                  pure ExitSuccess
            Left e -> handleApiServerStartupError e

    newApiLayer
        :: forall s k .
            ( IsOurs s
            , KeyToAddress (HttpBridge n) k
            , NFData s
            , PersistKey k
            , PersistState s
            , Show s
            )
        => NetworkLayer IO t (Block Tx)
        -> IO (ApiLayer s t k)
    newApiLayer nl = do
        let g0 = staticBlockchainParameters nl
        let tl = HttpBridge.newTransactionLayer @n
        wallets <- maybe (pure []) (Sqlite.findDatabases @k tr) databaseDir
        Server.newApiLayer tr g0 nl tl dbFactory wallets

    dbFactory
        :: forall s k .
            ( IsOurs s
            , NFData s
            , PersistKey k
            , PersistState s
            , Show s
            )
        => DBFactory IO s t k
    dbFactory =
        Sqlite.mkDBFactory cfg tr databaseDir

    dummyPool :: StakePoolLayer IO
    dummyPool = StakePoolLayer
        { listStakePools =
            error "StakePoolLayer: not implemented for http-bridge"
        }

    handleNetworkStartupError :: ErrStartup -> IO ExitCode
    handleNetworkStartupError = \case
        ErrStartupCommandExited pe -> case pe of
            ProcessDidNotStart _cmd exc -> do
                failWith tr $
                    "Could not start the node backend. " <> T.pack (show exc)
            ProcessHasExited _cmd st -> do
                failWith tr $
                    "The node exited with status " <> T.pack (show st)
        ErrStartupNodeNotListening -> do
            failWith tr
                "Waited too long for http-bridge to become available. Giving up!"

    handleApiServerStartupError :: ListenError -> IO ExitCode
    handleApiServerStartupError = \case
        ListenErrorHostDoesNotExist host ->
            failWith tr $ mempty
                <> "Can't listen on "
                <> T.pack (show host)
                <> ". It does not exist."
        ListenErrorInvalidAddress host ->
            failWith tr $ mempty
                <> "Can't listen on "
                <> T.pack (show host)
                <> ". Invalid address."
        ListenErrorAddressAlreadyInUse mPort ->
            failWith tr $ mempty
                <> "The API server listen port "
                <> maybe "(unknown)" (T.pack . show) mPort
                <> " is already in use."
        ListenErrorOperationNotPermitted ->
            failWith tr $ mempty
                <> "Cannot listen on the given port. "
                <> "The operation is not permitted."
