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

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
import Cardano.CLI
    ( waitForService )
import Cardano.Launcher
    ( ProcessHasExited (..), installSignalHandlers )
import Cardano.Wallet
    ( WalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.HttpBridge.Network
    ( ErrStartup (..), HttpBridgeBackend (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( ErrNetworkTip (..), NetworkLayer (..), isNetworkUnreachable )
import Cardano.Wallet.Network.Ports
    ( PortNumber, defaultRetryPolicy )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Block )
import Control.Concurrent.Async
    ( race_ )
import Control.Exception
    ( throwIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Retry
    ( RetryPolicyM, retrying )
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
import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-http-bridge@ main function.
serveWallet
    :: forall t k n s a. (t ~ HttpBridge n, s ~ SeqState t, k ~ SeqKey)
    => (KeyToAddress t k, KnownNetwork n)
    => (CM.Configuration, Switchboard Text, Trace IO Text)
    -- ^ Logging config.
    -> Maybe FilePath
    -- ^ Database file.
    -> Listen
    -- ^ HTTP API Server port.
    -> HttpBridgeBackend
    -- ^ Whether and how to launch or use the node backend.
    -> Maybe (PortNumber -> PortNumber -> NetworkLayer IO Tx (Block Tx) ->
              Sqlite.SqliteContext -> DBLayer IO s t k -> WalletLayer s t k ->
              IO a)
    -- ^ Optional operation to run under the wallet server (e.g. test suite).
    -> IO ExitCode
serveWallet (cfg, sb, tr) dbFile listen bridge mAction = do
    installSignalHandlers tr
    logInfo tr "Wallet backend server starting..."
    logInfo tr $ "Node is Http-Bridge on " <> toText (networkVal @n)
    withDBLayer $ \(ctx, db) ->
        HttpBridge.withNetworkLayer @n tr bridge $ \case
            Right (bridgePort, nl) -> do
                waitForService "http-bridge" (sb, tr) $
                    waitForConnection nl defaultRetryPolicy
                wl <- newWalletLayer db nl
                let mkCallback action apiPort =
                        action (fromIntegral apiPort) bridgePort nl ctx db wl
                withServer wl (mkCallback <$> mAction)
                pure ExitSuccess
            Left e -> handleNetworkStartupError e
  where
        withServer
            :: WalletLayer s t k
            -> Maybe (Int -> IO a)
            -> IO ()
        withServer wallet action = do
            Server.withListeningSocket listen $ \(port, socket) -> do
                let tracerIPC = appendName "daedalus-ipc" tr
                let tracerApi = appendName "api" tr
                let beforeMainLoop = logInfo tr $
                        "Wallet backend server listening on: " <> toText port
                let settings = Warp.defaultSettings
                        & setBeforeMainLoop beforeMainLoop
                let ipcServer = daedalusIPC tracerIPC port
                let apiServer = Server.start settings tracerApi socket wallet
                let withAction = maybe id (\cb -> race_ (cb port)) action
                withAction $ race_ ipcServer apiServer

        newWalletLayer
            :: DBLayer IO s t k
            -> NetworkLayer IO Tx (Block Tx)
            -> IO (WalletLayer s t k)
        newWalletLayer db nl = do
            let g0 = staticBlockchainParameters nl
            let tl = HttpBridge.newTransactionLayer @n
            Wallet.newWalletLayer tr g0 db nl tl

        withDBLayer
            :: ((Sqlite.SqliteContext, DBLayer IO s t k) -> IO a')
            -> IO a'
        withDBLayer action = do
            let tracerDB = appendName "database" tr
            Sqlite.withDBLayerCtx cfg tracerDB dbFile action

        handleNetworkStartupError :: ErrStartup -> IO ExitCode
        handleNetworkStartupError = \case
            ErrStartupCommandExited pe -> case pe of
                ProcessDidNotStart _cmd exc -> do
                    logAlert tr $ "Could not start the node backend. " <> T.pack (show exc)
                    pure (ExitFailure 1)
                ProcessHasExited _cmd st -> do
                    logAlert tr $ "The node exited with status " <> T.pack (show st)
                    pure (ExitFailure 1)
            ErrStartupNodeNotListening -> do
                logAlert tr "Waited too long for http-bridge to become available. Giving up!"
                pure (ExitFailure 1)

-- | Wait until 'networkTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForConnection
    :: NetworkLayer IO tx block
    -> RetryPolicyM IO
    -> IO ()
waitForConnection nw policy = do
    r <- retrying policy shouldRetry (const $ runExceptT (networkTip nw))
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkTipNotFound ->
            return True
        Left (ErrNetworkTipNetworkUnreachable e) ->
            return $ isNetworkUnreachable e
