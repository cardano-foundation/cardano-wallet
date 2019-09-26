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
-- Jörmungandr backend.
--
-- The "Cardano.Wallet.Jormungandr.Network" module handles synchronization with
-- the @jormungandr@ process.
--
-- The block format decoders are in "Cardano.Wallet.Jormungandr.Network".
--
-- Functionality specific to this backend for creating transactions is in
-- "Cardano.Wallet.Jormungandr.Transaction".

module Cardano.Wallet.Jormungandr
    ( serveWallet
    ) where

import Prelude

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Setup
    ( shutdown )
import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
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
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.Jormungandr.Network
    ( ErrGetBlockchainParams (..)
    , ErrStartup (..)
    , JormungandrBackend (..)
    , withNetworkLayer
    )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Model
    ( getGenesisBlockHash )
import Cardano.Wallet.Primitive.Types
    ( Block, Hash (..) )
import Control.Concurrent.Async
    ( race_ )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( hPutStrLn, stderr )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-jormungandr@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall t k n s.
       (n ~ 'Testnet, t ~ Jormungandr n, s ~ SeqState t, k ~ SeqKey)
    => (CM.Configuration, Switchboard Text, Trace IO Text)
    -- ^ Logging config.
    -> Maybe FilePath
    -- ^ Database file.
    -> Listen
    -- ^ HTTP API Server port.
    -> JormungandrBackend
    -- ^ Whether and how to launch or use the node backend.
    -> IO ExitCode
serveWallet (cfg, sb, tr) dbFile listen lj = do
    installSignalHandlers tr
    logInfo tr "Wallet backend server starting..."
    logInfo tr $ "Node is Jörmungandr on " <> toText (networkVal @n)
    withDBLayer cfg tr $ \db ->
        withNetworkLayer tr lj $ \_cp -> \case
            Right nl -> do
                newWalletLayer tr db nl >>= startServer tr
                pure ExitSuccess
            Left e -> handleNetworkStartupError e
  where
    startServer
        :: Trace IO Text
        -> WalletLayer s t k
        -> IO ()
    startServer tracer wallet = do
        Server.withListeningSocket listen $ \(port, socket) -> do
            let tracerIPC = appendName "daedalus-ipc" tracer
            let tracerApi = appendName "api" tracer
            let beforeMainLoop = logInfo tracer $
                    "Wallet backend server listening on: " <> toText port
            let settings = Warp.defaultSettings
                    & setBeforeMainLoop beforeMainLoop
            let ipcServer = daedalusIPC tracerIPC port
            let apiServer = Server.start settings tracerApi socket wallet
            race_ ipcServer apiServer

    newWalletLayer
        :: Trace IO Text
        -> DBLayer IO s t k
        -> NetworkLayer IO Tx (Block Tx)
        -> IO (WalletLayer s t k)
    newWalletLayer tracer db nl = do
        let (block0, bp) = staticBlockchainParameters nl
        let tl = newTransactionLayer @n (getGenesisBlockHash bp)
        Wallet.newWalletLayer tracer (block0, bp) db nl tl

    withDBLayer
        :: CM.Configuration
        -> Trace IO Text
        -> (DBLayer IO s t k -> IO a)
        -> IO a
    withDBLayer logCfg tracer action = do
        let tracerDB = appendName "database" tracer
        Sqlite.withDBLayer logCfg tracerDB dbFile action

    handleNetworkStartupError :: ErrStartup -> IO ExitCode
    handleNetworkStartupError = \case
        ErrStartupGetBlockchainParameters e -> case e of
            ErrGetBlockchainParamsNetworkUnreachable _ ->
                handleNetworkUnreachable
            ErrGetBlockchainParamsGenesisNotFound h ->
                handleGenesisNotFound h
            ErrGetBlockchainParamsIncompleteParams _ ->
                handleNoInitialPolicy
        ErrStartupGenesisBlockFailed file -> do
            logAlert tr $ mempty
                <> "As far as I can tell, this isn't a valid block file: "
                <> T.pack file
            pure (ExitFailure 1)
        ErrStartupCommandExited pe -> case pe of
            ProcessDidNotStart _cmd exc -> do
                logAlert tr $
                    "Could not start the node backend. " <> T.pack (show exc)
                pure (ExitFailure 1)
            ProcessHasExited _cmd st -> do
                logAlert tr $ "The node exited with status " <> T.pack (show st)
                pure (ExitFailure 1)
        ErrStartupNodeNotListening -> do
            logAlert tr $
                "Waited too long for Jörmungandr to become available. " <>
                "Giving up!"
            pure (ExitFailure 1)

    handleGenesisNotFound :: Hash "Genesis" -> IO ExitCode
    handleGenesisNotFound block0H = do
        logAlert tr
            "Failed to retrieve the genesis block. The block doesn't exist!"
        shutdown sb
        hPutStrLn stderr $ mconcat
            [ "Hint: double-check the genesis hash you've just gave "
            , "me via '--genesis-hash' (i.e. ", showT block0H, ")."
            ]
        pure (ExitFailure 1)

    handleNetworkUnreachable :: IO ExitCode
    handleNetworkUnreachable = do
        logAlert tr "It looks like Jörmungandr is down?"
        pure (ExitFailure 1)

    handleNoInitialPolicy :: IO ExitCode
    handleNoInitialPolicy = do
        logAlert tr $ mconcat
            [ "I successfully retrieved the genesis block from Jörmungandr, "
            , "but there's no initial fee policy defined?"
            ]
        pure (ExitFailure 1)
