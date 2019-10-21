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

import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.CLI
    ( Port (..), failWith, waitForService )
import Cardano.Launcher
    ( ProcessHasExited (..), installSignalHandlers )
import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..) )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB
    ( DBFactory )
import Cardano.Wallet.DB.Sqlite
    ( PersistState )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..)
    , ErrGetBlockchainParams (..)
    , ErrStartup (..)
    , JormungandrBackend (..)
    , JormungandrConnParams (..)
    , withNetworkLayer
    )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForNetwork )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PersistKey )
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
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block, Hash (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Concurrent.Async
    ( race_ )
import Control.DeepSeq
    ( NFData )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import Network.Socket
    ( SockAddr, getSocketName )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import System.Exit
    ( ExitCode (..) )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-jormungandr@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall t n .
        ( n ~ 'Testnet
        , t ~ Jormungandr n
        )
    => (CM.Configuration, Trace IO Text)
    -- ^ Logging config.
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ HTTP API Server port.
    -> JormungandrBackend
    -- ^ Whether and how to launch or use the node backend.
    -> (SockAddr -> Port "node" -> BlockchainParameters -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet (cfg, tr) databaseDir hostPref listen lj beforeMainLoop = do
    installSignalHandlers tr
    logInfo tr "Wallet backend server starting..."
    logInfo tr $ "Node is Jörmungandr on " <> toText (networkVal @n)
    withNetworkLayer tr lj $ \case
        Right (cp, nl) -> do
            let nPort = Port $ baseUrlPort $ _restApi cp
            waitForService "Jörmungandr" tr nPort $
                waitForNetwork nl defaultRetryPolicy
            let (_, bp) = staticBlockchainParameters nl
            let rndTl = newTransactionLayer @n (getGenesisBlockHash bp)
            let seqTl = newTransactionLayer @n (getGenesisBlockHash bp)
            rndApi <- apiLayer tr rndTl (toWLBlock <$> nl)
            seqApi <- apiLayer tr seqTl (toWLBlock <$> nl)
            startServer tr nPort bp rndApi seqApi
        Left e -> handleNetworkStartupError e
  where
    startServer
        :: Trace IO Text
        -> Port "node"
        -> BlockchainParameters
        -> ApiLayer (RndState t) t RndKey
        -> ApiLayer (SeqState t) t SeqKey
        -> IO ExitCode
    startServer tracer nPort bp rndWallet seqWallet = do
        Server.withListeningSocket hostPref listen $ \case
            Right (wPort, socket) -> do
                sockAddr <- getSocketName socket
                let tracerIPC = appendName "daedalus-ipc" tracer
                let tracerApi = appendName "api" tracer
                let settings = Warp.defaultSettings
                        & setBeforeMainLoop (beforeMainLoop sockAddr nPort bp)
                let ipcServer = daedalusIPC tracerIPC wPort
                let apiServer =
                        Server.start settings tracerApi socket rndWallet seqWallet
                race_ ipcServer apiServer
                pure ExitSuccess
            Left e -> handleApiServerStartupError e

    toWLBlock = J.convertBlock

    apiLayer
        :: forall s k.
            ( IsOurs s
            , NFData s
            , Show s
            , PersistState s
            , PersistKey k
            )
        => Trace IO Text
        -> TransactionLayer t k
        -> NetworkLayer IO t (Block Tx)
        -> IO (ApiLayer s t k)
    apiLayer tracer tl nl = do
        let (block0, bp) = staticBlockchainParameters nl
        wallets <- maybe (pure []) (Sqlite.findDatabases @k tr) databaseDir
        Server.newApiLayer tracer (block0, bp) nl tl dbFactory wallets

    dbFactory
        :: forall s k .
            ( IsOurs s
            , NFData s
            , Show s
            , PersistState s
            , PersistKey k
            )
        => DBFactory IO s t k
    dbFactory = Sqlite.mkDBFactory cfg tr databaseDir

    handleNetworkStartupError :: ErrStartup -> IO ExitCode
    handleNetworkStartupError = \case
        ErrStartupGetBlockchainParameters e -> case e of
            ErrGetBlockchainParamsNetworkUnreachable _ ->
                handleNetworkUnreachable
            ErrGetBlockchainParamsGenesisNotFound h ->
                handleGenesisNotFound h
            ErrGetBlockchainParamsIncompleteParams _ ->
                handleNoInitialPolicy
        ErrStartupInvalidGenesisBlock file ->
            failWith tr $ mempty
                <> "As far as I can tell, this isn't a valid block file: "
                <> T.pack file
        ErrStartupInvalidGenesisHash h ->
            failWith tr $ mempty
                <> "As far as I can tell, this isn't a valid block hash: "
                <> T.pack h
        ErrStartupCommandExited pe -> case pe of
            ProcessDidNotStart _cmd exc ->
                failWith tr $
                    "Could not start the node backend. " <> T.pack (show exc)
            ProcessHasExited _cmd st ->
                failWith tr $
                    "The node exited with status " <> T.pack (show st)
        ErrStartupNodeNotListening -> do
            failWith tr $ mempty
                <> "Waited too long for Jörmungandr to become available. "
                <> "Giving up!"

    handleGenesisNotFound :: Hash "Genesis" -> IO ExitCode
    handleGenesisNotFound block0H = do
        failWith tr $ T.pack $ mconcat
            [ "Failed to retrieve the genesis block. The block doesn't exist! "
            , "Hint: double-check the genesis hash you've just gave "
            , "me via '--genesis-block-hash' (i.e. " <> showT block0H <> ")."
            ]

    handleNetworkUnreachable :: IO ExitCode
    handleNetworkUnreachable = do
        failWith tr
            "It looks like Jörmungandr is down? Hint: double-check Jörmungandr\
            \ server's port."

    handleNoInitialPolicy :: IO ExitCode
    handleNoInitialPolicy = do
        failWith tr $ mempty
            <> "I successfully retrieved the genesis block from Jörmungandr, "
            <> "but there's no initial fee policy defined?"

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
