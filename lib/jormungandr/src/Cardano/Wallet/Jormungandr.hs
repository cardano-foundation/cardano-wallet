{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.CLI
    ( Port (..), failWith, waitForService )
import Cardano.Launcher
    ( ProcessHasExited (..), installSignalHandlers )
import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
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
    , withNetworkLayer
    )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForNetwork )
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
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block, Hash (..) )
import Control.Concurrent.Async
    ( race_ )
import Control.DeepSeq
    ( NFData )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
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
    => (CM.Configuration, Switchboard Text, Trace IO Text)
    -- ^ Logging config.
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> Listen
    -- ^ HTTP API Server port.
    -> JormungandrBackend
    -- ^ Whether and how to launch or use the node backend.
    -> (Port "wallet" -> Port "node" -> BlockchainParameters -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet (cfg, sb, tr) databaseDir listen lj beforeMainLoop = do
    installSignalHandlers tr
    logInfo tr "Wallet backend server starting..."
    logInfo tr $ "Node is Jörmungandr on " <> toText (networkVal @n)
    withNetworkLayer tr lj $ \case
        Right (cp, nl) -> do
            let nPort = Port $ baseUrlPort $ cp ^. #restApi
            waitForService "Jörmungandr" (sb, tr) nPort $
                waitForNetwork nl defaultRetryPolicy
            let (_, bp) = staticBlockchainParameters nl
            rndApi <- apiLayer tr (toWLBlock <$> nl)
            seqApi <- apiLayer tr (toWLBlock <$> nl)
            startServer tr nPort bp rndApi seqApi
            pure ExitSuccess
        Left e -> handleNetworkStartupError e
  where
    startServer
        :: Trace IO Text
        -> Port "node"
        -> BlockchainParameters
        -> ApiLayer (RndState t) t RndKey
        -> ApiLayer (SeqState t) t SeqKey
        -> IO ()
    startServer tracer nPort bp rndWallet seqWallet = do
        Server.withListeningSocket listen $ \(wPort, socket) -> do
            let tracerIPC = appendName "daedalus-ipc" tracer
            let tracerApi = appendName "api" tracer
            let settings = Warp.defaultSettings
                    & setBeforeMainLoop (beforeMainLoop (Port wPort) nPort bp)
            let ipcServer = daedalusIPC tracerIPC wPort
            let apiServer =
                    Server.start settings tracerApi socket rndWallet seqWallet
            race_ ipcServer apiServer

    toWLBlock = J.convertBlock

    apiLayer
        :: forall s k .
            ( KeyToAddress (Jormungandr 'Testnet) k
            , IsOurs s
            , NFData s
            , Show s
            , PersistState s
            , PersistKey k
            )
        => Trace IO Text
        -> NetworkLayer IO t (Block Tx)
        -> IO (ApiLayer s t k)
    apiLayer tracer nl = do
        let (block0, bp) = staticBlockchainParameters nl
        let tl = newTransactionLayer @n (getGenesisBlockHash bp)
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
            failWith (sb, tr) $ mempty
                <> "As far as I can tell, this isn't a valid block file: "
                <> T.pack file
        ErrStartupInvalidGenesisHash h ->
            failWith (sb, tr) $ mempty
                <> "As far as I can tell, this isn't a valid block hash: "
                <> T.pack h
        ErrStartupCommandExited pe -> case pe of
            ProcessDidNotStart _cmd exc ->
                failWith (sb, tr) $
                    "Could not start the node backend. " <> T.pack (show exc)
            ProcessHasExited _cmd st ->
                failWith (sb, tr) $
                    "The node exited with status " <> T.pack (show st)
        ErrStartupNodeNotListening -> do
            failWith (sb, tr) $ mempty
                <> "Waited too long for Jörmungandr to become available. "
                <> "Giving up!"

    handleGenesisNotFound :: Hash "Genesis" -> IO ExitCode
    handleGenesisNotFound block0H = do
        failWith (sb, tr) $ T.pack $ mconcat
            [ "Failed to retrieve the genesis block. The block doesn't exist! "
            , "Hint: double-check the genesis hash you've just gave "
            , "me via '--genesis-block-hash' (i.e. " <> showT block0H <> ")."
            ]

    handleNetworkUnreachable :: IO ExitCode
    handleNetworkUnreachable = do
        failWith (sb, tr)
            "It looks like Jörmungandr is down? Hint: double-check Jörmungandr\
            \ server's port."

    handleNoInitialPolicy :: IO ExitCode
    handleNoInitialPolicy = do
        failWith (sb, tr) $ mempty
            <> "I successfully retrieved the genesis block from Jörmungandr, "
            <> "but there's no initial fee policy defined?"
