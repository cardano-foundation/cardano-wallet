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

      -- * Utilities
    , toSPBlock
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.CLI
    ( Port (..), failWith )
import Cardano.Launcher
    ( ProcessHasExited (..), installSignalHandlers )
import Cardano.Pool.Metrics
    ( StakePoolLayer, monitorStakePools, newStakePoolLayer )
import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..), defaultWorkerAfter )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB
    ( DBFactory )
import Cardano.Wallet.DB.Sqlite
    ( PersistState )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
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
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress
    , NetworkDiscriminant
    , NetworkDiscriminantVal
    , PersistKey
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( DecodeAddress, EncodeAddress, IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block, BlockHeader (..), Hash (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.Async
    ( race_ )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( void )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..) )
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
import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Pool.Metrics as Pool
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-jormungandr@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall (n :: NetworkDiscriminant) t.
        ( t ~ Jormungandr
        , NetworkDiscriminantVal n
        , DecodeAddress n
        , EncodeAddress n
        , KeyToAddress n RndKey
        , KeyToAddress n SeqKey
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
    logInfo tr $ "Node is Jörmungandr on " <> toText (networkDiscriminantVal @n)
    withNetworkLayer tr lj $ \case
        Right (cp, nl) -> do
            let nPort = Port $ baseUrlPort $ _restApi cp
            let (_, bp) = staticBlockchainParameters nl
            let rndTl = newTransactionLayer @n (getGenesisBlockHash bp)
            let seqTl = newTransactionLayer @n (getGenesisBlockHash bp)
            let poolDBPath = Pool.defaultFilePath <$> databaseDir
            Pool.withDBLayer cfg tr poolDBPath $ \db -> do
                spl <- stakePoolLayer tr nl db
                rndApi <- apiLayer tr rndTl nl
                seqApi <- apiLayer tr seqTl nl
                startServer tr nPort bp rndApi seqApi spl
        Left e -> handleNetworkStartupError e
  where
    startServer
        :: Trace IO Text
        -> Port "node"
        -> BlockchainParameters
        -> ApiLayer (RndState n) t RndKey
        -> ApiLayer (SeqState n) t SeqKey
        -> StakePoolLayer IO
        -> IO ExitCode
    startServer tracer nPort bp rndWallet seqWallet spl = do
        Server.withListeningSocket hostPref listen $ \case
            Right (wPort, socket) -> do
                sockAddr <- getSocketName socket
                let tracerIPC = appendName "daedalus-ipc" tracer
                let tracerApi = appendName "api" tracer
                let settings = Warp.defaultSettings
                        & setBeforeMainLoop (beforeMainLoop sockAddr nPort bp)
                let ipcServer = daedalusIPC tracerIPC wPort
                let apiServer =
                        Server.start
                            settings tracerApi socket rndWallet seqWallet spl
                race_ ipcServer apiServer
                pure ExitSuccess
            Left e -> handleApiServerStartupError e

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
        -> NetworkLayer IO t J.Block
        -> IO (ApiLayer s t k)
    apiLayer tracer tl nl = do
        let (block0, bp) = staticBlockchainParameters nl
        wallets <- maybe (pure []) (Sqlite.findDatabases @k tr) databaseDir
        Server.newApiLayer tracer (block0, bp) nl' tl dbFactory wallets
      where
        nl' = toWLBlock <$> nl

    stakePoolLayer
        :: Trace IO Text
        -> NetworkLayer IO t J.Block
        -> Pool.DBLayer IO
        -> IO (StakePoolLayer IO)
    stakePoolLayer trRoot nl db = do
        void $ forkFinally (monitorStakePools tr' nl' db) onExit
        newStakePoolLayer db nl trRoot
      where
        tr' = appendName "stake-pools" trRoot
        nl' = toSPBlock <$> nl
        onExit = defaultWorkerAfter tr'

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

--------------------------------------------------------------------------------
-- Exported Utilities
--------------------------------------------------------------------------------

-- | Covert a raw block to one that the "Cardano.Pool.Metrics" module accepts.
toSPBlock :: J.Block -> Pool.Block
toSPBlock b = Pool.Block
     (convertHeader header)
     (fromMaybe (error "block has no producer") $ J.producedBy header)
   where
     header = J.header b
     convertHeader :: J.BlockHeader -> BlockHeader
     convertHeader h = BlockHeader
         (J.slot h)
         (Quantity $ fromIntegral $ J.chainLength h)
         (J.headerHash h)
         (J.parentHeaderHash h)

toWLBlock :: J.Block -> Block Tx
toWLBlock = J.convertBlock
