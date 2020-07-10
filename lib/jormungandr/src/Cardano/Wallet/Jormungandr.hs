{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
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

      -- * Tracing
    , Tracers' (..)
    , Tracers
    , TracerSeverities
    , tracerLabels
    , tracerDescriptions
    , nullTracers
    , setupTracers
    , tracerSeverities

      -- * Log messages
    , ApplicationLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Trace
    ( Trace, appendName )
import Cardano.CLI
    ( Port (..) )
import Cardano.DB.Sqlite
    ( DBLog )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Pool.Jormungandr.Metadata
    ( ApiStakePool )
import Cardano.Pool.Jormungandr.Metrics
    ( ErrListStakePools
    , StakePoolLayer (..)
    , StakePoolLog
    , monitorStakePools
    , newStakePoolLayer
    )
import Cardano.Wallet
    ( WalletLog )
import Cardano.Wallet.Api
    ( ApiLayer, ApiV2 )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress, EncodeAddress )
import Cardano.Wallet.DB.Sqlite
    ( DefaultFieldValues (..), PersistState )
import Cardano.Wallet.Jormungandr.Api.Server
    ( server )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..)
    , ErrGetBlockchainParams (..)
    , ErrStartup (..)
    , JormungandrBackend (..)
    , JormungandrConnParams (..)
    , NetworkLayerLog
    , withNetworkLayer
    )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Logging
    ( filterTraceSeverity, trMessageText )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress
    , Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , PersistPrivateKey
    , WalletKey
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Address
    , Block
    , BlockHeader (..)
    , ChimericAccount
    , GenesisParameters (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , WalletId
    )
import Cardano.Wallet.Registry
    ( WorkerLog (..), defaultWorkerAfter )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Applicative
    ( Const (..) )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.Async
    ( link )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( void )
import Control.Tracer
    ( Tracer (..), nullTracer, traceWith )
import Data.Function
    ( (&) )
import Data.Functor.Contravariant
    ( contramap )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import GHC.Generics
    ( Generic )
import Network.Ntp
    ( NtpClient (..), NtpTrace (..), withWalletNtpClient )
import Network.Socket
    ( SockAddr, Socket, getSocketName )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.IOManager
    ( withIOManager )

import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Pool.Jormungandr.Metrics as Pool
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Cardano.Wallet.Primitive.Types as W
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
        , DelegationAddress n JormungandrKey
        )
    => Tracers IO
    -- ^ Logging config.
    -> SyncTolerance
    -- ^ A time tolerance within we consider being synced
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ HTTP API Server port.
    -> JormungandrBackend
    -- ^ Whether and how to launch or use the node backend.
    -> (SockAddr -> Port "node" -> NetworkParameters -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet Tracers{..} sTolerance databaseDir hostPref listen backend beforeMainLoop = do
    traceWith applicationTracer $ MsgStarting backend
    traceWith applicationTracer $ MsgNetworkName $ networkDiscriminantVal @n
    Server.withListeningSocket hostPref listen $ \case
        Left e -> handleApiServerStartupError e
        Right (_wPort, socket) -> serveApp socket

  where
    serveApp socket = withNetworkLayer networkTracer backend $ \case
        Left e -> handleNetworkStartupError e
        Right (cp, (block0, np), nl) -> do
            let nPort = Port $ baseUrlPort $ _restApi cp
            let gp = genesisParameters np
            let byronTl = newTransactionLayer (getGenesisBlockHash gp)
            let icarusTl = newTransactionLayer (getGenesisBlockHash gp)
            let jormungandrTl = newTransactionLayer (getGenesisBlockHash gp)
            let poolDBPath = Pool.defaultFilePath <$> databaseDir
            Pool.withDBLayer stakePoolDbTracer poolDBPath $ \db ->
                withSystemTempDirectory "stake-pool-metadata" $ \md ->
                withIOManager $ \io ->
                withWalletNtpClient io ntpClientTracer $ \ntpClient -> do
                    link $ ntpThread ntpClient
                    poolApi <- stakePoolLayer (block0, gp) nl db md
                    byronApi <- apiLayer (block0, np) byronTl nl
                    icarusApi <- apiLayer (block0, np) icarusTl nl
                    jormungandrApi <- apiLayer (block0, np) jormungandrTl nl
                    startServer socket nPort np
                        byronApi
                        icarusApi
                        jormungandrApi
                        poolApi
                        ntpClient
                    pure ExitSuccess

    startServer
        :: Socket
        -> Port "node"
        -> NetworkParameters
        -> ApiLayer (RndState 'Mainnet) t ByronKey
        -> ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        -> ApiLayer (SeqState n JormungandrKey) t JormungandrKey
        -> StakePoolLayer ErrListStakePools IO
        -> NtpClient
        -> IO ()
    startServer socket nPort gp byron icarus jormungandr pools ntp = do
        sockAddr <- getSocketName socket
        let settings = Warp.defaultSettings
                & setBeforeMainLoop (beforeMainLoop sockAddr nPort gp)
        let application = Server.serve (Proxy @(ApiV2 n ApiStakePool))
                $ server byron icarus jormungandr pools ntp
        Server.start settings apiServerTracer tlsConfig socket application
      where
        tlsConfig = Nothing

    apiLayer
        :: forall s k.
            ( IsOurs s Address
            , IsOurs s ChimericAccount
            , PersistState s
            , PersistPrivateKey (k 'RootK)
            , WalletKey k
            )
        => (J.Block, NetworkParameters)
        -> TransactionLayer t k
        -> NetworkLayer IO t J.Block
        -> IO (ApiLayer s t k)
    apiLayer (block0, np) tl nl = do
        db <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues
                { defaultActiveSlotCoefficient =
                    getActiveSlotCoefficient (genesisParameters np)
                , defaultDesiredNumberOfPool =
                    desiredNumberOfStakePools (protocolParameters np)
                , defaultMinimumUTxOValue =
                    minimumUTxOvalue (protocolParameters np)
                }
            )
            databaseDir
        Server.newApiLayer
            walletEngineTracer (toWLBlock block0, np, sTolerance) nl' tl db
            Server.idleWorker
      where
        nl' = toWLBlock <$> nl

    stakePoolLayer
        :: (J.Block, GenesisParameters)
        -> NetworkLayer IO t J.Block
        -> Pool.DBLayer IO
        -> FilePath
        -> IO (StakePoolLayer ErrListStakePools IO)
    stakePoolLayer (block0, gp) nl db metadataDir = do
        void $ forkFinally (monitorStakePools tr (toSPBlock block0, k) nl' db) onExit
        getEpCst <- maybe (throwIO ErrStartupMissingIncentiveParameters) pure $
            J.rankingEpochConstants block0
        pure $ newStakePoolLayer tr block0H getEpCst db nl' metadataDir
      where
        nl' = toSPBlock <$> nl
        tr  = contramap (MsgFromWorker mempty) stakePoolEngineTracer
        onExit = defaultWorkerAfter stakePoolEngineTracer
        k = getEpochStability gp
        block0H = W.header $ toWLBlock block0

    handleNetworkStartupError :: ErrStartup -> IO ExitCode
    handleNetworkStartupError err = do
        traceWith applicationTracer $ MsgWalletStartupError err
        pure $ ExitFailure $ exitCodeNetwork err

    handleApiServerStartupError :: ListenError -> IO ExitCode
    handleApiServerStartupError err = do
        traceWith applicationTracer $ MsgServerStartupError err
        pure $ ExitFailure $ exitCodeApiServer err

--------------------------------------------------------------------------------
-- Exported Utilities
--------------------------------------------------------------------------------

-- | Covert a raw block to one that the "Cardano.Pool.Jormungandr.Metrics" module accepts.
toSPBlock :: J.Block -> Pool.Block
toSPBlock b = Pool.Block
     (convertHeader header)
     -- FIXME
     -- Allow defining different types for block vs genesis block in the network
     -- layer so that staticBlockchainParameter isn't partial.
     (fromMaybe (error "block has no producer") $ J.producedBy header)
     (J.poolRegistrationsFromBlock b)
   where
     header = J.header b
     convertHeader :: J.BlockHeader -> BlockHeader
     convertHeader h = BlockHeader
         (J.slot h)
         (Quantity $ fromIntegral $ J.chainLength h)
         (J.headerHash h)
         (J.parentHeaderHash h)

toWLBlock :: J.Block -> Block
toWLBlock = J.convertBlock

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages related to application startup and shutdown.
data ApplicationLog
    = MsgStarting JormungandrBackend
    | MsgNetworkName Text
    | MsgWalletStartupError ErrStartup
    | MsgServerStartupError ListenError
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText msg = case msg of
        MsgStarting backend ->
            "Wallet backend server starting. " <> toText backend <> "..."
        MsgNetworkName n -> "Node is Jörmungandr on " <> n
        MsgWalletStartupError startupErr -> case startupErr of
            ErrStartupGetBlockchainParameters e -> case e of
                ErrGetBlockchainParamsNetworkUnreachable _ ->
                    "It looks like Jörmungandr is down? Hint: double-check\
                    \  Jörmungandr server's port."
                ErrGetBlockchainParamsGenesisNotFound block0H ->
                    T.pack $ mconcat
                    [ "Failed to retrieve the genesis block. The block doesn't"
                    , " exist! Hint: double-check the genesis hash you've just "
                    , " gave me via '--genesis-block-hash'"
                    , "(i.e. " <> showT block0H <> ")."
                    ]
                ErrGetBlockchainParamsIncompleteParams _ -> mempty
                    <> "I successfully retrieved the genesis block from "
                    <> "Jörmungandr, but there's no initial fee policy defined?"
            ErrStartupInvalidGenesisBlock file -> mempty
                    <> "As far as I can tell, this isn't a valid block file: "
                    <> T.pack file
            ErrStartupInvalidGenesisHash h -> mempty
                    <> "As far as I can tell, this isn't a valid block hash: "
                    <> T.pack h
            ErrStartupCommandExited pe -> case pe of
                ProcessDidNotStart _cmd exc ->
                    "Could not start the node backend. " <> T.pack (show exc)
                ProcessHasExited _cmd st ->
                    "The node exited with status " <> T.pack (show st)
            ErrStartupNodeNotListening -> mempty
                    <> "Waited too long for Jörmungandr to become available. "
                    <> "Giving up!"
            ErrStartupMissingIncentiveParameters ->
                "Couldn't find incentive parameters in the genesis block."
        MsgServerStartupError startupErr -> case startupErr of
            ListenErrorHostDoesNotExist host -> mempty
                    <> "Can't listen on "
                    <> T.pack (show host)
                    <> ". It does not exist."
            ListenErrorInvalidAddress host -> mempty
                    <> "Can't listen on "
                    <> T.pack (show host)
                    <> ". Invalid address."
            ListenErrorAddressAlreadyInUse mPort -> mempty
                    <> "The API server listen port "
                    <> maybe "(unknown)" (T.pack . show) mPort
                    <> " is already in use."
            ListenErrorOperationNotPermitted -> mempty
                    <> "Cannot listen on the given port. "
                    <> "The operation is not permitted."

instance HasPrivacyAnnotation ApplicationLog
instance HasSeverityAnnotation ApplicationLog where
    getSeverityAnnotation ev = case ev of
        MsgStarting _ -> Info
        MsgNetworkName _ -> Info
        MsgWalletStartupError _ -> Alert
        MsgServerStartupError _ -> Alert

-- | Failure status codes for network backend errors.
exitCodeNetwork :: ErrStartup -> Int
exitCodeNetwork = \case
    ErrStartupGetBlockchainParameters e -> case e of
        ErrGetBlockchainParamsNetworkUnreachable _ -> 30
        ErrGetBlockchainParamsGenesisNotFound _ -> 31
        ErrGetBlockchainParamsIncompleteParams _ -> 32
    ErrStartupMissingIncentiveParameters -> 32
    ErrStartupInvalidGenesisBlock _ -> 33
    ErrStartupInvalidGenesisHash _ -> 34
    ErrStartupCommandExited pe -> case pe of
        ProcessDidNotStart _ _ -> 40
        ProcessHasExited _ _ -> 41
    ErrStartupNodeNotListening -> 42

-- | Failure status codes for HTTP API server errors.
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13

{-------------------------------------------------------------------------------
                                    Tracers
-------------------------------------------------------------------------------}

-- | The types of trace events produced by the Jörmungandr API server.
data Tracers' f = Tracers
    { applicationTracer     :: f ApplicationLog
    , apiServerTracer       :: f ApiLog
    , walletEngineTracer    :: f (WorkerLog WalletId WalletLog)
    , walletDbTracer        :: f DBLog
    , stakePoolEngineTracer :: f (WorkerLog Text StakePoolLog)
    , stakePoolDbTracer     :: f DBLog
    , networkTracer         :: f NetworkLayerLog
    , ntpClientTracer       :: f NtpTrace
    }

-- | All of the Jörmungandr 'Tracer's.
type Tracers m = Tracers' (Tracer m)

-- | The minimum severities for 'Tracers'. 'Nothing' indicates that tracing is
-- completely disabled.
type TracerSeverities = Tracers' (Const (Maybe Severity))

deriving instance Show TracerSeverities
deriving instance Eq TracerSeverities

-- | Construct a 'TracerSeverities' record with all tracers set to the given
-- severity.
tracerSeverities :: Maybe Severity -> TracerSeverities
tracerSeverities sev = Tracers
    { applicationTracer     = Const sev
    , apiServerTracer       = Const sev
    , walletDbTracer        = Const sev
    , walletEngineTracer    = Const sev
    , stakePoolEngineTracer = Const sev
    , stakePoolDbTracer     = Const sev
    , networkTracer         = Const sev
    , ntpClientTracer       = Const sev
    }

-- | Set up tracing with textual log messages.
setupTracers :: TracerSeverities -> Trace IO Text -> Tracers IO
setupTracers sev tr = Tracers
    { applicationTracer     = mkTrace applicationTracer     $ onoff applicationTracer tr
    , apiServerTracer       = mkTrace apiServerTracer       $ onoff apiServerTracer tr
    , walletEngineTracer    = mkTrace walletEngineTracer    $ onoff walletEngineTracer tr
    , walletDbTracer        = mkTrace walletDbTracer        $ onoff walletDbTracer tr
    , stakePoolEngineTracer = mkTrace stakePoolEngineTracer $ onoff stakePoolEngineTracer tr
    , stakePoolDbTracer     = mkTrace stakePoolDbTracer     $ onoff stakePoolDbTracer tr
    , networkTracer         = mkTrace networkTracer         $ onoff networkTracer tr
    , ntpClientTracer       = mkTrace ntpClientTracer       $ onoff ntpClientTracer tr
    }
  where
    onoff
        :: Monad m
        => (TracerSeverities -> Const (Maybe Severity) a)
        -> Trace m b
        -> Trace m b
    onoff f = case getConst (f sev) of
        Nothing -> const nullTracer
        Just s -> filterTraceSeverity s

    mkTrace
        :: (HasPrivacyAnnotation a, HasSeverityAnnotation a, ToText a)
        => (Tracers' (Const Text) -> Const Text a)
        -> Trace IO Text
        -> Tracer IO a
    mkTrace f = trMessageText . appendName (getConst $ f tracerLabels)

-- | Strings that the user can refer to tracers by.
tracerLabels :: Tracers' (Const Text)
tracerLabels = Tracers
    { applicationTracer     = Const "application"
    , apiServerTracer       = Const "api-server"
    , walletEngineTracer    = Const "wallet-engine"
    , walletDbTracer        = Const "wallet-db"
    , stakePoolEngineTracer = Const "pools-engine"
    , stakePoolDbTracer     = Const "pools-db"
    , networkTracer         = Const "network"
    , ntpClientTracer       = Const "ntp-client"
    }

-- | Names and descriptions of the tracers, for user documentation.
tracerDescriptions :: [(String, String)]
tracerDescriptions =
    [ ( lbl applicationTracer
      , "About start-up logic and the server's surroundings."
      )
    , ( lbl apiServerTracer
      , "About the HTTP API requests and responses."
      )
    , ( lbl walletEngineTracer
      , "About background wallet workers events and core wallet engine."
      )
    , ( lbl walletDbTracer
      , "About database operations of each wallet."
      )
    , ( lbl stakePoolEngineTracer
      , "About the background worker monitoring stake pools and stake pools engine."
      )
    , ( lbl stakePoolDbTracer
      , "About database operations on stake pools."
      )
    , ( lbl networkTracer
      , "About networking communications with the node."
      )
    , ( lbl ntpClientTracer
      , "About ntp-client."
      )
    ]
  where
    lbl f = T.unpack . getConst . f $ tracerLabels

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Tracers'
nullTracers :: Monad m => Tracers m
nullTracers = Tracers
    { applicationTracer     = nullTracer
    , apiServerTracer       = nullTracer
    , walletEngineTracer    = nullTracer
    , walletDbTracer        = nullTracer
    , stakePoolEngineTracer = nullTracer
    , stakePoolDbTracer     = nullTracer
    , networkTracer         = nullTracer
    , ntpClientTracer       = nullTracer
    }
