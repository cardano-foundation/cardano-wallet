{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main wallet server function for Shelley.
--
-- The "Cardano.Wallet.Shelley.Network" uses the mini-protocols (ChainSync and
-- TxSubmission) to talk with a core node and synchronize with the network.
--
-- Functionality specific to this backend for creating transactions is in
-- "Cardano.Wallet.Shelley.Transaction"

module Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , serveWallet

      -- * Tracing
    , Tracers' (..)
    , Tracers
    , TracerSeverities
    , tracerLabels
    , tracerDescriptions
    , setupTracers
    , tracerSeverities
    , nullTracers

      -- * Logs
    , ApplicationLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..), filterSeverity )
import Cardano.BM.Trace
    ( Trace, appendName, nullTracer )
import Cardano.DB.Sqlite
    ( DBLog )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Pool.DB.Log
    ( PoolDbLog )
import Cardano.Wallet
    ( WalletLog (..) )
import Cardano.Wallet.Api
    ( ApiLayer, ApiV2 )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..), TlsConfiguration )
import Cardano.Wallet.Api.Types
    ( ApiStakePool
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , EncodeStakeAddress
    )
import Cardano.Wallet.DB.Sqlite
    ( DefaultFieldValues (..), PersistState )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , PaymentAddress
    , PersistPrivateKey
    , WalletKey
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
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
    , ChimericAccount
    , GenesisParameters (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , Settings (..)
    , WalletId
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), WorkerLog (..), defaultWorkerAfter )
import Cardano.Wallet.Shelley.Api.Server
    ( server )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock
    , HasNetworkId (..)
    , Shelley
    , StandardCrypto
    , fromCardanoBlock
    )
import Cardano.Wallet.Shelley.Network
    ( NetworkLayerLog, withNetworkLayer )
import Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..)
    , StakePoolLog
    , monitorMetadata
    , monitorStakePools
    , newStakePoolLayer
    )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Applicative
    ( Const (..) )
import Control.Concurrent
    ( forkFinally )
import Control.Monad
    ( forM_, void )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )
import Network.Ntp
    ( NtpClient (..), NtpTrace, withWalletNtpClient )
import Network.Socket
    ( SockAddr, Socket, getSocketName )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog (..) )
import Ouroboros.Network.CodecCBORTerm
    ( CodecCBORTerm )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Exit
    ( ExitCode (..) )
import System.IOManager
    ( withIOManager )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
    SomeNetworkDiscriminant
        :: forall (n :: NetworkDiscriminant).
            ( NetworkDiscriminantVal n
            , PaymentAddress n IcarusKey
            , PaymentAddress n ByronKey
            , PaymentAddress n ShelleyKey
            , DelegationAddress n ShelleyKey
            , HasNetworkId n
            , DecodeAddress n
            , EncodeAddress n
            , DecodeStakeAddress n
            , EncodeStakeAddress n
            , Typeable n
            )
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

-- | The @cardano-wallet@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall t. t ~ IO Shelley
    => SomeNetworkDiscriminant
    -- ^ Proxy for the network discriminant
    -> Tracers IO
    -- ^ Logging config.
    -> SyncTolerance
    -- ^ A time tolerance within we consider being synced
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> Maybe (Pool.DBDecorator IO)
    -- ^ An optional decorator that can be used to monitor pool DB operations.
    -> HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ HTTP API Server port.
    -> Maybe TlsConfiguration
    -- ^ An optional TLS configuration
    -> Maybe Settings
    -- ^ Settings to be set at application start, will be written into DB.
    -> FilePath
    -- ^ Socket for communicating with the node
    -> Block
    -- ^ The genesis block, or some starting point.
    -> ( NetworkParameters
       , ( NodeToClientVersionData
         , CodecCBORTerm Text NodeToClientVersionData
         )
       )
    -- ^ Network parameters needed to connect to the underlying network.
    --
    -- See also: 'Cardano.Wallet.Shelley.Compatibility#KnownNetwork'.
    -> (SockAddr -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet
  (SomeNetworkDiscriminant proxy)
  Tracers{..}
  sTolerance
  databaseDir
  mPoolDatabaseDecorator
  hostPref
  listen
  tlsConfig
  settings
  socketPath
  block0
  (np, vData)
  beforeMainLoop = do
    let ntwrk = networkDiscriminantValFromProxy proxy
    traceWith applicationTracer $ MsgStarting socketPath
    traceWith applicationTracer $ MsgNetworkName ntwrk
    Server.withListeningSocket hostPref listen $ \case
        Left e -> handleApiServerStartupError e
        Right (_, socket) -> serveApp socket
  where
    poolDatabaseDecorator = fromMaybe Pool.undecoratedDB mPoolDatabaseDecorator

    serveApp socket = withIOManager $ \io -> do
        withNetworkLayer networkTracer np socketPath vData $ \nl -> do
            withWalletNtpClient io ntpClientTracer $ \ntpClient -> do
                let gp = genesisParameters np
                let net = networkIdVal proxy
                randomApi <- apiLayer (newTransactionLayer net) nl
                    Server.idleWorker
                icarusApi  <- apiLayer (newTransactionLayer net) nl
                    Server.idleWorker
                shelleyApi <- apiLayer (newTransactionLayer net) nl
                    (Server.manageRewardBalance proxy)

                withPoolsMonitoring databaseDir gp nl $ \spl -> do
                    startServer
                        proxy
                        socket
                        randomApi
                        icarusApi
                        shelleyApi
                        spl
                        ntpClient
                    pure ExitSuccess

    networkDiscriminantValFromProxy
        :: forall n. (NetworkDiscriminantVal n)
        => Proxy n
        -> Text
    networkDiscriminantValFromProxy _ =
        networkDiscriminantVal @n

    startServer
        :: forall n.
            ( PaymentAddress n IcarusKey
            , PaymentAddress n ByronKey
            , DelegationAddress n ShelleyKey
            , DecodeAddress n
            , EncodeAddress n
            , EncodeStakeAddress n
            , Typeable n
            , HasNetworkId n
            )
        => Proxy n
        -> Socket
        -> ApiLayer (RndState n) t ByronKey
        -> ApiLayer (SeqState n IcarusKey) t IcarusKey
        -> ApiLayer (SeqState n ShelleyKey) t ShelleyKey
        -> StakePoolLayer
        -> NtpClient
        -> IO ()
    startServer _proxy socket byron icarus shelley spl ntp = do
        sockAddr <- getSocketName socket
        let serverSettings = Warp.defaultSettings & setBeforeMainLoop
                (beforeMainLoop sockAddr)
        let application = Server.serve (Proxy @(ApiV2 n ApiStakePool)) $
                server byron icarus shelley spl ntp
        Server.start serverSettings apiServerTracer tlsConfig socket application

    withPoolsMonitoring
        :: Maybe FilePath
        -> GenesisParameters
        -> NetworkLayer IO t (CardanoBlock StandardCrypto)
        -> (StakePoolLayer -> IO a)
        -> IO a
    withPoolsMonitoring dir gp nl action =
        Pool.withDecoratedDBLayer
                poolDatabaseDecorator
                poolsDbTracer
                (Pool.defaultFilePath <$> dir)
                (timeInterpreter nl)
                $ \db@DBLayer{..} -> do

            forM_ settings $ atomically . putSettings
            void $ forkFinally (monitorStakePools tr gp nl db) onExit
            spl <- newStakePoolLayer nl db $ forkFinally (monitorMetadata tr gp db) onExit
            action spl
      where
        tr = contramap (MsgFromWorker mempty) poolsEngineTracer
        onExit = defaultWorkerAfter poolsEngineTracer

    apiLayer
        :: forall s k.
            ( IsOurs s Address
            , IsOurs s ChimericAccount
            , PersistState s
            , PersistPrivateKey (k 'RootK)
            , WalletKey k
            )
        => TransactionLayer t k
        -> NetworkLayer IO t (CardanoBlock StandardCrypto)
        -> (WorkerCtx (ApiLayer s t k) -> WalletId -> IO ())
        -> IO (ApiLayer s t k)
    apiLayer tl nl coworker = do
        let params = (block0, np, sTolerance)
        db <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues
                { defaultActiveSlotCoefficient =
                    getActiveSlotCoefficient gp
                , defaultDesiredNumberOfPool =
                    desiredNumberOfStakePools (protocolParameters np)
                , defaultMinimumUTxOValue =
                    minimumUTxOvalue (protocolParameters np)
                , defaultHardforkEpoch =
                    hardforkEpochNo (protocolParameters np)
                }
            )
            (timeInterpreter nl)
            databaseDir
        Server.newApiLayer walletEngineTracer params nl' tl db coworker
      where
        gp = genesisParameters np
        nl' = fromCardanoBlock gp <$> nl

    -- FIXME: reduce duplication (see Cardano.Wallet.Jormungandr)
    handleApiServerStartupError :: ListenError -> IO ExitCode
    handleApiServerStartupError err = do
        traceWith applicationTracer $ MsgServerStartupError err
        pure $ ExitFailure $ exitCodeApiServer err

-- | Failure status codes for HTTP API server errors.
-- FIXME: reduce duplication (see Cardano.Wallet.Jormungandr)
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages related to application startup and shutdown.
data ApplicationLog
    = MsgStarting FilePath
    | MsgNetworkName Text
    | MsgServerStartupError ListenError
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText = \case
        MsgStarting socket ->
            let addr = T.pack $ show socket
            in "Wallet backend server starting. Using " <> addr <> "."
        MsgNetworkName network ->
            "Node is Haskell Node on " <> network <> "."
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
    getSeverityAnnotation = \case
        MsgStarting _ -> Info
        MsgNetworkName _ -> Info
        MsgServerStartupError _ -> Alert

{-------------------------------------------------------------------------------
                                    Tracers
-------------------------------------------------------------------------------}

-- FIXME: reduce duplication (see Cardano.Wallet.Jormungandr) vvv

-- | The types of trace events produced by the Shelley API server.
data Tracers' f = Tracers
    { applicationTracer  :: f ApplicationLog
    , apiServerTracer    :: f ApiLog
    , walletEngineTracer :: f (WorkerLog WalletId WalletLog)
    , walletDbTracer     :: f DBLog
    , poolsEngineTracer  :: f (WorkerLog Text StakePoolLog)
    , poolsDbTracer      :: f PoolDbLog
    , ntpClientTracer    :: f NtpTrace
    , networkTracer      :: f NetworkLayerLog
    }

-- | All of the Shelley 'Tracer's.
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
    { applicationTracer  = Const sev
    , apiServerTracer    = Const sev
    , walletDbTracer     = Const sev
    , walletEngineTracer = Const sev
    , poolsEngineTracer  = Const sev
    , poolsDbTracer      = Const sev
    , ntpClientTracer    = Const sev
    , networkTracer      = Const sev
    }

-- | Set up tracing with textual log messages.
setupTracers
    :: TracerSeverities
    -> Trace IO Text
    -> Tracers IO
setupTracers sev tr = Tracers
    { applicationTracer  = onoff applicationTracer  $ mkTrace applicationTracer  tr
    , apiServerTracer    = onoff apiServerTracer    $ mkTrace apiServerTracer    tr
    , walletEngineTracer = onoff walletEngineTracer $ mkTrace walletEngineTracer tr
    , walletDbTracer     = onoff walletDbTracer     $ mkTrace walletDbTracer     tr
    , poolsEngineTracer  = onoff poolsEngineTracer  $ mkTrace poolsEngineTracer  tr
    , poolsDbTracer      = onoff poolsDbTracer      $ mkTrace poolsDbTracer      tr
    , ntpClientTracer    = onoff ntpClientTracer    $ mkTrace ntpClientTracer    tr
    , networkTracer      = onoff networkTracer      $ mkTrace networkTracer      tr
    }
  where
    onoff
        :: forall m a b. (Monad m, HasSeverityAnnotation b)
        => (TracerSeverities -> Const (Maybe Severity) a)
        -> Tracer m b
        -> Tracer m b
    onoff f = case getConst (f sev) of
        Nothing -> const nullTracer
        Just s -> filterSeverity (const $ pure s)

    mkTrace
        :: (HasPrivacyAnnotation a, HasSeverityAnnotation a, ToText a)
        => (Tracers' (Const Text) -> Const Text a)
        -> Trace IO Text
        -> Tracer IO a
    mkTrace f =
        trMessageText . appendName (getConst $ f tracerLabels)

-- | Strings that the user can refer to tracers by.
tracerLabels :: Tracers' (Const Text)
tracerLabels = Tracers
    { applicationTracer  = Const "application"
    , apiServerTracer    = Const "api-server"
    , walletEngineTracer = Const "wallet-engine"
    , walletDbTracer     = Const "wallet-db"
    , poolsEngineTracer  = Const "pools-engine"
    , poolsDbTracer      = Const "pools-db"
    , ntpClientTracer    = Const "ntp-client"
    , networkTracer      = Const "network"
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
    , ( lbl poolsEngineTracer
      , "About the background worker monitoring stake pools and stake pools engine."
      )
    , ( lbl poolsDbTracer
      , "About database operations on stake pools."
      )
    , ( lbl ntpClientTracer
      , "About ntp-client."
      )
    , ( lbl networkTracer
      , "About network communication with the node."
      )
    ]
  where
    lbl f = T.unpack . getConst . f $ tracerLabels

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Tracers'
nullTracers :: Monad m => Tracers m
nullTracers = Tracers
    { applicationTracer  = nullTracer
    , apiServerTracer    = nullTracer
    , walletEngineTracer = nullTracer
    , walletDbTracer     = nullTracer
    , poolsEngineTracer  = nullTracer
    , poolsDbTracer      = nullTracer
    , ntpClientTracer    = nullTracer
    , networkTracer      = nullTracer
    }
