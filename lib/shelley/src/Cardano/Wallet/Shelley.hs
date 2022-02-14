{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Cardano.Launcher.Node
    ( CardanoNodeConn )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Pool.DB.Log
    ( PoolDbLog )
import Cardano.Wallet.Api
    ( ApiLayer, ApiV2 )
import Cardano.Wallet.Api.Server
    ( HostPreference
    , Listen (..)
    , ListenError (..)
    , TlsConfiguration
    , WalletEngineLog
    )
import Cardano.Wallet.Api.Types
    ( ApiStakePool
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , EncodeStakeAddress
    )
import Cardano.Wallet.DB.Sqlite
    ( DBFactoryLog, DefaultFieldValues (..), PersistAddressBook )
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
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs, MaybeLight )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedState )
import Cardano.Wallet.Primitive.Slotting
    ( neverFails )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Block
    , NetworkParameters (..)
    , PoolMetadataGCStatus (..)
    , ProtocolParameters (..)
    , Settings (..)
    , SlottingParameters (..)
    , TokenMetadataServer (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), traceAfterThread )
import Cardano.Wallet.Shelley.Api.Server
    ( server )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, HasNetworkId (..), StandardCrypto, fromCardanoBlock )
import Cardano.Wallet.Shelley.Network
    ( NetworkLayerLog, withNetworkLayer )
import Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..)
    , StakePoolLog (..)
    , monitorMetadata
    , monitorStakePools
    , newStakePoolLayer
    )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataLog, newMetadataClient )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Applicative
    ( Const (..) )
import Control.Monad
    ( forM_, void )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust, fromMaybe )
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
    ( Socket, getSocketName )
import Network.URI
    ( URI (..), parseURI, uriToString )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Exit
    ( ExitCode (..) )
import System.IOManager
    ( withIOManager )
import Type.Reflection
    ( Typeable )
import UnliftIO.Concurrent
    ( forkFinally, forkIOWithUnmask, killThread )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )
import UnliftIO.STM
    ( newTVarIO )

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
    :: SomeNetworkDiscriminant
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
    -> Maybe TokenMetadataServer
    -> CardanoNodeConn
    -- ^ Socket for communicating with the node
    -> Block
    -- ^ The genesis block, or some starting point.
    -> ( NetworkParameters, NodeToClientVersionData)
    -- ^ Network parameters needed to connect to the underlying network.
    --
    -- See also: 'Cardano.Wallet.Shelley.Compatibility#KnownNetwork'.
    -> (URI -> IO ())
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
  tokenMetaUri
  conn
  block0
  (np, vData)
  beforeMainLoop = do
    let ntwrk = networkDiscriminantValFromProxy proxy
    traceWith applicationTracer $ MsgStarting conn
    traceWith applicationTracer $ MsgNetworkName ntwrk
    Server.withListeningSocket hostPref listen $ \case
        Left e -> handleApiServerStartupError e
        Right (_, socket) -> serveApp socket
  where
    poolDatabaseDecorator = fromMaybe Pool.undecoratedDB mPoolDatabaseDecorator

    serveApp socket = withIOManager $ \io -> do
        let net = networkIdVal proxy
        withNetworkLayer networkTracer net np conn vData sTolerance $ \nl -> do
            withWalletNtpClient io ntpClientTracer $ \ntpClient -> do
                randomApi <- apiLayer (newTransactionLayer net) nl
                    Server.idleWorker
                icarusApi  <- apiLayer (newTransactionLayer net) nl
                    Server.idleWorker
                shelleyApi <- apiLayer (newTransactionLayer net) nl
                    (Server.manageRewardBalance proxy)

                let txLayerUdefined = error "TO-DO in ADP-686"
                multisigApi <- apiLayer txLayerUdefined nl
                    Server.idleWorker

                withPoolsMonitoring databaseDir np nl $ \spl -> do
                    startServer
                        proxy
                        socket
                        randomApi
                        icarusApi
                        shelleyApi
                        multisigApi
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
            , DecodeStakeAddress n
            , Typeable n
            , HasNetworkId n
            )
        => Proxy n
        -> Socket
        -> ApiLayer (RndState n) ByronKey
        -> ApiLayer (SeqState n IcarusKey) IcarusKey
        -> ApiLayer (SeqState n ShelleyKey) ShelleyKey
        -> ApiLayer (SharedState n SharedKey) SharedKey
        -> StakePoolLayer
        -> NtpClient
        -> IO ()
    startServer _proxy socket byron icarus shelley multisig spl ntp = do
        serverUrl <- getServerUrl tlsConfig socket
        let serverSettings = Warp.defaultSettings & setBeforeMainLoop
                (beforeMainLoop serverUrl)
        let application = Server.serve (Proxy @(ApiV2 n ApiStakePool)) $
                server byron icarus shelley multisig spl ntp
        Server.start serverSettings apiServerTracer tlsConfig socket application

    withPoolsMonitoring
        :: Maybe FilePath
        -> NetworkParameters
        -> NetworkLayer IO (CardanoBlock StandardCrypto)
        -> (StakePoolLayer -> IO a)
        -> IO a
    withPoolsMonitoring dir (NetworkParameters _ sp _) nl action =
        Pool.withDecoratedDBLayer
                poolDatabaseDecorator
                poolsDbTracer
                (Pool.defaultFilePath <$> dir)
                (neverFails "withPoolsMonitoring never forecasts into the future"
                    $ timeInterpreter nl)
                $ \db@DBLayer{..} -> do

            gcStatus <- newTVarIO NotStarted
            forM_ settings $ atomically . putSettings

            let tr = poolsEngineTracer

            void $ forkFinally
                (monitorStakePools tr np nl db)
                (traceAfterThread (contramap MsgExitMonitoring tr))

            -- fixme: needs to be simplified as part of ADP-634
            let startMetadataThread = forkIOWithUnmask $ \unmask ->
                    unmask $ monitorMetadata gcStatus tr sp db
            metadataThread <- newMVar =<< startMetadataThread
            let restartMetadataThread = modifyMVar_ metadataThread $ \tid -> do
                    killThread tid
                    startMetadataThread

            spl <- newStakePoolLayer gcStatus nl db restartMetadataThread
            action spl

    apiLayer
        :: forall s k.
            ( IsOurs s Address
            , IsOurs s RewardAccount
            , MaybeLight s
            , PersistAddressBook s
            , PersistPrivateKey (k 'RootK)
            , WalletKey k
            )
        => TransactionLayer k SealedTx
        -> NetworkLayer IO (CardanoBlock StandardCrypto)
        -> (WorkerCtx (ApiLayer s k) -> WalletId -> IO ())
        -> IO (ApiLayer s k)
    apiLayer tl nl coworker = do
        let params = (block0, np, sTolerance)
        tokenMetaClient <- newMetadataClient tokenMetadataTracer tokenMetaUri
        db <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues
                { defaultActiveSlotCoefficient =
                    getActiveSlotCoefficient sp
                , defaultDesiredNumberOfPool =
                    desiredNumberOfStakePools pp
                , defaultMinimumUTxOValue = Coin 0
                    -- Unused; value does not matter anymore.
                , defaultHardforkEpoch = Nothing
                -- NOTE: see ADP-643
                --
                -- In ADP-470, we've made it possible to distinguish fees from
                -- deposits in the API. This however required a database
                -- migration for which the stake key deposit in vigor is needed.
                -- This value normally comes from the Shelley genesis file, but
                -- we have no direct access to it, nor can we reliably query the
                -- network layer to get the current parameters. Indeed, the
                -- `currentProtocolParameters` and `currentSlottingParameters`
                -- functions both rely on the LSQ protocol, which would:
                --
                --  a) Fail if the wallet and the node are drifting too much
                --  b) Return potentially outdated information if the node is not synced.
                --
                -- Since the migration is only strictly needed for pre-existing
                -- mainnet and testnet wallet, we currently hard-code the stake
                -- key deposit value that _should_ be used for the migration
                -- (which fortunately happens to be the same on both networks).
                --
                -- It'll do, but it ain't pretty. Without requiring the Shelley
                -- genesis to be provided as argument I currently have no better
                -- and safer idea than hard-coding it. And also have very little
                -- time to do anything fancier.
                , defaultKeyDeposit =
                    Coin 2_000_000
                }
            )
            (neverFails "db layer should never forecast into the future"
                $ timeInterpreter nl)
            databaseDir
        Server.newApiLayer
            walletEngineTracer params nl' tl db tokenMetaClient coworker
      where
        NetworkParameters gp sp pp = np
        nl' = fromCardanoBlock gp <$> nl

    handleApiServerStartupError :: ListenError -> IO ExitCode
    handleApiServerStartupError err = do
        traceWith applicationTracer $ MsgServerStartupError err
        pure $ ExitFailure $ exitCodeApiServer err

-- | Failure status codes for HTTP API server errors.
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13

getServerUrl :: Maybe TlsConfiguration -> Socket -> IO URI
getServerUrl tlsConfig = fmap (fromJust . parseURI . uri) . getSocketName
  where
    uri addr = scheme <> "://" <> show addr <> "/"
    scheme = case tlsConfig of
                Just _ -> "https"
                Nothing -> "http"

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages related to application startup and shutdown.
data ApplicationLog
    = MsgStarting CardanoNodeConn
    | MsgNetworkName Text
    | MsgServerStartupError ListenError
    | MsgFailedConnectSMASH URI
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText = \case
        MsgStarting conn ->
            "Wallet backend server starting. Using " <> toText conn <> "."
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
        MsgFailedConnectSMASH uri -> T.unwords
            [ "Failed connect to the given smash server or validate a healthy status."
            , "SMASH uri was: "
            , T.pack $ uriToString id uri ""
            ]

instance HasPrivacyAnnotation ApplicationLog
instance HasSeverityAnnotation ApplicationLog where
    getSeverityAnnotation = \case
        MsgStarting _ -> Info
        MsgNetworkName _ -> Info
        MsgServerStartupError _ -> Alert
        MsgFailedConnectSMASH _ -> Warning

{-------------------------------------------------------------------------------
                                    Tracers
-------------------------------------------------------------------------------}

-- | The types of trace events produced by the Shelley API server.
data Tracers' f = Tracers
    { applicationTracer   :: f ApplicationLog
    , apiServerTracer     :: f ApiLog
    , tokenMetadataTracer :: f TokenMetadataLog
    , walletEngineTracer  :: f WalletEngineLog
    , walletDbTracer      :: f DBFactoryLog
    , poolsEngineTracer   :: f StakePoolLog
    , poolsDbTracer       :: f PoolDbLog
    , ntpClientTracer     :: f NtpTrace
    , networkTracer       :: f NetworkLayerLog
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
    { applicationTracer   = Const sev
    , apiServerTracer     = Const sev
    , tokenMetadataTracer = Const sev
    , walletDbTracer      = Const sev
    , walletEngineTracer  = Const sev
    , poolsEngineTracer   = Const sev
    , poolsDbTracer       = Const sev
    , ntpClientTracer     = Const sev
    , networkTracer       = Const sev
    }

-- | Set up tracing with textual log messages.
setupTracers
    :: TracerSeverities
    -> Trace IO Text
    -> Tracers IO
setupTracers sev tr = Tracers
    { applicationTracer   = onoff applicationTracer   $ mkTrace applicationTracer   tr
    , apiServerTracer     = onoff apiServerTracer     $ mkTrace apiServerTracer     tr
    , tokenMetadataTracer = onoff tokenMetadataTracer $ mkTrace tokenMetadataTracer tr
    , walletEngineTracer  = onoff walletEngineTracer  $ mkTrace walletEngineTracer  tr
    , walletDbTracer      = onoff walletDbTracer      $ mkTrace walletDbTracer      tr
    , poolsEngineTracer   = onoff poolsEngineTracer   $ mkTrace poolsEngineTracer   tr
    , poolsDbTracer       = onoff poolsDbTracer       $ mkTrace poolsDbTracer       tr
    , ntpClientTracer     = onoff ntpClientTracer     $ mkTrace ntpClientTracer     tr
    , networkTracer       = onoff networkTracer       $ mkTrace networkTracer       tr
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
    { applicationTracer   = Const "application"
    , apiServerTracer     = Const "api-server"
    , tokenMetadataTracer = Const "token-metadata"
    , walletEngineTracer  = Const "wallet-engine"
    , walletDbTracer      = Const "wallet-db"
    , poolsEngineTracer   = Const "pools-engine"
    , poolsDbTracer       = Const "pools-db"
    , ntpClientTracer     = Const "ntp-client"
    , networkTracer       = Const "network"
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
    , ( lbl tokenMetadataTracer
      , "About the fetching of token metadata."
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
    { applicationTracer   = nullTracer
    , apiServerTracer     = nullTracer
    , tokenMetadataTracer = nullTracer
    , walletEngineTracer  = nullTracer
    , walletDbTracer      = nullTracer
    , poolsEngineTracer   = nullTracer
    , poolsDbTracer       = nullTracer
    , ntpClientTracer     = nullTracer
    , networkTracer       = nullTracer
    }
