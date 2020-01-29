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
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main wallet server function for the Byron-rewrite
-- Haskell node backend.
--
-- The "Cardano.Wallet.Byron.Network" uses the mini-protocols (ChainSync and
-- TxSubmission) to talk with a core node and synchronize with the network.
--
-- Functionality specific to this backend for creating transactions is in
-- "Cardano.Wallet.Byron.Transaction"

module Cardano.Wallet.Byron
    ( serveWallet

      -- * Tracing
    , Tracers' (..)
    , Tracers
    , TracerSeverities
    , tracerLabels
    , tracerDescriptions
    , setupTracers
    , tracerSeverities

      -- * Logs
    , ApplicationLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace, appendName )
import Cardano.DB.Sqlite
    ( DBLog )
import Cardano.Wallet
    ( WalletLog )
import Cardano.Wallet.Api
    ( ApiLayer, ApiV2 )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress, EncodeAddress )
import Cardano.Wallet.Byron.Compatibility
    ( Byron, ByronBlock, KnownNetwork (..), fromByronBlock )
import Cardano.Wallet.Byron.Network
    ( AddrInfo, newNetworkLayer )
import Cardano.Wallet.Byron.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Byron.Transaction.Size
    ( WorstSizeOf )
import Cardano.Wallet.DB.Sqlite
    ( DatabasesStartupLog, DefaultFieldValues (..), PersistState )
import Cardano.Wallet.Logging
    ( filterTraceSeverity, trMessageText )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
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
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Address
    , BlockchainParameters (..)
    , ChimericAccount
    , SyncTolerance
    , WalletId
    )
import Cardano.Wallet.Registry
    ( WorkerLog (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Applicative
    ( Const (..) )
import Control.DeepSeq
    ( NFData )
import Control.Tracer
    ( Tracer (..), nullTracer, traceWith )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Functor.Contravariant
    ( contramap )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )
import Network.Socket
    ( SockAddr, Socket, getSocketName )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog )
import System.Exit
    ( ExitCode (..) )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-shelley@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall (n :: NetworkDiscriminant) t.
        ( NetworkDiscriminantVal n
        , KnownNetwork n
        , DecodeAddress n
        , EncodeAddress n
        , WorstSizeOf Address n IcarusKey
        , WorstSizeOf Address n ByronKey
        , t ~ IO Byron
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
    -> AddrInfo
    -- ^ Socket for communicating with the node
    -> (SockAddr -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet Tracers{..} sTolerance databaseDir hostPref listen addrInfo beforeMainLoop = do
    traceWith applicationTracer $ MsgStarting addrInfo
    traceWith applicationTracer $ MsgNetworkName $ networkDiscriminantVal @n
    Server.withListeningSocket hostPref listen $ \case
        Left e -> handleApiServerStartupError e
        Right (_, socket) -> serveApp socket
  where
    bp = blockchainParameters @n

    serveApp socket = do
        nl <- newNetworkLayer nullTracer bp addrInfo (versionData @n)
        byronApi   <- apiLayer (newTransactionLayer @n) nl
        icarusApi  <- apiLayer (newTransactionLayer @n) nl
        startServer socket byronApi icarusApi $> ExitSuccess

    startServer
        :: Socket
        -> ApiLayer (RndState 'Mainnet) t ByronKey
        -> ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        -> IO ()
    startServer socket byron icarus = do
        sockAddr <- getSocketName socket
        let settings = Warp.defaultSettings & setBeforeMainLoop
                (beforeMainLoop sockAddr)
        let application = Server.serve (Proxy @(ApiV2 n)) $
                Server.byronServer byron icarus
        Server.start settings apiServerTracer socket application

    apiLayer
        :: forall s k.
            ( IsOurs s Address
            , IsOurs s ChimericAccount
            , NFData s
            , Show s
            , PersistState s
            , PersistPrivateKey (k 'RootK)
            , WalletKey k
            )
        => TransactionLayer t k
        -> NetworkLayer IO t ByronBlock
        -> IO (ApiLayer s t k)
    apiLayer tl nl = do
        let (block0, _) = staticBlockchainParameters nl
        let tracer = contramap MsgDatabaseStartup applicationTracer
        let params = (fromByronBlock genesisHash block0, bp, sTolerance)
        wallets <- maybe (pure []) (Sqlite.findDatabases @k tracer) databaseDir
        db <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues $ getActiveSlotCoefficient bp)
            databaseDir
        Server.newApiLayer
            walletEngineTracer params nl' tl db wallets
      where
        genesisHash = getGenesisBlockHash bp
        nl' = fromByronBlock genesisHash <$> nl

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
    = MsgStarting AddrInfo
    | MsgNetworkName NetworkDiscriminant
    | MsgServerStartupError ListenError
    | MsgDatabaseStartup DatabasesStartupLog
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText = \case
        MsgStarting info ->
            "Wallet backend server starting. " <> T.pack (show info) <> "..."
        MsgNetworkName n ->
            "Node is Haskell Node on " <> toText n
        MsgDatabaseStartup dbMsg ->
            toText dbMsg
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

instance DefinePrivacyAnnotation ApplicationLog
instance DefineSeverity ApplicationLog where
    defineSeverity = \case
        MsgStarting _ -> Info
        MsgNetworkName _ -> Info
        MsgDatabaseStartup ev -> defineSeverity ev
        MsgServerStartupError _ -> Alert

{-------------------------------------------------------------------------------
                                    Tracers
-------------------------------------------------------------------------------}

-- FIXME: reduce duplication (see Cardano.Wallet.Jormungandr) vvv

-- | The types of trace events produced by the Byron API server.
data Tracers' f = Tracers
    { applicationTracer  :: f ApplicationLog
    , apiServerTracer    :: f ApiLog
    , walletEngineTracer :: f (WorkerLog WalletId WalletLog)
    , walletDbTracer     :: f DBLog
    }

-- | All of the Byron 'Tracer's.
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
    }

-- | Set up tracing with textual log messages.
setupTracers :: TracerSeverities -> Trace IO Text -> Tracers IO
setupTracers sev tr = Tracers
    { applicationTracer  = mkTrace applicationTracer  $ onoff applicationTracer tr
    , apiServerTracer    = mkTrace apiServerTracer    $ onoff apiServerTracer tr
    , walletEngineTracer = mkTrace walletEngineTracer $ onoff walletEngineTracer tr
    , walletDbTracer     = mkTrace walletDbTracer     $ onoff walletDbTracer tr
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
        :: (DefinePrivacyAnnotation a, DefineSeverity a, ToText a)
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
    ]
  where
    lbl f = T.unpack . getConst . f $ tracerLabels
