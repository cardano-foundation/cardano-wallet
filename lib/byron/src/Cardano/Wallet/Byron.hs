{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
    ( Byron, ByronBlock, KnownNetwork (..), fromByronBlock, fromNetworkMagic )
import Cardano.Wallet.Byron.Network
    ( AddrInfo (..), withNetworkLayer )
import Cardano.Wallet.Byron.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Byron.Transaction.Size
    ( MaxSizeOf )
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
    , PaymentAddress
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
    , Block
    , BlockchainParameters (..)
    , ChimericAccount
    , ProtocolMagic
    , SyncTolerance
    , WalletId
    )
import Cardano.Wallet.Registry
    ( WorkerLog (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Codec.SerialiseTerm
    ( CodecCBORTerm )
import Control.Applicative
    ( Const (..) )
import Control.DeepSeq
    ( NFData )
import Control.Tracer
    ( Tracer (..), nullTracer, traceWith )
import Data.Function
    ( (&) )
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
import Network.Ntp
    ( NtpClient (..), NtpTrace (..), ntpSettings, withNtpClient )
import Network.Socket
    ( SockAddr, Socket, getSocketName )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Exit
    ( ExitCode (..) )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
    SomeNetworkDiscriminant
        :: forall (n :: NetworkDiscriminant).
            ( KnownNetwork n
            , NetworkDiscriminantVal n
            , PaymentAddress n IcarusKey
            , DecodeAddress n
            , EncodeAddress n
            , MaxSizeOf Address n IcarusKey
            , MaxSizeOf Address n ByronKey
            )
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

-- | The @cardano-wallet-shelley@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall t.
        ( t ~ IO Byron
        )
    => SomeNetworkDiscriminant
    -- ^ Proxy for the network discriminant
    -> Tracers IO
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
    -> Block
    -- ^ The genesis block, or some starting point.
    -> ( BlockchainParameters
       , ( NodeToClientVersionData
         , CodecCBORTerm Text NodeToClientVersionData
         )
       )
    -- ^ Network parameters needed to connect to the underlying network.
    --
    -- See also: 'Cardano.Wallet.Byron.Compatibility#KnownNetwork'.
    -> (SockAddr -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet
  (SomeNetworkDiscriminant proxy)
  Tracers{..}
  sTolerance
  databaseDir
  hostPref
  listen
  addrInfo
  block0
  (bp, vData)
  beforeMainLoop = do
    let ntwrk = networkDiscriminantValFromProxy proxy
    let magic = fromNetworkMagic $ networkMagic $ fst vData
    traceWith applicationTracer $ MsgStarting addrInfo
    traceWith applicationTracer $ MsgNetworkName ntwrk magic
    Server.withListeningSocket hostPref listen $ \case
        Left e -> handleApiServerStartupError e
        Right (_, socket) -> serveApp socket
  where
    serveApp socket = do
        withNetworkLayer nullTracer bp addrInfo vData $ \nl -> do
            withNtpClient ntpClientTracer ntpSettings $ \ntpClient -> do
                let pm = fromNetworkMagic $ networkMagic $ fst vData
                randomApi  <- apiLayer (newTransactionLayer proxy pm) nl
                icarusApi  <- apiLayer (newTransactionLayer proxy pm) nl
                startServer proxy socket randomApi icarusApi ntpClient
                pure ExitSuccess

    networkDiscriminantValFromProxy
        :: forall n. (NetworkDiscriminantVal n)
        => Proxy n
        -> NetworkDiscriminant
    networkDiscriminantValFromProxy _ =
        networkDiscriminantVal @n

    startServer
        :: forall n.
            ( PaymentAddress n IcarusKey
            , DecodeAddress n
            , EncodeAddress n
            )
        => Proxy n
        -> Socket
        -> ApiLayer (RndState 'Mainnet) t ByronKey
        -> ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        -> NtpClient
        -> IO ()
    startServer _proxy socket random icarus ntp = do
        sockAddr <- getSocketName socket
        let settings = Warp.defaultSettings & setBeforeMainLoop
                (beforeMainLoop sockAddr)
        let application = Server.serve (Proxy @(ApiV2 n)) $
                Server.byronServer random icarus ntp
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
        let tracer = contramap MsgDatabaseStartup applicationTracer
        let params = (block0, bp, sTolerance)
        wallets <- maybe (pure []) (Sqlite.findDatabases @k tracer) databaseDir
        db <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues $ getActiveSlotCoefficient bp)
            databaseDir
        Server.newApiLayer
            walletEngineTracer params nl' tl db wallets
      where
        BlockchainParameters
            { getGenesisBlockHash
            , getEpochLength
            } = bp
        nl' = fromByronBlock getGenesisBlockHash getEpochLength <$> nl

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
    | MsgNetworkName NetworkDiscriminant ProtocolMagic
    | MsgServerStartupError ListenError
    | MsgDatabaseStartup DatabasesStartupLog
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText = \case
        MsgStarting info ->
            let addr = T.pack $ show $ addrAddress info
            in "Wallet backend server starting. Using " <> addr <> "."
        MsgNetworkName n magic ->
            "Node is Haskell Node on " <> toText n <> " (" <> toText magic <> ")."
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
        MsgNetworkName _ _ -> Info
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
    , ntpClientTracer    :: f NtpTrace
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
    , ntpClientTracer    = Const sev
    }

-- | Set up tracing with textual log messages.
setupTracers :: TracerSeverities -> Trace IO Text -> Tracers IO
setupTracers sev tr = Tracers
    { applicationTracer  = mkTrace applicationTracer  $ onoff applicationTracer tr
    , apiServerTracer    = mkTrace apiServerTracer    $ onoff apiServerTracer tr
    , walletEngineTracer = mkTrace walletEngineTracer $ onoff walletEngineTracer tr
    , walletDbTracer     = mkTrace walletDbTracer     $ onoff walletDbTracer tr
    , ntpClientTracer    = mkTrace ntpClientTracer       $ onoff ntpClientTracer tr
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
    , ntpClientTracer    = Const "ntp-client"
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
    , ( lbl ntpClientTracer
      , "About ntp-client."
      )
    ]
  where
    lbl f = T.unpack . getConst . f $ tracerLabels
