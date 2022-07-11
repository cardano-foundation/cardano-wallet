{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
    , module Logging
    , module Tracers
    ) where

import Prelude

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
import Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..) )
import Cardano.Wallet.DB.Store.Checkpoints
    ( PersistAddressBook )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
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
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
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
    , NetworkParameters
    , PoolCertificate
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
    ( HasWorkerCtx (..) )
import Cardano.Wallet.Shelley.Api.Server
    ( server )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock
    , HasNetworkId (..)
    , NetworkId
    , StandardCrypto
    , fromCardanoBlock
    )
import Cardano.Wallet.Shelley.Logging as Logging
    ( ApplicationLog (..) )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..), networkDiscriminantToId )
import Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..)
    , withBlockfrostStakePoolLayer
    , withNodeStakePoolLayer
    , withStakePoolDbLayer
    )
import Cardano.Wallet.Shelley.Tracers as Tracers
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , nullTracers
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.TokenMetadata
    ( newMetadataClient )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Cont
    ( ContT (ContT), evalContT )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Network.Ntp
    ( NtpClient (..), NtpTrace, withWalletNtpClient )
import Network.Socket
    ( Socket, getSocketName )
import Network.URI
    ( URI (..), parseURI )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Ouroboros.Network.Client.Wallet
    ( PipeliningStrategy )
import System.Exit
    ( ExitCode (..) )
import System.IOManager
    ( withIOManager )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Layer as Sqlite
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: BlockchainSource
    -- ^ Source of the blockchain data
    -> NetworkParameters
    -- ^ Records the complete set of parameters
    -- currently in use by the network that are relevant to the wallet.
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
    -- ^ pipelining value depending  on block height
    -> SomeNetworkDiscriminant
    -- ^ Proxy for the network discriminant
    -> [PoolCertificate]
    -- ^ Shelley genesis pools
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
    -> Block
    -- ^ The genesis block, or some starting point.
    -- See also: 'Cardano.Wallet.Shelley.Compatibility#KnownNetwork'.
    -> (URI -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet
  blockchainSource
  netParams@NetworkParameters
    { protocolParameters
    , genesisParameters
    , slottingParameters
    }
  pipeliningStrategy
  network@(SomeNetworkDiscriminant proxyNetwork)
  shelleyGenesisPools
  Tracers{..}
  sTolerance
  databaseDir
  mPoolDatabaseDecorator
  hostPref
  listen
  tlsConfig
  settings
  tokenMetaUri
  block0
  beforeMainLoop = evalContT $ do
    lift $ case blockchainSource of
        NodeSource nodeConn _ -> trace $ MsgStartingNode nodeConn
        BlockfrostSource project -> trace $ MsgStartingLite project
    lift . trace $ MsgNetworkName $ networkName proxyNetwork
    netLayer <- withNetworkLayer
        networkTracer
        pipeliningStrategy
        blockchainSource
        network
        netParams
        sTolerance
    stakePoolLayer <- case blockchainSource of
        NodeSource _ _ -> do
            stakePoolDbLayer <- withStakePoolDbLayer
                poolsDbTracer
                databaseDir
                mPoolDatabaseDecorator
                netLayer
            withNodeStakePoolLayer
                poolsEngineTracer
                settings
                stakePoolDbLayer
                netParams
                shelleyGenesisPools
                netLayer
        BlockfrostSource bfProject -> do
            withBlockfrostStakePoolLayer poolsEngineTracer bfProject network
    randomApi <- withRandomApi netLayer
    icarusApi  <- withIcarusApi netLayer
    shelleyApi <- withShelleyApi netLayer
    multisigApi <- withMultisigApi netLayer
    ntpClient <- withNtpClient ntpClientTracer
    bindSocket >>= lift . \case
        Left err -> do
            trace $ MsgServerStartupError err
            pure $ ExitFailure $ exitCodeApiServer err
        Right (_port, socket) -> do
            startServer
                proxyNetwork
                socket
                randomApi
                icarusApi
                shelleyApi
                multisigApi
                stakePoolLayer
                ntpClient
            pure ExitSuccess

  where
    trace :: ApplicationLog -> IO ()
    trace = traceWith applicationTracer

    netId :: NetworkId
    netId = networkDiscriminantToId network

    bindSocket :: ContT r IO (Either ListenError (Warp.Port, Socket))
    bindSocket = ContT $ Server.withListeningSocket hostPref listen

    withRandomApi netLayer =
        lift $ apiLayer (newTransactionLayer netId) netLayer Server.idleWorker

    withIcarusApi netLayer =
        lift $ apiLayer (newTransactionLayer netId) netLayer Server.idleWorker

    withShelleyApi netLayer =
        lift $ apiLayer (newTransactionLayer netId) netLayer
            (Server.manageRewardBalance proxyNetwork)

    withMultisigApi netLayer =
        let txLayerUdefined = error "TO-DO in ADP-686"
        in lift $ apiLayer txLayerUdefined netLayer Server.idleWorker

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
        let serverSettings = Warp.defaultSettings
                & setBeforeMainLoop (beforeMainLoop serverUrl)
        let application = Server.serve (Proxy @(ApiV2 n ApiStakePool)) $
                server byron icarus shelley multisig spl ntp
        Server.start serverSettings apiServerTracer tlsConfig socket application

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
    apiLayer txLayer netLayer coworker = do
        tokenMetaClient <- newMetadataClient tokenMetadataTracer tokenMetaUri
        dbFactory <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues
                { defaultActiveSlotCoefficient =
                    getActiveSlotCoefficient slottingParameters
                , defaultDesiredNumberOfPool =
                    desiredNumberOfStakePools protocolParameters
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
                --  b) Return potentially outdated information if the node is
                --     not synced.
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
                $ timeInterpreter netLayer)
            databaseDir
        Server.newApiLayer
            walletEngineTracer
            (block0, netParams, sTolerance)
            (fromCardanoBlock genesisParameters <$> netLayer)
            txLayer
            dbFactory
            tokenMetaClient
            coworker

withNtpClient :: Tracer IO NtpTrace -> ContT r IO NtpClient
withNtpClient tr = do
    iom <- ContT withIOManager
    ContT $ withWalletNtpClient iom tr

networkName :: forall n. NetworkDiscriminantVal n => Proxy n -> Text
networkName _ = networkDiscriminantVal @n

-- | Failure status codes for HTTP API server errors.
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13

getServerUrl :: Maybe TlsConfiguration -> Socket -> IO URI
getServerUrl tlsConfig = (fromJust . parseURI . uri <$>) . getSocketName
  where
    uri addr = scheme <> "://" <> show addr <> "/"
    scheme = maybe "http" (const "https") tlsConfig
