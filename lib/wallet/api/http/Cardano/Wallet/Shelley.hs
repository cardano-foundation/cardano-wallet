{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ( serveWallet
    , module Tracers
    ) where

import Prelude

import Cardano.Wallet
    ( WalletException )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery
    ( IsOurs, MaybeLight )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState )
import Cardano.Wallet.Api
    ( ApiLayer, ApiV2 )
import Cardano.Wallet.Api.Http.Logging
    ( ApplicationLog (..) )
import Cardano.Wallet.Api.Http.Server
    ( server )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( HostPreference
    , Listen (..)
    , ListenError (..)
    , TlsConfiguration
    , toServerError
    )
import Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..) )
import Cardano.Wallet.DB.Store.Checkpoints
    ( PersistAddressBook )
import Cardano.Wallet.Flavor
    ( CredFromOf, KeyFlavorS (..), KeyOf, WalletFlavor (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Pools
    ( StakePoolLayer (..), withNodeStakePoolLayer, withStakePoolDbLayer )
import Cardano.Wallet.Primitive.Slotting
    ( neverFails )
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
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId
    , NetworkId
    , SNetworkId
    , networkDiscriminantVal
    , networkIdVal
    , withSNetworkId
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..) )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, StandardCrypto )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.TokenMetadata
    ( newMetadataClient )
import Cardano.Wallet.Tracers as Tracers
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , nullTracers
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    , tracerSeverities
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Exception.Extra
    ( handle )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Cont
    ( ContT (ContT), evalContT )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL
    ( view )
import Data.Generics.Product
    ( typed )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable )
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

import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Wallet.Api.Http.Shelley.Server as Server
import qualified Cardano.Wallet.DB.Layer as Sqlite
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Server as Servant

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
    -> NetworkId
    -- ^ Network identifier
    -> [PoolCertificate]
    -- ^ Shelley genesis pools
    -> Tracers IO
    -- ^ Logging config.
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
    , slottingParameters
    }
  pipeliningStrategy
  network
  shelleyGenesisPools
  Tracers{..}
  databaseDir
  mPoolDatabaseDecorator
  hostPref
  listen
  tlsConfig
  settings
  tokenMetaUri
  block0
  beforeMainLoop = withSNetworkId network $ \sNetwork -> evalContT $  do
    let netId = networkIdVal sNetwork
    lift $ case blockchainSource of
        NodeSource nodeConn _ _ -> trace $ MsgStartingNode nodeConn
    lift . trace
        $ MsgNetworkName
        $ networkDiscriminantVal sNetwork
    netLayer <- withNetworkLayer
        networkTracer
        pipeliningStrategy
        blockchainSource
        network
        netParams
    stakePoolLayer <- case blockchainSource of
        NodeSource{} -> do
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
    randomApi <- withRandomApi netId netLayer
    icarusApi  <- withIcarusApi netId netLayer
    shelleyApi <- withShelleyApi netId netLayer
    multisigApi <- withMultisigApi netId netLayer
    ntpClient <- withNtpClient ntpClientTracer
    bindSocket >>= lift . \case
        Left err -> do
            trace $ MsgServerStartupError err
            pure $ ExitFailure $ exitCodeApiServer err
        Right (_port, socket) -> do
            startServer
                sNetwork
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

    bindSocket :: ContT r IO (Either ListenError (Warp.Port, Socket))
    bindSocket = ContT $ Server.withListeningSocket hostPref listen

    withRandomApi netId netLayer =
        lift $ apiLayer (newTransactionLayer ByronKeyS netId)
            netLayer Server.idleWorker

    withIcarusApi netId netLayer =
        lift $ apiLayer (newTransactionLayer IcarusKeyS netId)
            netLayer Server.idleWorker

    withShelleyApi netId netLayer =
        lift $ apiLayer (newTransactionLayer ShelleyKeyS netId) netLayer
            $ \wrk _ -> Server.manageRewardBalance
                <$> view typed
                <*> pure netLayer
                <*> view typed
                $ wrk

    withMultisigApi netId netLayer =
        lift $ apiLayer (newTransactionLayer SharedKeyS netId) netLayer Server.idleWorker

    startServer
        :: forall n.
            ( HasSNetworkId n
            , Typeable n
            )
        => SNetworkId n
        -> Socket
        -> ApiLayer (RndState n)
        -> ApiLayer (SeqState n IcarusKey)
        -> ApiLayer (SeqState n ShelleyKey)
        -> ApiLayer (SharedState n SharedKey)
        -> StakePoolLayer
        -> NtpClient
        -> IO ()
    startServer _proxy socket byron icarus shelley multisig spl ntp = do
        serverUrl <- getServerUrl tlsConfig socket
        let serverSettings = Warp.defaultSettings
                & setBeforeMainLoop (beforeMainLoop serverUrl)
            api = Proxy @(ApiV2 n)
        let application = Server.serve api
                $ Servant.hoistServer api handleWalletExceptions
                $ server byron icarus shelley multisig spl ntp blockchainSource
        Server.start serverSettings apiServerTracer tlsConfig socket application

    apiLayer
        :: forall s k .
            ( IsOurs s Address
            , IsOurs s RewardAccount
            , MaybeLight s
            , PersistAddressBook s
            , WalletFlavor s
            , KeyOf s ~ k
            )
        => TransactionLayer k (CredFromOf s) SealedTx
        -> NetworkLayer IO (CardanoBlock StandardCrypto)
        -> (WorkerCtx (ApiLayer s) -> WalletId -> IO ())
        -> IO (ApiLayer s)
    apiLayer txLayer netLayer coworker = do
        tokenMetaClient <- newMetadataClient tokenMetadataTracer tokenMetaUri
        dbFactory <- Sqlite.newDBFactory
            (walletFlavor @s)
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
            (block0, netParams)
            netLayer
            txLayer
            dbFactory
            tokenMetaClient
            coworker

handleWalletExceptions :: forall x. Servant.Handler x -> Servant.Handler x
handleWalletExceptions =
    Servant.Handler
    . ExceptT
    . handle (pure . Left . toServerError @WalletException)
    . Servant.runHandler

withNtpClient :: Tracer IO NtpTrace -> ContT r IO NtpClient
withNtpClient tr = do
    iom <- ContT withIOManager
    ContT $ withWalletNtpClient iom tr

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
