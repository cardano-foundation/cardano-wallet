{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Network Layer for talking to Haskell re-written nodes.
--
-- Good to read before / additional resources:
--
-- - Module's documentation in `ouroboros-network/typed-protocols/src/Network/TypedProtocols.hs`
-- - Data Diffusion and Peer Networking in Shelley (see: https://raw.githubusercontent.com/wiki/input-output-hk/cardano-wallet/data_diffusion_and_peer_networking_in_shelley.pdf)
--     - In particular sections 4.1, 4.2, 4.6 and 4.8
module Cardano.Wallet.Shelley.Network
    ( -- * Top-Level Interface
      withNetworkLayer

    , Observer (query,startObserving,stopObserving)
    , newObserver
    , ObserverLog (..)

      -- * Logging
    , NetworkLayerLog (..)
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , CardanoMode
    , LocalChainSyncClient (NoLocalChainSyncClient)
    , LocalNodeClientProtocols (..)
    , LocalNodeClientProtocolsInMode
    , LocalNodeConnectInfo (..)
    , NodeToClientVersion (..)
    , SlotNo (..)
    , connectToLocalNode
    )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConn, nodeSocketFile )
import Cardano.Wallet.Byron.Compatibility
    ( byronCodecConfig, protocolParametersFromUpdateState )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer, produceTimings )
import Cardano.Wallet.Network
    ( ChainFollowLog (..)
    , ChainFollower (..)
    , ChainSyncLog (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    , mapChainFollower
    , mapChainSyncLog
    , withFollowStatsMonitoring
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , TimeInterpreterLog
    , currentRelativeTime
    , mkTimeInterpreter
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( StandardCrypto
    , fromAlonzoPParams
    , fromLedgerPParams
    , fromNonMyopicMemberRewards
    , fromPoint
    , fromPoolDistr
    , fromShelleyCoin
    , fromShelleyPParams
    , fromStakeCredential
    , fromTip
    , fromTip'
    , localNodeConnectInfo
    , nodeToClientVersions
    , optimumNumberOfPools
    , slottingParametersFromGenesis
    , toCardanoBlockHeader
    , toCardanoEra
    , toPoint
    , toShelleyCoin
    , toStakeCredential
    , unsealShelleyTx
    )
import Control.Applicative
    ( liftA3 )
import Control.Monad
    ( forever, guard, unless, void, when, (>=>) )
import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , STM
    , TMVar
    , TQueue
    , TVar
    , atomically
    , isEmptyTMVar
    , modifyTVar'
    , newEmptyTMVarIO
    , newTMVarIO
    , newTQueue
    , newTVarIO
    , putTMVar
    , readTMVar
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryReadTMVar
    , writeTVar
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IO.Unlift
    ( MonadIO, MonadUnliftIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE )
import Control.Retry
    ( RetryAction (..)
    , RetryPolicyM
    , RetryStatus (..)
    , capDelay
    , fibonacciBackoff
    , recoveringDynamic
    )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( runIdentity )
import Data.List
    ( isInfixOf )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map
    ( Map, (!) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage )
import Data.Set
    ( Set )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( DiffTime )
import Data.Void
    ( Void )
import Fmt
    ( Buildable (..), fmt, hexF, listF, mapF, pretty, (+|), (|+) )
import GHC.Stack
    ( HasCallStack )
import Network.Mux
    ( MuxError (..), MuxErrorType (..), WithMuxBearer (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoEras
    , CodecConfig (..)
    , EraCrypto
    , StandardAllegra
    , StandardAlonzo
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..), QueryAnytime (..), QueryHardFork (..), eraIndexToInt )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( MismatchEraInfo )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Interpreter, PastHorizonException (..) )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), DefaultCodecs, clientCodecs, defaultCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( HasNetworkProtocolVersion (..), SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..), getCompactGenesis )
import Ouroboros.Network.Block
    ( Point, Tip (..) )
import Ouroboros.Network.Client.Wallet
    ( LSQ (..)
    , LocalStateQueryCmd (..)
    , LocalTxSubmissionCmd (..)
    , chainSyncFollowTip
    , chainSyncWithBlocks
    , localStateQuery
    , localTxSubmission
    , send
    )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPeer, runPipelinedPeer )
import Ouroboros.Network.Mux
    ( MuxMode (..)
    , MuxPeer (..)
    , OuroborosApplication (..)
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , Handshake
    , LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( chainSyncClientPeer )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.Handshake.Version
    ( combineVersions, simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( localStateQueryClientPeer )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission, SubmitResult (..) )
import System.IO.Error
    ( isDoesNotExistError )
import UnliftIO.Async
    ( async, link )
import UnliftIO.Compat
    ( coerceHandlers )
import UnliftIO.Concurrent
    ( ThreadId )
import UnliftIO.Exception
    ( Handler (..), IOException )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Wallet.Primitive.SyncProgress as SyncProgress
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Codec.CBOR.Term as CBOR
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{- HLINT ignore "Use readTVarIO" -}
{- HLINT ignore "Use newTVarIO" -}
{- HLINT ignore "Use newEmptyTMVarIO" -}

{-------------------------------------------------------------------------------
    Create/Initialize a NetworkLayer
-------------------------------------------------------------------------------}

-- | Create an instance of 'NetworkLayer' by connecting to a local node.
withNetworkLayer
    :: HasCallStack
    => Tracer IO NetworkLayerLog
        -- ^ Logging of network layer startup
    -> Cardano.NetworkId
       -- ^ NetworkId for local node connection
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> CardanoNodeConn
        -- ^ Socket for communicating with the node
    -> NodeToClientVersionData
        -- ^ Codecs for the node's client
    -> SyncTolerance
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr net np conn ver tol action = do
    trTimings <- traceQueryTimings tr
    withNetworkLayerBase (tr <> trTimings) net np conn ver tol action

withNetworkLayerBase
    :: HasCallStack
    => Tracer IO NetworkLayerLog
    -> Cardano.NetworkId
    -> W.NetworkParameters
    -> CardanoNodeConn
    -> NodeToClientVersionData
    -> SyncTolerance
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayerBase tr net np conn versionData tol action = do
    -- NOTE: We keep client connections running for accessing the node tip,
    -- submitting transactions, querying parameters and delegations/rewards.
    --
    -- It is safe to retry when the connection is lost here because this client
    -- doesn't really do anything but sending messages to get the node's tip. It
    -- doesn't rely on the intersection to be up-to-date.
    let handlers = retryOnConnectionLost tr

    -- FIXME: Would be nice to remove these multiple vars.
    -- Not as trivial as it seems, since we'd need to preserve the @debounce@
    -- behaviour.
    (readNodeTip, networkParamsVar, interpreterVar, eraVar)
        <- connectNodeTipClient handlers

    localTxSubmissionQ <- connectLocalTxSubmissionClient handlers

    queryRewardQ <- connectDelegationRewardsClient handlers

    rewardsObserver <- newRewardBalanceFetcher tr readNodeTip queryRewardQ
    let readCurrentNodeEra = atomically $ readTMVar eraVar

    action $ NetworkLayer
        { chainSync = \trFollowLog follower -> do
            let withStats = withFollowStatsMonitoring
                    trFollowLog
                    (_syncProgress interpreterVar)
            withStats $ \trChainSyncLog -> do
                let mapB = toCardanoBlockHeader gp
                    mapP = fromPoint
                let client = mkWalletClient
                        (contramap (mapChainSyncLog mapB mapP) trChainSyncLog)
                        (mapChainFollower
                            toPoint
                            fromPoint
                            (fromTip' gp)
                            id
                            follower)
                        cfg
                traceWith trFollowLog MsgStartFollowing
                connectClient tr handlers client versionData conn
        , lightSync = Nothing

        , currentNodeTip =
            fromTip getGenesisBlockHash <$> atomically readNodeTip
        , currentNodeEra =
            -- NOTE: Is not guaranteed to be consistent with @currentNodeTip@
            readCurrentNodeEra
        , watchNodeTip = do
            _watchNodeTip readNodeTip
        , currentProtocolParameters =
            fst . fst <$> atomically (readTMVar networkParamsVar)
        , currentNodeProtocolParameters =
            snd . fst <$> atomically (readTMVar networkParamsVar)
        , currentSlottingParameters =
            snd <$> atomically (readTMVar networkParamsVar)
        , postTx =
            _postTx localTxSubmissionQ
        , stakeDistribution =
            _stakeDistribution queryRewardQ
        , getCachedRewardAccountBalance =
            _getCachedRewardAccountBalance rewardsObserver
        , fetchRewardAccountBalances =
            fetchRewardAccounts tr queryRewardQ
        , timeInterpreter =
            _timeInterpreter (contramap MsgInterpreterLog tr) interpreterVar
        , syncProgress = _syncProgress interpreterVar
        }
  where
    gp@W.GenesisParameters
        { getGenesisBlockHash
        , getGenesisBlockDate
        } = W.genesisParameters np
    sp = W.slottingParameters np
    connectInfo = localNodeConnectInfo sp net conn
    cfg = codecConfig sp

    connectNodeTipClient
        :: HasCallStack
        => RetryHandlers
        -> IO ( STM IO (Tip (CardanoBlock StandardCrypto))
              , TMVar IO ((W.ProtocolParameters, Maybe Cardano.ProtocolParameters), W.SlottingParameters)
              , TMVar IO (CardanoInterpreter StandardCrypto)
              , TMVar IO AnyCardanoEra
              )
    connectNodeTipClient handlers = do
        networkParamsVar <- newEmptyTMVarIO
        interpreterVar <- newEmptyTMVarIO
        eraVar <- newEmptyTMVarIO
        (nodeTipClient, readTip) <- mkTipSyncClient tr np
            (curry (atomically . repsertTMVar networkParamsVar))
            (atomically . repsertTMVar interpreterVar)
            (atomically . repsertTMVar eraVar)
        link =<< async (connectClient tr handlers nodeTipClient versionData conn)
        pure (readTip, networkParamsVar, interpreterVar, eraVar)

    connectLocalTxSubmissionClient
        :: RetryHandlers
        -> IO ( TQueue IO (LocalTxSubmissionCmd
                  (Cardano.TxInMode CardanoMode)
                  (Cardano.TxValidationErrorInMode CardanoMode)
                  IO)
              )
    connectLocalTxSubmissionClient handlers = do
        q <- atomically newTQueue
        let client = mkLocalTxSubmissionClient tr q
        link =<< async (connectCardanoApiClient tr handlers connectInfo client)
        pure q

    connectDelegationRewardsClient
        :: HasCallStack
        => RetryHandlers
        -> IO (TQueue IO
                (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO))
    connectDelegationRewardsClient handlers = do
        q <- atomically newTQueue
        let client = mkDelegationRewardsClient tr cfg q
        link =<< async (connectClient tr handlers client versionData conn)
        pure q

    -- NOTE1: only shelley transactions can be submitted like this, because they
    -- are deserialised as shelley transactions before submitting.
    --
    -- NOTE2: It is not ideal to query the current era again here because we
    -- should in practice use the same era as the one used to construct the
    -- transaction. However, when turning transactions to 'SealedTx', we loose
    -- all form of type-level indicator about the era. The 'SealedTx' type
    -- shouldn't be needed anymore since we've dropped jormungandr, so we could
    -- instead carry a transaction from cardano-api types with proper typing.
    _postTx localTxSubmissionQ tx = do
        liftIO $ traceWith tr $ MsgPostTx tx
        let cmd = CmdSubmitTx $ unsealShelleyTx tx
        liftIO (localTxSubmissionQ `send` cmd) >>= \case
            SubmitSuccess -> pure ()
            SubmitFail e -> throwE $ ErrPostTxValidationError $ T.pack $ show e

    _stakeDistribution queue coin = do
        liftIO $ traceWith tr $ MsgWillQueryRewardsForStake coin

        let qry :: LSQ (CardanoBlock StandardCrypto) IO (Maybe W.StakePoolsSummary)
            qry = liftA3 (liftA3 W.StakePoolsSummary)
                getNOpt
                queryNonMyopicMemberRewards
                stakeDistr

        mres <- bracketQuery "stakePoolsSummary" tr $
            queue `send` (SomeLSQ qry)

        -- The result will be Nothing if query occurs during the byron era
        traceWith tr $ MsgFetchStakePoolsData mres
        case mres of
            Just res@W.StakePoolsSummary{rewards,stake} -> do
                liftIO $ traceWith tr $ MsgFetchStakePoolsDataSummary
                    (Map.size stake)
                    (Map.size rewards)
                return res
            Nothing -> pure $ W.StakePoolsSummary 0 mempty mempty
      where
        stakeDistr
            :: LSQ (CardanoBlock StandardCrypto) IO
                (Maybe (Map W.PoolId Percentage))
        stakeDistr = shelleyBased
            (fromPoolDistr <$> LSQry Shelley.GetStakeDistribution)

        getNOpt :: LSQ (CardanoBlock StandardCrypto) IO (Maybe Int)
        getNOpt = onAnyEra
            (pure Nothing)
            (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
            (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
            (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
            (Just . fromIntegral . Alonzo._nOpt <$> LSQry Shelley.GetCurrentPParams)

        queryNonMyopicMemberRewards
            :: LSQ (CardanoBlock StandardCrypto) IO
                    (Maybe (Map W.PoolId W.Coin))
        queryNonMyopicMemberRewards = shelleyBased $
            (getRewardMap . fromNonMyopicMemberRewards)
                <$> LSQry (Shelley.GetNonMyopicMemberRewards stake)
          where
            stake :: Set (Either SL.Coin a)
            stake = Set.singleton $ Left $ toShelleyCoin coin

            fromJustRewards = fromMaybe
                (error "stakeDistribution: requested rewards not included in response")

            getRewardMap
                :: Map
                    (Either W.Coin W.RewardAccount)
                    (Map W.PoolId W.Coin)
                -> Map W.PoolId W.Coin
            getRewardMap =
                fromJustRewards . Map.lookup (Left coin)

    _watchNodeTip readTip cb = do
        observeForever readTip $ \tip -> do
            let header = fromTip getGenesisBlockHash tip
            bracketTracer (contramap (MsgWatcherUpdate header) tr) $ cb header

    -- TODO(#2042): Make wallets call manually, with matching
    -- stopObserving.
    _getCachedRewardAccountBalance rewardsObserver k = do
        startObserving rewardsObserver k
        fromMaybe (W.Coin 0) <$> query rewardsObserver k

    _timeInterpreter
        :: HasCallStack
        => Tracer IO TimeInterpreterLog
        -> TMVar IO (CardanoInterpreter sc)
        -> TimeInterpreter (ExceptT PastHorizonException IO)
    _timeInterpreter tr' var = do
        let readInterpreter = liftIO $ atomically $ readTMVar var
        mkTimeInterpreter tr' getGenesisBlockDate readInterpreter

    _syncProgress
        :: TMVar IO (CardanoInterpreter sc)
        -> SlotNo
        -> IO SyncProgress
    _syncProgress var slot = do
        liftIO (atomically $ tryReadTMVar var) >>= \case
            -- If the wallet has been started, but not yet been able to connect
            -- to the node, we don't have an interpreter summary, and can't
            -- calculate the syncProgress using a @SlotNo@.
            --
            -- If we want to guarantee the availability of @SyncProgress@, we
            -- could consider storing @UTCTime@ along with the follower tip in
            -- question, but that would make chain-following dependent on
            -- a TimeInterpreter.
            Nothing -> return NotResponding
            Just i -> do
                let ti = mkTimeInterpreter nullTracer getGenesisBlockDate (pure i)
                time <- currentRelativeTime ti
                let res = SyncProgress.syncProgress tol ti slot time
                case runIdentity $ runExceptT res of
                    -- Getting a past horizon error here should be unlikely, but
                    -- could happen if we switch from a in-sync node to a
                    -- not-in-sync node, either by restarting the wallet, or
                    -- restarting the node using the same socket but different
                    -- db.
                    Left _pastHorizon -> return NotResponding
                    Right p -> return p

{-------------------------------------------------------------------------------
    NetworkClient
    Node-to-client mini-protocol descriptions
-------------------------------------------------------------------------------}
-- | A protocol client that will never leave the initial state.
doNothingProtocol
    :: MonadTimer m => RunMiniProtocol 'InitiatorMode ByteString m a Void
doNothingProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = NodeToClientVersion -> OuroborosApplication
    'InitiatorMode
        -- Initiator ~ Client (as opposed to Responder / Server)
    LocalAddress
        -- Address type
    ByteString
        -- Concrete representation for bytes string
    m
        -- Underlying monad we run in
    Void
        -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- Irrelevant for initiator. Return type of 'ResponderMode' application.

-- | Construct a network client with the given communication channel, for the
-- purposes of syncing blocks to a single wallet.
mkWalletClient
    :: forall m block
    . ( block ~ CardanoBlock (StandardCrypto)
      , MonadThrow m, MonadST m, MonadTimer m, MonadAsync m)
    => Tracer m (ChainSyncLog block (Point block))
    -> ChainFollower m (Point block) (Tip block) (NonEmpty block)
    -> CodecConfig block
    -> NetworkClient m
mkWalletClient tr follower cfg v =
    nodeToClientProtocols (const $ return $ NodeToClientProtocols
        { localChainSyncProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                runPipelinedPeer nullTracer (cChainSyncCodec $ codecs v cfg) channel
                $ chainSyncClientPeerPipelined
                $ chainSyncWithBlocks tr follower

        , localTxSubmissionProtocol =
            doNothingProtocol

        , localStateQueryProtocol =
            doNothingProtocol
        }) v

-- | Construct a network client with the given communication channel, for the
-- purposes of querying delegations and rewards.
mkDelegationRewardsClient
    :: forall m. (MonadThrow m, MonadST m, MonadTimer m, MonadIO m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> TQueue m (LocalStateQueryCmd (CardanoBlock StandardCrypto) m)
        -- ^ Communication channel with the LocalStateQuery client
    -> NetworkClient m
mkDelegationRewardsClient tr cfg queryRewardQ v =
    nodeToClientProtocols (const $ return $ NodeToClientProtocols
        { localChainSyncProtocol =
            doNothingProtocol

        , localTxSubmissionProtocol =
            doNothingProtocol

        , localStateQueryProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer tr' codec channel
                $ localStateQueryClientPeer
                $ localStateQuery queryRewardQ
        }) v
  where
    tr' = contramap (MsgLocalStateQuery DelegationRewardsClient) tr
    codec = cStateQueryCodec (serialisedCodecs v cfg)


type CardanoInterpreter sc = Interpreter (CardanoEras sc)

-- | Construct a network client with the given communication channel, for the
-- purpose of:
--
--  * Tracking the node tip
--  * Tracking the latest protocol parameters state.
--  * Querying the history interpreter as necessary.
mkTipSyncClient
    :: forall m. (HasCallStack, MonadUnliftIO m, MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> ((W.ProtocolParameters, Maybe Cardano.ProtocolParameters) -> W.SlottingParameters -> m ())
        -- ^ Notifier callback for when parameters for tip change.
    -> (CardanoInterpreter StandardCrypto -> m ())
        -- ^ Notifier callback for when time interpreter is updated.
    -> (AnyCardanoEra -> m ())
        -- ^ Notifier callback for when the era is updated
    -> m (NetworkClient m, STM m (Tip (CardanoBlock StandardCrypto)))
mkTipSyncClient tr np onPParamsUpdate onInterpreterUpdate onEraUpdate = do
    (localStateQueryQ :: TQueue m (LocalStateQueryCmd (CardanoBlock StandardCrypto) m))
        <- atomically newTQueue

    tipVar <- newTVarIO (Just $ AnyCardanoEra ByronEra, TipGenesis)

    (onPParamsUpdate' :: ((W.ProtocolParameters, Maybe Cardano.ProtocolParameters), W.SlottingParameters) -> m ()) <-
        debounce $ \(pp, sp) -> do
            traceWith tr $ MsgProtocolParameters (fst pp) sp
            onPParamsUpdate pp sp

    let queryParams = do
            eraBounds <- W.EraInfo
                <$> LSQry (QueryAnytimeByron GetEraStart)
                <*> LSQry (QueryAnytimeShelley GetEraStart)
                <*> LSQry (QueryAnytimeAllegra GetEraStart)
                <*> LSQry (QueryAnytimeMary GetEraStart)
                <*> LSQry (QueryAnytimeAlonzo GetEraStart)

            sp <- byronOrShelleyBased
                (pure $ W.slottingParameters np)
                ((slottingParametersFromGenesis . getCompactGenesis)
                    <$> LSQry Shelley.GetGenesisConfig)

            pp <- onAnyEra
                (protocolParametersFromUpdateState eraBounds
                    <$> LSQry Byron.GetUpdateInterfaceState)
                (fromShelleyPParams eraBounds
                    <$> LSQry Shelley.GetCurrentPParams)
                (fromShelleyPParams eraBounds
                    <$> LSQry Shelley.GetCurrentPParams)
                (fromShelleyPParams eraBounds
                    <$> LSQry Shelley.GetCurrentPParams)
                (fromAlonzoPParams eraBounds
                    <$> LSQry Shelley.GetCurrentPParams)

            ppNode <- onAnyEra
                (pure Nothing)
                (Just . fromLedgerPParams Cardano.ShelleyBasedEraShelley
                    <$> LSQry Shelley.GetCurrentPParams)
                (Just . fromLedgerPParams Cardano.ShelleyBasedEraAllegra
                    <$> LSQry Shelley.GetCurrentPParams)
                (Just . fromLedgerPParams Cardano.ShelleyBasedEraMary
                    <$> LSQry Shelley.GetCurrentPParams)
                (Just . fromLedgerPParams Cardano.ShelleyBasedEraAlonzo
                    <$> LSQry Shelley.GetCurrentPParams)

            return ((pp, ppNode), sp)

    let queryInterpreter = LSQry (QueryHardFork GetInterpreter)

    let cfg = codecConfig (W.slottingParameters np)

    -- NOTE: These are updated every block. This is far more often than
    -- necessary.
    --
    -- By blocking (with `send`) we ensure we don't queue multiple queries.
    let onTipUpdate _tip = do
            let qry = (,,) <$> queryParams <*> queryInterpreter <*> currentEra
            (pparams, int, e) <- localStateQueryQ `send` (SomeLSQ qry)
            onPParamsUpdate' pparams
            onInterpreterUpdate int
            onEraUpdate e

    link =<< async (observeForever (readTVar tipVar) onTipUpdate)


    let client v = nodeToClientProtocols (const $ return $ NodeToClientProtocols
            { localChainSyncProtocol =
                let
                    codec = cChainSyncCodec $ codecs v cfg
                in
                InitiatorProtocolOnly $ MuxPeerRaw
                    $ \channel -> runPeer nullTracer codec channel
                    $ chainSyncClientPeer
                    $ chainSyncFollowTip toCardanoEra (curry (atomically . writeTVar tipVar))

            , localTxSubmissionProtocol = doNothingProtocol

            , localStateQueryProtocol =
                let
                    tr' = contramap (MsgLocalStateQuery TipSyncClient) tr
                    codec = cStateQueryCodec $ (serialisedCodecs v) cfg
                in
                InitiatorProtocolOnly $ MuxPeerRaw
                    $ \channel -> runPeer tr' codec channel
                    $ localStateQueryClientPeer
                    $ localStateQuery localStateQueryQ
            }) v
    return (client, snd <$> readTVar tipVar)
    -- FIXME: We can remove the era from the tip sync client now.

mkLocalTxSubmissionClient
    :: Tracer IO NetworkLayerLog
    -> TQueue IO (LocalTxSubmissionCmd
            (Cardano.TxInMode CardanoMode)
            (Cardano.TxValidationErrorInMode CardanoMode)
            IO )
    -> LocalNodeClientProtocolsInMode CardanoMode
mkLocalTxSubmissionClient _tr localTxSubmissionQ = LocalNodeClientProtocols
    { localChainSyncClient = NoLocalChainSyncClient
    , localTxSubmissionClient = Just $ localTxSubmission localTxSubmissionQ
    , localStateQueryClient = Nothing
    }
    -- FIXME: Put back logging for local Tx Submission.
    -- tr' = contramap MsgTxSubmission tr

{-------------------------------------------------------------------------------
    Thread for observing
    Reward Account Balance
-------------------------------------------------------------------------------}
newRewardBalanceFetcher
    :: Tracer IO NetworkLayerLog
    -- ^ Used to convert tips for logging
    -> STM IO (Tip (CardanoBlock StandardCrypto))
    -- ^ STM action for observing the current tip
    -> TQueue IO (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO)
    -> IO (Observer IO W.RewardAccount W.Coin)
newRewardBalanceFetcher tr readNodeTip queryRewardQ = do
    (ob, refresh) <- newObserver (contramap MsgObserverLog tr) fetch
    link =<< async (observeForever readNodeTip refresh)
    return ob
  where
    fetch
        :: Tip (CardanoBlock StandardCrypto)
        -> Set W.RewardAccount
        -> IO (Maybe (Map W.RewardAccount W.Coin))
    fetch _tip accounts | Set.null accounts = pure (Just mempty)
    fetch _tip accounts = do
        -- NOTE: We no longer need the tip to run LSQ queries. The local state
        -- query client will automatically acquire the latest tip.
        Just <$> fetchRewardAccounts tr queryRewardQ accounts

fetchRewardAccounts
    :: Tracer IO NetworkLayerLog
    -> TQueue IO (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO)
    -> Set W.RewardAccount
    -> IO (Map W.RewardAccount W.Coin)
fetchRewardAccounts tr queryRewardQ accounts = do
        liftIO $ traceWith tr $
            MsgFetchRewardAccountBalance accounts

        let qry = onAnyEra
                (pure (byronValue, []))
                shelleyQry
                shelleyQry
                shelleyQry
                shelleyQry

        (res,logs) <- bracketQuery "queryRewards" tr (send queryRewardQ (SomeLSQ qry))
        liftIO $ mapM_ (traceWith tr) logs
        return res
  where
    byronValue :: Map W.RewardAccount W.Coin
    byronValue = Map.fromList . map (, W.Coin 0) $ Set.toList accounts

    shelleyQry
        :: (Crypto.HashAlgorithm (SL.ADDRHASH (EraCrypto shelleyEra)))
        => LSQ
            (Shelley.ShelleyBlock shelleyEra)
            IO
            (Map W.RewardAccount W.Coin, [NetworkLayerLog])
    shelleyQry =
       fmap fromBalanceResult
        . LSQry
        . Shelley.GetFilteredDelegationsAndRewardAccounts
        $ Set.map toStakeCredential accounts

    fromBalanceResult
        :: ( Map (SL.Credential 'SL.Staking crypto)
                 (SL.KeyHash 'SL.StakePool crypto)
            , SL.RewardAccounts crypto
            )
        -> (Map W.RewardAccount W.Coin, [NetworkLayerLog])
    fromBalanceResult (deleg, rewardAccounts) =
        ( Map.mapKeys fromStakeCredential $
            Map.map fromShelleyCoin rewardAccounts
        , [MsgAccountDelegationAndRewards deleg rewardAccounts]
        )

-- | Monitors values for keys, and allows clients to @query@ them.
--
-- Designed to be used for observing reward balances, where we want to cache the
-- balances of /all/ the wallets' accounts on tip change, and allow wallet
-- workers to @query@ the cache later, often, and whenever they want.
--
-- NOTE: One could imagine replacing @query@ getter with a push-based approach.
data Observer m key value = Observer
    { startObserving :: key -> m ()
    , stopObserving :: key -> m ()
    , query :: key -> m (Maybe value)
    }

-- | Given a way to fetch values for a set of keys, create:
-- 1. An @Observer@ for consuming values
-- 2. A refresh action
--
-- The @env@ parameter can be used to pass in information needed for refreshing,
-- like the current tip when fetching rewards.
--
-- If the given @fetch@ function returns @Nothing@ the the cache will not be
-- updated.
--
-- If it returns @Just values@, the cache will be set to @values@.
newObserver
    :: forall m key value env. (MonadSTM m, Ord key, Eq value)
    => Tracer m (ObserverLog key value)
    -> (env -> Set key -> m (Maybe (Map key value)))
    -> m (Observer m key value, env -> m ())
newObserver tr fetch = do
    cacheVar <- newTVarIO Map.empty
    toBeObservedVar <- newTVarIO Set.empty
    return (observer cacheVar toBeObservedVar, refresh cacheVar toBeObservedVar)
  where
    observer
        :: TVar m (Map key value)
        -> TVar m (Set key)
        -> Observer m key value
    observer cacheVar observedKeysVar =
        Observer
            { startObserving = \k -> do
                wasAdded <- atomically $ do
                    notAlreadyThere <- Set.notMember k <$> readTVar observedKeysVar
                    modifyTVar' observedKeysVar (Set.insert k)
                    return notAlreadyThere
                when wasAdded $ traceWith tr $ MsgAddedObserver k
            , stopObserving = \k -> do
                atomically $ do
                    modifyTVar' observedKeysVar (Set.delete k)
                    modifyTVar' cacheVar (Map.delete k)
                traceWith tr $ MsgRemovedObserver k
            , query = \k -> Map.lookup k <$> readTVarIO cacheVar
            }

    refresh
        :: TVar m (Map key value)
        -> TVar m (Set key)
        -> env
        -> m ()
    refresh cacheVar observedKeysVar env = do
        keys <- readTVarIO observedKeysVar
        oldValues <- readTVarIO cacheVar
        traceWith tr $ MsgWillFetch keys
        mvalues <- fetch env keys

        case mvalues of
            Nothing -> pure ()
            Just values -> do
                traceWith tr $ MsgDidFetch values
                when (oldValues /= values) $
                    traceWith tr $ MsgDidChange values
                atomically $ writeTVar cacheVar values

{-------------------------------------------------------------------------------
    Codecs
-------------------------------------------------------------------------------}

codecVersion
    :: NodeToClientVersion
    -> BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion version = verMap ! version
    where verMap = supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto))

codecConfig :: W.SlottingParameters -> CodecConfig (CardanoBlock c)
codecConfig sp = CardanoCodecConfig
    (byronCodecConfig sp)
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig

-- | A group of codecs which will deserialise block data.
codecs
    :: MonadST m
    => NodeToClientVersion
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> ClientCodecs (CardanoBlock StandardCrypto) m
codecs nodeToClientVersion cfg =
    clientCodecs cfg (codecVersion nodeToClientVersion) nodeToClientVersion

-- | A group of codecs which won't deserialise block data. Often only the block
-- headers are needed. It's more efficient and easier not to deserialise.
serialisedCodecs
    :: MonadST m
    => NodeToClientVersion
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> DefaultCodecs (CardanoBlock StandardCrypto) m
serialisedCodecs nodeToClientVersion cfg =
    defaultCodecs cfg (codecVersion nodeToClientVersion) nodeToClientVersion

{-------------------------------------------------------------------------------
    I/O -- Connect to the cardano-node process.
-------------------------------------------------------------------------------}
-- | Construct a network client with the given protocols.
--
-- TODO: This functions overlaps with 'connectClient'.
-- However, it is more modern in that it uses "Cardano.Api".
--
-- Do we want to switch to "Cardano.Api" for the NodeToClient protocols?
--
-- Cons:
--
-- * "Cardano.Api" is not polymorphic in the underlying monad.
-- But in order to use /checked exception/ in the client monad,
-- we would need the 'connectToLocalNode' function to be polymorphic
-- in the monad (and therefore also in the exceptions).
--
-- Pro:
--
-- * "Cardano.Api" is simpler in that it does not require so much plumbing.
connectCardanoApiClient
    :: Tracer IO NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> RetryHandlers
    -> LocalNodeConnectInfo CardanoMode
    -> LocalNodeClientProtocolsInMode CardanoMode
    -> IO ()
connectCardanoApiClient tr handlers info =
    recoveringNodeConnection tr handlers . connectToLocalNode info

-- Connect a client to a network, see `mkWalletClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkWalletClient tr gp queue) mainnetVersionData conn
connectClient
    :: Tracer IO NetworkLayerLog
    -> RetryHandlers
    -> NetworkClient IO
    -> NodeToClientVersionData
    -> CardanoNodeConn
    -> IO ()
connectClient tr handlers client vData conn = withIOManager $ \iocp -> do
    let versions = combineVersions
            [ simpleSingletonVersions v vData (client v)
            | v <- nodeToClientVersions
            ]
    let tracers = NetworkConnectTracers
            { nctMuxTracer = nullTracer
            , nctHandshakeTracer = contramap MsgHandshakeTracer tr
            }
    let socket = localSnocket iocp
    recoveringNodeConnection tr handlers $
        connectTo socket tracers versions (nodeSocketFile conn)

recoveringNodeConnection
    :: Tracer IO NetworkLayerLog
    -> RetryHandlers
    -> IO a
    -> IO a
recoveringNodeConnection tr handlers action =
    recoveringDynamic policy (coerceHandlers handlers) $ \status -> do
        traceWith tr $ MsgCouldntConnect (rsIterNumber status)
        action
  where
    -- .25s -> .25s -> .5s → .75s → 1.25s → 2s
    policy :: RetryPolicyM IO
    policy = fibonacciBackoff 250_000 & capDelay 2_000_000

-- | Shorthand for the list of exception handlers used with 'recovering'.
type RetryHandlers = [RetryStatus -> Handler IO RetryAction]

-- | Handlers that are retrying on every connection lost.
retryOnConnectionLost :: Tracer IO NetworkLayerLog -> RetryHandlers
retryOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' True
    , const $ Handler $ handleMuxError tr' True
    ]
  where
    tr' = contramap MsgConnectionLost tr

-- When the node's connection vanished, we may also want to handle things in a
-- slightly different way depending on whether we are a waller worker or just
-- the node's tip thread.
handleIOException
    :: Tracer IO (Maybe IOException)
    -> Bool -- ^ 'True' = retry on 'ResourceVanishedError'
    -> IOException
    -> IO RetryAction
handleIOException tr onResourceVanished e
    -- There's a race-condition when starting the wallet and the node at the
    -- same time: the socket might not be there yet when we try to open it.
    -- In such case, we simply retry a bit later and hope it's there.
    | isDoesNotExistError e =
        pure ConsultPolicy

    -- If the nonblocking UNIX domain socket connection cannot be completed
    -- immediately (i.e. connect() returns EAGAIN), try again. This happens
    -- because the node's listen queue is quite short.
    | isTryAgainError e =
        pure ConsultPolicy

    | isResourceVanishedError e = do
        traceWith tr $ Just e
        pure $ if onResourceVanished then ConsultPolicy else DontRetry

    | otherwise = do
        pure DontRetry
  where
    isResourceVanishedError = isInfixOf "resource vanished" . show
    isTryAgainError = isInfixOf "resource exhausted" . show

handleMuxError
    :: Tracer IO (Maybe IOException)
    -> Bool -- ^ 'True' = retry on 'ResourceVanishedError'
    -> MuxError
    -> IO RetryAction
handleMuxError tr onResourceVanished = pure . errorType >=> \case
    MuxUnknownMiniProtocol -> pure DontRetry
    MuxDecodeError -> pure DontRetry
    MuxIngressQueueOverRun -> pure DontRetry
    MuxInitiatorOnly -> pure DontRetry
    MuxShutdown _ -> pure DontRetry -- fixme: #2212 consider cases
    MuxCleanShutdown -> pure DontRetry
    MuxIOException e ->
        handleIOException tr onResourceVanished e
    MuxBearerClosed -> do
        traceWith tr Nothing
        pure $ if onResourceVanished then ConsultPolicy else DontRetry

    -- MuxSDU*Timeout errors arise because the bandwidth of the
    -- interprocess communication socket dropped unexpectedly,
    -- and the socket library decided to cut off the connection
    -- after ~ 30 seconds.
    -- Chances are that the system is overloaded, let's retry in 30 seconds.
    MuxSDUReadTimeout -> pure $ ConsultPolicyOverrideDelay 30_000_000
    MuxSDUWriteTimeout -> pure $ ConsultPolicyOverrideDelay 30_000_000

{-------------------------------------------------------------------------------
    Helper functions of the Control.* and STM variety
-------------------------------------------------------------------------------}
-- | Return a function to run an action only if its single parameter has changed
-- since the previous time it was called.
debounce :: (Eq a, MonadSTM m) => (a -> m ()) -> m (a -> m ())
debounce action = do
    mvar <- newTMVarIO Nothing
    pure $ \cur -> do
        prev <- atomically $ takeTMVar mvar
        unless (Just cur == prev) $ action cur
        atomically $ putTMVar mvar (Just cur)

-- | Trigger an action initially, and when the value changes.
--
-- There's no guarantee that we will see every intermediate value.
observeForever :: (MonadSTM m, Eq a) => STM m a -> (a -> m ()) -> m ()
observeForever readVal action = go Nothing
  where
    go old = do
        new <- atomically $ do
            new <- readVal
            guard (old /= Just new)
            return new
        action new
        go (Just new)

-- | Put if empty, replace if not empty.
repsertTMVar :: TMVar IO a -> a -> STM IO ()
repsertTMVar var x = do
    e <- isEmptyTMVar var
    unless e $ void $ takeTMVar var
    putTMVar var x

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Convenience function to trace around a local state query.
-- See 'addTimings'.
bracketQuery
    :: MonadUnliftIO m
    => String
    -> Tracer m NetworkLayerLog
    -> m a
    -> m a
bracketQuery label tr = bracketTracer (contramap (MsgQuery label) tr)

-- | A tracer transformer which processes 'MsgQuery' logs to make new
-- 'MsgQueryTime' logs, so that we can get logs like:
--
-- >>> Query getAccountBalance took 51.664463s
traceQueryTimings :: Tracer IO NetworkLayerLog -> IO (Tracer IO NetworkLayerLog)
traceQueryTimings tr = produceTimings msgQuery trDiffTime
  where
    trDiffTime = contramap (uncurry MsgQueryTime) tr
    msgQuery = \case
        MsgQuery l b -> Just (l, b)
        _ -> Nothing

-- | Consider a "slow query" to be something that takes 200ms or more.
isSlowQuery :: String -> DiffTime -> Bool
isSlowQuery _label = (>= 0.2)

data NetworkLayerLog where
    MsgCouldntConnect :: Int -> NetworkLayerLog
    MsgConnectionLost :: Maybe IOException -> NetworkLayerLog
    MsgTxSubmission
        :: (TraceSendRecv
            (LocalTxSubmission (Cardano.TxInMode CardanoMode) (Cardano.TxValidationErrorInMode CardanoMode)))
        -> NetworkLayerLog
    MsgLocalStateQuery
        :: QueryClientName
        -> (TraceSendRecv
            (LocalStateQuery (CardanoBlock StandardCrypto) (Point (CardanoBlock StandardCrypto)) (Query (CardanoBlock StandardCrypto))))
        -> NetworkLayerLog
    MsgHandshakeTracer ::
      (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace) -> NetworkLayerLog
    MsgPostTx :: W.SealedTx -> NetworkLayerLog
    MsgNodeTip :: W.BlockHeader -> NetworkLayerLog
    MsgProtocolParameters :: W.ProtocolParameters -> W.SlottingParameters -> NetworkLayerLog
    MsgLocalStateQueryError :: QueryClientName -> String -> NetworkLayerLog
    MsgLocalStateQueryEraMismatch :: MismatchEraInfo (CardanoEras StandardCrypto) -> NetworkLayerLog
    MsgFetchRewardAccountBalance
        :: Set W.RewardAccount
        -> NetworkLayerLog
    MsgAccountDelegationAndRewards
        :: forall era crypto. (Map (SL.Credential 'SL.Staking era) (SL.KeyHash 'SL.StakePool crypto))
        -> SL.RewardAccounts era
        -> NetworkLayerLog
    MsgDestroyCursor :: ThreadId -> NetworkLayerLog
    MsgWillQueryRewardsForStake :: W.Coin -> NetworkLayerLog
    MsgFetchStakePoolsData :: Maybe W.StakePoolsSummary -> NetworkLayerLog
    MsgFetchStakePoolsDataSummary :: Int -> Int -> NetworkLayerLog
      -- ^ Number of pools in stake distribution, and rewards map,
      -- respectively.
    MsgWatcherUpdate :: W.BlockHeader -> BracketLog -> NetworkLayerLog
    MsgInterpreter :: CardanoInterpreter StandardCrypto -> NetworkLayerLog
    -- TODO: Combine ^^ and vv
    MsgInterpreterLog :: TimeInterpreterLog -> NetworkLayerLog
    MsgQuery :: String -> BracketLog -> NetworkLayerLog
    MsgQueryTime :: String -> DiffTime -> NetworkLayerLog
    MsgObserverLog
        :: ObserverLog W.RewardAccount W.Coin
        -> NetworkLayerLog

data QueryClientName
    = TipSyncClient
    | DelegationRewardsClient
    deriving (Show, Eq)

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)

instance ToText NetworkLayerLog where
    toText = \case
        MsgCouldntConnect n -> T.unwords
            [ "Couldn't connect to node (x" <> toText (n + 1) <> ")."
            , "Retrying in a bit..."
            ]
        MsgConnectionLost Nothing  ->
            "Connection lost with the node."
        MsgConnectionLost (Just e) -> T.unwords
            [ toText (MsgConnectionLost Nothing)
            , T.pack (show e)
            ]
        MsgTxSubmission msg ->
            T.pack (show msg)
        MsgHandshakeTracer (WithMuxBearer conn h) ->
            pretty conn <> " " <> T.pack (show h)
        MsgPostTx tx ->
            "Posting transaction, serialized as:\n"+|hexF (serialisedTx tx)|+""
        MsgLocalStateQuery client msg ->
            T.pack (show client <> " " <> show msg)
        MsgNodeTip bh -> T.unwords
            [ "Network node tip is"
            , pretty bh
            ]
        MsgProtocolParameters pparams sparams -> T.unlines
            [ "Protocol parameters for tip are:"
            , pretty pparams
            , "Slotting parameters for tip are:"
            , pretty sparams
            ]
        MsgLocalStateQueryError client e -> T.pack $ mconcat
            [ "Error when querying local state parameters for "
            , show client
            , ": "
            , e
            ]
        MsgLocalStateQueryEraMismatch mismatch ->
            "Local state query for the wrong era - this is fine. " <>
            T.pack (show mismatch)
        MsgFetchRewardAccountBalance accts -> T.unwords
            [ "Querying the reward account balance for"
            , fmt $ listF accts
            ]
        MsgAccountDelegationAndRewards delegations rewards -> T.unlines
            [ "  delegations = " <> T.pack (show delegations)
            , "  rewards = " <> T.pack (show rewards)
            ]
        MsgDestroyCursor threadId -> T.unwords
            [ "Destroying cursor connection at"
            , T.pack (show threadId)
            ]
        MsgWillQueryRewardsForStake c ->
            "Will query non-myopic rewards using the stake " <> pretty c
        MsgFetchStakePoolsData d ->
            "Fetched pool data from node tip using LSQ: " <> pretty d
        MsgFetchStakePoolsDataSummary inStake inRewards -> mconcat
            [ "Fetched pool data from node tip using LSQ. Got "
            , T.pack (show inStake)
            , " pools in the stake distribution, and "
            , T.pack (show inRewards)
            , " pools in the non-myopic member reward map."
            ]
        MsgWatcherUpdate tip b ->
            "Update watcher with tip: " <> pretty tip <>
            ". Callback " <> toText b <> "."
        MsgQuery label msg ->
            T.pack label <> ": " <> toText msg
        MsgQueryTime qry diffTime ->
            "Query " <> T.pack qry <> " took " <> T.pack (show diffTime) <>
            if isSlowQuery qry diffTime then " (too slow)" else ""
        MsgInterpreter interpreter ->
            "Updated the history interpreter: " <> T.pack (show interpreter)
        MsgInterpreterLog msg -> toText msg
        MsgObserverLog msg -> "Reward observer: " <> toText msg

instance HasPrivacyAnnotation NetworkLayerLog
instance HasSeverityAnnotation NetworkLayerLog where
    getSeverityAnnotation = \case
        MsgCouldntConnect 0                -> Debug
        MsgCouldntConnect 1                -> Notice
        MsgCouldntConnect{}                -> Warning
        MsgConnectionLost{}                -> Warning
        MsgTxSubmission{}                  -> Info
        MsgHandshakeTracer{}               -> Debug
        MsgPostTx{}                        -> Debug
        MsgLocalStateQuery{}               -> Debug
        MsgNodeTip{}                       -> Debug
        MsgProtocolParameters{}            -> Info
        MsgLocalStateQueryError{}          -> Error
        MsgLocalStateQueryEraMismatch{}    -> Debug
        MsgAccountDelegationAndRewards{}   -> Debug
        MsgDestroyCursor{}                 -> Debug
        MsgWillQueryRewardsForStake{}      -> Info
        MsgFetchStakePoolsData{}           -> Debug
        MsgFetchStakePoolsDataSummary{}    -> Info
        MsgWatcherUpdate{}                 -> Debug
        MsgInterpreter{}                   -> Debug
        MsgQuery _ msg                     -> getSeverityAnnotation msg
        MsgQueryTime qry dt
            | isSlowQuery qry dt           -> Notice
            | otherwise                    -> Debug
        MsgInterpreterLog msg              -> getSeverityAnnotation msg
        MsgFetchRewardAccountBalance{}       -> Debug
        MsgObserverLog (MsgDidChange _)    -> Notice
        MsgObserverLog{}                   -> Debug

data ObserverLog key value
    = MsgWillFetch (Set key)
    | MsgDidFetch (Map key value)
    | MsgDidChange (Map key value)
    | MsgAddedObserver key
    | MsgRemovedObserver key
    deriving (Eq, Show)

instance (Ord key, Buildable key, Buildable value)
    => ToText (ObserverLog key value) where
    toText (MsgWillFetch keys) = mconcat
        [ "Will fetch values for keys "
        , fmt $ listF keys
        ]
    toText (MsgDidFetch m) = mconcat
        [ "Did fetch values "
        , fmt $ mapF m
        ]
    toText (MsgDidChange m) = mconcat
        [ "New values: "
        , fmt $ mapF m
        ]
    toText (MsgAddedObserver key) = mconcat
        [ "Started observing values for key "
        , pretty key
        ]
    toText (MsgRemovedObserver key) = mconcat
        [ "Stopped observing values for key "
        , pretty key
        ]

{-------------------------------------------------------------------------------
    Local State Query Helpers
-------------------------------------------------------------------------------}

byronOrShelleyBased
    :: LSQ Byron.ByronBlock m a
    -> (forall shelleyEra. LSQ (Shelley.ShelleyBlock (shelleyEra StandardCrypto)) m a)
    -> LSQ (CardanoBlock StandardCrypto) m a
byronOrShelleyBased onByron onShelleyBased = onAnyEra
    onByron
    onShelleyBased
    onShelleyBased
    onShelleyBased
    onShelleyBased

-- | Create a local state query specific to the each era.
--
-- This combinator treats @MismatchEraInfo@ as impossible, which is true if the
-- @LSQEra@ value the @LSQ@ interpreter uses always matches the era of the
-- acquired point.
--
-- Where possible, the more convenient @shelleyBased@ or @byronOrShelleyBased@
-- should be used. This more raw helper was added to simplify dealing with
-- @PParams@ in alonzo.
onAnyEra
    :: LSQ Byron.ByronBlock m a
    -> LSQ (Shelley.ShelleyBlock StandardShelley) m a
    -> LSQ (Shelley.ShelleyBlock StandardAllegra) m a
    -> LSQ (Shelley.ShelleyBlock StandardMary) m a
    -> LSQ (Shelley.ShelleyBlock StandardAlonzo) m a
    -> LSQ (CardanoBlock StandardCrypto) m a
onAnyEra onByron onShelley onAllegra onMary onAlonzo = currentEra >>= \case
    AnyCardanoEra ByronEra -> mapQuery QueryIfCurrentByron onByron
    AnyCardanoEra ShelleyEra -> mapQuery QueryIfCurrentShelley onShelley
    AnyCardanoEra AllegraEra -> mapQuery QueryIfCurrentAllegra onAllegra
    AnyCardanoEra MaryEra -> mapQuery QueryIfCurrentMary onMary
    AnyCardanoEra AlonzoEra -> mapQuery QueryIfCurrentAlonzo onAlonzo
  where
    mapQuery
        :: (forall r. BlockQuery block1 r
            -> BlockQuery block2
                ((Either (MismatchEraInfo (CardanoEras StandardCrypto))) r))
        -> LSQ block1 m a
        -> LSQ block2 m a
    mapQuery _ (LSQPure x) = LSQPure x
    mapQuery f (LSQBind ma f') = LSQBind (mapQuery f ma) (mapQuery f . f')
    mapQuery f (LSQry q) = unwrap <$> LSQry (f q)

    unwrap = either (error "impossible: byronOrShelleyBased query resulted in an \
        \era mismatch") id


-- | Return Nothings in Byron, or @Just result@ in Shelley.
shelleyBased
    :: (forall shelleyEra. LSQ (Shelley.ShelleyBlock (shelleyEra StandardCrypto)) m a)
    -> LSQ (CardanoBlock StandardCrypto) m (Maybe a)
shelleyBased onShelleyBased = byronOrShelleyBased
    (pure Nothing) -- on byron
    (Just <$> onShelleyBased)

-- NOTE:
-- In theory we should be able to know the current era from the tip sync
-- client. But there are is a problem from the combination of:
-- 1. We can't tell the era from a rollback message
-- 2. The initial tip we get is from a rollback message
--
-- which would make us unable to send Local State Queries until the node has
-- updated its tip once.
currentEra :: LSQ (CardanoBlock StandardCrypto) m AnyCardanoEra
currentEra = eraIndexToAnyCardanoEra <$> LSQry (QueryHardFork GetCurrentEra)

-- | Provides a mapping from 'EraIndex' to 'AnyCardanoEra'.
--
-- This mapping replaces a conversion between enumerations.
--
-- The following is used as a reference for the index mapping:
-- https://github.com/input-output-hk/cardano-node/blob/3531289c9f79eab7ac5d3272ce6e6821504fec4c/cardano-api/src/Cardano/Api/Eras.hs#L188
--
eraIndexToAnyCardanoEra :: EraIndex xs -> AnyCardanoEra
eraIndexToAnyCardanoEra index =
    case eraIndexToInt index of
        0 -> AnyCardanoEra ByronEra
        1 -> AnyCardanoEra ShelleyEra
        2 -> AnyCardanoEra AllegraEra
        3 -> AnyCardanoEra MaryEra
        4 -> AnyCardanoEra AlonzoEra
        _ -> error "eraIndexToAnyCardanoEra: unknown era"
