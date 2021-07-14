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
{-# LANGUAGE PatternSynonyms #-}
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
      pattern Cursor
    , withNetworkLayer

    , Observer (query,startObserving,stopObserving)
    , newObserver
    , ObserverLog (..)

      -- * Logging
    , NetworkLayerLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConn, nodeSocketFile )
import Cardano.Wallet.Byron.Compatibility
    ( byronCodecConfig, protocolParametersFromUpdateState )
import Cardano.Wallet.Logging
    ( BracketLog (..), bracketTracer, produceTimings )
import Cardano.Wallet.Network
    ( Cursor, ErrPostTx (..), NetworkLayer (..), mapCursor )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , TimeInterpreterLog
    , currentRelativeTime
    , mkTimeInterpreter
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance )
import Cardano.Wallet.Shelley.Compatibility
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , StandardCrypto
    , fromCardanoHash
    , fromChainHash
    , fromNonMyopicMemberRewards
    , fromPoolDistr
    , fromShelleyCoin
    , fromShelleyPParams
    , fromStakeCredential
    , fromTip
    , fromTip'
    , nodeToClientVersion
    , optimumNumberOfPools
    , slottingParametersFromGenesis
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
    , newEmptyTMVar
    , newTMVarIO
    , newTQueue
    , newTVar
    , putTMVar
    , readTMVar
    , readTVar
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
    ( RetryPolicyM, RetryStatus (..), capDelay, fibonacciBackoff, recovering )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( runIdentity )
import Data.List
    ( isInfixOf )
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
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( DiffTime )
import Data.Void
    ( Void )
import Fmt
    ( Buildable (..), fmt, listF, mapF, pretty )
import GHC.Stack
    ( HasCallStack )
import Network.Mux
    ( MuxError (..), MuxErrorType (..), WithMuxBearer (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoApplyTxErr
    , CardanoEras
    , CodecConfig (..)
    , GenTx (..)
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
    ( Point
    , SlotNo (..)
    , Tip (..)
    , blockPoint
    , genesisPoint
    , getPoint
    , pointHash
    , pointSlot
    )
import Ouroboros.Network.Client.Wallet
    ( ChainSyncCmd (..)
    , ChainSyncLog (..)
    , LSQ (..)
    , LocalStateQueryCmd (..)
    , LocalTxSubmissionCmd (..)
    , chainSyncFollowTip
    , chainSyncWithBlocks
    , localStateQuery
    , localTxSubmission
    , mapChainSyncLog
    , send
    )
import Ouroboros.Network.CodecCBORTerm
    ( CodecCBORTerm )
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
    , NodeToClientVersion
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager
    )
import Ouroboros.Network.Point
    ( WithOrigin (..), fromWithOrigin )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( chainSyncClientPeer )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.Handshake.Version
    ( simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( localStateQueryClientPeer )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission, SubmitResult (..) )
import System.IO.Error
    ( isDoesNotExistError )
import UnliftIO.Async
    ( Async, async, asyncThreadId, cancel, link )
import UnliftIO.Compat
    ( coerceHandlers )
import UnliftIO.Concurrent
    ( ThreadId )
import UnliftIO.Exception
    ( Handler (..), IOException )

import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Wallet.Primitive.SyncProgress as SyncProgress
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Codec.CBOR.Term as CBOR
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL

{- HLINT ignore "Use readTVarIO" -}
{- HLINT ignore "Use newTVarIO" -}
{- HLINT ignore "Use newEmptyTMVarIO" -}

-- | Network layer cursor for Shelley. Mostly useless since the protocol itself is
-- stateful and the node's keep track of the associated connection's cursor.
data instance Cursor = Cursor
    (Async ())
    (Point (CardanoBlock StandardCrypto))
    (TQueue IO (ChainSyncCmd (CardanoBlock StandardCrypto) IO))

-- | Create an instance of the network layer
withNetworkLayer
    :: HasCallStack
    => Tracer IO NetworkLayerLog
        -- ^ Logging of network layer startup
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> CardanoNodeConn
        -- ^ Socket for communicating with the node
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
        -- ^ Codecs for the node's client
    -> SyncTolerance
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr np conn ver tol action = do
    trTimings <- traceQueryTimings tr
    withNetworkLayerBase (tr <> trTimings) np conn ver tol action

withNetworkLayerBase
    :: HasCallStack
    => Tracer IO NetworkLayerLog
    -> W.NetworkParameters
    -> CardanoNodeConn
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
    -> SyncTolerance
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayerBase tr np conn (versionData, _) tol action = do
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
    (readNodeTip, networkParamsVar, interpreterVar, eraVar, localTxSubmissionQ)
        <- connectNodeTipClient handlers

    queryRewardQ <- connectDelegationRewardsClient handlers

    rewardsObserver <- newRewardBalanceFetcher tr readNodeTip queryRewardQ
    let readCurrentNodeEra = atomically $ readTMVar eraVar

    action $ NetworkLayer
        { currentNodeTip =
            fromTip getGenesisBlockHash <$> atomically readNodeTip
        , currentNodeEra =
            -- NOTE: Is not guaranteed to be consistent with @currentNodeTip@
            readCurrentNodeEra
        , watchNodeTip = do
            _watchNodeTip readNodeTip
        , nextBlocks =
            _nextBlocks
        , initCursor =
            _initCursor
        , destroyCursor =
            _destroyCursor
        , cursorSlotNo =
            _cursorSlotNo
        , currentProtocolParameters =
            fst <$> atomically (readTMVar networkParamsVar)
        , currentSlottingParameters =
            snd <$> atomically (readTMVar networkParamsVar)
        , postTx = \sealed -> do
            era <- liftIO readCurrentNodeEra
            _postTx localTxSubmissionQ era sealed
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
    cfg = codecConfig sp

    -- Put if empty, replace if not empty.
    repsertTMVar var x = do
        e <- isEmptyTMVar var
        unless e $ void $ takeTMVar var
        putTMVar var x


    connectNodeTipClient
        :: HasCallStack
        => RetryHandlers
        -> IO ( STM IO (Tip (CardanoBlock StandardCrypto))
              , TMVar IO (W.ProtocolParameters, W.SlottingParameters)
              , TMVar IO (CardanoInterpreter StandardCrypto)
              , TMVar IO AnyCardanoEra
              , TQueue IO (LocalTxSubmissionCmd
                  (GenTx (CardanoBlock StandardCrypto))
                  (CardanoApplyTxErr StandardCrypto)
                  IO)
              )
    connectNodeTipClient handlers = do
        localTxSubmissionQ <- atomically newTQueue
        networkParamsVar <- atomically newEmptyTMVar
        interpreterVar <- atomically newEmptyTMVar
        eraVar <- atomically newEmptyTMVar
        (nodeTipClient, readTip) <- mkTipSyncClient tr np
            localTxSubmissionQ
            (curry (atomically . repsertTMVar networkParamsVar))
            (atomically . repsertTMVar interpreterVar)
            (atomically . repsertTMVar eraVar)
        link =<< async (connectClient tr handlers nodeTipClient versionData conn)
        pure (readTip, networkParamsVar, interpreterVar, eraVar, localTxSubmissionQ)

    connectDelegationRewardsClient
        :: HasCallStack
        => RetryHandlers
        -> IO (TQueue IO
                (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO))
    connectDelegationRewardsClient handlers = do
        cmdQ <- atomically newTQueue
        let cl = mkDelegationRewardsClient tr cfg cmdQ
        link =<< async (connectClient tr handlers cl versionData conn)
        pure cmdQ

    _initCursor :: HasCallStack => [W.BlockHeader] -> IO Cursor
    _initCursor headers = do
        chainSyncQ <- atomically newTQueue
        client <- mkWalletClient (contramap MsgChainSyncCmd tr) cfg gp chainSyncQ
        let handlers = failOnConnectionLost tr
        thread <- async (connectClient tr handlers client versionData conn)
        link thread
        let points = reverse $ genesisPoint :
                (toPoint getGenesisBlockHash <$> headers)
        let findIt = chainSyncQ `send` CmdFindIntersection points
        traceWith tr $ MsgFindIntersection headers
        res <- findIt
        case res of
            Just intersection -> do
                traceWith tr
                    $ MsgIntersectionFound
                    $ fromChainHash getGenesisBlockHash
                    $ pointHash intersection
                pure $ Cursor thread intersection chainSyncQ
            _ -> fail $ unwords
                [ "initCursor: intersection not found? This can't happen"
                , "because we always give at least the genesis point."
                , "Here are the points we gave: " <> show headers
                ]

    _destroyCursor (Cursor thread _ _) = do
        liftIO $ traceWith tr $ MsgDestroyCursor (asyncThreadId thread)
        cancel thread

    _nextBlocks (Cursor thread _ chainSyncQ) = do
        let toCursor point = Cursor thread point chainSyncQ
        liftIO $ mapCursor toCursor <$> chainSyncQ `send` CmdNextBlocks

    _cursorSlotNo (Cursor _ point _) = do
        fromWithOrigin (SlotNo 0) $ pointSlot point

    -- NOTE1: only shelley transactions can be submitted like this, because they
    -- are deserialised as shelley transactions before submitting.
    --
    -- NOTE2: It is not ideal to query the current era again here because we
    -- should in practice use the same era as the one used to construct the
    -- transaction. However, when turning transactions to 'SealedTx', we loose
    -- all form of type-level indicator about the era. The 'SealedTx' type
    -- shouldn't be needed anymore since we've dropped jormungandr, so we could
    -- instead carry a transaction from cardano-api types with proper typing.
    _postTx localTxSubmissionQ era tx = do
        liftIO $ traceWith tr $ MsgPostTx tx
        case era of
            AnyCardanoEra ByronEra ->
                throwE $ ErrPostTxProtocolFailure "Invalid era: Byron"

            AnyCardanoEra ShelleyEra -> do
                let cmd = CmdSubmitTx $ unsealShelleyTx GenTxShelley tx
                result <- liftIO $ localTxSubmissionQ `send` cmd
                case result of
                    SubmitSuccess -> pure ()
                    SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)

            AnyCardanoEra AllegraEra -> do
                let cmd = CmdSubmitTx $ unsealShelleyTx GenTxAllegra tx
                result <- liftIO $ localTxSubmissionQ `send` cmd
                case result of
                    SubmitSuccess -> pure ()
                    SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)

            AnyCardanoEra MaryEra -> do
                let cmd = CmdSubmitTx $ unsealShelleyTx GenTxMary tx
                result <- liftIO $ localTxSubmissionQ `send` cmd
                case result of
                    SubmitSuccess -> pure ()
                    SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)
            AnyCardanoEra AlonzoEra -> do
                let cmd = CmdSubmitTx $ unsealShelleyTx GenTxAlonzo tx
                result <- liftIO $ localTxSubmissionQ `send` cmd
                case result of
                    SubmitSuccess -> pure ()
                    SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)


    _stakeDistribution queue coin = do
        liftIO $ traceWith tr $ MsgWillQueryRewardsForStake coin

        let qry :: LSQ (CardanoBlock StandardCrypto) IO (Maybe W.StakePoolsSummary)
            qry = liftA3 (liftA3 W.StakePoolsSummary)
                getNOpt
                queryNonMyopicMemberRewards
                stakeDistr

        mres <- bracketQuery "stakePoolsSummary" tr $
            queue `send` (SomeLSQ qry )

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
        getNOpt = shelleyBased $
            optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams

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


--------------------------------------------------------------------------------
--
-- Network Client

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
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
    :: (MonadThrow m, MonadST m, MonadTimer m, MonadAsync m)
    => Tracer m (ChainSyncLog Text Text)
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> W.GenesisParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd (CardanoBlock StandardCrypto) m)
        -- ^ Communication channel with the ChainSync client
    -> m (NetworkClient m)
mkWalletClient tr cfg gp chainSyncQ = do
    stash <- atomically newTQueue
    pure $ nodeToClientProtocols (const $ return $ NodeToClientProtocols
        { localChainSyncProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                runPipelinedPeer nullTracer (cChainSyncCodec $ codecs cfg) channel
                $ chainSyncClientPeerPipelined
                $ chainSyncWithBlocks tr' (fromTip' gp) chainSyncQ stash

        , localTxSubmissionProtocol =
            doNothingProtocol

        , localStateQueryProtocol =
            doNothingProtocol
        })
        nodeToClientVersion
  where
    tr' = contramap (mapChainSyncLog showB showP) tr
    showB = showP . blockPoint
    showP p = case (getPoint p) of
        Origin -> "Origin"
        At blk -> mconcat
            [ "(slotNo "
            , T.pack $ show $ unSlotNo $ Point.blockPointSlot blk
            , ", "
            , pretty $ fromCardanoHash $ Point.blockPointHash blk
            , ")"
            ]

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
mkDelegationRewardsClient tr cfg queryRewardQ =
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
        })
        nodeToClientVersion
  where
    tr' = contramap (MsgLocalStateQuery DelegationRewardsClient) tr
    codec = cStateQueryCodec (serialisedCodecs cfg)

{-------------------------------------------------------------------------------
                                     Codecs
-------------------------------------------------------------------------------}

codecVersion :: BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion = verMap ! nodeToClientVersion
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
    => CodecConfig (CardanoBlock StandardCrypto)
    -> ClientCodecs (CardanoBlock StandardCrypto) m
codecs cfg = clientCodecs cfg codecVersion nodeToClientVersion

-- | A group of codecs which won't deserialise block data. Often only the block
-- headers are needed. It's more efficient and easier not to deserialise.
serialisedCodecs
    :: MonadST m
    => CodecConfig (CardanoBlock StandardCrypto)
    -> DefaultCodecs (CardanoBlock StandardCrypto) m
serialisedCodecs cfg = defaultCodecs cfg codecVersion nodeToClientVersion

{-------------------------------------------------------------------------------
                                     Tip sync
-------------------------------------------------------------------------------}

type CardanoInterpreter sc = Interpreter (CardanoEras sc)

-- | Construct a network client with the given communication channel, for the
-- purpose of:
--
--  * Submitting transactions
--  * Tracking the node tip
--  * Tracking the latest protocol parameters state.
--  * Querying the history interpreter as necessary.
mkTipSyncClient
    :: forall m. (HasCallStack, MonadUnliftIO m, MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> TQueue m
        (LocalTxSubmissionCmd
            (GenTx (CardanoBlock StandardCrypto))
            (CardanoApplyTxErr StandardCrypto)
            m)
        -- ^ Communication channel with the LocalTxSubmission client
    -> (W.ProtocolParameters -> W.SlottingParameters -> m ())
        -- ^ Notifier callback for when parameters for tip change.
    -> (CardanoInterpreter StandardCrypto -> m ())
        -- ^ Notifier callback for when time interpreter is updated.
    -> (AnyCardanoEra -> m ())
        -- ^ Notifier callback for when the era is updated
    -> m (NetworkClient m, STM m (Tip (CardanoBlock StandardCrypto)))
mkTipSyncClient tr np localTxSubmissionQ onPParamsUpdate onInterpreterUpdate onEraUpdate = do
    (localStateQueryQ :: TQueue m (LocalStateQueryCmd (CardanoBlock StandardCrypto) m))
        <- atomically newTQueue

    tipVar <- atomically $ newTVar (Just $ AnyCardanoEra ByronEra, TipGenesis)

    (onPParamsUpdate' :: (W.ProtocolParameters, W.SlottingParameters) -> m ()) <-
        debounce $ \(pp, sp) -> do
            traceWith tr $ MsgProtocolParameters pp sp
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
            pp <- byronOrShelleyBased
                (protocolParametersFromUpdateState eraBounds
                    <$> LSQry Byron.GetUpdateInterfaceState)
                (fromShelleyPParams eraBounds
                    <$> LSQry Shelley.GetCurrentPParams)
            return (pp, sp)

    let queryInterpreter = LSQry (QueryHardFork GetInterpreter)

    let cfg = codecConfig (W.slottingParameters np)

    -- NOTE: These are updated every block. This is far more often than
    -- necessary.
    --
    -- By blocking (with `send`) we ensure we don't queue multiple queries.
    let onTipUpdate _tip = do
            let qry = (,,) <$> queryParams <*> queryInterpreter <*> currentEra
            (pparams, int, era) <- localStateQueryQ `send` (SomeLSQ qry)
            onPParamsUpdate' pparams
            onInterpreterUpdate int
            onEraUpdate era

    link =<< async (observeForever (readTVar tipVar) onTipUpdate)


    let client = nodeToClientProtocols (const $ return $ NodeToClientProtocols
            { localChainSyncProtocol =
                let
                    codec = cChainSyncCodec $ codecs cfg
                in
                InitiatorProtocolOnly $ MuxPeerRaw
                    $ \channel -> runPeer nullTracer codec channel
                    $ chainSyncClientPeer
                    $ chainSyncFollowTip toCardanoEra (curry (atomically . writeTVar tipVar))

            , localTxSubmissionProtocol =
                let
                    tr' = contramap MsgTxSubmission tr
                    codec = cTxSubmissionCodec $ serialisedCodecs cfg
                in
                InitiatorProtocolOnly $ MuxPeerRaw
                    $ \channel -> runPeer tr' codec channel
                    $ localTxSubmissionClientPeer
                    $ localTxSubmission localTxSubmissionQ

            , localStateQueryProtocol =
                let
                    tr' = contramap (MsgLocalStateQuery TipSyncClient) tr
                    codec = cStateQueryCodec $ serialisedCodecs cfg
                in
                InitiatorProtocolOnly $ MuxPeerRaw
                    $ \channel -> runPeer tr' codec channel
                    $ localStateQueryClientPeer
                    $ localStateQuery localStateQueryQ
            })
            nodeToClientVersion
    return (client, snd <$> readTVar tipVar)
    -- FIXME: We can remove the era from the tip sync client now.

-- Reward Account Balances

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

        let qry = byronOrShelleyBased (pure (byronValue, [])) $
                   fmap fromBalanceResult
                    . LSQry
                    . Shelley.GetFilteredDelegationsAndRewardAccounts
                    $ Set.map toStakeCredential accounts

        (res,logs) <- bracketQuery "queryRewards" tr (send queryRewardQ (SomeLSQ qry))
        liftIO $ mapM_ (traceWith tr) logs
        return res
  where
    byronValue :: Map W.RewardAccount W.Coin
    byronValue = Map.fromList . map (, minBound) $ Set.toList accounts

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
    cacheVar <- atomically $ newTVar Map.empty
    toBeObservedVar <- atomically $ newTVar Set.empty
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
            , query = \k -> do
                m <- atomically (readTVar cacheVar)
                return $ Map.lookup k m
            }

    refresh
        :: TVar m (Map key value)
        -> TVar m (Set key)
        -> env
        -> m ()
    refresh cacheVar observedKeysVar env = do
        keys <- atomically $ readTVar observedKeysVar
        oldValues <- atomically $ readTVar cacheVar
        traceWith tr $ MsgWillFetch keys
        mvalues <- fetch env keys

        case mvalues of
            Nothing -> pure ()
            Just values -> do
                traceWith tr $ MsgDidFetch values
                when (oldValues /= values) $
                    traceWith tr $ MsgDidChange values
                atomically $ writeTVar cacheVar values

-- | Return a function to run an action only if its single parameter has changed
-- since the previous time it was called.
debounce :: (Eq a, MonadSTM m) => (a -> m ()) -> m (a -> m ())
debounce action = do
    mvar <- newTMVarIO Nothing
    pure $ \cur -> do
        prev <- atomically $ takeTMVar mvar
        unless (Just cur == prev) $ action cur
        atomically $ putTMVar mvar (Just cur)

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

-- | A protocol client that will never leave the initial state.
doNothingProtocol
    :: MonadTimer m => RunMiniProtocol 'InitiatorMode ByteString m a Void
doNothingProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

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
    let versions = simpleSingletonVersions nodeToClientVersion vData client
    let tracers = NetworkConnectTracers
            { nctMuxTracer = nullTracer
            , nctHandshakeTracer = contramap MsgHandshakeTracer tr
            }
    let socket = localSnocket iocp (nodeSocketFile conn)
    recovering policy (coerceHandlers handlers) $ \status -> do
        traceWith tr $ MsgCouldntConnect (rsIterNumber status)
        connectTo socket tracers versions (nodeSocketFile conn)
  where
    -- .25s -> .25s -> .5s → .75s → 1.25s → 2s
    policy :: RetryPolicyM IO
    policy = fibonacciBackoff 250_000 & capDelay 2_000_000

-- | Shorthand for the list of exception handlers used with 'recovering'.
type RetryHandlers = [RetryStatus -> Handler IO Bool]

-- | Handlers that are retrying on every connection lost.
retryOnConnectionLost :: Tracer IO NetworkLayerLog -> RetryHandlers
retryOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' True
    , const $ Handler $ handleMuxError tr' True
    ]
  where
    tr' = contramap MsgConnectionLost tr

-- | Handlers that are failing if the connection is lost
failOnConnectionLost :: Tracer IO NetworkLayerLog -> RetryHandlers
failOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' False
    , const $ Handler $ handleMuxError tr' False
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
    -> IO Bool
handleIOException tr onResourceVanished e
    -- There's a race-condition when starting the wallet and the node at the
    -- same time: the socket might not be there yet when we try to open it.
    -- In such case, we simply retry a bit later and hope it's there.
    | isDoesNotExistError e =
        pure True

    -- If the nonblocking UNIX domain socket connection cannot be completed
    -- immediately (i.e. connect() returns EAGAIN), try again. This happens
    -- because the node's listen queue is quite short.
    | isTryAgainError e =
        pure True

    | isResourceVanishedError e = do
        traceWith tr $ Just e
        pure onResourceVanished

    | otherwise =
        pure False
  where
    isResourceVanishedError = isInfixOf "resource vanished" . show
    isTryAgainError = isInfixOf "resource exhausted" . show

handleMuxError
    :: Tracer IO (Maybe IOException)
    -> Bool -- ^ 'True' = retry on 'ResourceVanishedError'
    -> MuxError
    -> IO Bool
handleMuxError tr onResourceVanished = pure . errorType >=> \case
    MuxUnknownMiniProtocol -> pure False
    MuxDecodeError -> pure False
    MuxIngressQueueOverRun -> pure False
    MuxInitiatorOnly -> pure False
    MuxSDUReadTimeout -> pure False
    MuxSDUWriteTimeout -> pure False
    MuxShutdown _ -> pure False -- fixme: #2212 consider cases
    MuxIOException e ->
        handleIOException tr onResourceVanished e
    MuxBearerClosed -> do
        traceWith tr Nothing
        pure onResourceVanished
    MuxBlockedOnCompletionVar _ -> pure False -- TODO: Is this correct?

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data NetworkLayerLog where
    MsgCouldntConnect :: Int -> NetworkLayerLog
    MsgConnectionLost :: Maybe IOException -> NetworkLayerLog
    MsgTxSubmission
        :: (TraceSendRecv
            (LocalTxSubmission (GenTx (CardanoBlock StandardCrypto)) (CardanoApplyTxErr StandardCrypto)))
        -> NetworkLayerLog
    MsgLocalStateQuery
        :: QueryClientName
        -> (TraceSendRecv
            (LocalStateQuery (CardanoBlock StandardCrypto) (Point (CardanoBlock StandardCrypto)) (Query (CardanoBlock StandardCrypto))))
        -> NetworkLayerLog
    MsgHandshakeTracer ::
      (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace) -> NetworkLayerLog
    MsgFindIntersection :: [W.BlockHeader] -> NetworkLayerLog
    MsgIntersectionFound :: (W.Hash "BlockHeader") -> NetworkLayerLog
    MsgFindIntersectionTimeout :: NetworkLayerLog
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
    MsgChainSyncCmd :: (ChainSyncLog Text Text) -> NetworkLayerLog
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
        MsgFindIntersectionTimeout ->
            "Couldn't find an intersection in a timely manner. Retrying..."
        MsgFindIntersection points -> T.unwords
            [ "Looking for an intersection with the node's local chain with:"
            , T.intercalate ", " (pretty <$> points)
            ]
        MsgIntersectionFound point -> T.unwords
            [ "Intersection found:", pretty point ]
        MsgPostTx (W.SealedTx bytes) -> T.unwords
            [ "Posting transaction, serialized as:"
            , T.decodeUtf8 $ convertToBase Base16 bytes
            ]
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
        MsgChainSyncCmd a -> toText a
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
        MsgFindIntersectionTimeout         -> Warning
        MsgFindIntersection{}              -> Debug
        -- MsgFindIntersection is duplicated by MsgStartFollowing
        MsgIntersectionFound{}             -> Debug
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
        MsgChainSyncCmd cmd                -> getSeverityAnnotation cmd
        MsgInterpreter{}                   -> Debug
        MsgQuery _ msg                     -> getSeverityAnnotation msg
        MsgQueryTime qry dt
            | isSlowQuery qry dt           -> Notice
            | otherwise                    -> Debug
        MsgInterpreterLog msg              -> getSeverityAnnotation msg
        MsgFetchRewardAccountBalance{}       -> Debug
        MsgObserverLog (MsgDidChange _)    -> Notice
        MsgObserverLog{}                   -> Debug



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

--
-- LSQ Helpers
--

-- Create a local state query specific to the current era — either Byron or one
-- of the Shelley-based eras (Shelley, Allegra, Mary).
--
-- This combinator treats @MismatchEraInfo@ as impossible, which is true if the
-- @LSQEra@ value the @LSQ@ interpreter uses always matches the era of the
-- acquired point.
byronOrShelleyBased
    :: LSQ Byron.ByronBlock m a
    -> (forall shelleyEra. WalletSupportedShelleyEra (shelleyEra StandardCrypto) => LSQ (Shelley.ShelleyBlock (shelleyEra StandardCrypto)) m a)
    -> LSQ (CardanoBlock StandardCrypto) m a
byronOrShelleyBased onByron onShelleyBased = currentEra >>= \case
    AnyCardanoEra ByronEra -> mapQuery QueryIfCurrentByron onByron
    AnyCardanoEra ShelleyEra -> mapQuery QueryIfCurrentShelley onShelleyBased
    AnyCardanoEra AllegraEra -> mapQuery QueryIfCurrentAllegra onShelleyBased
    AnyCardanoEra MaryEra -> mapQuery QueryIfCurrentMary onShelleyBased
    AnyCardanoEra AlonzoEra -> error "todo: QueryIfCurrentAlonzo"
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
    :: (forall shelleyEra. WalletSupportedShelleyEra (shelleyEra StandardCrypto) => LSQ (Shelley.ShelleyBlock (shelleyEra StandardCrypto)) m a)
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
        _ -> error "eraIndexToAnyCardanoEra: unknown era"

-- | Workaround to deal with @GetCurrentPParams@ now returning a
-- 'SL.Core.PParams era' type family application, instead of a concrete datatype
-- @SL.PParams era@ abstracted over @era@.
--
-- If we continue getting problems like this, we should maybe switch to rely
-- more on cardano-api.
type WalletSupportedShelleyEra era =
    (SL.Core.PParams era ~ SL.PParams era)
