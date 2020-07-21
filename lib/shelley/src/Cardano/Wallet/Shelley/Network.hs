{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

    , NodePoolLsqData (..)

      -- * Logging
    , NetworkLayerLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Byron.Compatibility
    ( byronCodecConfig, protocolParametersFromUpdateState )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Cardano.Wallet.Network
    ( Cursor
    , ErrGetAccountBalance (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , GetStakeDistribution
    , NetworkLayer (..)
    , mapCursor
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, interpreterFromGenesis, mkTimeInterpreter )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , TPraosStandardCrypto
    , fromCardanoHash
    , fromChainHash
    , fromNonMyopicMemberRewards
    , fromPoolDistr
    , fromShelleyPParams
    , fromTip
    , fromTip'
    , optimumNumberOfPools
    , toPoint
    , toShelleyCoin
    , toStakeCredential
    , unsealShelleyTx
    )
import Control.Concurrent
    ( ThreadId )
import Control.Concurrent.Async
    ( Async, async, asyncThreadId, cancel, link )
import Control.Concurrent.Chan
    ( dupChan, newChan, readChan, writeChan )
import Control.Exception
    ( throwIO )
import Control.Exception
    ( IOException )
import Control.Monad
    ( forever, unless, (>=>) )
import Control.Monad.Catch
    ( Handler (..) )
import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , TQueue
    , TVar
    , atomically
    , newTMVarM
    , newTQueue
    , newTVar
    , putTMVar
    , readTVar
    , takeTMVar
    , writeTVar
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Control.Retry
    ( RetryPolicyM, RetryStatus (..), capDelay, fibonacciBackoff, recovering )
import Control.Tracer
    ( Tracer, contramap, nullTracer, traceWith )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Function
    ( (&) )
import Data.List
    ( isInfixOf )
import Data.Map
    ( Map )
import Data.Map
    ( (!) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Void
    ( Void )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), listF', mapF, pretty )
import GHC.Stack
    ( prettyCallStack )
import GHC.Stack
    ( HasCallStack )
import Network.Mux
    ( MuxError (..), MuxErrorType (..), WithMuxBearer (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoApplyTxErr
    , CardanoEras
    , CardanoGenTx
    , CodecConfig (..)
    , GenTx (..)
    , Query (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( QueryHardFork (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( MismatchEraInfo )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Interpreter )
import Ouroboros.Consensus.HardFork.History.Summary
    ( PastHorizonException (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), DefaultCodecs, clientCodecs, defaultCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( HasNetworkProtocolVersion (..), SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Shelley.Ledger
    ( Crypto )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Shelley.Protocol
    ( TPraosCrypto )
import Ouroboros.Network.Block
    ( Point
    , SlotNo (..)
    , Tip (..)
    , blockPoint
    , castTip
    , genesisPoint
    , getPoint
    , getTipPoint
    , pointHash
    , pointSlot
    )
import Ouroboros.Network.Client.Wallet
    ( ChainSyncCmd (..)
    , ChainSyncLog (..)
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
    , NodeToClientVersion (..)
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
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( localStateQueryClientPeer )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure, LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission, SubmitResult (..) )
import System.IO.Error
    ( isDoesNotExistError )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Term as CBOR
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL

{- HLINT ignore "Use readTVarIO" -}

-- | Network layer cursor for Shelley. Mostly useless since the protocol itself is
-- stateful and the node's keep track of the associated connection's cursor.
data instance Cursor (m Shelley) = Cursor
    (Async ())
    (Point (CardanoBlock TPraosStandardCrypto))
    (TQueue m (ChainSyncCmd (CardanoBlock TPraosStandardCrypto) m))

-- | Create an instance of the network layer
withNetworkLayer
    :: forall sc a. (sc ~ TPraosStandardCrypto)
    => Tracer IO (NetworkLayerLog sc)
        -- ^ Logging of network layer startup
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> FilePath
        -- ^ Socket for communicating with the node
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
        -- ^ Codecs for the node's client
    -> (NetworkLayer IO (IO Shelley) (CardanoBlock sc) -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr np addrInfo versionData action = do
    -- NOTE: We keep client connections running for accessing the node tip,
    -- submitting transactions, querying parameters and delegations/rewards.
    --
    -- It is safe to retry when the connection is lost here because this client
    -- doesn't really do anything but sending messages to get the node's tip. It
    -- doesn't rely on the intersection to be up-to-date.
    let handlers = retryOnConnectionLost tr

    (nodeTipChan, protocolParamsVar, interpreterVar, localTxSubmissionQ) <-
        connectNodeTipClient handlers

    queryRewardQ <- connectDelegationRewardsClient handlers

    nodeTipVar <- atomically $ newTVar TipGenesis :: IO (TVar IO (Tip (CardanoBlock sc)))
    let updateNodeTip = readChan nodeTipChan >>= (atomically . writeTVar nodeTipVar)
    link =<< async (forever updateNodeTip)

    action $ NetworkLayer
            { currentNodeTip = liftIO $ _currentNodeTip nodeTipVar
            , watchNodeTip = _watchNodeTip nodeTipChan
            , nextBlocks = _nextBlocks
            , initCursor = _initCursor
            , destroyCursor = _destroyCursor
            , cursorSlotNo = _cursorSlotNo
            , getProtocolParameters = atomically $ readTVar protocolParamsVar
            , postTx = _postSealedTx localTxSubmissionQ
            , stakeDistribution = _stakeDistribution queryRewardQ
            , getAccountBalance = _getAccountBalance nodeTipVar queryRewardQ
            , timeInterpreter = _timeInterpreterQuery interpreterVar
            }
  where
    gp@W.GenesisParameters
        { getGenesisBlockHash
        , getGenesisBlockDate
        } = W.genesisParameters np
    cfg = codecConfig gp

    connectNodeTipClient handlers = do
        localTxSubmissionQ <- atomically newTQueue
        nodeTipChan <- newChan
        protocolParamsVar <- atomically $ newTVar $ W.protocolParameters np
        interpreterVar <- atomically $ newTVar Nothing
        nodeTipClient <- mkTipSyncClient tr np
            localTxSubmissionQ
            (writeChan nodeTipChan)
            (atomically . writeTVar protocolParamsVar)
            (atomically . writeTVar interpreterVar . Just)
        link =<< async (connectClient tr handlers nodeTipClient versionData addrInfo)
        pure (nodeTipChan, protocolParamsVar, interpreterVar, localTxSubmissionQ)

    connectDelegationRewardsClient handlers = do
        cmdQ <- atomically newTQueue
        let cl = mkDelegationRewardsClient tr cfg cmdQ
        link =<< async (connectClient tr handlers cl versionData addrInfo)
        pure cmdQ

    _initCursor headers = do
        chainSyncQ <- atomically newTQueue
        client <- mkWalletClient (contramap MsgChainSyncCmd tr) gp chainSyncQ
        let handlers = failOnConnectionLost tr
        thread <- async (connectClient tr handlers client versionData addrInfo)
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

    _getAccountBalance nodeTipVar queryRewardQ acct = do
        tip <- liftIO . atomically $ readTVar nodeTipVar
        let bh = fromTip' gp tip
        liftIO $ traceWith tr $ MsgGetRewardAccountBalance bh acct
        let cred = toStakeCredential acct
        let q = QueryIfCurrentShelley (Shelley.GetFilteredDelegationsAndRewardAccounts (Set.singleton cred))
        let cmd = CmdQueryLocalState (getTipPoint tip) q
        liftIO (queryRewardQ `send` cmd) >>= \case
            Right (Right (deleg, rewardAccounts)) -> do
                liftIO $ traceWith tr $
                    MsgAccountDelegationAndRewards acct deleg rewardAccounts
                case Map.elems rewardAccounts of
                    [SL.Coin amt] -> pure (Quantity (fromIntegral amt))
                    _ -> throwE $ ErrGetAccountBalanceAccountNotFound acct
            Right (Left _) -> pure minBound -- wrong era
            Left acqFail -> do
                -- NOTE: this could possibly happen in rare circumstances when
                -- the chain is switched and the local state query is made
                -- before the node tip variable is updated.
                liftIO $ traceWith tr $
                    MsgLocalStateQueryError DelegationRewardsClient $
                    show acqFail
                throwE $ ErrGetAccountBalanceNetworkUnreachable $
                    ErrNetworkUnreachable $
                    T.pack $ "Unexpected " ++ show acqFail

    _currentNodeTip nodeTipVar =
        fromTip getGenesisBlockHash <$>
            atomically (readTVar nodeTipVar)

    _postTx localTxSubmissionQ tx = do
        liftIO $ traceWith tr $ MsgPostTx tx
        result <- liftIO $ localTxSubmissionQ `send` CmdSubmitTx tx
        case result of
            SubmitSuccess -> pure ()
            SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)

    -- fixme: only shelley transactions can be submitted like this, because they
    -- are deserialised as shelley transactions before submitting.
    _postSealedTx localTxSubmissionQ tx = do
        liftIO $ traceWith tr $ MsgPostSealedTx tx
        _postTx localTxSubmissionQ (unsealShelleyTx tx)

    handleQueryFailure :: forall e r. Show e => IO (Either e r) -> ExceptT ErrNetworkUnavailable IO r
    handleQueryFailure =
        withExceptT (\e -> ErrNetworkUnreachable $ T.pack $ "Unexpected " ++ show e) . ExceptT

    _stakeDistribution queue pt coin = do
        stakeMap <- handleQueryFailure
            (queue `send` CmdQueryLocalState pt (QueryIfCurrentShelley Shelley.GetStakeDistribution))
        let toStake = Set.singleton $ Left $ toShelleyCoin coin
        liftIO $ traceWith tr $ MsgWillQueryRewardsForStake coin
        rewardsPerAccount <- handleQueryFailure
            (queue `send` CmdQueryLocalState pt (QueryIfCurrentShelley (Shelley.GetNonMyopicMemberRewards toStake)))
        pparams <- handleQueryFailure
            (queue `send` CmdQueryLocalState pt (QueryIfCurrentShelley Shelley.GetCurrentPParams))

        let fromJustRewards = fromMaybe (error "stakeDistribution: requested rewards not included in response")
        let getRewardMap = fromJustRewards . Map.lookup (Left coin) . fromNonMyopicMemberRewards

        -- The result will be Nothing if query occurs during the byron era
        let mres = eitherToMaybe $ NodePoolLsqData
                <$> fmap optimumNumberOfPools pparams
                <*> fmap getRewardMap rewardsPerAccount
                <*> fmap fromPoolDistr stakeMap
        liftIO $ traceWith tr $ MsgFetchedNodePoolLsqData mres
        case mres of
            Just res -> do
                liftIO $ traceWith tr $ MsgFetchedNodePoolLsqDataSummary
                    (Map.size $ stake res)
                    (Map.size $ rewards res)
                return res
            Nothing -> pure $ NodePoolLsqData 0 mempty mempty

    _watchNodeTip nodeTipChan cb = do
        chan <- dupChan nodeTipChan
        let toBlockHeader = fromTip getGenesisBlockHash
        forever $ do
            header <- toBlockHeader <$> readChan chan
            bracketTracer (contramap (MsgWatcherUpdate header) tr) $
                cb header

    _timeInterpreterQuery :: HasCallStack => TVar IO (Maybe (CardanoInterpreter sc)) -> TimeInterpreter IO
    _timeInterpreterQuery var query = do
        cached <- atomically (readTVar var)
        let interpret = maybe (interpreterFromGenesis gp) (mkTimeInterpreter getGenesisBlockDate) cached
        case interpret query of
            Right res -> pure res
            Left e -> do
                traceWith tr $ MsgInterpreterPastHorizon (pretty query) e
                throwIO e

type instance GetStakeDistribution (IO Shelley) (CardanoBlock sc) m
    = (Point (CardanoBlock sc)
   -> W.Coin
   -> ExceptT ErrNetworkUnavailable m NodePoolLsqData)

data NodePoolLsqData = NodePoolLsqData
    { nOpt :: Int
    , rewards :: Map W.PoolId (Quantity "lovelace" Word64)
    , stake :: Map W.PoolId Percentage
    } deriving (Show, Eq)

instance Buildable NodePoolLsqData where
    build NodePoolLsqData{nOpt,rewards,stake} = listF' id
        [ "Stake: " <> mapF (Map.toList stake)
        , "Non-myopic member rewards: " <> mapF (Map.toList rewards)
        , "Optimum number of pools: " <> pretty nOpt
        ]

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
    :: (MonadThrow m, MonadST m, MonadTimer m, MonadAsync m, Crypto sc, TPraosCrypto sc)
    => Tracer m (ChainSyncLog Text Text)
    -> W.GenesisParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd (CardanoBlock sc) m)
        -- ^ Communication channel with the ChainSync client
    -> m (NetworkClient m)
mkWalletClient tr gp chainSyncQ = do
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
        NodeToClientV_3
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
    cfg = codecConfig gp

-- | Construct a network client with the given communication channel, for the
-- purposes of querying delegations and rewards.
mkDelegationRewardsClient
    :: forall sc m. (MonadThrow m, MonadST m, MonadTimer m, TPraosCrypto sc)
    => Tracer m (NetworkLayerLog sc)
        -- ^ Base trace for underlying protocols
    -> CodecConfig (CardanoBlock sc)
    -> TQueue m (LocalStateQueryCmd (CardanoBlock sc) m)
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
        NodeToClientV_3
  where
    tr' = contramap (MsgLocalStateQuery DelegationRewardsClient) tr
    codec = cStateQueryCodec (serialisedCodecs cfg)

{-------------------------------------------------------------------------------
                                     Codecs
-------------------------------------------------------------------------------}

-- | The protocol client version. Distinct from the codecs version.
nodeToClientVersion :: NodeToClientVersion
nodeToClientVersion = NodeToClientV_3

codecVersion :: forall sc. TPraosCrypto sc => BlockNodeToClientVersion (CardanoBlock sc)
codecVersion = verMap ! nodeToClientVersion
    where verMap = supportedNodeToClientVersions (Proxy @(CardanoBlock sc))

codecConfig :: W.GenesisParameters -> CodecConfig (CardanoBlock sc)
codecConfig gp = CardanoCodecConfig (byronCodecConfig gp) ShelleyCodecConfig

-- | A group of codecs which will deserialise block data.
codecs
    :: forall m sc. (MonadST m, TPraosCrypto sc)
    => CodecConfig (CardanoBlock sc)
    -> ClientCodecs (CardanoBlock sc) m
codecs cfg = clientCodecs cfg codecVersion

-- | A group of codecs which won't deserialise block data. Often only the block
-- headers are needed. It's more efficient and easier not to deserialise.
serialisedCodecs
    :: forall m sc. (MonadST m, TPraosCrypto sc)
    => CodecConfig (CardanoBlock sc)
    -> DefaultCodecs (CardanoBlock sc) m
serialisedCodecs cfg = defaultCodecs cfg codecVersion

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
    :: forall sc m. (HasCallStack, MonadThrow m, MonadST m, MonadTimer m, TPraosCrypto sc)
    => Tracer m (NetworkLayerLog sc)
        -- ^ Base trace for underlying protocols
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> TQueue m
        (LocalTxSubmissionCmd
            (GenTx (CardanoBlock sc))
            (CardanoApplyTxErr sc)
            m)
        -- ^ Communication channel with the LocalTxSubmission client
    -> (Tip (CardanoBlock sc) -> m ())
        -- ^ Notifier callback for when tip changes
    -> (W.ProtocolParameters -> m ())
        -- ^ Notifier callback for when parameters for tip change.
    -> (CardanoInterpreter sc -> m ())
        -- ^ Notifier callback for when time interpreter is updates
    -> m (NetworkClient m)
mkTipSyncClient tr np localTxSubmissionQ onTipUpdate onPParamsUpdate onInterpreterUpdate = do
    localStateQueryQ <- atomically newTQueue

    (onPParamsUpdate' :: W.ProtocolParameters -> m ()) <-
        debounce $ \pp -> do
            traceWith tr $ MsgProtocolParameters pp
            onPParamsUpdate pp

    let
        queryLocalState
            :: Point (CardanoBlock sc)
            -> m ()
        queryLocalState pt = do
            pp <- localStateQueryQ `send`
                CmdQueryLocalState pt (QueryIfCurrentShelley Shelley.GetCurrentPParams)
            handleParamsUpdate fromShelleyPParams pp

            st <- localStateQueryQ `send`
                CmdQueryLocalState pt (QueryIfCurrentByron Byron.GetUpdateInterfaceState)
            handleParamsUpdate protocolParametersFromUpdateState st

        handleParamsUpdate
            :: (p -> W.ProtocolParameters)
            -> Either AcquireFailure (Either (MismatchEraInfo (CardanoEras sc)) p)
            -> m ()
        handleParamsUpdate convert = \case
            Left (e :: AcquireFailure) ->
                traceWith tr $ MsgLocalStateQueryError TipSyncClient $ show e
            Right (Right ls) ->
                    onPParamsUpdate' $ convert ls
            Right (Left mismatch) ->
                traceWith tr $ MsgLocalStateQueryEraMismatch mismatch

        queryInterpreter
            :: Point (CardanoBlock sc)
            -> m ()
        queryInterpreter pt = do
            res <- localStateQueryQ `send` CmdQueryLocalState pt (QueryHardFork GetInterpreter)
            case res of
                Left (e :: AcquireFailure) ->
                    traceWith tr $ MsgLocalStateQueryError TipSyncClient $ show e
                Right interpreter -> do
                    traceWith tr $ MsgInterpreter interpreter
                    onInterpreterUpdate interpreter

        gp@W.GenesisParameters
             { getGenesisBlockHash
             } = W.genesisParameters np
        cfg = codecConfig gp

    onTipUpdate' <- debounce @(Tip (CardanoBlock sc)) @m $ \tip' -> do
        let tip = castTip tip'
        traceWith tr $ MsgNodeTip $
            fromTip getGenesisBlockHash tip
        onTipUpdate tip
        queryLocalState (getTipPoint tip)
        -- NOTE: interpeter is updated every block. This is far more often than
        -- necessary.
        queryInterpreter (getTipPoint tip)

    pure $ nodeToClientProtocols (const $ return $ NodeToClientProtocols
        { localChainSyncProtocol =
            let
                codec = cChainSyncCodec $ serialisedCodecs @m cfg
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer nullTracer codec channel
                $ chainSyncClientPeer
                $ chainSyncFollowTip onTipUpdate'

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
        NodeToClientV_3


-- | Return a function to run an action only if its single parameter has changed
-- since the previous time it was called.
debounce :: (Eq a, MonadSTM m) => (a -> m ()) -> m (a -> m ())
debounce action = do
    mvar <- newTMVarM Nothing
    pure $ \cur -> do
        prev <- atomically $ takeTMVar mvar
        unless (Just cur == prev) $ action cur
        atomically $ putTMVar mvar (Just cur)

-- | A protocol client that will never leave the initial state.
doNothingProtocol
    :: MonadTimer m => RunMiniProtocol 'InitiatorMode ByteString m a Void
doNothingProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

-- Connect a client to a network, see `mkWalletClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkWalletClient tr gp queue) mainnetVersionData addrInfo
connectClient
    :: Tracer IO (NetworkLayerLog sc)
    -> [RetryStatus -> Handler IO Bool]
    -> NetworkClient IO
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
    -> FilePath
    -> IO ()
connectClient tr handlers client (vData, vCodec) addr = withIOManager $ \iocp -> do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions nodeToClientVersion vData vDict client
    let tracers = NetworkConnectTracers
            { nctMuxTracer = nullTracer
            , nctHandshakeTracer = contramap MsgHandshakeTracer tr
            }
    let socket = localSnocket iocp addr
    recovering policy handlers $ \status -> do
        traceWith tr $ MsgCouldntConnect (rsIterNumber status)
        connectTo socket tracers versions addr
  where
    -- .25s -> .25s -> .5s → .75s → 1.25s → 2s
    policy :: RetryPolicyM IO
    policy = fibonacciBackoff 250_000 & capDelay 2_000_000

-- | Handlers that are retrying on every connection lost.
retryOnConnectionLost
    :: Tracer IO (NetworkLayerLog sc)
    -> [RetryStatus -> Handler IO Bool]
retryOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' True
    , const $ Handler $ handleMuxError tr' True
    ]
  where
    tr' = contramap MsgConnectionLost tr

-- | Handlers that are failing if the connection is lost
failOnConnectionLost
    :: Tracer IO (NetworkLayerLog sc)
    -> [RetryStatus -> Handler IO Bool]
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

    | isResourceVanishedError e = do
        traceWith tr $ Just e
        pure onResourceVanished

    | otherwise =
        pure False
  where
    isResourceVanishedError = isInfixOf "resource vanished" . show

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
    MuxShutdown -> pure False
    MuxIOException e ->
        handleIOException tr onResourceVanished e
    MuxBearerClosed -> do
        traceWith tr Nothing
        pure onResourceVanished

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data NetworkLayerLog sc where
    MsgCouldntConnect :: Int -> NetworkLayerLog sc
    MsgConnectionLost :: Maybe IOException -> NetworkLayerLog sc
    MsgTxSubmission
        :: (TraceSendRecv
            (LocalTxSubmission (GenTx (CardanoBlock sc)) (CardanoApplyTxErr sc)))
        -> NetworkLayerLog sc
    MsgLocalStateQuery
        :: QueryClientName
        -> (TraceSendRecv
            (LocalStateQuery (CardanoBlock sc) (Query (CardanoBlock sc))))
        -> NetworkLayerLog sc
    MsgHandshakeTracer ::
      (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace) -> NetworkLayerLog sc
    MsgFindIntersection :: [W.BlockHeader] -> NetworkLayerLog sc
    MsgIntersectionFound :: (W.Hash "BlockHeader") -> NetworkLayerLog sc
    MsgFindIntersectionTimeout :: NetworkLayerLog sc
    MsgPostTx :: CardanoGenTx sc -> NetworkLayerLog sc
    MsgPostSealedTx :: W.SealedTx -> NetworkLayerLog sc
    MsgNodeTip :: W.BlockHeader -> NetworkLayerLog sc
    MsgProtocolParameters :: W.ProtocolParameters -> NetworkLayerLog sc
    MsgLocalStateQueryError :: QueryClientName -> String -> NetworkLayerLog sc
    MsgLocalStateQueryEraMismatch :: MismatchEraInfo (CardanoEras sc) -> NetworkLayerLog sc
    MsgGetRewardAccountBalance :: W.BlockHeader -> W.ChimericAccount -> NetworkLayerLog sc
    MsgAccountDelegationAndRewards
        :: W.ChimericAccount
        -> (Map (SL.Credential 'SL.Staking sc) (SL.KeyHash 'SL.StakePool sc))
        -> SL.RewardAccounts sc
        -> NetworkLayerLog sc
    MsgDestroyCursor :: ThreadId -> NetworkLayerLog sc
    MsgWillQueryRewardsForStake :: W.Coin -> NetworkLayerLog sc
    MsgFetchedNodePoolLsqData :: Maybe NodePoolLsqData -> NetworkLayerLog sc
    MsgFetchedNodePoolLsqDataSummary :: Int -> Int -> NetworkLayerLog sc
      -- ^ Number of pools in stake distribution, and rewards map,
      -- respectively.
    MsgWatcherUpdate :: W.BlockHeader -> BracketLog -> NetworkLayerLog sc
    MsgChainSyncCmd :: (ChainSyncLog Text Text) -> NetworkLayerLog sc
    MsgInterpreter :: CardanoInterpreter sc -> NetworkLayerLog sc
    MsgInterpreterPastHorizon :: Text -> PastHorizonException -> NetworkLayerLog sc

data QueryClientName
    = TipSyncClient
    | DelegationRewardsClient
    deriving (Show, Eq)

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)

instance TPraosCrypto sc => ToText (NetworkLayerLog sc) where
    toText = \case
        MsgCouldntConnect n -> T.unwords
            [ "Couldn't connect to node (x" <> toText (n + 1) <> ")."
            , "Retrying in a bit..."
            ]
        MsgConnectionLost Nothing  ->
            "Connection lost with the node."
        MsgConnectionLost (Just e) -> T.unwords
            [ toText @(NetworkLayerLog sc) (MsgConnectionLost Nothing)
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
        MsgPostSealedTx (W.SealedTx bytes) -> T.unwords
            [ "Posting transaction, serialized as:"
            , T.decodeUtf8 $ convertToBase Base16 bytes
            ]
        MsgPostTx genTx -> T.unwords
            [ "Posting transaction:"
            , T.pack $ show genTx
            ]
        MsgLocalStateQuery client msg ->
            T.pack (show client <> " " <> show msg)
        MsgNodeTip bh -> T.unwords
            [ "Network node tip is"
            , pretty bh
            ]
        MsgProtocolParameters params -> T.unlines
            [ "Protocol parameters for tip are:"
            , pretty params
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
        MsgGetRewardAccountBalance bh acct -> T.unwords
            [ "Querying the reward account balance for"
            , pretty acct
            , "at"
            , pretty bh
            ]
        MsgAccountDelegationAndRewards acct delegations rewards -> T.unlines
            [ "Result for account " <> pretty acct <> ":"
            , "  delegations = " <> T.pack (show delegations)
            , "  rewards = " <> T.pack (show rewards)
            ]
        MsgDestroyCursor threadId -> T.unwords
            [ "Destroying cursor connection at"
            , T.pack (show threadId)
            ]
        MsgWillQueryRewardsForStake c ->
            "Will query non-myopic rewards using the stake " <> pretty c
        MsgFetchedNodePoolLsqData d ->
            "Fetched pool data from node tip using LSQ: " <> pretty d
        MsgFetchedNodePoolLsqDataSummary inStake inRewards -> mconcat
            [ "Fetched pool data from node tip using LSQ. Got "
            , T.pack (show inStake)
            , " pools in the stake distribution, and "
            , T.pack (show inRewards)
            , " pools in the non-myopic member reward map."
            ]
        MsgWatcherUpdate tip b ->
            "Update watcher with tip: " <> pretty tip <>
            ". Callback " <> toText b <> "."
        MsgChainSyncCmd a -> toText a
        MsgInterpreter interpreter ->
            "Updated the history interpreter: " <> T.pack (show interpreter)
        MsgInterpreterPastHorizon query (PastHorizon callstack eras) ->
            "Time interpreter queried past the horizon. " <>
            "This should not have happened.\n" <>
            "Query is:\n" <> query <> "\n" <>
            "Eras are:\n" <>
            T.unlines (map (T.pack . show) eras) <> "\n" <>
            T.pack (prettyCallStack callstack)

instance HasPrivacyAnnotation (NetworkLayerLog b)
instance HasSeverityAnnotation (NetworkLayerLog b) where
    getSeverityAnnotation = \case
        MsgCouldntConnect 0                -> Debug
        MsgCouldntConnect 1                -> Notice
        MsgCouldntConnect{}                -> Warning
        MsgConnectionLost{}                -> Warning
        MsgTxSubmission{}                  -> Info
        MsgHandshakeTracer{}               -> Info
        MsgFindIntersectionTimeout         -> Warning
        MsgFindIntersection{}              -> Info
        MsgIntersectionFound{}             -> Info
        MsgPostSealedTx{}                  -> Debug
        MsgPostTx{}                        -> Debug
        MsgLocalStateQuery{}               -> Debug
        MsgNodeTip{}                       -> Debug
        MsgProtocolParameters{}            -> Info
        MsgLocalStateQueryError{}          -> Error
        MsgLocalStateQueryEraMismatch{}    -> Debug
        MsgGetRewardAccountBalance{}       -> Info
        MsgAccountDelegationAndRewards{}   -> Info
        MsgDestroyCursor{}                 -> Notice
        MsgWillQueryRewardsForStake{}      -> Info
        MsgFetchedNodePoolLsqData{}        -> Debug
        MsgFetchedNodePoolLsqDataSummary{} -> Info
        MsgWatcherUpdate{}                 -> Debug
        MsgChainSyncCmd cmd                -> getSeverityAnnotation cmd
        MsgInterpreter{}                   -> Debug
        MsgInterpreterPastHorizon{}        -> Critical
