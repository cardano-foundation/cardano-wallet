{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
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
module Cardano.Wallet.Byron.Network
    ( -- * Top-Level Interface
      pattern Cursor
    , withNetworkLayer

      -- * Logging
    , NetworkLayerLog
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Chain.Byron.API
    ( ApplyMempoolPayloadErr (..) )
import Cardano.Wallet.Byron.Compatibility
    ( Byron
    , fromChainHash
    , fromSlotNo
    , fromTip
    , toEpochSlots
    , toGenTx
    , toPoint
    , txParametersFromUpdateState
    )
import Cardano.Wallet.Network
    ( Cursor
    , ErrGetBlock (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    , mapCursor
    )
import Control.Concurrent.Async
    ( async, link )
import Control.Exception
    ( IOException )
import Control.Monad
    ( forever, unless, (>=>) )
import Control.Monad.Catch
    ( Handler (..) )
import Control.Monad.Class.MonadAsync
    ( MonadAsync (race) )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , TQueue
    , atomically
    , newEmptyTMVarM
    , newTMVarM
    , newTQueue
    , newTVar
    , putTMVar
    , readTQueue
    , readTVar
    , takeTMVar
    , writeTQueue
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
    ( RetryPolicyM
    , RetryStatus (..)
    , capDelay
    , constantDelay
    , fibonacciBackoff
    , recovering
    , retrying
    )
import Control.Tracer
    ( Tracer, contramap, nullTracer, traceWith )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.List
    ( isInfixOf )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Void
    ( Void )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Network.Mux
    ( AppType (..), MuxError (..), MuxErrorType (..), WithMuxBearer )
import Network.TypedProtocol.Codec
    ( Codec )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..)
    , GenTx
    , Query (..)
    , decodeByronBlock
    , decodeByronGenTx
    , decodeByronHeaderHash
    , encodeByronBlock
    , encodeByronGenTx
    , encodeByronHeaderHash
    )
import Ouroboros.Consensus.Byron.Node
    ()
import Ouroboros.Consensus.Node.Run
    ( RunNode (..) )
import Ouroboros.Network.Block
    ( Point (..)
    , Serialised (..)
    , SlotNo (..)
    , Tip (..)
    , blockPoint
    , castTip
    , decodePoint
    , decodeTip
    , encodePoint
    , encodeTip
    , genesisPoint
    , getTipPoint
    , pointHash
    , pointSlot
    , unwrapCBORinCBOR
    )
import Ouroboros.Network.Channel
    ( Channel )
import Ouroboros.Network.Codec
    ( DeserialiseFailure )
import Ouroboros.Network.CodecCBORTerm
    ( CodecCBORTerm )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPeer )
import Ouroboros.Network.Mux
    ( MuxPeer (..), OuroborosApplication (..), RunMiniProtocol (..) )
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
    ( fromWithOrigin )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    , chainSyncClientPeer
    )
import Ouroboros.Network.Protocol.ChainSync.Codec
    ( codecChainSync, codecChainSyncSerialised )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Version
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( ClientStAcquiring (..)
    , ClientStQuerying (..)
    , LocalStateQueryClient (..)
    , localStateQueryClientPeer
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Codec
    ( codecLocalStateQuery )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure, LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..)
    , LocalTxSubmissionClient (..)
    , localTxSubmissionClientPeer
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
    ( codecLocalTxSubmission )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )
import System.IO.Error
    ( isDoesNotExistError )

import qualified Cardano.Chain.Update.Validation.Interface as U
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Term as CBOR
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

{- HLINT ignore "Use readTVarIO" -}

-- | Network layer cursor for Byron. Mostly useless since the protocol itself is
-- stateful and the node's keep track of the associated connection's cursor.
data instance Cursor (m Byron) = Cursor
    (Point ByronBlock)
    (TQueue m (ChainSyncCmd m))

-- | Create an instance of the network layer
withNetworkLayer
    :: Tracer IO NetworkLayerLog
        -- ^ Logging of network layer startup
        -- FIXME: Use a typed message instead of a 'Text'
    -> W.GenesisBlockParameters
        -- ^ Initial blockchain parameters
    -> FilePath
        -- ^ Socket for communicating with the node
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
        -- ^ Codecs for the node's client
    -> (NetworkLayer IO (IO Byron) ByronBlock -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr gbp addrInfo versionData action = do
    localTxSubmissionQ <- atomically newTQueue

    -- NOTE: We keep a client connection running for accessing the node tip,
    -- submitting transactions, and querying parameters.
    -- It is safe to retry when the connection is lost here because this client
    -- doesn't really do anything but sending dummy messages to get the node's
    -- tip. It doesn't rely on the intersection to be up-to-date.
    nodeTipVar <- atomically $ newTVar TipGenesis
    txParamsVar <- atomically $ newTVar (W.txParameters gbp)
    nodeTipClient <- mkTipSyncClient tr
        localTxSubmissionQ
        (atomically . writeTVar nodeTipVar)
        (atomically . writeTVar txParamsVar)
    let handlers = retryOnConnectionLost tr
    link =<< async (connectClient tr handlers (const nodeTipClient) versionData addrInfo)

    action
        NetworkLayer
            { currentNodeTip = liftIO $ _currentNodeTip nodeTipVar
            , nextBlocks = _nextBlocks
            , initCursor = _initCursor
            , cursorSlotId = _cursorSlotId
            , getTxParameters = atomically $ readTVar txParamsVar
            , postTx = _postTx localTxSubmissionQ
            , stakeDistribution = _stakeDistribution
            , getAccountBalance = _getAccountBalance
            }
  where
    bp@W.BlockchainParameters
        { getGenesisBlockHash
        , getEpochLength
        } = W.staticParameters gbp

    _initCursor headers = do
        chainSyncQ <- atomically newTQueue
        let client = const $ mkWalletClient bp chainSyncQ
        let handlers = failOnConnectionLost tr
        link =<< async (connectClient tr handlers client versionData addrInfo)
        let points = reverse $ genesisPoint : (toPoint getGenesisBlockHash getEpochLength <$> headers)
        let policy = constantDelay 500
        let findIt = chainSyncQ `send` CmdFindIntersection points
        traceWith tr $ MsgFindIntersection headers
        let shouldRetry _ = \case
                Left (_ :: ErrNetworkUnavailable) -> do
                    traceWith tr MsgFindIntersectionTimeout
                    pure True
                Right Nothing -> pure False
                Right Just{}  -> pure False
        retrying policy shouldRetry (const findIt) >>= \case
            Right (Just intersection) -> do
                traceWith tr
                    $ MsgIntersectionFound
                    $ fromChainHash getGenesisBlockHash
                    $ pointHash intersection
                pure $ Cursor intersection chainSyncQ
            _ -> fail $ unwords
                [ "initCursor: intersection not found? This can't happen"
                , "because we always give at least the genesis point."
                , "Here are the points we gave: " <> show headers
                ]

    _nextBlocks (Cursor _ chainSyncQ) = do
        let toCursor point = Cursor point chainSyncQ
        fmap (mapCursor toCursor) $ withExceptT ErrGetBlockNetworkUnreachable $
            ExceptT (chainSyncQ `send` CmdNextBlocks)

    _cursorSlotId (Cursor point _) = do
        fromSlotNo getEpochLength $ fromWithOrigin (SlotNo 0) $ pointSlot point

    _getAccountBalance _ =
        pure (Quantity 0)

    _currentNodeTip nodeTipVar =
        fromTip getGenesisBlockHash getEpochLength <$> atomically (readTVar nodeTipVar)

    _postTx localTxSubmissionQ tx = do
        liftIO $ traceWith tr $ MsgPostSealedTx tx
        result <- withExceptT ErrPostTxNetworkUnreachable $
            ExceptT (localTxSubmissionQ `send` CmdSubmitTx (toGenTx tx))
        case result of
            Nothing  -> pure ()
            Just err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)

    _stakeDistribution =
        notImplemented "stakeDistribution"

--------------------------------------------------------------------------------
--
-- Interface with the Network Client


-- | We interact with the 'NetworkClient' via a commands instrumenting the
-- client to move within the state-machine protocol. Commands are sent from a
-- parent thread via a shared 'TQueue'.
--
--
-- MAIN THREAD                   | NETWORK CLIENT THREAD
--                               |
--     *---------------*         |
--     |               |         |
--     | Wallet Engine |         |
--     |               |         |
--     *---------------*         |
--            |  ^               |
--            v  |               |
--     *---------------*         |        *----------------*
--     |               |         |        |                |
--     | Network Layer |<===[ TQueue ]===>| Network Client |
--     |               |         |        |                |
--     *---------------*         |        *----------------*
--                               |                |  ^
--                               |                v  |
--                               |     (ChainSync + TxSubmission)
--
-- The NetworkClient is idling most of the time and blocking on the TQueue while
-- waiting for commands. Upon receiving a command, it interprets it by sending
-- the corresponding instruction to the node and responding via a given
-- callback.
--
-- See also 'send' for invoking commands.
data ChainSyncCmd (m :: * -> *)
    = CmdFindIntersection
        [Point ByronBlock]
        (Maybe (Point ByronBlock) -> m ())
    | CmdNextBlocks
        (NextBlocksResult (Point ByronBlock) ByronBlock -> m ())

-- | Sending command to the localTxSubmission client. See also 'ChainSyncCmd'.
data LocalTxSubmissionCmd (m :: * -> *)
    = CmdSubmitTx
        (GenTx ByronBlock)
        (Maybe ApplyMempoolPayloadErr -> m ())

-- | Command to send to the localStateQuery client. See also 'ChainSyncCmd'.
data LocalStateQueryCmd (m :: * -> *)
    = CmdQueryLocalState
        (Point ByronBlock)
        (LocalStateQueryResult -> m ())

-- | Shorthand for the possible outcomes of acquiring local state parameters.
type LocalStateQueryResult = Either AcquireFailure U.State

-- | Helper function to easily send commands to the node's client and read
-- responses back.
--
-- >>> queue `send` CmdNextBlocks
-- RollForward cursor nodeTip blocks
--
-- >>> queue `send` CmdNextBlocks
-- AwaitReply
send
    :: (MonadSTM m, MonadAsync m, MonadTimer m)
    => TQueue m (cmd m)
    -> ((a -> m ()) -> cmd m)
    -> m (Either ErrNetworkUnavailable a)
send queue cmd = do
    tvar <- newEmptyTMVarM
    atomically $ writeTQueue queue (cmd (atomically . putTMVar tvar))
    race timeout (atomically $ takeTMVar tvar) <&> \case
        Left{}  -> Left (ErrNetworkUnreachable "timeout")
        Right a -> Right a
  where
    timeout = threadDelay 30

--------------------------------------------------------------------------------
--
-- Network Client

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
    'InitiatorApp
        -- Initiator ~ Client (as opposed to Responder / Server)
    ByteString
        -- Concrete representation for bytes string
    m
        -- Underlying monad we run in
    Void
        -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- Irrelevant for 'InitiatorApplication'. Return type of 'Responder'
        -- application.

-- | Construct a network client with the given communication channel, for the
-- purposes of syncing blocks to a single wallet.
mkWalletClient
    :: (MonadThrow m, MonadST m, MonadTimer m)
    => W.BlockchainParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd m)
        -- ^ Communication channel with the ChainSync client
    -> NetworkClient m
mkWalletClient bp chainSyncQ =
    nodeToClientProtocols NodeToClientProtocols
        { localChainSyncProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                chainSyncWithBlocks nullTracer bp chainSyncQ channel
        , localTxSubmissionProtocol = doNothingProtocol
        , localStateQueryProtocol = doNothingProtocol
        }
        NodeToClientV_2

-- | Construct a network client with the given communication channel, for the
-- purpose of:
--
--  * Submitting transactions
--  * Tracking the node tip
--  * Tracking the latest protocol parameters state.
mkTipSyncClient
    :: (MonadThrow m, MonadST m, MonadTimer m, MonadAsync m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> TQueue m (LocalTxSubmissionCmd m)
        -- ^ Communication channel with the LocalTxSubmission client
    -> (Tip ByronBlock -> m ())
        -- ^ Notifier callback for when tip changes
    -> (W.TxParameters -> m ())
        -- ^ Notifier callback for when parameters for tip change.
    -> m (NetworkClient m)
mkTipSyncClient tr localTxSubmissionQ onTipUpdate onTxParamsUpdate = do
    localStateQueryQ <- atomically newTQueue

    onTxParamsUpdate' <- debounce $ \txParams -> do
        traceWith tr $ MsgTxParameters txParams
        onTxParamsUpdate txParams

    let
        onTipUpdate' tip = do
            traceWith tr $ MsgNodeTip tip
            onTipUpdate tip
            queryLocalState (getTipPoint tip)

        queryLocalState pt =
            (localStateQueryQ `send` CmdQueryLocalState pt) >>= handleLocalState

        handleLocalState = \case
            Left (e :: ErrNetworkUnavailable) ->
                traceWith tr $ MsgLocalStateQueryError $ show e
            Right (Left (e :: AcquireFailure)) ->
                traceWith tr $ MsgLocalStateQueryError $ show e
            Right (Right ls) ->
                onTxParamsUpdate' $ txParametersFromUpdateState ls

    pure $ nodeToClientProtocols NodeToClientProtocols
        { localChainSyncProtocol =
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                chainSyncFollowTip nullTracer onTipUpdate' channel
        , localTxSubmissionProtocol =
            let tr' = contramap MsgTxSubmission tr in
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localTxSubmission tr' localTxSubmissionQ channel
        , localStateQueryProtocol =
            let tr' = contramap MsgLocalStateQuery tr in
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localStateQuery tr' localStateQueryQ channel
        }
        NodeToClientV_2

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
    :: MonadTimer m => RunMiniProtocol 'InitiatorApp ByteString m a Void
doNothingProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

-- Connect a client to a network, see `mkWalletClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkWalletClient tr bp queue) mainnetVersionData addrInfo
connectClient
    :: Tracer IO NetworkLayerLog
    -> [RetryStatus -> Handler IO Bool]
    -> (ConnectionId LocalAddress -> NetworkClient IO)
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
    -> FilePath
    -> IO ()
connectClient tr handlers client (vData, vCodec) addr = withIOManager $ \iocp -> do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_2 vData vDict client
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
    :: Tracer IO NetworkLayerLog
    -> [RetryStatus -> Handler IO Bool]
retryOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' True
    , const $ Handler $ handleMuxError tr' True
    ]
  where
    tr' = contramap MsgConnectionLost tr

-- | Handlers that are failing if the connection is lost
failOnConnectionLost
    :: Tracer IO NetworkLayerLog
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
    MuxIOException e ->
        handleIOException tr onResourceVanished e
    MuxBearerClosed -> do
        traceWith tr Nothing
        pure onResourceVanished

-- | Client for the 'Chain Sync' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor. Once started, the client simply runs ad-infinitum but one may
-- interact with it via a 'TQueue' of commands / messages used to move inside
-- the state-machine.
--
-- In a typical usage, 'chainSyncWithBlocks' would be executed in a forked
-- thread and given a 'TQueue' over which the parent thread as control.
--
-- >>> forkIO $ void $ chainSyncWithBlocks tr queue channel
-- ()
-- >>> writeTQueue queue ...
--
--                                    Agency
--     -------------------------------------------------------------------------
--     Client has agency*                | Idle
--     Server has agency*                | Intersect, Next
--
--     * A peer has agency if it is expected to send the next message.
--
--      *-----------*
--      | Intersect |◀══════════════════════════════╗
--      *-----------*         FindIntersect         ║
--            │                                     ║
--            │                                *---------*              *------*
--            │ Intersect.{Found,NotFound}     |         |═════════════▶| Done |
--            └───────────────────────────────╼|         |   MsgDone    *------*
--                                             |   Idle  |
--         ╔═══════════════════════════════════|         |
--         ║            RequestNext            |         |⇦ START
--         ║                                   *---------*
--         ▼                                        ╿
--      *------*       Roll.{Backward,Forward}      │
--      | Next |────────────────────────────────────┘
--      *------*
--
chainSyncWithBlocks
    :: forall m protocol.
        ( protocol ~ ChainSync ByronBlock (Tip ByronBlock)
        , MonadThrow m, MonadST m, MonadSTM m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> W.BlockchainParameters
        -- ^ Blockchain parameters
    -> TQueue m (ChainSyncCmd m)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
chainSyncWithBlocks tr bp queue channel = do
    runPeer tr codec channel (chainSyncClientPeer client)
  where
    W.BlockchainParameters
        { getGenesisBlockHash
        , getEpochLength
        } = bp

    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecChainSync
        encodeByronBlock
        (unwrapCBORinCBOR $ decodeByronBlock (toEpochSlots getEpochLength))
        (encodePoint encodeByronHeaderHash)
        (decodePoint decodeByronHeaderHash)
        (encodeTip encodeByronHeaderHash)
        (decodeTip decodeByronHeaderHash)

    client :: ChainSyncClient ByronBlock (Tip ByronBlock) m Void
    client = ChainSyncClient clientStIdle
      where
        -- Client in the state 'Idle'. We wait for requests / commands on an
        -- 'TQueue'. Commands start a chain of messages and state transitions
        -- before finally returning to 'Idle', waiting for the next command.
        clientStIdle
            :: m (ClientStIdle ByronBlock (Tip ByronBlock) m Void)
        clientStIdle = atomically (readTQueue queue) >>= \case
            CmdFindIntersection points respond -> pure $
                SendMsgFindIntersect points (clientStIntersect respond)

            CmdNextBlocks respond -> pure $
                SendMsgRequestNext
                    (clientStNext ([], 1000) respond)
                    (pure $ clientStNext ([], 1) respond)

        clientStIntersect
            :: (Maybe (Point ByronBlock) -> m ())
            -> ClientStIntersect ByronBlock (Tip ByronBlock) m Void
        clientStIntersect respond = ClientStIntersect
            { recvMsgIntersectFound = \intersection _tip ->
                ChainSyncClient $ do
                    respond (Just intersection)
                    clientStIdle

            , recvMsgIntersectNotFound = \_tip ->
                ChainSyncClient $ do
                    respond Nothing
                    clientStIdle
            }

        clientStNext
            :: ([ByronBlock], Int)
            -> (NextBlocksResult (Point ByronBlock) ByronBlock -> m ())
            -> ClientStNext ByronBlock (Tip ByronBlock) m Void
        clientStNext (blocks, n) respond
            | n <= 1 = ClientStNext
                { recvMsgRollBackward = onRollback
                , recvMsgRollForward = \block tip ->
                    ChainSyncClient $ do
                        let cursor' = blockPoint block
                        let blocks' = reverse (block:blocks)
                        let tip'    = fromTip getGenesisBlockHash getEpochLength tip
                        respond (RollForward cursor' tip' blocks')
                        clientStIdle
                }
            | otherwise = ClientStNext
                { recvMsgRollBackward = onRollback
                , recvMsgRollForward = \block _ ->
                    ChainSyncClient $ pure $ SendMsgRequestNext
                        (clientStNext (block:blocks,n-1) respond)
                        (pure $ clientStNext (block:blocks,1) respond)
                }
          where
            onRollback point _tip = ChainSyncClient $ do
                respond (RollBackward point)
                clientStIdle

-- | Client for the 'Chain Sync' mini-protocol, which provides notifications
-- when the node tip changes.
--
-- This is used in the same way as 'chainSyncWithBlocks', except that only one
-- of these clients is necessary, rather than one client per wallet.
chainSyncFollowTip
    :: forall m protocol.
        ( protocol ~ ChainSync (Serialised ByronBlock) (Tip (Serialised ByronBlock))
        , MonadThrow m, MonadST m
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> (Tip ByronBlock -> m ())
        -- ^ Callback for when the tip changes.
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
chainSyncFollowTip tr onTipUpdate channel =
    runPeer tr codec channel (chainSyncClientPeer client)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecChainSyncSerialised
        (encodePoint encodeByronHeaderHash)
        (decodePoint decodeByronHeaderHash)
        (encodeTip encodeByronHeaderHash)
        (decodeTip decodeByronHeaderHash)

    client :: ChainSyncClient (Serialised ByronBlock) (Tip (Serialised ByronBlock)) m Void
    client = ChainSyncClient (clientStIdle False)
      where
        -- Client in the state 'Idle'. We immediately request the next block.
        clientStIdle
            :: Bool
            -> m (ClientStIdle (Serialised ByronBlock) (Tip (Serialised ByronBlock)) m Void)
        clientStIdle synced = pure $ SendMsgRequestNext
            (clientStNext synced)
            (pure $ clientStNext synced)

        -- In the CanAwait state, we take the tip point given by the node and
        -- ask for the intersection of that point. This fast-fowards us to the
        -- tip. Once synchronised with the tip, we expect to be waiting for the
        -- server to send AwaitReply most of the time.
        clientStNext
            :: Bool
            -> ClientStNext (Serialised ByronBlock) (Tip (Serialised ByronBlock)) m Void
        clientStNext False = ClientStNext
                { recvMsgRollBackward = const findIntersect
                , recvMsgRollForward = const findIntersect
                }
          where
            findIntersect tip = ChainSyncClient $
                pure $ SendMsgFindIntersect [getTipPoint tip] clientStIntersect

        clientStNext True = ClientStNext
                { recvMsgRollBackward = const doUpdate
                , recvMsgRollForward = const doUpdate
                }
          where
            doUpdate tip = ChainSyncClient $ do
                onTipUpdate (castTip tip)
                clientStIdle True

        -- After an intersection is found, we return to idle with the sync flag
        -- set.
        clientStIntersect
            :: ClientStIntersect (Serialised ByronBlock) (Tip (Serialised ByronBlock)) m Void
        clientStIntersect = ClientStIntersect
            { recvMsgIntersectFound = \_intersection _tip ->
                ChainSyncClient $ clientStIdle True
            , recvMsgIntersectNotFound = \_tip ->
                ChainSyncClient $ clientStIdle False
            }

-- | Client for the 'Local Tx Submission' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
--
--                                    Agency
--     -------------------------------------------------------------------------
--     Client has agency*                | Idle
--     Server has agency*                | Busy
--     * A peer has agency if it is expected to send the next message.
--
--      *-----------*
--      |    Busy   |◀══════════════════════════════╗
--      *-----------*            SubmitTx           ║
--         │     │                                  ║
--         │     │                             *---------*              *------*
--         │     │        AcceptTx             |         |═════════════▶| Done |
--         │     └────────────────────────────╼|         |   MsgDone    *------*
--         │              RejectTx             |   Idle  |
--         └──────────────────────────────────╼|         |
--                                             |         |⇦ START
--                                             *---------*
localTxSubmission
    :: forall m protocol.
        ( MonadThrow m, MonadTimer m, MonadST m
        , protocol ~ LocalTxSubmission (GenTx ByronBlock) ApplyMempoolPayloadErr
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> TQueue m (LocalTxSubmissionCmd m)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
localTxSubmission tr queue channel =
    runPeer tr codec channel (localTxSubmissionClientPeer client)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecLocalTxSubmission
        encodeByronGenTx -- Tx -> CBOR.Encoding
        decodeByronGenTx -- CBOR.Decoder s Tx
        (nodeEncodeApplyTxError (Proxy @ByronBlock)) -- ApplyTxErr -> CBOR.Encoding
        (nodeDecodeApplyTxError (Proxy @ByronBlock)) -- CBOR.Decoder s ApplyTxErr

    client
        :: LocalTxSubmissionClient (GenTx ByronBlock) ApplyMempoolPayloadErr m Void
    client = LocalTxSubmissionClient clientStIdle
      where
        clientStIdle
            :: m (LocalTxClientStIdle (GenTx ByronBlock) ApplyMempoolPayloadErr m Void)
        clientStIdle = atomically (readTQueue queue) <&> \case
            CmdSubmitTx tx respond ->
                SendMsgSubmitTx tx (\e -> respond e >> clientStIdle)

-- | Client for the 'Local State Query' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
--
--                                    Agency
--     -------------------------------------------------------------------------
--     Client has agency*                | Idle, Acquired
--     Server has agency*                | Acquiring, Querying
--     * A peer has agency if it is expected to send the next message.
--
--
--                ┌───────────────┐    Done      ┌───────────────┐
--        ┌──────▶│     Idle      ├─────────────▶│     Done      │
--        │       └───┬───────────┘              └───────────────┘
--        │           │       ▲
--        │   Acquire │       │
--        │           │       │ Failure
--        │           ▼       │
--        │       ┌───────────┴───┐              Result
--        │       │   Acquiring   │◀─────────────────────┐
--        │       └───┬───────────┘                      │
-- Release│           │       ▲                          │
--        │           │       │                          │
--        │  Acquired ▼       │ ReAcquire                │
--        │       ┌───────────┴───┐             ┌────────┴───────┐
--        └───────┤   Acquired    │────────────>│   Querying     │
--                └───────────────┘             └────────────────┘
--
localStateQuery
    :: forall m protocol.
        ( MonadThrow m, MonadTimer m, MonadST m
        , protocol ~ LocalStateQuery ByronBlock (Query ByronBlock)
        )
    => Tracer m (TraceSendRecv protocol)
        -- ^ Base tracer for the mini-protocols
    -> TQueue m (LocalStateQueryCmd m)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
localStateQuery tr queue channel =
    runPeer tr codec channel (localStateQueryClientPeer client)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecLocalStateQuery
          (encodePoint encodeByronHeaderHash)
          (decodePoint decodeByronHeaderHash)
          nodeEncodeQuery
          nodeDecodeQuery
          nodeEncodeResult
          nodeDecodeResult

    client :: LocalStateQueryClient ByronBlock (Query ByronBlock) m Void
    client = LocalStateQueryClient clientStIdle
      where
        clientStIdle
            :: m (LSQ.ClientStIdle ByronBlock (Query ByronBlock) m Void)
        clientStIdle = awaitNextCmd <&> \case
            CmdQueryLocalState pt respond ->
                LSQ.SendMsgAcquire pt (clientStAcquiring respond)

        clientStAcquiring
            :: (LocalStateQueryResult -> m ())
            -> LSQ.ClientStAcquiring ByronBlock (Query ByronBlock) m Void
        clientStAcquiring respond = LSQ.ClientStAcquiring
            { recvMsgAcquired = clientStAcquired respond
            , recvMsgFailure = \failure -> do
                    respond (Left failure)
                    clientStIdle
            }

        clientStAcquired
            :: (LocalStateQueryResult -> m ())
            -> LSQ.ClientStAcquired ByronBlock (Query ByronBlock) m Void
        clientStAcquired respond = LSQ.SendMsgQuery
            GetUpdateInterfaceState
            (clientStQuerying respond)

        -- By re-acquiring rather releasing the state with 'MsgRelease' it
        -- enables optimisations on the server side.
        clientStAcquiredAgain
            :: m (LSQ.ClientStAcquired ByronBlock (Query ByronBlock) m Void)
        clientStAcquiredAgain = awaitNextCmd <&> \case
            CmdQueryLocalState pt respond ->
                LSQ.SendMsgReAcquire pt (clientStAcquiring respond)

        clientStQuerying
            :: (LocalStateQueryResult -> m ())
            -> LSQ.ClientStQuerying ByronBlock (Query ByronBlock) m Void U.State
        clientStQuerying respond = LSQ.ClientStQuerying
            { recvMsgResult = \result -> do
                    respond (Right result)
                    clientStAcquiredAgain
            }

    awaitNextCmd :: m (LocalStateQueryCmd m)
    awaitNextCmd = atomically $ readTQueue queue

--------------------------------------------------------------------------------
--
-- Temporary

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data NetworkLayerLog
    = MsgCouldntConnect Int
    | MsgConnectionLost (Maybe IOException)
    | MsgTxSubmission (TraceSendRecv (LocalTxSubmission (GenTx ByronBlock) ApplyMempoolPayloadErr))
    | MsgLocalStateQuery (TraceSendRecv (LocalStateQuery ByronBlock (Query ByronBlock)))
    | MsgHandshakeTracer (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    | MsgFindIntersection [W.BlockHeader]
    | MsgIntersectionFound (W.Hash "BlockHeader")
    | MsgFindIntersectionTimeout
    | MsgPostSealedTx W.SealedTx
    | MsgNodeTip (Tip ByronBlock)
    | MsgTxParameters W.TxParameters
    | MsgLocalStateQueryError String

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
        MsgHandshakeTracer msg ->
            T.pack (show msg)
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
        MsgLocalStateQuery msg ->
            T.pack (show msg)
        MsgNodeTip tip -> T.unwords
            [ "Network node tip is:"
            , T.pack (show tip)
            ]
        MsgTxParameters params -> T.unwords
            [ "TxParams for tip are:"
            , pretty params
            ]
        MsgLocalStateQueryError e -> T.unwords
            [ "Error when querying local state parameters:"
            , T.pack e
            ]

instance HasPrivacyAnnotation NetworkLayerLog
instance HasSeverityAnnotation NetworkLayerLog where
    getSeverityAnnotation = \case
        MsgCouldntConnect 0        -> Debug
        MsgCouldntConnect 1        -> Notice
        MsgCouldntConnect{}        -> Warning
        MsgConnectionLost{}        -> Warning
        MsgTxSubmission{}          -> Info
        MsgHandshakeTracer{}       -> Info
        MsgFindIntersectionTimeout -> Warning
        MsgFindIntersection{}      -> Info
        MsgIntersectionFound{}     -> Info
        MsgPostSealedTx{}          -> Debug
        MsgLocalStateQuery{}       -> Debug
        MsgNodeTip{}               -> Info
        MsgTxParameters{}          -> Info
        MsgLocalStateQueryError{}  -> Error
