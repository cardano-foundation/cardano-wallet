{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.Wallet.Byron.Compatibility
    ( Byron, fromSlotNo, fromTip, toEpochSlots, toGenTx, toPoint )
import Cardano.Wallet.Network
    ( Cursor
    , ErrCurrentNodeTip (..)
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
    ( (>=>) )
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
    , newTQueue
    , putTMVar
    , readTQueue
    , takeTMVar
    , writeTQueue
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
    ( Tracer, contramap, traceWith )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.List
    ( isInfixOf )
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
    ( AppType (..), MuxError (..), MuxErrorType (..), MuxTrace, WithMuxBearer )
import Network.TypedProtocol.Codec
    ( Codec )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..)
    , GenTx
    , decodeByronBlock
    , decodeByronGenTx
    , decodeByronHeaderHash
    , encodeByronBlock
    , encodeByronGenTx
    , encodeByronHeaderHash
    )
import Ouroboros.Network.Block
    ( Point (..)
    , SlotNo (..)
    , Tip (..)
    , blockPoint
    , decodePoint
    , decodeTip
    , encodePoint
    , encodeTip
    , genesisPoint
    , pointSlot
    , unwrapCBORinCBOR
    )
import Ouroboros.Network.Channel
    ( Channel )
import Ouroboros.Network.Codec
    ( DeserialiseFailure )
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
    ( codecChainSync )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Version
    ( CodecCBORTerm, DictVersion (..), simpleSingletonVersions )
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

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.Serialise as CBOR
import qualified Data.Text as T

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
    -> W.BlockchainParameters
        -- ^ Static blockchain parameters
    -> FilePath
        -- ^ Socket for communicating with the node
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
        -- ^ Codecs for the node's client
    -> (NetworkLayer IO (IO Byron) ByronBlock -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr bp addrInfo versionData action = do
    localTxSubmissionQ <- atomically newTQueue

    -- NOTE: We keep a client connection running just for accessing the node tip.
    nodeTipQ <- atomically newTQueue
    let nodeTipClient = const $ mkNetworkClient tr bp nodeTipQ localTxSubmissionQ
    link =<< async (connectClient tr nodeTipClient versionData addrInfo)

    action
        NetworkLayer
            { currentNodeTip = _currentNodeTip nodeTipQ
            , nextBlocks = _nextBlocks
            , initCursor = _initCursor localTxSubmissionQ
            , cursorSlotId = _cursorSlotId
            , postTx = _postTx localTxSubmissionQ
            , stakeDistribution = _stakeDistribution
            , getAccountBalance = _getAccountBalance
            }
  where
    W.BlockchainParameters
        { getGenesisBlockHash
        , getEpochLength
        } = bp

    _initCursor localTxSubmissionQ headers = do
        chainSyncQ <- atomically newTQueue
        let client = const $ mkNetworkClient tr bp chainSyncQ localTxSubmissionQ
        link =<< async (connectClient tr client versionData addrInfo)
        let points = genesisPoint : (toPoint getEpochLength <$> headers)
        let policy = constantDelay 500
        let findIt = chainSyncQ `send` CmdFindIntersection points
        let shouldRetry _ = \case
                Left (_ :: ErrNetworkUnavailable) -> do
                    traceWith tr $ MsgFindIntersectionTimeout headers
                    pure True
                Right Nothing -> pure False
                Right Just{}  -> pure False
        retrying policy shouldRetry (const findIt) >>= \case
            Right (Just intersection) ->
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

    _currentNodeTip nodeTipQ =
        liftIO (nodeTipQ `send` CmdCurrentNodeTip) >>= \case
            Left e ->
                throwE $ ErrCurrentNodeTipNetworkUnreachable e
            Right tip ->
                pure $ fromTip getGenesisBlockHash getEpochLength tip

    _postTx localTxSubmissionQ tx = do
        result <- withExceptT ErrPostTxNetworkUnreachable $
            ExceptT (localTxSubmissionQ `send` CmdSubmitTx (toGenTx tx))
        case result of
            Nothing  -> pure ()
            Just err -> throwE $ ErrPostTxBadRequest $ T.pack err

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
    | CmdCurrentNodeTip
        (Tip ByronBlock -> m ())

-- | Sending command to the localTxSubmission client. See also 'ChainSyncCmd'.
data LocalTxSubmissionCmd (m :: * -> *)
    = CmdSubmitTx
        (GenTx ByronBlock)
        (Maybe String -> m ())

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

-- | Construct a network client with the given communication channel
mkNetworkClient
    :: (MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> W.BlockchainParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd m)
        -- ^ Communication channel with the ChainSync client
    -> TQueue m (LocalTxSubmissionCmd m)
        -- ^ Communication channel with the LocalTxSubmission client
    -> NetworkClient m
mkNetworkClient tr bp chainSyncQ localTxSubmissionQ =
    nodeToClientProtocols NodeToClientProtocols
        { localChainSyncProtocol =
            let tr' = contramap MsgChainSync tr in
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                chainSyncWithBlocks tr' bp chainSyncQ channel
        , localTxSubmissionProtocol =
            let tr' = contramap MsgTxSubmission tr in
            InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                localTxSubmission tr' localTxSubmissionQ channel
        }

-- Connect a client to a network, see `mkNetworkClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkNetworkClient tr bp queue) mainnetVersionData addrInfo
connectClient
    :: Tracer IO NetworkLayerLog
    -> (ConnectionId LocalAddress -> NetworkClient IO)
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
    -> FilePath
    -> IO ()
connectClient tr client (vData, vCodec) addr = withIOManager $ \iocp -> do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_1 vData vDict client
    let tracers = NetworkConnectTracers
            { nctMuxTracer = contramap MsgMuxTracer tr
            , nctHandshakeTracer = contramap MsgHandshakeTracer tr
            }
    let socket = localSnocket iocp addr
    recovering policy
        [ const $ Handler handleIOException
        , const $ Handler handleMuxError
        ] $ \status -> do
            traceWith tr $ MsgCouldntConnect (rsIterNumber status)
            connectTo socket tracers versions addr
  where
    -- .25s -> .25s -> .5s → .75s → 1.25s → 2s
    policy :: RetryPolicyM IO
    policy = fibonacciBackoff 250_000 & capDelay 2_000_000

    -- There's a race-condition when starting the wallet and the node at the
    -- same time: the socket might not be there yet when we try to open it.
    -- In such case, we simply retry a bit later and hope it's there.
    handleIOException :: IOException -> IO Bool
    handleIOException e
        | isDoesNotExistError e     = pure True
        | isResourceVanishedError e = do
            traceWith tr $ MsgConnectionLost (Just e)
            pure True
        | otherwise = pure False
      where
        isResourceVanishedError = isInfixOf "resource vanished" . show

    -- Recover frmo error when the connection with the node is lost.
    handleMuxError :: MuxError -> IO Bool
    handleMuxError = pure . errorType >=> \case
        MuxUnknownMiniProtocol -> pure False
        MuxDecodeError -> pure False
        MuxIngressQueueOverRun -> pure False
        MuxInitiatorOnly -> pure False
        MuxIOException e -> handleIOException e
        MuxBearerClosed -> do
            traceWith tr $ MsgConnectionLost Nothing
            pure True


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
            CmdCurrentNodeTip respond -> pure $
                SendMsgFindIntersect [genesisPoint] (clientStNodeTip respond)

            CmdFindIntersection points respond -> pure $
                SendMsgFindIntersect points (clientStIntersect respond)

            CmdNextBlocks respond -> pure $
                SendMsgRequestNext
                    (clientStNext ([], 1000) respond)
                    (pure $ clientStNext ([], 1) respond)

        clientStNodeTip
            :: (Tip ByronBlock -> m ())
            -> ClientStIntersect ByronBlock (Tip ByronBlock) m Void
        clientStNodeTip respond = ClientStIntersect
            { recvMsgIntersectFound = \_ tip ->
                ChainSyncClient $ do
                    respond tip
                    clientStIdle

            , recvMsgIntersectNotFound = \tip ->
                ChainSyncClient $ do
                    respond tip
                    clientStIdle
            }

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
        , protocol ~ LocalTxSubmission (GenTx ByronBlock) String
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
        CBOR.encode      -- String -> CBOR.Encoding
        CBOR.decode      -- CBOR.Decoder s String

    client
        :: LocalTxSubmissionClient (GenTx ByronBlock) String m Void
    client = LocalTxSubmissionClient clientStIdle
      where
        clientStIdle
            :: m (LocalTxClientStIdle (GenTx ByronBlock) String m Void)
        clientStIdle = atomically (readTQueue queue) <&> \case
            CmdSubmitTx tx respond ->
                SendMsgSubmitTx tx (\e -> respond e >> clientStIdle)

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
    | MsgChainSync (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))
    | MsgTxSubmission (TraceSendRecv (LocalTxSubmission (GenTx ByronBlock) String))
    | MsgMuxTracer (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
    | MsgHandshakeTracer (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    | MsgFindIntersectionTimeout [W.BlockHeader]

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
        MsgChainSync msg ->
            T.pack (show msg)
        MsgMuxTracer msg ->
            T.pack (show msg)
        MsgHandshakeTracer msg ->
            T.pack (show msg)
        MsgFindIntersectionTimeout points -> T.unwords
            [ "Couldn't find an intersection in a timely manner for: "
            , T.intercalate ", " (pretty <$> points)
            , ". Trying again..."
            ]

instance DefinePrivacyAnnotation NetworkLayerLog
instance DefineSeverity NetworkLayerLog where
    defineSeverity = \case
        MsgCouldntConnect 0          -> Debug
        MsgCouldntConnect 1          -> Notice
        MsgCouldntConnect{}          -> Warning
        MsgConnectionLost{}          -> Warning
        MsgTxSubmission{}            -> Info
        MsgChainSync{}               -> Debug
        MsgMuxTracer{}               -> Debug
        MsgHandshakeTracer{}         -> Debug
        MsgFindIntersectionTimeout{} -> Warning
