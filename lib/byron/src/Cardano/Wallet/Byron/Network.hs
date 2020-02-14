{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    , newNetworkLayer

      -- * Transport Helpers
    , AddrInfo
    , localSocketAddrInfo
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.Wallet.Byron.Compatibility
    ( Byron
    , byronEpochLength
    , fromSlotNo
    , fromTip
    , genesisBlock
    , genesisTip
    , toByronHash
    , toEpochSlots
    , toPoint
    )
import Cardano.Wallet.Logging
    ( trMessage )
import Cardano.Wallet.Network
    ( Cursor
    , ErrGetBlock (..)
    , ErrNetworkUnavailable (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    , mapCursor
    )
import Codec.SerialiseTerm
    ( CodecCBORTerm )
import Control.Concurrent.Async
    ( async, link )
import Control.Exception
    ( catch, throwIO )
import Control.Monad
    ( void )
import Control.Monad.Class.MonadAsync
    ( MonadAsync (race) )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , TQueue (..)
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
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), withExceptT )
import Control.Tracer
    ( Tracer, contramap )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( (<&>) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Void
    ( Void )
import GHC.Stack
    ( HasCallStack )
import Network.Mux.Interface
    ( AppType (..) )
import Network.Mux.Types
    ( MuxError )
import Network.Socket
    ( AddrInfo (..), Family (..), SockAddr (..), SocketType (..) )
import Network.TypedProtocol.Channel
    ( Channel )
import Network.TypedProtocol.Codec
    ( Codec )
import Network.TypedProtocol.Codec.Cbor
    ( DeserialiseFailure )
import Network.TypedProtocol.Driver
    ( TraceSendRecv, runPeer )
import Ouroboros.Consensus.Ledger.Byron
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
    )
import Ouroboros.Network.Mux
    ( OuroborosApplication (..) )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localTxSubmissionClientNull
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
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..), localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
    ( codecLocalTxSubmission )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.Serialise as CBOR
import qualified Data.Text as T
import qualified Network.Socket as Socket

-- | Network layer cursor for Byron. Mostly useless since the protocol itself is
-- stateful and the node's keep track of the associated connection's cursor.
data instance Cursor (m Byron) = Cursor
    (Point ByronBlock)
    (TQueue m (ChainSyncCmd m))

-- | Create an instance of the network layer
newNetworkLayer
    :: Trace IO Text
        -- ^ Logging of network layer startup
    -> W.BlockchainParameters
        -- ^ Static blockchain parameters
    -> AddrInfo
        -- ^ Socket for communicating with the node
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
        -- ^ Codecs for the node's client
    -> NetworkLayer IO (IO Byron) ByronBlock
newNetworkLayer tr bp addrInfo versionData = NetworkLayer
    { currentNodeTip = _currentNodeTip
    , nextBlocks = _nextBlocks
    , initCursor = _initCursor
    , cursorSlotId = _cursorSlotId
    , postTx = _postTx
    , staticBlockchainParameters = _staticBlockchainParameters
    , stakeDistribution = _stakeDistribution
    , getAccountBalance = _getAccountBalance
    }
  where
    _initCursor localTxSubmissionQ headers = do
        chainSyncQ <- atomically newTQueue
        let client = mkNetworkClient tr bp chainSyncQ localTxSubmissionQ
        link =<< async (connectClient client versionData addrInfo)

        let points = genesisPoint : (toPoint <$> headers)
        chainSyncQ `send` CmdFindIntersection points >>= \case
            Right(Just intersection) ->
                pure $ Cursor intersection chainSyncQ
            _ -> fail
                "initCursor: intersection not found? This can't happen \
                \because we always give at least the genesis point..."

    _nextBlocks (Cursor _ chainSyncQ) = do
        let toCursor point = Cursor point chainSyncQ
        fmap (mapCursor toCursor) $ withExceptT ErrGetBlockNetworkUnreachable $
            ExceptT (chainSyncQ `send` CmdNextBlocks)

    _cursorSlotId (Cursor point _) = do
        fromSlotNo $ fromWithOrigin (SlotNo 0) $ pointSlot point

    _getAccountBalance _ =
        pure (Quantity 0)

    _staticBlockchainParameters =
        -- FIXME: Actually pass in the block0 as a parameter
        ( genesisBlock $ toByronHash $ coerce $ W.getGenesisBlockHash bp
        , bp
        )

    _currentNodeTip =
        notImplemented "currentNodeTip"

    _postTx =
        notImplemented "postTx"

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
data NetworkClientCmd (m :: * -> *)
    = CmdFindIntersection
        [Point ByronBlock]
        (Maybe (Point ByronBlock) -> m ())
    | CmdNextBlocks
        (NextBlocksResult (m Byron) ByronBlock -> m ())
    | CmdCurrentNodeTip
        (Tip ByronBlock -> m ())

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
    timeout = threadDelay 60

--------------------------------------------------------------------------------
--
-- Network Client

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
    'InitiatorApp
        -- Initiator ~ Client (as opposed to Responder / Server)
    ConnectionId
        -- An identifier for the peer: here, a local and remote socket.
    NodeToClientProtocols
        -- Specifies which mini-protocols our client is talking.
        -- 'NodeToClientProtocols' allows for two mini-protocols:
        --  - Chain Sync
        --  - Tx submission
    m
        -- Underlying monad we run in
    ByteString
        -- Concrete representation for bytes string
    Void
        -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- Irrelevant for 'InitiatorApplication'. Return type of 'Responder'
        -- application.

-- | Construct a network client with the given communication channel
mkNetworkClient
    :: (MonadIO m, MonadThrow m, MonadST m, MonadTimer m)
    => Trace m Text
        -- ^ Base trace for underlying protocols
    -> W.BlockchainParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd m)
        -- ^ Communication channel with the ChainSync client
    -> NetworkClient m
mkNetworkClient tr bp chainSyncQ =
    OuroborosInitiatorApplication $ \pid -> \case
        ChainSyncWithBlocksPtcl ->
            let tr' = contramap (T.pack . show) $ trMessage tr in
            chainSyncWithBlocks tr' pid (W.getGenesisBlockHash bp) chainSyncQ
        LocalTxSubmissionPtcl ->
            localTxSubmission nullTracer pid

-- Connect a client to a network, see `mkNetworkClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkNetworkClient tr bp queue) mainnetVersionData addrInfo
connectClient
    :: NetworkClient IO
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
    -> AddrInfo
    -> IO ()
connectClient client (vData, vCodec) addr = do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_1 vData vDict client
    let tracers = NetworkConnectTracers nullTracer nullTracer
    connectTo tracers versions Nothing addr `catch` handleMuxError
  where
    -- `connectTo` might rise an exception: we are the client and the protocols
    -- specify that only  client can lawfuly close a connection, but the other
    -- side might just disappear.
    --
    -- NOTE: This handler does nothing.
    handleMuxError :: MuxError -> IO ()
    handleMuxError = throwIO

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
-- >>> forkIO $ void $ chainSyncWithBlocks peerId tr queue channel
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
    :: forall m protocol peerId.
        ( protocol ~ ChainSync ByronBlock (Tip ByronBlock)
        , MonadThrow m, MonadST m, MonadSTM m
        )
    => Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
        -- ^ Base tracer for the mini-protocols
    -> peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> W.Hash "Genesis"
        -- ^ Hash of the genesis block
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
chainSyncWithBlocks tr pid genesisHash queue channel = do
    nodeTipVar <- newTMVarM genesisTip
    runPeer tr codec pid channel (chainSyncClientPeer $ client nodeTipVar)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecChainSync
        encodeByronBlock
        (decodeByronBlock (toEpochSlots byronEpochLength))
        (encodePoint encodeByronHeaderHash)
        (decodePoint decodeByronHeaderHash)
        (encodeTip encodeByronHeaderHash)
        (decodeTip decodeByronHeaderHash)

    client
        :: TMVar m (Tip ByronBlock)
        -> ChainSyncClient ByronBlock (Tip ByronBlock) m Void
    client nodeTipVar = ChainSyncClient clientStIdle
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

            CmdCurrentNodeTip respond -> do
                respond =<< atomically (readTMVar nodeTipVar)
                clientStIdle

        clientStIntersect
            :: (Maybe (Point ByronBlock) -> m ())
            -> ClientStIntersect ByronBlock (Tip ByronBlock) m Void
        clientStIntersect respond = ClientStIntersect
            { recvMsgIntersectFound = \intersection tip ->
                ChainSyncClient $ do
                    swapTMVarM nodeTipVar tip
                    respond (Just intersection)
                    clientStIdle

            , recvMsgIntersectNotFound = \tip ->
                ChainSyncClient $ do
                    swapTMVarM nodeTipVar tip
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
                        swapTMVarM nodeTipVar tip
                        let cursor  = blockPoint block
                        let blocks' = reverse (block:blocks)
                        respond (RollForward cursor (fromTip genesisHash tip) blocks')
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
            onRollback point tip = ChainSyncClient $ do
                swapTMVarM nodeTipVar tip
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
    :: forall m protocol peerId.
        ( MonadThrow m, MonadTimer m, MonadST m
        , protocol ~ LocalTxSubmission (GenTx ByronBlock) String
        )
    => Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
        -- ^ Base tracer for the mini-protocols
    -> peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
localTxSubmission tr pid channel =
    runPeer tr codec pid channel (localTxSubmissionClientPeer client)
  where
    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecLocalTxSubmission
        encodeByronGenTx -- Tx -> CBOR.Encoding
        decodeByronGenTx -- CBOR.Decoder s Tx
        CBOR.encode      -- String -> CBOR.Encoding
        CBOR.decode      -- CBOR.Decoder s String

    client :: LocalTxSubmissionClient (GenTx ByronBlock) String m Void
    client = localTxSubmissionClientNull

--------------------------------------------------------------------------------
--
-- Transport

localSocketAddrInfo :: FilePath -> AddrInfo
localSocketAddrInfo socketPath = AddrInfo
    { addrFlags = []
    , addrFamily = AF_UNIX
    , addrProtocol = Socket.defaultProtocol
    , addrAddress = SockAddrUnix socketPath
    , addrCanonName = Nothing
    , addrSocketType = Stream
    }

--------------------------------------------------------------------------------
--
-- Internal

swapTMVarM :: MonadSTM m => TMVar m a -> a -> m ()
swapTMVarM var = void . atomically . swapTMVar var

--------------------------------------------------------------------------------
--
-- Temporary

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
