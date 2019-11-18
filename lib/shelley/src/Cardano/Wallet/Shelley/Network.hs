{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
--{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints
--   -fno-warn-unused-matches -fno-warn-unused-local-binds #-}

{-
Good to read before / additional resources:

- Module's documentation in `ouroboros-network/typed-protocols/src/Network/TypedProtocols.hs`
- Data Diffusion and Peer Networking in Shelley (see: https://raw.githubusercontent.com/wiki/input-output-hk/cardano-wallet/data_diffusion_and_peer_networking_in_shelley.pdf)
    - In particular sections 4.1, 4.2, 4.6 and 4.8
-}

module Cardano.Wallet.Shelley.Network
    ( main
    , genesisTip
    , setup
    , prettyResponse
      -- * Top-Level Interface
    , NetworkLayer (..)
    , ErrNextBlocks (..)
    , ChainParameters (..)

    -- * Re-Export
    , EpochSlots (..)
    , ProtocolMagicId (..)

      -- * Constructors
    , newNetworkClient
    , connectClient
    , dummyNodeToClientVersion

      -- * Transport Helpers
    , localSocketAddrInfo
    , localSocketFilePath
    ) where

import Prelude

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Crypto
    ( ProtocolMagicId (..) )
import Codec.SerialiseTerm
    ( CodecCBORTerm )
import Network.Mux.Interface
    ( AppType (..) )
import Network.Mux.Types
    ( MuxError )
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
import Ouroboros.Consensus.NodeId
    ( CoreNodeId (..) )
import Ouroboros.Network.Block
    ( Point, Tip (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
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
    , nodeToClientCodecCBORTerm
    )
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

import Control.Exception
    ( catch, throwIO )
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
    ( MonadTimer )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Tracer
    ( Tracer, contramap )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Text
    ( Text )
import Data.Void
    ( Void )
import Network.Socket
    ( AddrInfo (..), Family (..), SockAddr (..), SocketType (..) )

import qualified Codec.Serialise as CBOR
import qualified Network.Socket as Socket
import qualified Ouroboros.Network.Block as Abstract


-- ----------------
-- Development

import Cardano.BM.Trace
    ( nullTracer )
import Control.Concurrent
    ( forkIO )
import Control.Monad
    ( forM_, void )


setup :: IO (NetworkLayer IO)
setup = do
    -- I have cardano-node running from ../cardano-node
    let nodeId = CoreNodeId 0
    let path = "../cardano-node/socket/" <> (localSocketFilePath nodeId)
    let addr =  localSocketAddrInfo path
    print addr

    let params = ChainParameters
         { epochSlots = EpochSlots 21600
         , protocolMagic = ProtocolMagicId 764824073
         }
    (client, network) <- newNetworkClient nullTracer params

    -- Launch the stateful connection in a separate thread
    void $ forkIO $ connectClient client dummyNodeToClientVersion addr
    return network

prettyResponse :: [ByronBlock] -> IO ()
prettyResponse response = do
    forM_ response $ \b -> do
        print $ Abstract.unSlotNo $ byronBlockSlotNo b


genesisTip :: Tip ByronBlock
genesisTip = Tip Abstract.genesisPoint Abstract.genesisBlockNo

-- How to run the node:
--
-- $ git clone https://github.com/input-output-hk/cardano-node
-- $ cd cardano-node
-- $ ./scripts/mainnet.sh
--
-- Then run main:
main :: IO ()
main = do
    network <- setup
    -- Use the network layer to send requests to the node
    Right response <- runExceptT $ nextBlocks network genesisTip
    prettyResponse response

--------------------------------------------------------------------------------
--
-- Stateless Interface
--
-- This is a first attempt at using the ChainSync mini protocol in a stateless
-- manner. Note that this is a bit unfortunate as the

-- | A stateless interface for dealing with the Haskell 'NetworkClient'
--
-- See 'newNetworkClient' and 'connectClient' methods to construct a
-- 'NetworkLayer':
--
--     let nodeId = CoreNodeId 0
--     let addr = localSocketAddrInfo (localSocketFilePath nodeId)
--     let params = ChainParameters
--          { epochSlots = EpochSlots 21600
--          , protocolMagic = ProtocolMagicId 1097911063
--          }
--     (client, network) <- newNetworkClient nullTracer params
--
--     -- Launch the stateful connection in a separate thread
--     void $ forkIO $ connectClient client dummyNodeToClientVersion addr
--
--     -- Use the network layer to send requests to the node
--     response <- runExceptT $ nextBlocks network genesisPoint
--     point <- networkTip network
--     response' <- runExceptT $ nextBlocks network point
--
data NetworkLayer m = NetworkLayer
    { nextBlocks :: (Tip ByronBlock) -> ExceptT ErrNextBlocks m [ByronBlock]
    , networkTip :: m (Tip ByronBlock)
    }

-- | This translates the 'NetworkLayer' interface from the wallet backend.
data ChainSyncRequest m
    = ReqNextBlocks (Tip ByronBlock) (Either ErrNextBlocks [ByronBlock] -> m ())
    | ReqNetworkTip ((Tip ByronBlock) -> m ())

-- | What is considered errors in the 'NetworkLayer'
data ErrNextBlocks
    = ErrNextBlocksNoIntersection
    | ErrNextBlocksRollBack
    deriving (Show)


--------------------------------------------------------------------------------
--
-- Concrete Types

type Tx = GenTx ByronBlock

data ChainParameters = ChainParameters
    { epochSlots :: EpochSlots
        -- ^ Number of slots per epoch.
    , protocolMagic :: ProtocolMagicId
        -- ^ Protocol magic (e.g. mainnet=764824073, testnet=1097911063)
    }

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
    'InitiatorApp
        -- ^ Initiator ~ Client (as opposed to Responder / Server)
    ConnectionId
        -- ^ An identifier for the peer: here, a local and remote socket.
    NodeToClientProtocols
        -- ^ Specifies which mini-protocols our client is talking.
        -- 'NodeToClientProtocols' allows for two mini-protocols:
        --  - Chain Sync
        --  - Tx submission
    m
        -- ^ Underlying monad we run in
    ByteString
        -- ^ Concrete representation for bytes string
    Void
        -- ^ -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- ^ Irrelevant for 'InitiatorApplication'. Return type of 'Responder'
        -- application.


--------------------------------------------------------------------------------
--
-- Network Client

-- Connect a client to a network, see `newNetworkClient` to construct a network
-- client interface.
--
-- >>> connectClient (newNetworkClient t params) dummyNodeToClientVersion addr
connectClient
    :: forall vData. (vData ~ NodeToClientVersionData)
    => NetworkClient IO
    -> (vData, CodecCBORTerm Text vData)
    -> AddrInfo
    -> IO ()
connectClient client (vData, vCodec) addr = do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_1 vData vDict client
    let connectTo' = connectTo $ NetworkConnectTracers nullTracer nullTracer
    -- TODO(anviking): what are these traces?
    connectTo' versions Nothing addr `catch` handleMuxError
  where
    -- `connectTo` might rise an exception: we are the client and the protocols
    -- specify that only  client can lawfuly close a connection, but the other
    -- side might just disappear.
    --
    -- NOTE: This handler does nothing.
    handleMuxError :: MuxError -> IO ()
    handleMuxError = throwIO

-- | A dummy network magic for a local cluster. When connecting to mainnet or
-- testnet, this should match the underlying network's configuration.
dummyNodeToClientVersion
    :: (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
dummyNodeToClientVersion =
    ( NodeToClientVersionData { networkMagic = NetworkMagic 764824073 }
    , nodeToClientCodecCBORTerm
    )

-- | Construct a new network client handling 'ChainSync' and 'LocalTxSubmission'
-- mini-protocols.
newNetworkClient
    :: forall m. (MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m String
    -> ChainParameters
    -> m (NetworkClient m, NetworkLayer m)
newNetworkClient t params = do
    chainSyncQueue <- atomically newTQueue

    let client = OuroborosInitiatorApplication $ \pid -> \case
            ChainSyncWithBlocksPtcl ->
                chainSyncWithBlocks @m pid t params chainSyncQueue
            LocalTxSubmissionPtcl ->
                localTxSubmission @m pid t

    let interface = NetworkLayer
            { nextBlocks = \p -> ExceptT $ do
                tvar <- newEmptyTMVarM
                let request = ReqNextBlocks p (atomically . putTMVar tvar)
                atomically $ writeTQueue chainSyncQueue request
                atomically $ takeTMVar tvar
            , networkTip = do
                tvar <- newEmptyTMVarM
                let request = ReqNetworkTip (atomically . putTMVar tvar)
                atomically $ writeTQueue chainSyncQueue request
                atomically $ takeTMVar tvar
            }

    return (client, interface)

-- | Client for the 'Chain Sync' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
chainSyncWithBlocks
    :: forall m protocol peerId. (protocol ~ ChainSync ByronBlock (Tip ByronBlock))
    => (MonadThrow m, MonadST m, MonadSTM m, Show peerId)
    => peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> Tracer m String
        -- ^ Base tracer for the mini-protocols
    -> ChainParameters
        -- ^ Some chain parameters necessary to encode/decode Byron 'Block'
    -> TQueue m (ChainSyncRequest m)
        -- ^ We use a 'TQueue' as a communication channel between the stateless
        -- interface and the stateful connection. Requests are pushed to the queue
        -- which handles them one-by-one.
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
chainSyncWithBlocks pid t params queue channel =
    runPeer trace codec pid channel (chainSyncClientPeer client)
  where
    trace :: Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
    trace = contramap show t

    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecChainSync
        encodeByronBlock
        (decodeByronBlock (epochSlots params))
        (Abstract.encodePoint encodeByronHeaderHash)
        (Abstract.decodePoint decodeByronHeaderHash)
        (Abstract.encodeTip encodeByronHeaderHash)
        (Abstract.decodeTip decodeByronHeaderHash)

    {-| A peer has agency if it is expected to send the next message.

                                    Agency
     ---------------------------------------------------------------------------
     Client has agency                 | Idle
     Server has agency                 | Intersect, Next

      *-----------*
      | Intersect |◀══════════════════════════════╗
      *-----------*         FindIntersect         ║
            │                                     ║
            │                                *---------*                *------*
            │ Intersect.{Unchanged,Improved} |         |═══════════════▶| Done |
            └───────────────────────────────╼|         |     MsgDone    *------*
                                             |   Idle  |
         ╔═══════════════════════════════════|         |
         ║            RequestNext            |         |⇦ START
         ║                                   *---------*
         ▼                                        ╿
      *------*       Roll.{Backward,Forward}      │
      | Next |────────────────────────────────────┘
      *------*

    -}

    client :: ChainSyncClient ByronBlock (Tip ByronBlock) m Void
    client = ChainSyncClient clientStIdle
      where
        -- Client in the state 'Idle'. We wait for requests / commands on an
        -- 'TQueue'. Commands start a chain of messages and state transitions
        -- before finally returning to 'Idle', waiting for the next command.
        clientStIdle :: m (ClientStIdle ByronBlock (Tip ByronBlock) m Void)
        clientStIdle = atomically (readTQueue queue) >>= \case
            ReqNextBlocks tip respond -> do
                let n = 1000 -- Arbitrary size of the batch to fetch
                let point = tipPoint tip

                -- Start by setting the intersection to a new point.
                --
                -- This is a "naive" approach. A better implementation would
                -- actually keep track of the latest tip, and check whether we
                -- need to re-define the intersection or, can simply continue
                -- with the one that was previously established (in practice, we
                -- do process blocks in sequence so, the `point` argument is
                -- rather anecdotal).
                pure $ SendMsgFindIntersect [point] $ ClientStIntersect
                    -- Node's internal cursor is now correctly set to 'point'
                    { recvMsgIntersectFound = \_intersection _tip ->
                        ChainSyncClient $
                            clientStFetchingBlocks n point [] respond

                    -- Couldn't find an intersection, point is not on the chain.
                    -- This can happen if the node has rolled back and the point
                    -- we knew of is no longer on the chain.
                    , recvMsgIntersectNotFound = \_tip ->
                        ChainSyncClient $ do
                            respond (Left ErrNextBlocksNoIntersection)
                            clientStIdle
                    }

            ReqNetworkTip respond -> do
                -- Alternatively, we could simply 'SendMsgRequestNext' which
                -- also returns a tip and is probably less expensive on the
                -- node's end.
                pure $ SendMsgFindIntersect [] $ ClientStIntersect
                    { recvMsgIntersectFound = \_intersection tip ->
                        ChainSyncClient $ respond tip *> clientStIdle
                    , recvMsgIntersectNotFound = \tip ->
                        ChainSyncClient $ respond tip *> clientStIdle
                    }


        -- 'Next' state coming from a 'ReqNextBlocks' request. We stay in that
        -- state for a couple of messages, accumulating blocks as we go and
        -- returning after a certain number has been fetched.
        --
        -- Note that the process is interrupted prematurely if there's no more
        -- block to fetched (caught up with the tip). Also, we consider
        -- rollbacks as an error since the wallet engine can't handle them at
        -- the moment.
        clientStFetchingBlocks
            :: Int
                -- ^ Number of messages to fetch
            -> Point ByronBlock
                -- ^ Starting point
            -> [ByronBlock]
                -- ^ ByronBlock accumulator
            -> (Either ErrNextBlocks [ByronBlock] -> m ())
                -- ^ Success or Error callback
            -> m (ClientStIdle ByronBlock (Tip ByronBlock) m Void)
        clientStFetchingBlocks 0 _ blocks respond = do
            respond (Right blocks)
            clientStIdle
        clientStFetchingBlocks n start blocks respond = pure $ SendMsgRequestNext
            (ClientStNext
                { recvMsgRollForward = \block _tip -> ChainSyncClient $
                    clientStFetchingBlocks (n-1) start (block:blocks) respond

                , recvMsgRollBackward = \point _tip -> ChainSyncClient $
                    -- NOTE
                    -- The server always start by asking the client to rollback
                    -- to the intersection point. Other form of rollbacks aren't
                    -- _yet_ expected but will eventually have to be handled.
                    -- For now, we consider this to be an error.
                    if point == start
                        then clientStFetchingBlocks n start [] respond
                        else do
                            respond (Left ErrNextBlocksRollBack)
                            clientStIdle
                }
            )
            (do
                -- NOTE
                -- In the 'Next' state, a client can receive either an immediate
                -- response (previous handler), or a response indicating that
                -- the blocks can't be retrieve _on the moment_ but will be
                -- after a while. In this case, we simply yield a response to
                -- our interface immediately and, discard whatever messages we
                -- eventually get back from the node once the block becomes
                -- available.
                respond (Right [])
                pure $ ClientStNext
                    { recvMsgRollForward = \_block _tip ->
                        ChainSyncClient clientStIdle
                    , recvMsgRollBackward = \_point _tip ->
                        ChainSyncClient clientStIdle
                    }
            )

-- | Client for the 'Local Tx Submission' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
localTxSubmission
    :: forall m protocol peerId. ()
    => (MonadThrow m, MonadTimer m, MonadST m, Show peerId)
    => (protocol ~ LocalTxSubmission Tx String)
    => peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> Tracer m String
        -- ^ Base tracer for the mini-protocols
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
localTxSubmission pid t channel =
    runPeer trace codec pid channel (localTxSubmissionClientPeer client)
  where
    trace :: Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
    trace = contramap show t

    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecLocalTxSubmission
        encodeByronGenTx -- Tx -> CBOR.Encoding
        decodeByronGenTx -- CBOR.Decoder s Tx
        CBOR.encode      -- String -> CBOR.Encoding
        CBOR.decode      -- CBOR.Decoder s String

    {-| A peer has agency if it is expected to send the next message.

                                    Agency
     ---------------------------------------------------------------------------
     Client has agency                 | Idle
     Server has agency                 | Busy

      *-----------*
      |    Busy   |◀══════════════════════════════╗
      *-----------*            SubmitTx           ║
         │     │                                  ║
         │     │                             *---------*                *------*
         │     │        AcceptTx             |         |═══════════════▶| Done |
         │     └────────────────────────────╼|         |     MsgDone    *------*
         │              RejectTx             |   Idle  |
         └──────────────────────────────────╼|         |
                                             |         |⇦ START
                                             *---------*

    -}
    client :: LocalTxSubmissionClient Tx String m Void
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

localSocketFilePath :: CoreNodeId -> FilePath
localSocketFilePath (CoreNodeId n) =
    "node-core-" ++ show n ++ ".socket"
