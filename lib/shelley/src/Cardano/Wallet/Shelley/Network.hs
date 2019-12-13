{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
--{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints
--   -fno-warn-unused-matches -fno-warn-unused-local-binds #-}

{-
Good to read before / additional resources:

- Module's documentation in `ouroboros-network/typed-protocols/src/Network/TypedProtocols.hs`
- Data Diffusion and Peer Networking in Shelley (see: https://raw.githubusercontent.com/wiki/input-output-hk/cardano-wallet/data_diffusion_and_peer_networking_in_shelley.pdf)
    - In particular sections 4.1, 4.2, 4.6 and 4.8


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
-}

module Cardano.Wallet.Shelley.Network
    ( main
    , convertBlockHeader
    , convertTx
      -- * Top-Level Interface
    , ChainParameters (..)

    -- * Re-Export
    , EpochSlots (..)
    , ProtocolMagicId (..)

      -- * Constructors
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
    , ByronHash (..)
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
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTimer
    ( MonadTimer )
import Control.Tracer
    ( Tracer, contramap )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Void
    ( Void )
import Network.Socket
    ( AddrInfo (..), Family (..), SockAddr (..), SocketType (..) )

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.Serialise as CBOR
import qualified Data.ByteArray as BA
import qualified Data.List.NonEmpty as NE
import qualified Network.Socket as Socket
import qualified Ouroboros.Consensus.Ledger.Byron as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

-- ----------------
-- Development

import Cardano.BM.Trace
    ( nullTracer )
import Control.Monad
    ( (>=>) )

convertTx :: CC.Tx -> W.Tx
convertTx tx = W.Tx
    { W.txId = convertHash $ CC.hash tx
    , W.resolvedInputs = map convertTxIn $ NE.toList $ CC.txInputs tx
    , W.outputs = map convertTxOut $ NE.toList $ CC.txOutputs tx
    }
  where
    convertHash = W.Hash . BA.convert

    convertTxIn (CC.TxInUtxo txH idx) = (W.TxIn (convertHash txH) idx, coin)
       where
          coin = error "1. We can't get the coin of Byron inputs!"

    convertTxOut o = W.TxOut
        (convertAddr $ CC.txOutAddress o)
        (convertCoin $ CC.txOutValue o)
      where
        convertAddr = error "todo use toBinary"
        -- Byron addresses should be no problem. I'm not sure how Shelley
        -- addresses will look.
        convertCoin = W.Coin . CC.unsafeGetLovelace

convertBlockHeader :: ByronBlock -> W.BlockHeader
convertBlockHeader b = W.BlockHeader
    { W.slotId =
        error "2. can't easily get the SlotId"
            -- The cardano-ledger equivalent is EpochAndSlotCount, i.e epoch
            -- number and the slot counted from the start of the epoch.
            --
            -- But block headers only contain the slot number (SlotNo), counted
            -- from genesis.
    , W.blockHeight = convertBlockNo $ O.blockNo b
            -- Not in block headers.
    , W.headerHash = convertHash $ O.blockHash b
    , W.parentHeaderHash = convertChainHash $ O.blockPrevHash b
    }
  where
    convertHash = W.Hash . BA.convert . O.unByronHash

    convertChainHash x = case x of
        O.BlockHash h ->
            convertHash h
        O.GenesisHash ->
            error "how do we represent the genesis hash?"
            -- Seems like a minor problem.
    convertBlockNo = Quantity . fromIntegral . O.unBlockNo

-- Goal: see what info we can extract from blocks!
rollForward :: ByronBlock -> IO ()
rollForward b = do
    let slot = show (O.unSlotNo . byronBlockSlotNo $ b)
    let hash = show $ byronBlockHash b
    putStrLn "\n\n"
    putStrLn $ "=== SlotNo " ++ slot
    putStrLn $ "=== " ++ hash
    putStrLn "\n"
    case byronBlockRaw b  of
        CC.ABOBBlock cb -> do
            let body = CC.blockBody cb
            let txPay = CC.bodyTxPayload body
            let txAuxs = CC.aUnTxPayload txPay
            let txs = map CC.taTx txAuxs
            print $ map convertTx txs
        CC.ABOBBoundary _ ->
            putStrLn "Our friend the Epoch Boundary Block!"

rollBackward :: Point ByronBlock -> IO ()
rollBackward p = do
    putStrLn $ "rollback to: " ++ show p


-- A point where there is high activity on the mainnet
point1 :: Point ByronBlock
point1 =
    let
        Right h = CC.decodeAbstractHash "8a7505ec268a934c9c532fcf21185baf849c0de04a6d12cdf80b0fc45fe6ecb3"
        slot = 21600*19+6540

    in O.Point $ Point.block
        (O.SlotNo slot)
        (ByronHash h)

-- How to run the node:
--
-- $ git clone https://github.com/input-output-hk/cardano-node
-- $ cd cardano-node
-- $ ./scripts/mainnet.sh
--
-- Then run main:
main :: IO ()
main = do
    -- I have cardano-node running from ../cardano-node
    let nodeId = CoreNodeId 0
    let path = "../cardano-node/socket/" <> (localSocketFilePath nodeId)
    let addr =  localSocketAddrInfo path

    let params = ChainParameters
         { epochSlots = EpochSlots 21600
         , protocolMagic = ProtocolMagicId 764824073
         }

    let t = nullTracer
    let client = OuroborosInitiatorApplication $ \pid -> \case
            ChainSyncWithBlocksPtcl ->
                chainSyncWithBlocks pid t params rollForward rollBackward
            LocalTxSubmissionPtcl ->
                (localTxSubmission pid t) >=> const (return ())
    connectClient client dummyNodeToClientVersion addr

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
    ()
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


-- | Client for the 'Chain Sync' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
chainSyncWithBlocks
    :: forall m protocol peerId. (protocol ~ ChainSync ByronBlock (Tip ByronBlock))
    => (MonadThrow m, Show peerId, MonadST m)
    => peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> Tracer m String
        -- ^ Base tracer for the mini-protocols
    -> ChainParameters
        -- ^ Some chain parameters necessary to encode/decode Byron 'Block'
    -> (ByronBlock -> m ())
        -- ^ Action to take when receiving a block
    -> (Point ByronBlock -> m ())
        -- ^ Action to take when receiving a block
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m ()
chainSyncWithBlocks pid t params forward backward channel =
    runPeer trace codec pid channel (chainSyncClientPeer client)
  where
    trace :: Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
    trace = contramap show t

    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecChainSync
        encodeByronBlock
        (decodeByronBlock (epochSlots params))
        (O.encodePoint encodeByronHeaderHash)
        (O.decodePoint decodeByronHeaderHash)
        (O.encodeTip encodeByronHeaderHash)
        (O.decodeTip decodeByronHeaderHash)

    client :: ChainSyncClient ByronBlock (Tip ByronBlock) m ()
    client = ChainSyncClient clientStIdle
      where

        -- Find intersection between wallet and node chains.
        clientStIdle :: m (ClientStIdle ByronBlock (Tip ByronBlock) m ())
        clientStIdle = do
                let n = 100 -- Arbitrary size of the batch to fetch
                let point = point1
                pure $ SendMsgFindIntersect [point] $ ClientStIntersect
                    { recvMsgIntersectFound = \_intersection _tip ->
                        ChainSyncClient $
                            clientStFetchingBlocks n point
                    , recvMsgIntersectNotFound = \_tip ->
                        ChainSyncClient $ do
                            clientStIdle
                    }

        clientStFetchingBlocks
            :: Int
                -- ^ Number of messages to fetch
            -> Point ByronBlock
                -- ^ Starting point
            -> m (ClientStIdle ByronBlock (Tip ByronBlock) m ())
        clientStFetchingBlocks 0 _ = do
            pure $ SendMsgDone ()
        clientStFetchingBlocks n start = pure $ SendMsgRequestNext
            (ClientStNext
                { recvMsgRollForward = \block _tip -> ChainSyncClient $ do
                    forward block
                    clientStFetchingBlocks (n-1) start
                , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
                    backward point
                    clientStFetchingBlocks n start
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
