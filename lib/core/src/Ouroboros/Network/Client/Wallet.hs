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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Ourouboros mini-protocols clients for implementing cardano-wallet. These
-- clients implement the logic and lift away concerns related to concrete
-- data-type representation so that the code can be re-used / shared between
-- Byron and Shelley.
module Ouroboros.Network.Client.Wallet
    (
      -- * ChainSyncFollowTip
      chainSyncFollowTip

      -- * ChainSyncWithBlocks
    , ChainSyncCmd (..)
    , chainSyncWithBlocks

      -- * LocalTxSubmission
    , LocalTxSubmissionCmd (..)
    , localTxSubmission

      -- * LocalStateQuery
    , LocalStateQueryCmd (..)
    , LocalStateQueryResult
    , localStateQuery

      -- * Helpers
    , send
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ErrNetworkUnavailable (..), NextBlocksResult (..) )
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
import Data.Word
    ( Word32 )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Network.Mux
    ( AppType (..), MuxError (..), MuxErrorType (..), WithMuxBearer )
import Network.TypedProtocol.Codec
    ( Codec )
import Network.TypedProtocol.Pipelined
    ( N (..), Nat (..), natToInt )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Ledger.Abstract
    ( Query (..) )
import Ouroboros.Consensus.Node.Run
    ( RunNode (..) )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , HasHeader (..)
    , Point (..)
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
    ( TraceSendRecv, runPeer, runPipelinedPeer )
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
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..), chainSyncClientPeerPipelined )
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
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as P
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

--------------------------------------------------------------------------------
--
-- chainSyncFollowTip

-- | Client for the 'Chain Sync' mini-protocol, which provides notifications
-- when the node tip changes.
--
-- This is used in the same way as 'chainSyncWithBlocks', except that only one
-- of these clients is necessary, rather than one client per wallet.
chainSyncFollowTip
    :: forall m block. (Monad m)
    => (Tip block -> m ())
    -- ^ Callback for when the tip changes.
    -> ChainSyncClient (Serialised block) (Tip (Serialised block)) m Void
chainSyncFollowTip onTipUpdate =
    ChainSyncClient (clientStIdle False)
  where
    -- Client in the state 'Idle'. We immediately request the next block.
    clientStIdle
        :: Bool
        -> m (ClientStIdle (Serialised block) (Tip (Serialised block)) m Void)
    clientStIdle synced = pure $ SendMsgRequestNext
        (clientStNext synced)
        (pure $ clientStNext synced)

    -- In the CanAwait state, we take the tip point given by the node and
    -- ask for the intersection of that point. This fast-fowards us to the
    -- tip. Once synchronised with the tip, we expect to be waiting for the
    -- server to send AwaitReply most of the time.
    clientStNext
        :: Bool
        -> ClientStNext (Serialised block) (Tip (Serialised block)) m Void
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
        :: ClientStIntersect (Serialised block) (Tip (Serialised block)) m Void
    clientStIntersect = ClientStIntersect
        { recvMsgIntersectFound = \_intersection _tip ->
            ChainSyncClient $ clientStIdle True
        , recvMsgIntersectNotFound = \_tip ->
            ChainSyncClient $ clientStIdle False
        }

--------------------------------------------------------------------------------
--
-- chainSyncWithBlocks

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
data ChainSyncCmd (m :: * -> *) block
    = CmdFindIntersection
        [Point block]
        (Maybe (Point block) -> m ())
    | CmdNextBlocks
        (NextBlocksResult (Point block) block -> m ())

-- | A little type-alias to ease signatures in 'chainSyncWithBlocks'
type RequestNextStrategy m n block
    =  (NextBlocksResult (Point block) block -> m ())
    -> P.ClientPipelinedStIdle n block (Tip block) m Void

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
    :: forall m block. (Monad m, MonadSTM m, HasHeader block)
    => Quantity "block" Word32
        -- ^ Epoch stability (a.k.a k)
    -> (Tip block -> W.BlockHeader)
        -- ^ Convert an abstract tip to a concrete 'BlockHeader'
        --
        -- TODO: We probably need a better type for representing Tip as well!
    -> TQueue m (ChainSyncCmd m block)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> ChainSyncClientPipelined block (Tip block) m Void
chainSyncWithBlocks (Quantity epochStability) fromTip queue =
    ChainSyncClientPipelined (clientStIdle oneByOne)
  where
    k :: Natural
    k = fromIntegral epochStability

    -- TODO
    -- This should probably be a function argument.
    maxInFlight :: Natural
    maxInFlight = 1000

    -- Return the _number of slots between two tips.
    tipDistance :: BlockNo -> Tip block -> Natural
    tipDistance (BlockNo n) TipGenesis =
        1 + fromIntegral n
    tipDistance (BlockNo n) (Tip _ _ (BlockNo n')) =
        fromIntegral @Integer $ abs $ fromIntegral n - fromIntegral n'

    -- Client in the state 'Idle'. We wait for requests / commands on an
    -- 'TQueue'. Commands start a chain of messages and state transitions
    -- before finally returning to 'Idle', waiting for the next command.
    clientStIdle
        :: RequestNextStrategy m 'Z block
        -> m (P.ClientPipelinedStIdle 'Z block (Tip block) m Void)
    clientStIdle strategy = atomically (readTQueue queue) >>= \case
        CmdFindIntersection points respond -> pure $
            P.SendMsgFindIntersect points (clientStIntersect respond)
        CmdNextBlocks respond ->
            pure $ strategy respond

    clientStIntersect
        :: (Maybe (Point block) -> m ())
        -> P.ClientPipelinedStIntersect block (Tip block) m Void
    clientStIntersect respond = P.ClientPipelinedStIntersect
        { recvMsgIntersectFound = \intersection _tip -> do
            respond (Just intersection)
            clientStIdle oneByOne

        , recvMsgIntersectNotFound = \_tip -> do
            respond Nothing
            clientStIdle oneByOne
        }

    -- Simple strategy that sends a request and waits for an answer.
    oneByOne
        :: RequestNextStrategy m 'Z block
    oneByOne respond = P.SendMsgRequestNext
        (collectResponses respond [] Zero)
        (pure $ collectResponses respond [] Zero)

    -- We only pipeline requests when it's safe to pipeline them. Safe
    -- is when all blocks are considered 'stable' and immutable, i.e.
    -- when we land more than `k` blocks from the tip, so when we start
    -- more than `k + maxInFlight` blocks from the tip
    --
    --     0                       tip - k           tip
    --     |---------------------------|-------------->
    --               pipelined            one by one
    --
    pipeline
        :: Nat n
        -> RequestNextStrategy m n block
    pipeline (Succ n) respond | natToInt (Succ n) == fromIntegral maxInFlight =
        P.CollectResponse Nothing $ collectResponses respond [] n
    pipeline n respond =
        P.SendMsgRequestNextPipelined $ pipeline (Succ n) respond

    collectResponses
        :: (NextBlocksResult (Point block) block -> m ())
        -> [block]
        -> Nat n
        -> P.ClientStNext n block (Tip block) m Void
    collectResponses respond blocks Zero = P.ClientStNext
        { P.recvMsgRollForward = \block tip -> do
            let cursor' = blockPoint block
            let blocks' = reverse (block:blocks)
            let tip'    = fromTip tip
            respond (RollForward cursor' tip' blocks')
            let distance = tipDistance (blockNo block) tip
            let strategy = if distance > k + maxInFlight
                    then pipeline Zero
                    else oneByOne
            clientStIdle strategy

        , P.recvMsgRollBackward = \point _tip -> do
            respond (RollBackward point)
            clientStIdle oneByOne
        }
    collectResponses respond blocks (Succ n) = P.ClientStNext
        { P.recvMsgRollForward = \block _tip -> pure $
            P.CollectResponse Nothing $ collectResponses respond (block:blocks) n
        , P.recvMsgRollBackward = \_point _tip -> pure $ error
            "rolled backward while pipelining requests! This shouldn't be \
            \possible because we are making sure to only pipeline requests \
            \in areas where the node can't possibly roll back."
        }

--------------------------------------------------------------------------------
--
-- LocalStateQuery

-- | Command to send to the localStateQuery client. See also 'ChainSyncCmd'.
data LocalStateQueryCmd (m :: * -> *) block state
    = CmdQueryLocalState
        (Point block)
        (Query block state)
        (LocalStateQueryResult state -> m ())

-- | Shorthand for the possible outcomes of acquiring local state parameters.
type LocalStateQueryResult state = Either AcquireFailure state

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
    :: forall m block state.  (MonadThrow m, MonadTimer m)
    => TQueue m (LocalStateQueryCmd m block state)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> LocalStateQueryClient block (Query block) m Void
localStateQuery queue =
    LocalStateQueryClient clientStIdle
  where
    clientStIdle
        :: m (LSQ.ClientStIdle block (Query block) m Void)
    clientStIdle = awaitNextCmd <&> \case
        CmdQueryLocalState pt query respond ->
            LSQ.SendMsgAcquire pt (clientStAcquiring query respond)

    clientStAcquiring
        :: Query block state
        -> (LocalStateQueryResult state -> m ())
        -> LSQ.ClientStAcquiring block (Query block) m Void
    clientStAcquiring query respond = LSQ.ClientStAcquiring
        { recvMsgAcquired = clientStAcquired query respond
        , recvMsgFailure = \failure -> do
                respond (Left failure)
                clientStIdle
        }

    clientStAcquired
        :: Query block state
        -> (LocalStateQueryResult state -> m ())
        -> LSQ.ClientStAcquired block (Query block) m Void
    clientStAcquired query respond =
        LSQ.SendMsgQuery query (clientStQuerying respond)

    -- By re-acquiring rather releasing the state with 'MsgRelease' it
    -- enables optimisations on the server side.
    clientStAcquiredAgain
        :: m (LSQ.ClientStAcquired block (Query block) m Void)
    clientStAcquiredAgain = awaitNextCmd <&> \case
        CmdQueryLocalState pt query respond ->
            LSQ.SendMsgReAcquire pt (clientStAcquiring query respond)

    clientStQuerying
        :: (LocalStateQueryResult state -> m ())
        -> LSQ.ClientStQuerying block (Query block) m Void state
    clientStQuerying respond = LSQ.ClientStQuerying
        { recvMsgResult = \result -> do
            respond (Right result)
            clientStAcquiredAgain
        }

    awaitNextCmd :: m (LocalStateQueryCmd m block state)
    awaitNextCmd = atomically $ readTQueue queue

--------------------------------------------------------------------------------
--
-- LocalTxSubmission


-- | Sending command to the localTxSubmission client. See also 'ChainSyncCmd'.
data LocalTxSubmissionCmd (m :: * -> *) tx err
    = CmdSubmitTx tx (Maybe err -> m ())

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
    :: forall m tx err. ( MonadThrow m, MonadTimer m)
    => TQueue m (LocalTxSubmissionCmd m tx err)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> LocalTxSubmissionClient tx err m Void
localTxSubmission queue =
    LocalTxSubmissionClient clientStIdle
  where
    clientStIdle
        :: m (LocalTxClientStIdle tx err m Void)
    clientStIdle = atomically (readTQueue queue) <&> \case
        CmdSubmitTx tx respond ->
            SendMsgSubmitTx tx (\e -> respond e >> clientStIdle)

--------------------------------------------------------------------------------
--
-- Helpers

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
