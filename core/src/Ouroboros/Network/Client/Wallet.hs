{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Ouroboros mini-protocols clients for implementing cardano-wallet. These
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

      -- * Logs
    , ChainSyncLog (..)
    , mapChainSyncLog
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Slotting.Slot
    ( WithOrigin (..) )
import Cardano.Wallet.Network
    ( NextBlocksResult (..) )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , TQueue
    , atomically
    , isEmptyTQueue
    , newEmptyTMVarM
    , putTMVar
    , readTQueue
    , takeTMVar
    , tryReadTQueue
    , writeTQueue
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Functor
    ( (<&>) )
import Data.Maybe
    ( isNothing )
import Data.Text.Class
    ( ToText (..) )
import Data.Void
    ( Void )
import Network.TypedProtocol.Pipelined
    ( N (..), Nat (..), natToInt )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Ledger.Abstract
    ( Query )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , HasHeader (..)
    , Point (..)
    , Serialised (..)
    , Tip (..)
    , blockNo
    , blockPoint
    , blockSlot
    , castTip
    , getTipPoint
    , pointSlot
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( ClientStAcquiring (..)
    , ClientStQuerying (..)
    , LocalStateQueryClient (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Cardano.Wallet.Primitive.Types as W
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
    -> ChainSyncClient (Serialised block) (Tip (block)) m Void
chainSyncFollowTip onTipUpdate =
    ChainSyncClient (clientStIdle False)
  where
    -- Client in the state 'Idle'. We immediately request the next block.
    clientStIdle
        :: Bool
        -> m (ClientStIdle (Serialised block) (Tip block) m Void)
    clientStIdle synced = pure $ SendMsgRequestNext
        (clientStNext synced)
        (pure $ clientStNext synced)

    -- In the CanAwait state, we take the tip point given by the node and
    -- ask for the intersection of that point. This fast-fowards us to the
    -- tip. Once synchronised with the tip, we expect to be waiting for the
    -- server to send AwaitReply most of the time.
    clientStNext
        :: Bool
        -> ClientStNext (Serialised block) (Tip block) m Void
    clientStNext False = ClientStNext
            { recvMsgRollBackward = const findIntersect
            , recvMsgRollForward = const findIntersect
            }
      where
        findIntersect tip = ChainSyncClient $
            pure $ SendMsgFindIntersect [getTipPoint $ castTip tip] clientStIntersect

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
        :: ClientStIntersect (Serialised block) (Tip block) m Void
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
data ChainSyncCmd block (m :: * -> *)
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
-- Once started, the client simply runs ad-infinitum but one may
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
    => Tracer m (ChainSyncLog block (Point block))
    -> (Tip block -> W.BlockHeader)
        -- ^ Convert an abstract tip to a concrete 'BlockHeader'
        --
        -- TODO: We probably need a better type for representing Tip as well!

    -> TQueue m (ChainSyncCmd block m)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.

    -> TQueue m (NextBlocksResult (Point block) block)
        -- ^ An internal queue used for buffering responses collected while
        -- pipelining. As argument to simplify code below. Responses are first
        -- poped from this buffer if not empty, otherwise they'll simply trigger
        -- an exchange with the node.

    -> ChainSyncClientPipelined block (Tip block) m Void
chainSyncWithBlocks tr fromTip queue responseBuffer =
    ChainSyncClientPipelined $ clientStIdle oneByOne
  where
    -- Return the _number of slots between two tips.
    tipDistance :: BlockNo -> Tip block -> Natural
    tipDistance (BlockNo n) TipGenesis =
        1 + fromIntegral n
    tipDistance (BlockNo n) (Tip _ _ (BlockNo n')) =
        fromIntegral @Integer $ abs $ fromIntegral n - fromIntegral n'

    -- | Keep only blocks from the list that are before or exactly at the given
    -- point.
    rollback :: Point block -> [block] -> [block]
    rollback pt = filter (\b -> At (blockSlot b) <= pointSlot pt)

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
            -- We are the only consumer & producer of this queue, so it's fine
            -- to run 'isEmpty' and 'read' in two separate atomatic operations.
            atomically (isEmptyTQueue responseBuffer) >>= \case
                True  ->
                    pure $ strategy respond
                False -> do
                    atomically (readTQueue responseBuffer) >>= respond
                    clientStIdle strategy

    -- When the client intersect, we are effectively starting "a new session",
    -- so any buffered responses no longer apply and must be discarded.
    clientStIntersect
        :: (Maybe (Point block) -> m ())
        -> P.ClientPipelinedStIntersect block (Tip block) m Void
    clientStIntersect respond = P.ClientPipelinedStIntersect
        { recvMsgIntersectFound = \intersection _tip -> do
            respond (Just intersection)
            flush responseBuffer
            clientStIdle oneByOne

        , recvMsgIntersectNotFound = \_tip -> do
            respond Nothing
            flush responseBuffer
            clientStIdle oneByOne
        }

    -- Simple strategy that sends a request and waits for an answer.
    oneByOne
        :: RequestNextStrategy m 'Z block
    oneByOne respond = P.SendMsgRequestNext
        (collectResponses respond [] Zero)
        (pure $ collectResponses respond [] Zero)

    -- We only pipeline requests when we are far from the tip. As soon as we
    -- reach the tip however, there's no point pipelining anymore, so we start
    -- collecting responses one by one.
    --
    --     0                                  tip
    --     |-----------------------------------|----->
    --                   pipelined               one by one
    pipeline
        :: Int
        -> Nat n
        -> RequestNextStrategy m n block
    pipeline goal (Succ n) respond | natToInt (Succ n) == goal =
        P.CollectResponse Nothing $ collectResponses respond [] n
    pipeline goal n respond =
        P.SendMsgRequestNextPipelined $ pipeline goal (Succ n) respond

    collectResponses
        :: (NextBlocksResult (Point block) block -> m ())
        -> [block]
        -> Nat n
        -> P.ClientStNext n block (Tip block) m Void
    collectResponses respond blocks Zero = P.ClientStNext
        { P.recvMsgRollForward = \block tip -> do
            traceWith tr $ MsgChainRollForward block (getTipPoint tip)
            let cursor' = blockPoint block
            let blocks' = reverse (block:blocks)
            let tip'    = fromTip tip
            respond (RollForward cursor' tip' blocks')
            let distance = tipDistance (blockNo block) tip
            traceWith tr $ MsgTipDistance distance
            let strategy = if distance <= 1
                    then oneByOne
                    else pipeline (fromIntegral $ min distance 1000) Zero
            clientStIdle strategy

        -- When the last message we receive is a request to rollback, we have
        -- two possibilities:
        --
        -- a) Either, we are asked to rollback to a point that is within the
        -- blocks we have just collected. So it suffices to remove blocks from
        -- the list, and apply the remaining portion.
        --
        -- b) We are asked to rollback even further and discard all the blocks
        -- we just collected. In which case, we simply discard all blocks and
        -- rollback to that point as if nothing happened.
        , P.recvMsgRollBackward = \point tip -> do
            case rollback point blocks of
                [] -> do -- b)
                    traceWith tr $ MsgChainRollBackward point 0
                    respond (RollBackward point)
                    clientStIdle oneByOne

                xs -> do -- a)
                    traceWith tr $ MsgChainRollBackward point (length xs)
                    let cursor' = blockPoint $ head xs
                    let blocks' = reverse xs
                    let tip'    = fromTip tip
                    respond (RollForward cursor' tip' blocks')
                    clientStIdle oneByOne
        }

    collectResponses respond blocks (Succ n) = P.ClientStNext
        { P.recvMsgRollForward = \block _tip ->
        pure $ P.CollectResponse Nothing $ collectResponses respond (block:blocks) n

        -- This scenario is slightly more complicated than for the 'Zero' case.
        -- Again, there are two possibilities:
        --
        -- a) Either we rollback to a point we have just collected, so it
        -- suffices to discard blocks from the list and continue.
        --
        -- b) Or, we need to reply immediately, but we still have to collect the
        -- remaining responses. BUT, we can only reply once to a given command.
        -- So instead, we buffer all the remaining responses in a queue and, upon
        -- receiving future requests, we'll simply read them from the buffer!
        , P.recvMsgRollBackward = \point _tip ->
            case rollback point blocks of
                [] -> do -- b)
                    let save = atomically . writeTQueue responseBuffer
                    respond (RollBackward point)
                    pure $ P.CollectResponse Nothing $ collectResponses save [] n
                xs -> do -- a)
                    pure $ P.CollectResponse Nothing $ collectResponses respond xs n
        }



--------------------------------------------------------------------------------
--
-- LocalStateQuery

-- | Command to send to the localStateQuery client. See also 'ChainSyncCmd'.
data LocalStateQueryCmd block (m :: * -> *)
    = forall state. CmdQueryLocalState
        (Point block)
        (Query block state)
        (LocalStateQueryResult state -> m ())

-- | Shorthand for the possible outcomes of acquiring local state parameters.
type LocalStateQueryResult state = Either AcquireFailure state

-- | Client for the 'Local State Query' mini-protocol.
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
    :: forall m block . (MonadThrow m, MonadSTM m)
    => TQueue m (LocalStateQueryCmd block m)
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
        :: forall state. Query block state
        -> (LocalStateQueryResult state -> m ())
        -> LSQ.ClientStAcquiring block (Query block) m Void
    clientStAcquiring query respond = LSQ.ClientStAcquiring
        { recvMsgAcquired = clientStAcquired query respond
        , recvMsgFailure = \failure -> do
                respond (Left failure)
                clientStIdle
        }

    clientStAcquired
        :: forall state. Query block state
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
        :: forall state. (LocalStateQueryResult state -> m ())
        -> LSQ.ClientStQuerying block (Query block) m Void state
    clientStQuerying respond = LSQ.ClientStQuerying
        { recvMsgResult = \result -> do
            respond (Right result)
            clientStAcquiredAgain
        }

    awaitNextCmd :: m (LocalStateQueryCmd block m)
    awaitNextCmd = atomically $ readTQueue queue

--------------------------------------------------------------------------------
--
-- LocalTxSubmission


-- | Sending command to the localTxSubmission client. See also 'ChainSyncCmd'.
data LocalTxSubmissionCmd tx err (m :: * -> *)
    = CmdSubmitTx tx (SubmitResult err -> m ())

-- | Client for the 'Local Tx Submission' mini-protocol.
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
    :: forall m tx err. (MonadThrow m, MonadSTM m)
    => TQueue m (LocalTxSubmissionCmd tx err m)
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

flush :: (MonadSTM m) => TQueue m a -> m ()
flush queue =
    atomically $ dropUntil isNothing queue
  where
    dropUntil predicate q =
        (predicate <$> tryReadTQueue q) >>= \case
            True  -> pure ()
            False -> dropUntil predicate q

-- | Helper function to easily send commands to the node's client and read
-- responses back.
--
-- >>> queue `send` CmdNextBlocks
-- RollForward cursor nodeTip blocks
--
-- >>> queue `send` CmdNextBlocks
-- AwaitReply
send
    :: MonadSTM m
    => TQueue m (cmd m)
    -> ((a -> m ()) -> cmd m)
    -> m a
send queue cmd = do
    tvar <- newEmptyTMVarM
    atomically $ writeTQueue queue (cmd (atomically . putTMVar tvar))
    atomically $ takeTMVar tvar

-- Tracing

data ChainSyncLog block point
    = MsgChainRollForward block point
    | MsgChainRollBackward point Int
    | MsgTipDistance Natural

mapChainSyncLog
    :: (b1 -> b2)
    -> (p1 -> p2)
    -> ChainSyncLog b1 p1
    -> ChainSyncLog b2 p2
mapChainSyncLog f g = \case
    MsgChainRollForward block point -> MsgChainRollForward (f block) (g point)
    MsgChainRollBackward point n -> MsgChainRollBackward (g point) n
    MsgTipDistance d -> MsgTipDistance d

instance (ToText block, ToText point)
    => ToText (ChainSyncLog block point) where
    toText = \case
        MsgChainRollForward b tip ->
            "ChainSync roll forward: " <> toText b <> " tip is " <> toText tip
        MsgChainRollBackward b 0 ->
            "ChainSync roll backward: " <> toText b
        MsgChainRollBackward b bufferSize -> mconcat
            [ "ChainSync roll backward: "
            , toText b
            , ", handled inside buffer with remaining length "
            , toText bufferSize
            ]
        MsgTipDistance d -> "Tip distance: " <> toText d

instance HasPrivacyAnnotation (ChainSyncLog block point)

instance HasSeverityAnnotation (ChainSyncLog block point) where
    getSeverityAnnotation = \case
        MsgChainRollForward{} -> Debug
        MsgChainRollBackward{} -> Debug
        MsgTipDistance{} -> Debug
