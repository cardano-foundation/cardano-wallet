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
    , chainSyncWithBlocks

      -- * LocalTxSubmission
    , LocalTxSubmissionCmd (..)
    , localTxSubmission

      -- * LocalStateQuery
    , LSQ (..)
    , LocalStateQueryCmd (..)
    , localStateQuery

      -- * Helpers
    , send
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( Tracer, traceWith )
import Cardano.Slotting.Slot
    ( WithOrigin (..) )
import Cardano.Wallet.Network
    ( ChainFollower (..), ChainSyncLog (..) )
import Control.Monad
    ( ap, liftM )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , TQueue
    , atomically
    , newEmptyTMVarIO
    , peekTQueue
    , putTMVar
    , readTQueue
    , takeTMVar
    , tryReadTQueue
    , writeTQueue
    )
import Control.Monad.Class.MonadThrow
    ( Exception, MonadThrow, throwIO )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.Functor
    ( (<&>) )
import Data.Kind
    ( Type )
import Data.List
    ( sortBy )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( comparing )
import Data.Void
    ( Void )
import Network.TypedProtocol.Pipelined
    ( N (..), Nat (..), natToInt )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( BlockQuery )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..) )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , HasHeader (..)
    , Point (..)
    , Tip (..)
    , blockNo
    , blockPoint
    , blockSlot
    , castTip
    , getTipPoint
    , pointSlot
    )
import Ouroboros.Network.Point
    ( blockPointSlot )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( ClientStAcquiring (..), LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Data.List.NonEmpty as NE
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
    :: forall m block era. (Monad m)
    => (block -> era)
    -> (Maybe era -> Tip block -> m ())
    -- ^ Callback for when the tip changes.
    -> ChainSyncClient block (Point block) (Tip block) m Void
chainSyncFollowTip toCardanoEra onTipUpdate =
    ChainSyncClient (clientStIdle False)
  where
    -- Client in the state 'Idle'. We immediately request the next block.
    clientStIdle
        :: Bool
        -> m (ClientStIdle block (Point block) (Tip block) m Void)
    clientStIdle synced = pure $ SendMsgRequestNext
        (clientStNext synced)
        (pure $ clientStNext synced)

    -- In the CanAwait state, we take the tip point given by the node and
    -- ask for the intersection of that point. This fast-fowards us to the
    -- tip. Once synchronised with the tip, we expect to be waiting for the
    -- server to send AwaitReply most of the time.
    clientStNext
        :: Bool
        -> ClientStNext block (Point block) (Tip block) m Void
    clientStNext False = ClientStNext
            { recvMsgRollBackward = const findIntersect
            , recvMsgRollForward = const findIntersect
            }
      where
        findIntersect tip = ChainSyncClient $
            pure $ SendMsgFindIntersect [getTipPoint $ castTip tip] clientStIntersect

    -- On tip update, we'll also propagate the era inferred from blocks we
    -- received. In case of rollback, we only have a 'Point' and they are
    -- era-agnostic (for now at least!) which isn't a big deal really because
    -- the era will simply be updated on the next RollForward which follows
    -- immediately after.
    clientStNext True = ClientStNext
            { recvMsgRollBackward = doUpdate . const Nothing
            , recvMsgRollForward  = doUpdate . Just . toCardanoEra
            }
      where
        doUpdate
            :: Maybe era
            -> Tip block
            -> ChainSyncClient block (Point block) (Tip block) m Void
        doUpdate era tip = ChainSyncClient $ do
            onTipUpdate era (castTip tip)
            clientStIdle True

    -- After an intersection is found, we return to idle with the sync flag
    -- set.
    clientStIntersect
        :: ClientStIntersect block (Point block) (Tip block) m Void
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

-- | A little type-alias to ease signatures in 'chainSyncWithBlocks'
type RequestNextStrategy m n block
    = P.ClientPipelinedStIdle n block (Point block) (Tip block) m Void

-- | Helper type for the different ways we handle rollbacks.
--
-- Helps remove some boilerplate.
data LocalRollbackResult block
    = Buffer [block]
    -- ^ The rollback could be handled by filtering the buffer. (The `[block]`
    -- corresponds to the new, filtered buffer.)
    | FollowerExact
    -- ^ `ChainFollower` was asked to rollback, and rolled back to the requested
    -- point exactly.
    | FollowerNeedToReNegotiate
    -- ^ The `ChainFollower` was asked to rollback, but rolled back further than
    -- requested. We must re-negotiate the intersection with the node.

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
    :: forall m block. (Monad m, MonadSTM m, MonadThrow m, HasHeader block)
    => Tracer m (ChainSyncLog block (Point block))
    -> ChainFollower m (Point block) (Tip block) (NonEmpty block)
    -> ChainSyncClientPipelined block (Point block) (Tip block) m Void
chainSyncWithBlocks tr chainFollower =
    ChainSyncClientPipelined clientStNegotiateIntersection
  where
    -- Return the _number of slots between two tips.
    tipDistance :: BlockNo -> Tip block -> Natural
    tipDistance (BlockNo n) TipGenesis =
        1 + fromIntegral n
    tipDistance (BlockNo n) (Tip _ _ (BlockNo n')) =
        fromIntegral @Integer $ abs $ fromIntegral n - fromIntegral n'

    -- | Keep only blocks from the list that are before or exactly at the given
    -- point.
    rollbackBuffer :: Point block -> [block] -> [block]
    rollbackBuffer pt = filter (\b -> At (blockSlot b) <= pointSlot pt)

    clientStNegotiateIntersection
        :: m (P.ClientPipelinedStIdle 'Z block (Point block) (Tip block) m Void)
    clientStNegotiateIntersection = do
        points <- readLocalTip chainFollower
        -- Cave: An empty list is interpreted as requesting the genesis point.
        let points' = if null points
                then [Point Origin]
                else sortBy (flip compareSlot) points -- older points last
        traceWith tr $ MsgChainFindIntersect points'
        pure $ P.SendMsgFindIntersect points' clientStIntersect

    -- Receive the result of the MsgFindIntersection request
    clientStIntersect
        :: P.ClientPipelinedStIntersect block (Point block) (Tip block) m Void
    clientStIntersect = P.ClientPipelinedStIntersect
        { P.recvMsgIntersectFound = \_point tip -> do
            -- Here, the node tells us which  point  from the possible
            -- intersections is the latest point on the chain.
            -- However, we do not have to roll back to this point here;
            -- when we send a MsgRequestNext message, the node will reply
            -- with a MsgRollBackward message to this point first.
            --
            -- This behavior is not in the network specification yet, but see
            -- https://input-output-rnd.slack.com/archives/CDA6LUXAQ/p1623322238039900
            traceWith tr $ MsgChainTip (getTipPoint tip)
            clientStIdle oneByOne
        , P.recvMsgIntersectNotFound = \_tip -> do
            -- No intersection was found.
            -- As the read-pointer on the node could be unknown to us,
            -- we now explicitly request the genesis point.
            --
            -- See also
            -- https://input-output-rnd.slack.com/archives/CDA6LUXAQ/p1634644689103100
            clientStNegotiateGenesis
            }

    -- Explicitly negotiate the genesis point
    clientStNegotiateGenesis
        :: m (P.ClientPipelinedStIdle 'Z block (Point block) (Tip block) m Void)
    clientStNegotiateGenesis = do
        let genesis = [Point Origin]
        traceWith tr $ MsgChainFindIntersect genesis
        pure $ P.SendMsgFindIntersect genesis $
            clientStIntersect
                { P.recvMsgIntersectNotFound = \_tip ->
                    throwIO ErrChainSyncNoIntersectGenesis
                }

    clientStIdle
        :: RequestNextStrategy m 'Z block
        -> m (P.ClientPipelinedStIdle 'Z block (Point block) (Tip block) m Void)
    clientStIdle strategy = pure strategy

    -- Simple strategy that sends a request and waits for an answer.
    oneByOne :: RequestNextStrategy m 'Z block
    oneByOne = P.SendMsgRequestNext
        (collectResponses [] Zero)
        (pure $ collectResponses [] Zero)

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
    pipeline goal (Succ n) | natToInt (Succ n) == goal =
        P.CollectResponse Nothing $ collectResponses [] n
    pipeline goal n =
        P.SendMsgRequestNextPipelined $ pipeline goal (Succ n)

    collectResponses
        :: [block]
        -> Nat n
        -> P.ClientStNext n block (Point block) (Tip block) m Void
    collectResponses blocks Zero = P.ClientStNext
        { P.recvMsgRollForward = \block tip -> do
            traceWith tr $ MsgChainTip (getTipPoint tip)

            let blocks' = NE.reverse (block :| blocks)
            traceWith tr $ MsgChainRollForward blocks' (getTipPoint tip)
            handleRollforward blocks' tip

            let distance = tipDistance (blockNo block) tip
            traceWith tr $ MsgTipDistance distance
            let strategy = if distance <= 1
                    then oneByOne
                    else pipeline (fromIntegral $ min distance 1000) Zero
            clientStIdle strategy

        , P.recvMsgRollBackward = \point tip -> do
            traceWith tr $ MsgChainTip (getTipPoint tip)
            r <- handleRollback blocks point tip
            case r of
                Buffer xs -> do
                    case reverse xs of
                        []          -> pure ()
                        (b:blocks') -> handleRollforward (b :| blocks') tip
                    clientStIdle oneByOne
                FollowerExact ->
                    clientStIdle oneByOne
                FollowerNeedToReNegotiate ->
                    clientStNegotiateIntersection
        }

    collectResponses blocks (Succ n) = P.ClientStNext
        { P.recvMsgRollForward = \block _tip ->
            pure $ P.CollectResponse Nothing $ collectResponses (block:blocks) n

        , P.recvMsgRollBackward = \point tip -> do
            traceWith tr $ MsgChainTip (getTipPoint tip)
            r <- handleRollback blocks point tip
            pure $ P.CollectResponse Nothing $ case r of
                Buffer xs -> collectResponses xs n
                FollowerExact -> collectResponses [] n
                FollowerNeedToReNegotiate -> dropResponsesAndRenegotiate n
        }

    handleRollforward :: NonEmpty block -> Tip block -> m ()
    handleRollforward blocks tip = do
        rollForward chainFollower blocks tip
        traceWith tr $ MsgLocalTip (blockPoint $ NE.last blocks)

    handleRollback
        :: [block]
        -> Point block
        -> Tip block
        -> m (LocalRollbackResult block)
    handleRollback buffer point _tip = do
        let buffer' = rollbackBuffer point buffer
        traceWith tr $ MsgChainRollBackward point (length buffer')
        case buffer' of
            [] -> do
                actual <- rollBackward chainFollower point
                if actual == point
                    then do
                        traceWith tr $ MsgLocalTip point
                        pure FollowerExact
                    else do
                        pure FollowerNeedToReNegotiate
            xs -> do
                pure $ Buffer xs

    -- | Discards the in-flight requests, and re-negotiates the intersection
    -- afterwards.
    dropResponsesAndRenegotiate
        :: Nat n
        -> P.ClientStNext n block (Point block) (Tip block) m Void
    dropResponsesAndRenegotiate (Succ n) =
        P.ClientStNext
            { P.recvMsgRollForward = \_block _tip ->
                pure $ P.CollectResponse Nothing $ dropResponsesAndRenegotiate n
            , P.recvMsgRollBackward = \_point _tip ->
                pure $ P.CollectResponse Nothing $ dropResponsesAndRenegotiate n
            }
    dropResponsesAndRenegotiate Zero =
        P.ClientStNext
            { P.recvMsgRollForward = \_block _tip ->
                clientStNegotiateIntersection
            , P.recvMsgRollBackward = \_point _tip ->
                clientStNegotiateIntersection
            }

compareSlot :: Point block -> Point block -> Ordering
compareSlot (Point Origin) (Point Origin) = EQ
compareSlot (Point Origin) _ = LT
compareSlot _ (Point Origin) = GT
compareSlot (Point (At b1)) (Point (At b2)) = comparing blockPointSlot b1 b2

--------------------------------------------------------------------------------
--
-- LocalStateQuery

-- | Type of commands that are stored in a queue for local state queries.
data LocalStateQueryCmd block m = forall a. SomeLSQ
    (LSQ block m a)
    (a -> m ())

-- | Client for the 'Local State Query' mini-protocol.
--
--                                    Agency
--     -------------------------------------------------------------------------
--     Client has agency*                | Idle, Acquired
--     Server has agency*                | Acquiring, Querying
--     * A peer has agency if it is expected to send the next message.
--
--                ┌───────────────┐    Done      ┌──────────┐
--        ┌──────▶│     Idle      ├─────────────▶│   Done   │
--        │       └───┬───────────┘              └──────────┘
--        │           │       ▲
--        │   Acquire │       │
--        │           │       │ Failure
--        │           ▼       │
--        │       ┌───────────┴───┐
--        │       │   Acquiring   │
--        │       └───┬───────────┘
-- Release│           │       ▲
--        │           │       │
--        │  Acquired │       │ ReAcquire
--        │           │       │
--        │           ▼       │
--        │       ┌───────────┴───┐   Query     ┌──────────┐
--        └───────┤   Acquired    ├────────────▶│ Querying │
--                │               │◀────────────┤          │
--                └───────────────┘     Result  └──────────┘
--
localStateQuery
    :: forall m block . (MonadIO m, MonadSTM m)
    => TQueue m (LocalStateQueryCmd block m)
        -- ^ We use a 'TQueue' as a communication channel to drive queries from
        -- outside of the network client to the client itself.
        -- Requests are pushed to the queue which are then transformed into
        -- messages to keep the state-machine moving.
    -> LocalStateQueryClient block (Point block) (Query block) m Void
localStateQuery queue =
    LocalStateQueryClient clientStIdle
  where
    clientStIdle
        :: m (LSQ.ClientStIdle block (Point block) (Query block) m Void)
    clientStIdle =
        LSQ.SendMsgAcquire Nothing . clientStAcquiring <$> awaitNextCmd

    clientStAcquiring
        :: LocalStateQueryCmd block m
        -> LSQ.ClientStAcquiring block (Point block) (Query block) m Void
    clientStAcquiring qry = LSQ.ClientStAcquiring
        { recvMsgAcquired = clientStAcquired qry
        , recvMsgFailure = \_failure -> do
            pure $ LSQ.SendMsgAcquire Nothing (clientStAcquiring qry)
        }

    clientStAcquired
        :: LocalStateQueryCmd block m
        -> m (LSQ.ClientStAcquired block (Point block) (Query block) m Void)
    clientStAcquired (SomeLSQ cmd respond) = pure $ go cmd $ \res ->
        -- We currently release the handle to the node state after
        -- each query in the queue. This allows the node to release
        -- resources (such as a stake distribution snapshot) after
        -- each query.
        -- 
        -- However, we /could/ read all LocalStateQueryCmds from the TQueue,
        -- and run them against the same tip, if re-acquiring takes a long time.
        -- As of Jan 2021, it seems like queries themselves take significantly
        -- longer than the acquiring.
        LSQ.SendMsgRelease $ do
            -- In order to remove the query from the queue as soon as possible,
            -- @respond@ should return quickly and not throw any synchronous
            -- exception.
            -- In practice, we only use the 'send' helper here, so that works.
            --
            -- (Asynchronous exceptions are fine, as the connection to the node
            -- will not attempt to recover from that, and it doesn't matter
            -- whether a command is left in the queue or not.)
            respond res
            finalizeCmd
            clientStIdle
      where
          go
              :: forall a. LSQ block m a
              -> (a -> (LSQ.ClientStAcquired block (Point block) (Query block) m Void))
              -> (LSQ.ClientStAcquired block (Point block) (Query block) m Void)
          go (LSQPure a) cont = cont a
          go (LSQry qry) cont = LSQ.SendMsgQuery (BlockQuery qry)
            -- We only need to support queries of the type `BlockQuery`.
            $ LSQ.ClientStQuerying $ \res -> do
                  pure $ cont res
                  -- It would be nice to trace the time it takes to run the
                  -- queries. We don't have a good opportunity to run IO after a
                  -- point is acquired, but before the query is send, however.
                  -- Heinrich: Actually, this can be done by adding a 'Tracer m'
                  -- to the scope and using it here. However, I believe that we
                  -- already have sufficiently good logging of execution times
                  -- in Cardano.Wallet.Shelley.Network .
          go (LSQBind ma f) cont = go ma $ \a -> do
              go (f a) $ \b -> cont b

    -- | Note that we for LSQ and TxSubmission use peekTQueue when starting the
    -- request, and only remove the command from the queue after we have
    -- processed the response from the node.
    --
    -- If the connection to the node drops, this makes cancelled commands
    -- automatically retry on reconnection.
    --
    -- IMPORTANT: callers must also `finalizeCmd`, because of the above.
    awaitNextCmd :: m (LocalStateQueryCmd block m)
    awaitNextCmd = atomically $ peekTQueue queue

    finalizeCmd :: m ()
    finalizeCmd = atomically $ tryReadTQueue queue >>= \case
        Just _ -> return ()
        Nothing -> error "finalizeCmd: queue is not empty"

-- | Monad for composing local state queries for the node /tip/.
--
-- /Warning/: Partial functions inside the @LSQ@ monad may cause the entire
-- wallet to crash when interpreted by @localStateQuery@.
data LSQ block (m :: Type -> Type) a where
    LSQPure :: a -> LSQ block m a
    LSQBind :: LSQ block m a -> (a -> LSQ block m b) -> LSQ block m b

    -- | A local state query.
    LSQry :: (BlockQuery block res) -> LSQ block m res

instance Functor (LSQ block m) where
    fmap = liftM

instance Applicative (LSQ block m) where
    pure  = LSQPure
    (<*>) = ap

instance Monad (LSQ block m) where
    return = pure
    (>>=)  = LSQBind

--------------------------------------------------------------------------------
--
-- LocalTxSubmission


-- | Type of commands that are stored in a queue for localTxSubmission.
data LocalTxSubmissionCmd tx err (m :: Type -> Type)
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
    -> LocalTxSubmissionClient tx err m ()
localTxSubmission queue = LocalTxSubmissionClient clientStIdle
  where
    clientStIdle
        :: m (LocalTxClientStIdle tx err m ())
    clientStIdle = atomically (peekTQueue queue) <&> \case
        CmdSubmitTx tx respond ->
            SendMsgSubmitTx tx $ \res -> do
                respond res
                -- Same note about peekTQueue from `localStateQuery` applies
                -- here.
                _processedCmd <- atomically (readTQueue queue)
                clientStIdle

{-------------------------------------------------------------------------------
    Helpers
-------------------------------------------------------------------------------}

-- | Helper function to send commands to the node via a 'TQueue'
-- and receive results.
--
-- One of the main purposes of this functions is to handle an existentially
-- quantified type.
-- In typical use, the @cmd m@ involves existential quantification over
-- the type @a@, so that the 'TQueue' has elements with a monomorphic type.
-- However, the type signature of `send` allows us to retrieve this particular
-- type @a@ for later use again.
send
    :: MonadSTM m
    => TQueue m (cmd m)
    -> ((a -> m ()) -> cmd m)
    -> m a
send queue cmd = do
    tvar <- newEmptyTMVarIO
    atomically $ writeTQueue queue (cmd (atomically . putTMVar tvar))
    atomically $ takeTMVar tvar

{-------------------------------------------------------------------------------
    Errors
-------------------------------------------------------------------------------}
data ErrChainSync
    = ErrChainSyncNoIntersectGenesis
    -- ^ The node does not give us genesis when we request it with a
    -- 'MsgFindIntersect' message in the ChainSync protocol.
    -- This should not happen.
    deriving (Eq, Show)

instance Exception ErrChainSync
