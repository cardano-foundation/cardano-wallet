{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)
    , NextBlocksResult (..)
    , mapCursor
    , Cursor
    , follow
    , FollowAction (..)
    , FollowExit (..)
    , GetStakeDistribution
    , getSlottingParametersForTip

    -- * Errors
    , ErrNetworkUnavailable (..)
    , ErrCurrentNodeTip (..)
    , ErrGetBlock (..)
    , ErrGetTxParameters (..)
    , ErrPostTx (..)
    , ErrGetAccountBalance (..)

    -- * Logging
    , FollowLog (..)

    -- * Initialization
    , defaultRetryPolicy
    , waitForNetwork
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, queryEpochLength, querySlotLength )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , ProtocolParameters
    , SlotNo (..)
    , SlottingParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( AsyncCancelled (..) )
import Control.Exception
    ( AsyncException (..)
    , Exception (..)
    , SomeException
    , asyncExceptionFromException
    , handle
    )
import Control.Monad
    ( when )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Retry
    ( RetryPolicyM, constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import UnliftIO.Exception
    ( throwIO )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data NetworkLayer m target block = NetworkLayer
    { nextBlocks
        :: Cursor target
        -> ExceptT ErrGetBlock m (NextBlocksResult (Cursor target) block)
        -- ^ Starting from the given 'Cursor', fetches a contiguous sequence of
        -- blocks from the node, if they are available. An updated cursor will
        -- be returned with a 'RollFoward' result.
        --
        -- Blocks are returned in ascending slot order, without skipping blocks.
        --
        -- If the node does not have any blocks after the specified cursor
        -- point, it will return 'AwaitReply'.
        --
        -- If the node has adopted an alternate fork of the chain, it will
        -- return 'RollBackward' with a new cursor.

    , initCursor
        :: [BlockHeader] -> m (Cursor target)
        -- ^ Creates a cursor from the given block header so that 'nextBlocks'
        -- can be used to fetch blocks.

    , destroyCursor
        :: Cursor target -> m ()
        -- ^ Cleanup network connection once we're done with them.

    , cursorSlotNo
        :: Cursor target -> SlotNo
        -- ^ Get the slot corresponding to a cursor.

    , currentNodeTip
        :: ExceptT ErrCurrentNodeTip m BlockHeader
        -- ^ Get the current tip from the chain producer

    , watchNodeTip
        :: (BlockHeader -> m ())
        -> m ()
        -- ^ Register a callback for when the node tip changes.
        -- This function should never finish, unless the callback throws an
        -- exception, which will be rethrown by this function.

    , getProtocolParameters
        :: m ProtocolParameters

    , postTx
        :: SealedTx -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , stakeDistribution
        :: GetStakeDistribution target m

    , getAccountBalance
        :: RewardAccount
        -> ExceptT ErrGetAccountBalance m (Quantity "lovelace" Word64)

    , timeInterpreter
        :: HasCallStack => TimeInterpreter m
    }

instance Functor m => Functor (NetworkLayer m target) where
    fmap f nl = nl
        { nextBlocks = fmap (fmap f) . nextBlocks nl
        }

{-------------------------------------------------------------------------------
                                  Errors
-------------------------------------------------------------------------------}

-- | Network is unavailable
data ErrNetworkUnavailable
    = ErrNetworkUnreachable Text
      -- ^ Cannot connect to network backend.
    | ErrNetworkInvalid Text
      -- ^ Network backend reports that the requested network is invalid.
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkUnavailable

-- | Error while trying to get the node tip
data ErrCurrentNodeTip
    = ErrCurrentNodeTipNetworkUnreachable ErrNetworkUnavailable
    | ErrCurrentNodeTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrCurrentNodeTip

-- | Error while trying to get one or more blocks
data ErrGetBlock
    = ErrGetBlockNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockNotFound (Hash "BlockHeader")
    deriving (Generic, Show, Eq)

-- | Error while querying local parameters state.
data ErrGetTxParameters
    = ErrGetTxParametersTip ErrCurrentNodeTip
    | ErrGetTxParametersNetworkUnreachable ErrNetworkUnavailable
    | ErrGetTxParametersNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrGetTxParameters

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnavailable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

data ErrGetAccountBalance
    = ErrGetAccountBalanceNetworkUnreachable ErrNetworkUnavailable
    | ErrGetAccountBalanceAccountNotFound RewardAccount
    deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
                              Initialization
-------------------------------------------------------------------------------}

-- | Wait until 'currentNodeTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForNetwork
    :: ExceptT ErrNetworkUnavailable IO ()
    -> RetryPolicyM IO
    -> IO ()
waitForNetwork getStatus policy = do
    r <- retrying policy shouldRetry (const $ runExceptT getStatus)
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkInvalid{} ->
            return False
        Left ErrNetworkUnreachable{} ->
            return True

-- | A default 'RetryPolicy' with a delay that starts short, and that retries
-- for no longer than a minute.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (3600 * second) (constantDelay second)
  where
    second = 1000*1000

{-------------------------------------------------------------------------------
                                 Queries
-------------------------------------------------------------------------------}

type family GetStakeDistribution target (m :: * -> *) :: *

-- | Use the HFC history interpreter to get the slot and epoch lengths current
-- for the network tip.
--
-- This may throw a 'PastHorizonException' in some cases.
getSlottingParametersForTip
    :: Monad m
    => NetworkLayer m target block
    -> m SlottingParameters
getSlottingParametersForTip nl = do
    tip <- either (const 0) slotNo <$> runExceptT (currentNodeTip nl)

    -- TODO: #2226 Query activeSlotCoeff from ledger.
    -- This requires code changes in the shelley ledger.
    let getActiveSlotCoeff = pure (ActiveSlotCoefficient 1.0)

    SlottingParameters
        <$> timeInterpreter nl (querySlotLength tip)
        <*> timeInterpreter nl (queryEpochLength tip)
        <*> getActiveSlotCoeff

{-------------------------------------------------------------------------------
                                Chain Sync
-------------------------------------------------------------------------------}

-- | A cursor is local state kept by the chain consumer to use as the starting
-- position for 'nextBlocks'. The actual type is opaque and determined by the
-- backend @target@.
data family Cursor target

-- | The result of 'nextBlocks', which is instructions for what the chain
-- consumer should do next.
data NextBlocksResult cursor block
    = AwaitReply
        -- ^ There are no blocks available from the node, so wait.
    | RollForward cursor BlockHeader [block]
        -- ^ Apply the given contiguous non-empty sequence of blocks. Use the
        -- updated cursor to get the next batch. The given block header is the
        -- current tip of the node.
    | RollBackward cursor
        -- ^ The chain consumer must roll back its state, then use the cursor to
        -- get the next batch of blocks.

instance Functor (NextBlocksResult cursor) where
    fmap f = \case
        AwaitReply -> AwaitReply
        RollForward cur bh bs -> RollForward cur bh (fmap f bs)
        RollBackward cur -> RollBackward cur

mapCursor :: (a -> b) -> NextBlocksResult a block -> NextBlocksResult b block
mapCursor fn = \case
    AwaitReply -> AwaitReply
    RollForward cur bh bs -> RollForward (fn cur) bh bs
    RollBackward cur -> RollBackward (fn cur)

-- | @FollowAction@ enables the callback of @follow@ to signal if the
-- chain-following should @ExitWith@, @Continue@, or if the current callback
-- should be forgotten and retried (@Retry@).
--
-- NOTE: @Retry@ is needed to handle data-races in
-- 'Cardano.Pool.Jormungandr.Metrics', where it is essensial that we fetch the
-- stake distribution while the node-tip
--
-- FIXME:
-- Retry actions with the Haskell nodes are not possible (or at least, requires
-- some additional manipulation to find a new intersection). As a possible fix,
-- we could use a type family to define 'FollowAction' in terms of the
-- underlying target. 'RetryImmediately' and 'RetryLater' could be authorized in
-- the context of Jormungandr but absent in the context of the Haskell nodes.
data FollowAction err
    = ExitWith err
      -- ^ Stop following the chain.
    | Continue
      -- ^ Continue following the chain.
    | RetryImmediately
      -- ^ Forget about the blocks in the current callback, and retry immediately.
    | RetryLater
      -- ^ Like 'RetryImmediately' but only retries after a short delay
    deriving (Eq, Show, Functor)

-- | Possibly scenarios that would cause 'follow' to exit so that client code
-- can decide what to do.
data FollowExit
    = FollowInterrupted
    | FollowRollback SlotNo
    | FollowFailure
    deriving (Eq, Show)


-- | Subscribe to a blockchain and get called with new block (in order)!
--
-- Exits when the node switches to a different chain with the greatest known
-- common tip between the follower and the node. This makes it easier for client
-- to re-start following from a different point if they have, for instance,
-- rolled back to a point further in the past. If this occurs, clients will need
-- to restart the chain follower from a known list of headers, re-initializing
-- the cursor.
--
-- Exits with 'Nothing' in case of error.
follow
    :: forall target block e. (Show e)
    => NetworkLayer IO target block
    -- ^ The @NetworkLayer@ used to poll for new blocks.
    -> Tracer IO FollowLog
    -- ^ Logger trace
    -> [BlockHeader]
    -- ^ A list of known tips to start from.
    -- Blocks /after/ the tip will be yielded.
    -> (NE.NonEmpty block
        -> (BlockHeader, ProtocolParameters)
        -> IO (FollowAction e))
    -- ^ Callback with blocks and the current tip of the /node/.
    -- @follow@ stops polling and terminates if the callback errors.
    -> (block -> BlockHeader)
    -- ^ Getter on the abstract 'block' type
    -> IO FollowExit
follow nl tr cps yield header =
    sleep 0 False =<< initCursor nl cps
  where
    delay0 :: Int
    delay0 = 500*1000 -- 500ms

    retryDelay :: Int -> Int
    retryDelay 0 = delay0
    retryDelay delay = min (2*delay) (10 * delay0)

    -- | Wait a short delay before querying for blocks again. We also take this
    -- opportunity to refresh the chain tip as it has probably increased in
    -- order to refine our syncing status.
    sleep :: Int -> Bool -> Cursor target -> IO FollowExit
    sleep delay hasRolledForward cursor = handle retry $ do
        when (delay > 0) (threadDelay delay)
        step delay hasRolledForward cursor
      where
        retry (e :: SomeException) = case asyncExceptionFromException e of
            Just ThreadKilled -> do
                destroyCursor nl cursor $> FollowInterrupted
            Just UserInterrupt -> do
                destroyCursor nl cursor $> FollowInterrupted
            Nothing | fromException e == Just AsyncCancelled -> do
                destroyCursor nl cursor $> FollowInterrupted
            Just _ -> do
                traceWith tr $ MsgUnhandledException eT
                destroyCursor nl cursor $> FollowFailure
            _ -> do
                traceWith tr $ MsgUnhandledException eT
                destroyCursor nl cursor $> FollowFailure
          where
            eT = T.pack (show e)

    step :: Int -> Bool -> Cursor target -> IO FollowExit
    step delay hasRolledForward cursor = runExceptT (nextBlocks nl cursor) >>= \case
        Left e -> do
            traceWith tr $ MsgNextBlockFailed e
            sleep (retryDelay delay) hasRolledForward cursor

        Right AwaitReply -> do
            traceWith tr MsgSynced
            sleep delay0 hasRolledForward cursor

        Right (RollForward cursor' _ []) -> do -- FIXME Make RollForward return NE
            traceWith tr MsgSynced
            sleep delay0 hasRolledForward cursor'

        Right (RollForward cursor' tip (blockFirst : blocksRest)) -> do
            let blocks = blockFirst :| blocksRest
            traceWith tr $ MsgApplyBlocks (header <$> blocks)
            params <- getProtocolParameters nl
            action <- yield blocks (tip, params)
            traceWith tr $ MsgFollowAction (fmap show action)
            continueWith cursor' True action

        Right (RollBackward cursor') ->
            -- After negotiating a tip, the node asks us to rollback to the
            -- intersection. We may have to rollback to our /current/ tip.
            --
            -- This would do nothing, but @follow@ handles rollback by exiting
            -- such that a new negotiation is required, leading to an infinite
            -- loop.
            --
            -- So this becomes a bit intricate:

            case (cursorSlotNo nl cursor', cps, hasRolledForward) of
                (sl, [], False) -> do
                    -- The following started from @Origin@.
                    -- This is the initial rollback.
                    -- We can infer that we are asked to rollback to Origin, and
                    -- we can ignore it.
                    traceWith tr $ MsgWillIgnoreRollback sl "initial rollback, \
                        \cps=[]"
                    step delay0 hasRolledForward cursor'
                (sl, _:_, False) | sl == slotNo (last cps) -> do
                    traceWith tr $ MsgWillIgnoreRollback sl "initial rollback, \
                        \rollback point equals the last checkpoint"
                    step delay0 hasRolledForward cursor'
                (sl, _, _) -> do
                    traceWith tr $ MsgWillRollback sl
                    destroyCursor nl cursor' $> FollowRollback sl
            -- Some alternative solutions would be to:
            -- 1. Make sure we have a @BlockHeader@/@SlotNo@ for @Origin@
            -- 2. Stop forcing @follow@ to quit on rollback
      where
        continueWith
            :: Cursor target
            -> Bool
            -> FollowAction e
            -> IO FollowExit
        continueWith cursor' hrf = \case
            ExitWith _ -> -- NOTE error logged as part of `MsgFollowAction`
                return FollowInterrupted
            Continue ->
                step delay0 hrf cursor'
            RetryImmediately ->
                step delay0 hrf cursor
            RetryLater ->
                sleep delay0 hrf cursor

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data FollowLog
    = MsgFollowAction (FollowAction String)
    | MsgUnhandledException Text
    | MsgNextBlockFailed ErrGetBlock
    | MsgSynced
    | MsgApplyBlocks (NonEmpty BlockHeader)
    | MsgWillRollback SlotNo
    | MsgWillIgnoreRollback SlotNo Text -- Reason
    deriving (Show, Eq)

instance ToText FollowLog where
    toText = \case
        MsgFollowAction action -> case action of
            ExitWith e -> "Failed to roll forward: " <> T.pack e
            _ -> T.pack $ "Follower says " <> show action
        MsgUnhandledException err ->
            "Unexpected error following the chain: " <> err
        MsgNextBlockFailed e ->
            T.pack $ "Failed to get next blocks: " <> show e
        MsgSynced ->
            "In sync with the node."
        MsgApplyBlocks hdrs ->
            let (slFst, slLst) =
                    ( slotNo $ NE.head hdrs
                    , slotNo $ NE.last hdrs
                    )
            in mconcat
                [ "Applying blocks [", pretty slFst, " ... ", pretty slLst, "]" ]
        MsgWillRollback sl ->
            "Will rollback to " <> pretty sl
        MsgWillIgnoreRollback sl reason ->
            "Will ignore rollback to " <> pretty sl
                <> " because of " <> pretty reason

instance HasPrivacyAnnotation FollowLog
instance HasSeverityAnnotation FollowLog where
    getSeverityAnnotation = \case
        MsgFollowAction (ExitWith _) -> Error
        MsgFollowAction _ -> Debug
        MsgUnhandledException _ -> Error
        MsgNextBlockFailed _ -> Warning
        MsgSynced -> Debug
        MsgApplyBlocks _ -> Info
        MsgWillRollback _ -> Debug
        MsgWillIgnoreRollback _ _ -> Debug
