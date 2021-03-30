{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    , FollowExceptionRecovery (..)

    -- * Errors
    , ErrPostTx (..)
    , ErrGetAccountBalance (..)

    -- * Logging
    , FollowLog (..)

    -- * Initialization
    , defaultRetryPolicy
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..), contramap )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ProtocolParameters
    , SlotNo (..)
    , SlottingParameters (..)
    , StakePoolsSummary
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Control.Monad
    ( when )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Retry
    ( RetryPolicyM, constantDelay, limitRetriesByCumulativeDelay )
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
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import Safe
    ( lastMay )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( SomeException, bracket, handle )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data NetworkLayer m block = NetworkLayer
    { nextBlocks
        :: Cursor
        -> IO (NextBlocksResult Cursor block)
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
        :: [BlockHeader] -> m Cursor
        -- ^ Creates a cursor from the given block header so that 'nextBlocks'
        -- can be used to fetch blocks.

    , destroyCursor
        :: Cursor -> m ()
        -- ^ Cleanup network connection once we're done with them.

    , cursorSlotNo
        :: Cursor -> SlotNo
        -- ^ Get the slot corresponding to a cursor.

    , currentNodeTip
        :: m BlockHeader
        -- ^ Get the current tip from the chain producer
        --
    , currentNodeEra
        :: m AnyCardanoEra
        -- ^ Get the era the node is currently in.

    , currentProtocolParameters
        :: m ProtocolParameters
        -- ^ Get the last known protocol parameters. In principle, these can
        -- only change once per epoch.

    , currentSlottingParameters
        :: m SlottingParameters
        -- ^ Get the last known slotting parameters. In principle, these can
        -- only change once per era.

    , watchNodeTip
        :: (BlockHeader -> m ())
        -> m ()
        -- ^ Register a callback for when the node tip changes.
        -- This function should never finish, unless the callback throws an
        -- exception, which will be rethrown by this function.

    , postTx
        :: SealedTx -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , stakeDistribution
        :: Coin -- Stake to consider for rewards
        -> m StakePoolsSummary

    , getAccountBalance
        :: RewardAccount
        -> ExceptT ErrGetAccountBalance m Coin

    , timeInterpreter
        :: TimeInterpreter (ExceptT PastHorizonException m)
    , syncProgress
        :: SlotNo -> m (SyncProgress)
    }

instance Functor m => Functor (NetworkLayer m) where
    fmap f nl = nl
        { nextBlocks = fmap (fmap f) . nextBlocks nl
        }

{-------------------------------------------------------------------------------
                                  Errors
-------------------------------------------------------------------------------}

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance ToText ErrPostTx where
    toText = \case
        ErrPostTxBadRequest msg -> msg
        ErrPostTxProtocolFailure msg -> msg

newtype ErrGetAccountBalance
    = ErrGetAccountBalanceAccountNotFound RewardAccount
    deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
                              Initialization
-------------------------------------------------------------------------------}

-- | A default 'RetryPolicy' with a delay that starts short, and that retries
-- for no longer than a minute.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (3600 * second) (constantDelay second)
  where
    second = 1000*1000

{-------------------------------------------------------------------------------
                                Chain Sync
-------------------------------------------------------------------------------}

--- | A cursor is local state kept by the chain consumer to use as the starting
--- position for 'nextBlocks'. The actual type is opaque and determined by the
--- backend target.
data family Cursor

-- | The result of 'nextBlocks', which is instructions for what the chain
-- consumer should do next.
data NextBlocksResult cursor block
    = RollForward cursor BlockHeader [block]
        -- ^ Apply the given contiguous non-empty sequence of blocks. Use the
        -- updated cursor to get the next batch. The given block header is the
        -- current tip of the node.
    | RollBackward cursor
        -- ^ The chain consumer must roll back its state, then use the cursor to
        -- get the next batch of blocks.

instance Functor (NextBlocksResult cursor) where
    fmap f = \case
        RollForward cur bh bs -> RollForward cur bh (fmap f bs)
        RollBackward cur -> RollBackward cur

mapCursor :: (a -> b) -> NextBlocksResult a block -> NextBlocksResult b block
mapCursor fn = \case
    RollForward cur bh bs -> RollForward (fn cur) bh bs
    RollBackward cur -> RollBackward (fn cur)

-- | @FollowAction@ enables the callback of @follow@ to signal if the
-- chain-following should @ExitWith@, @Continue@, or if the current callback
-- should be forgotten and retried (@Retry@).
data FollowAction err
    = ExitWith err
      -- ^ Stop following the chain.
    | Continue
      -- ^ Continue following the chain.
    deriving (Eq, Show, Functor)

-- | Possibly scenarios that would cause 'follow' to exit so that client code
-- can decide what to do.
data FollowExit
    = FollowRollback SlotNo
    | FollowFailure
    | FollowDone
    deriving (Eq, Show)

data FollowExceptionRecovery
    = RetryOnExceptions
    | AbortOnExceptions

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
    :: forall block tr e. (Show e)
    => NetworkLayer IO block
    -- ^ The @NetworkLayer@ used to poll for new blocks.
    -> Tracer IO (FollowLog tr)
    -- ^ Logger trace
    -> IO [BlockHeader]
    -- ^ A way to get a list of known tips to start from.
    -- Blocks /after/ the tip will be yielded.
    -> (NE.NonEmpty block
        -> BlockHeader
        -> Tracer IO tr
        -> IO (FollowAction e))
    -- ^ Callback with blocks and the current tip of the /node/.
    -- @follow@ stops polling and terminates if the callback errors.
    -> (SlotNo -> IO (Either e SlotNo))
    -- ^ Callback with blocks and the current tip of the /node/.
    -- @follow@ stops polling and terminates if the callback errors.
    -> FollowExceptionRecovery
    -- ^ Whether to recover from exceptions or not.
    -> (block -> BlockHeader)
    -- ^ Getter on the abstract 'block' type
    -> IO ()
follow nl tr readCursor forward' backward recovery header =
    loop True
  where
    loop firstTime = do
        cursor <- readCursor
        when firstTime $ traceWith tr $ MsgStartFollowing cursor

        -- Trace the sync progress based on the last "local checkpoint".
        --
        -- It appears that @forward@ doesn't get called if we are already
        -- in-sync. So if we want the @LogState@ to update, we need to trace
        -- something here.
        case lastMay cursor of
            Just c -> traceWith tr . MsgFollowerTip $ Just c
            Nothing -> traceWith tr . MsgFollowerTip $ Nothing

        let forward blocks tip innerTr = do
                res <- forward' blocks tip innerTr
                traceWith tr . MsgFollowerTip . Just $ header $ NE.last blocks
                return res

        (follow' nl tr cursor forward header) >>= \act -> do
            case act of
                FollowFailure ->
                    -- NOTE: follow' is tracing the error, so we don't have to
                    -- here
                    case recovery of
                        RetryOnExceptions -> loop False
                        AbortOnExceptions -> return ()
                FollowRollback requestedSlot -> do
                    -- NOTE: follow' is tracing MsgWillRollback
                    backward requestedSlot >>= \case
                        Left e -> do
                            traceWith tr $ MsgFailedRollingBack $ T.pack (show e)
                        Right actualSlot -> do
                            traceWith tr $ MsgDidRollback requestedSlot actualSlot
                    loop False
                FollowDone ->
                    -- TODO: Pool used to log MsgHaltMonitoring
                    return ()

-- | A new, more convenient, wrapping @follow@ function was added above.
--
-- This is the old one. It was kept for now to minimise changes and potential
-- mistakes, as it is pretty intricate.
follow'
    :: forall block tr e. (Show e)
    => NetworkLayer IO block
    -- ^ The @NetworkLayer@ used to poll for new blocks.
    -> Tracer IO (FollowLog tr)
    -- ^ Logger trace
    -> [BlockHeader]
    -- ^ A list of known tips to start from.
    -- Blocks /after/ the tip will be yielded.
    -> (NE.NonEmpty block
        -> BlockHeader
        -> Tracer IO tr
        -> IO (FollowAction e))
    -- ^ Callback with blocks and the current tip of the /node/.
    -- @follow@ stops polling and terminates if the callback errors.
    -> (block -> BlockHeader)
    -- ^ Getter on the abstract 'block' type
    -> IO FollowExit
follow' nl tr cps yield header =
    bracket (initCursor nl cps) (destroyCursor nl) (sleep 0 False)
  where
    innerTr = contramap MsgFollowLog tr
    delay0 :: Int
    delay0 = 500*1000 -- 500ms

    -- | Wait a short delay before querying for blocks again. We also take this
    -- opportunity to refresh the chain tip as it has probably increased in
    -- order to refine our syncing status.
    sleep :: Int -> Bool -> Cursor -> IO FollowExit
    sleep delay hasRolledForward cursor = handle exitOnAnyException $ do
        when (delay > 0) (threadDelay delay)
        step hasRolledForward cursor
      where
        -- Any unhandled synchronous exception should be logged and cause the
        -- chain follower to exit.
        exitOnAnyException :: SomeException -> IO FollowExit
        exitOnAnyException e = do
            traceWith tr $ MsgUnhandledException $ T.pack $ show e
            pure FollowFailure

    step :: Bool -> Cursor -> IO FollowExit
    step hasRolledForward cursor = nextBlocks nl cursor >>= \case
        RollForward cursor' _ [] -> do
            -- FIXME Make RollForward return NE
            -- This case seems to never happen.
            sleep delay0 hasRolledForward cursor'

        RollForward cursor' tip (blockFirst : blocksRest) -> do
            let blocks = blockFirst :| blocksRest
            traceWith tr $ MsgApplyBlocks tip (header <$> blocks)
            action <- yield blocks tip innerTr
            traceWith tr $ MsgFollowAction (fmap show action)
            continueWith cursor' True action

        RollBackward cursor' ->
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
                    step hasRolledForward cursor'
                (sl, _:_, False) | sl == slotNo (last cps) -> do
                    traceWith tr $ MsgWillIgnoreRollback sl "initial rollback, \
                        \rollback point equals the last checkpoint"
                    step hasRolledForward cursor'
                (sl, _, _) -> do
                    traceWith tr $ MsgWillRollback sl
                    destroyCursor nl cursor' $> FollowRollback sl
            -- Some alternative solutions would be to:
            -- 1. Make sure we have a @BlockHeader@/@SlotNo@ for @Origin@
            -- 2. Stop forcing @follow@ to quit on rollback
    continueWith
        :: Cursor
        -> Bool
        -> FollowAction e
        -> IO FollowExit
    continueWith cursor' hrf = \case
        ExitWith _ -> -- NOTE error logged as part of `MsgFollowAction`
            return FollowDone
        Continue ->
            step hrf cursor'

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data FollowLog tr
    = MsgStartFollowing [BlockHeader]
    | MsgHaltMonitoring
    | MsgFollowAction (FollowAction String)
    | MsgUnhandledException Text
    | MsgFollowerTip (Maybe BlockHeader)
    | MsgApplyBlocks BlockHeader (NonEmpty BlockHeader)
    | MsgFollowLog tr -- Inner tracer
    | MsgWillRollback SlotNo
    | MsgDidRollback SlotNo SlotNo
    | MsgFailedRollingBack Text -- Reason
    | MsgWillIgnoreRollback SlotNo Text -- Reason
    deriving (Show, Eq)

instance ToText tr => ToText (FollowLog tr) where
    toText = \case
        MsgStartFollowing cps -> mconcat
            [ "Chain following starting. Requesting intersection using "
            , T.pack . show $ length cps
            , " checkpoints"
            , maybe "" ((", the latest being " <>) . pretty) (lastMay cps)
            ]
        MsgHaltMonitoring ->
            "Stopping following as requested."
        MsgFollowAction action -> case action of
            ExitWith e -> "Failed to roll forward: " <> T.pack e
            _ -> T.pack $ "Follower says " <> show action
        MsgUnhandledException err ->
            "Unexpected error following the chain: " <> err
        MsgFollowerTip p -> "Tip" <> pretty p
        MsgApplyBlocks tipHdr hdrs ->
            let slot = pretty . slotNo
                buildRange (x :| []) = x
                buildRange xs = NE.head xs <> ".." <> NE.last xs
                blockHeights = pretty . getQuantity . blockHeight <$> hdrs
            in mconcat
                [ "Applying block numbers [", buildRange blockHeights, "]"
                , "  Wallet/node slots: ", slot (NE.last hdrs)
                , "/", slot tipHdr
                ]
        MsgWillIgnoreRollback sl reason ->
            "Will ignore rollback to " <> pretty sl
                <> " because of " <> pretty reason
        MsgWillRollback sl ->
            "Will rollback to " <> pretty sl
        MsgDidRollback requested actual -> mconcat
            [ "Did rollback to "
            , pretty actual
            , " after request to rollback to "
            , pretty requested
            ]
        MsgFailedRollingBack reason -> "Failed rolling back: " <>
            reason
        MsgFollowLog msg -> toText msg

instance HasPrivacyAnnotation (FollowLog tr)
instance HasSeverityAnnotation tr => HasSeverityAnnotation (FollowLog tr) where
    getSeverityAnnotation = \case
        MsgStartFollowing _ -> Info
        MsgHaltMonitoring -> Info
        MsgFollowerTip _ -> Debug
        MsgFollowAction (ExitWith _) -> Error
        MsgFollowAction _ -> Debug
        MsgUnhandledException _ -> Error
        MsgApplyBlocks _ _ -> Info
        MsgFollowLog msg -> getSeverityAnnotation msg
        MsgWillRollback _ -> Info
        MsgDidRollback _ _ -> Info
        MsgFailedRollingBack _ -> Error
        MsgWillIgnoreRollback _ _ -> Debug
