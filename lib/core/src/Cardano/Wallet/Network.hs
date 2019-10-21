{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)
    , NextBlocksResult (..)
    , Cursor
    , follow
    , FollowAction (..)

    -- * Errors
    , ErrNetworkUnavailable (..)
    , ErrNetworkTip (..)
    , ErrGetBlock (..)
    , ErrPostTx (..)

    -- * Initialization
    , isNetworkUnreachable
    , defaultRetryPolicy
    , waitForNetwork
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logDebug, logError, logInfo, logWarning )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochNo
    , Hash (..)
    , PoolId (..)
    , SlotId
    , Tx
    , TxWitness
    )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( AsyncException (..)
    , Exception (..)
    , SomeException
    , asyncExceptionFromException
    , catch
    )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetriesByCumulativeDelay
    , retrying
    )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( throwIO )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data NetworkLayer m target block = NetworkLayer
    { nextBlocks
        :: Cursor target
        -> ExceptT ErrGetBlock m (NextBlocksResult target block)
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

    , findIntersection
        :: Cursor target -> m (Maybe BlockHeader)
        -- ^ Attempt to find an intersection between the node's unstable blocks
        -- and a given list of headers. This can be useful if we need to know
        -- whether we are 'in sync' with the node or, close enough.

    , initCursor
        :: [BlockHeader] -> Cursor target
        -- ^ Creates a cursor from the given block header so that 'nextBlocks'
        -- can be used to fetch blocks.

    , cursorSlotId
        :: Cursor target -> SlotId
        -- ^ Get the slot corresponding to a cursor.

    , networkTip
        :: ExceptT ErrNetworkTip m BlockHeader
        -- ^ Get the current network tip from the chain producer

    , postTx
        :: (Tx target, [TxWitness]) -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , staticBlockchainParameters
        :: (block, BlockchainParameters)

    , stakeDistribution
        :: ExceptT ErrNetworkUnavailable m
            ( EpochNo
            , Map PoolId (Quantity "lovelace" Word64)
            )
    }

instance Functor m => Functor (NetworkLayer m target) where
    fmap f nl = nl { nextBlocks = fmap (fmap f) . nextBlocks nl
                   , staticBlockchainParameters = (f block0, bp) }
      where
        (block0, bp) = staticBlockchainParameters nl

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

-- | Error while trying to get the network tip
data ErrNetworkTip
    = ErrNetworkTipNetworkUnreachable ErrNetworkUnavailable
    | ErrNetworkTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkTip

-- | Error while trying to get one or more blocks
data ErrGetBlock
    = ErrGetBlockNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockNotFound (Hash "BlockHeader")
    deriving (Show, Eq)

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnavailable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

{-------------------------------------------------------------------------------
                              Initialization
-------------------------------------------------------------------------------}

-- | Exception predicate for 'ErrNetworkUnreachable'.
isNetworkUnreachable :: ErrNetworkUnavailable -> Bool
isNetworkUnreachable (ErrNetworkUnreachable _) = True
isNetworkUnreachable (ErrNetworkInvalid _) = False


-- | Wait until 'networkTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForNetwork
    :: NetworkLayer IO tx block
    -> RetryPolicyM IO
    -> IO ()
waitForNetwork nw policy = do
    r <- retrying policy shouldRetry (const $ runExceptT (networkTip nw))
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkTipNotFound ->
            return True
        Left (ErrNetworkTipNetworkUnreachable e) ->
            return $ isNetworkUnreachable e

-- | A default 'RetryPolicy' with a delay that starts short, and that retries
-- for no longer than a minute.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (60 * second) (exponentialBackoff 10000)
  where
    second = 1000*1000

{-------------------------------------------------------------------------------
                                Chain Sync
-------------------------------------------------------------------------------}

-- | A cursor is local state kept by the chain consumer to use as the starting
-- position for 'nextBlocks'. The actual type is opaque and determined by the
-- backend @target@.
data family Cursor target

-- | The result of 'nextBlocks', which is instructions for what the chain
-- consumer should do next.
data NextBlocksResult target block
    = AwaitReply
        -- ^ There are no blocks available from the node, so wait.
    | RollForward (Cursor target) BlockHeader [block]
        -- ^ Apply the given contiguous non-empty sequence of blocks. Use the
        -- updated cursor to get the next batch. The given block header is the
        -- current tip of the node.
    | RollBackward (Cursor target)
        -- ^ The chain consumer must roll back its state, then use the cursor to
        -- get the next batch of blocks.

instance Functor (NextBlocksResult target) where
    fmap f = \case
        AwaitReply -> AwaitReply
        RollForward cur bh bs -> RollForward cur bh (fmap f bs)
        RollBackward cur -> RollBackward cur

-- | @FollowAction@ enables the callback of @follow@ to signal if the
-- chain-following should @ExitWith@, @Continue@, or if the current callback
-- should be forgotten and retried (@Retry@).
--
-- NOTE: @Retry@ is needed to handle data-races in
-- 'Cardano.Pool.Metrics', where it is essensial that we fetch the stake
-- distribution while the node-tip
data FollowAction err
    = ExitWith err
      -- ^ Stop following the chain.
    | Continue
      -- ^ Continue following the chain.
    | Retry
      -- ^ Forget about the blocks in the current callback, and retry again.
    deriving (Eq, Show)

-- | Subscribe to a blockchain and get called with new block (in order)!
follow
    :: forall target block e0 e1. (Show e0, Show e1)
    => NetworkLayer IO target block
    -- ^ The @NetworkLayer@ used to poll for new blocks.
    -> Trace IO Text
    -- ^ Logger trace
    -> [BlockHeader]
    -- ^ A list of known tips to start from. Blocks /after/ the tip will be yielded.
    -> (NE.NonEmpty block -> BlockHeader -> IO (FollowAction e0))
    -- ^ Callback with blocks and the current tip of the /node/.
    -- @follow@ stops polling and terminates if the callback errors.
    -> (SlotId -> IO (FollowAction e1))
    -- ^ Callback with a point of rollback when needed.
    -- @follow@ stops polling and terminates if the callback errors.
    -> (block -> BlockHeader)
    -- ^ Getter on the abstract 'block' type
    -> IO ()
follow nl tr cps yield rollback header =
    sleep 0 (initCursor nl cps)
  where
    delay0 :: Int
    delay0 = 1000*1000 -- 1 second

    retryDelay :: Int -> Int
    retryDelay delay = min (2*delay) (60 * delay0)

    -- | Wait a short delay before querying for blocks again. We also take this
    -- opportunity to refresh the chain tip as it has probably increased in
    -- order to refine our syncing status.
    sleep :: Int -> Cursor target -> IO ()
    sleep delay cursor = do
        when (delay > 0) (threadDelay delay)
        step cursor `catch` retry
      where
        retry (e :: SomeException) = case asyncExceptionFromException e of
            Just ThreadKilled ->
                return ()
            Just UserInterrupt ->
                return ()
            Just _ ->
                logError tr $ "Non-recoverable error following the chain: " <> eT
            _ -> do
                logError tr $ "Recoverable error following the chain: " <> eT
                sleep (retryDelay delay) cursor
          where
            eT = T.pack (show e)

    step :: Cursor target -> IO ()
    step cursor = runExceptT (nextBlocks nl cursor) >>= \case
        Left e -> do
            logWarning tr $ T.pack $ "Failed to get next blocks: " <> show e
            sleep delay0 cursor

        Right AwaitReply -> do
            logDebug tr "In sync with the node."
            sleep delay0 cursor

        Right (RollForward cursor' _ []) -> do -- FIXME Make RollForward return NE
            logDebug tr "In sync with the node."
            sleep delay0 cursor'

        Right (RollForward cursor' nodeTip (blockFirst : blocksRest)) -> do
            let blocks = blockFirst :| blocksRest
            let (slFst, slLst) =
                    ( slotId . header . NE.head $ blocks
                    , slotId . header . NE.last $ blocks
                    )
            liftIO $ logInfo tr $ mconcat
                [ "Applying blocks [", pretty slFst, " ... ", pretty slLst, "]" ]

            yield blocks nodeTip
                >>= handle cursor' "Failed to roll forward: "

        Right (RollBackward cursor') -> do
            let point = cursorSlotId nl cursor'
            logInfo tr $ "Rolling back to " <> pretty point
            rollback point
                >>= handle cursor' "Failed to roll backward: "
      where
        handle ::
            Show e
            => Cursor target
            -> String
            -> FollowAction e
            -> IO ()
        handle cursor' msg x = case x of
                ExitWith e ->
                    logError tr $ T.pack $
                        msg <> show e
                Continue ->
                    step cursor'
                Retry ->
                    step cursor
