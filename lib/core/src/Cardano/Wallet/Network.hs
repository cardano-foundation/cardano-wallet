{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Errors
    , ErrNetworkUnavailable (..)
    , ErrNetworkTip (..)
    , ErrGetBlock (..)
    , ErrPostTx (..)
    , isNetworkUnreachable

    -- * Helpers
    , defaultRetryPolicy
    , waitForNetwork
    , follow
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logDebug, logError, logInfo, logNotice, logWarning )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), TxWitness )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( Exception (..), SomeException )
import Control.Monad
    ( when )
import Control.Monad.Catch
    ( Handler )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
    ( RetryPolicyM
    , RetryStatus (..)
    , exponentialBackoff
    , limitRetriesByCumulativeDelay
    , logRetries
    , recovering
    , retrying
    )
import Data.Text
    ( Text )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( throwIO )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data NetworkLayer m tx block = NetworkLayer
    { nextBlocks :: BlockHeader -> ExceptT ErrGetBlock m [block]
        -- ^ Fetches a contiguous sequence of blocks from the node, starting
        -- from the first block available with a slot greater than the given
        -- block header.
        --
        -- Blocks are returned in ascending slot order, without skipping blocks.
        --
        -- This function will not necessarily return all blocks available after
        -- the given point in time, but will return a reasonably-sized sequence.
        --
        -- It may return the empty list if the node does not have any blocks
        -- after the specified starting slot.

    , networkTip
        :: ExceptT ErrNetworkTip m BlockHeader
        -- ^ Get the current network tip from the chain producer

    , postTx
        :: (tx, [TxWitness]) -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , staticBlockchainParameters
        :: (block, BlockchainParameters)
    }

instance Functor m => Functor (NetworkLayer m tx) where
    fmap f nl = nl { nextBlocks = fmap (fmap f) . nextBlocks nl
                   , staticBlockchainParameters = (f block0, bp) }
      where
        (block0, bp) = staticBlockchainParameters nl


-- | Network is unavailable
data ErrNetworkUnavailable
    = ErrNetworkUnreachable Text
      -- ^ Cannot connect to network backend.
    | ErrNetworkInvalid Text
      -- ^ Network backend reports that the requested network is invalid.
    deriving (Generic, Show, Eq)

-- | Exception predicate for 'ErrNetworkUnreachable'.
isNetworkUnreachable :: ErrNetworkUnavailable -> Bool
isNetworkUnreachable (ErrNetworkUnreachable _) = True
isNetworkUnreachable (ErrNetworkInvalid _) = False

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

-- | Subscribe to a blockchain and get called with new block (in order)!
follow
    :: Show e
    => NetworkLayer IO tx block
    -- ^ The @NetworkLayer@ used to poll for new blocks.
    -> Trace IO Text
    -> BlockHeader
    -- ^ The local tip to start at. Blocks /after/ the tip will be yielded.
    -> (NE.NonEmpty block -> BlockHeader -> ExceptT e IO ())
    -- ^ Callback with blocks and the current tip of the /node/. @follow@ stops
    -- polling and terminates if the callback errors.
    -> (block -> BlockHeader)
    -- ^ Getter on the abstract 'block' type
    -> IO ()
follow nl tr start yield header =
    sleep 0 start
  where
    pause :: Int
    pause = 2*1000*1000 -- 2 seconds

    -- | Wait a short delay before querying for blocks again. We also take this
    -- opportunity to refresh the chain tip as it has probably increased in
    -- order to refine our syncing status.
    sleep :: Int -> BlockHeader -> IO ()
    sleep delay localTip = do
        when (delay > 0) (threadDelay delay)
        let io = runExceptT (networkTip nl) >>= \case
                Right nodeTip ->
                    step (localTip, nodeTip)
                Left e -> do
                    logWarning tr $ T.pack $
                        "Failed to get network tip: " <> show e
                    sleep delay localTip
        recovering policy [shouldRetry] (const io)

    -- | Will retry after 30s, 1min, 2min, 4min, 8min, ...
    policy :: Monad m => RetryPolicyM m
    policy = exponentialBackoff (30*1000*1000)

    shouldRetry :: RetryStatus -> Handler IO Bool
    shouldRetry = logRetries
            -- Could do something more subtil her
            (\(_e :: SomeException) -> pure True)
            (\_ e _ -> logError tr $ T.pack $
                "Unexpected failure while following the chain: " <> show e
            )

    step :: (BlockHeader, BlockHeader) -> IO ()
    step (localTip, nodeTip) = do
        runExceptT (nextBlocks nl localTip) >>= \case
            Left e -> do
                logWarning tr $ T.pack $ "Failed to get next blocks: " <> show e
                sleep pause localTip
            Right [] -> do
                logDebug tr "In sync with the node."
                sleep pause localTip
            Right (blockFirst : blocksRest) -> do
                let blocks = blockFirst NE.:| blocksRest

                let nextLocalTip = header $ NE.last blocks
                let (slotFirst, slotLast) =
                        ( slotId . header . NE.head $ blocks
                        , slotId . header . NE.last $ blocks
                        )
                liftIO $ logInfo tr $ mconcat
                    [ "Applying blocks ["
                    , pretty slotFirst
                    , " ... "
                    , pretty slotLast
                    , "]"
                    ]

                runExceptT (yield blocks nodeTip) >>= \case
                    Left e ->
                        liftIO $ logNotice tr $ T.pack $
                            "Stopped following chain: " <> show e
                    Right () -> do
                        step (nextLocalTip, nodeTip)
