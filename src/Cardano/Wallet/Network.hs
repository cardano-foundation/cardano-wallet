{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Errors
    , ErrNetworkUnreachable(..)
    , ErrNetworkTip(..)
    , ErrPostTx(..)

    -- * Deprecated (to be removed)
    , TickResult(..)
    , tick
    , listen
    , drain
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SignedTx, SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( Exception )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Text
    ( Text )
import Data.Time.Units
    ( Millisecond, toMicroseconds )
import GHC.Generics
    ( Generic )

data NetworkLayer m = NetworkLayer
    { nextBlocks :: SlotId -> ExceptT ErrNetworkUnreachable m [Block]
        -- ^ Gets some blocks from the node. It will not necessarily return all
        -- the blocks that the node has, but will receive a reasonable-sized
        -- chunk. It will never return blocks from before the given slot. It
        -- may return an empty list if the node does not have any blocks from
        -- after the starting slot.

    , networkTip
        :: ExceptT ErrNetworkTip m (Hash "BlockHeader", BlockHeader)
        -- ^ Get the current network tip from the chain producer

    , postTx
        :: SignedTx -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer
    }

-- | Network is not reachable
newtype ErrNetworkUnreachable
    = ErrNetworkUnreachable Text
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkUnreachable

-- | Error while trying to get the network tip
data ErrNetworkTip
    = ErrNetworkTipNetworkUnreachable ErrNetworkUnreachable
    | ErrNetworkTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkTip

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnreachable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

-- | Repeatedly fetch data from a given source function, and call an action for
-- each element retrieved.
--
-- If the data source indicates that it has no more data at present ('Sleep'),
-- then sleep for the interval @delay@, and then try the fetch again.
tick
    :: forall st m a r. (MonadIO m)
    => (st -> m (TickResult a, st))
    -- ^ A way to get a new elements
    -> (a -> m ())
    -- ^ Action to be taken on new elements
    -> Millisecond
    -- ^ Tick time
    -> st
    -- ^ Initial state.
    -> m r
tick next action delay !st = do
    (res, !st') <- next st
    case res of
        GotChunk a -> action a
        Sleep -> liftIO $ threadDelay $ (fromIntegral . toMicroseconds) delay
    tick next action delay st'

-- | The result type of the element fetch function provided to 'tick'.
data TickResult a
    = GotChunk !a -- ^ Have a result, and there may be more available.
    | Sleep -- ^ There is no result available now, so wait.
    deriving (Show, Eq, Foldable)

-- | Retrieve blocks from a chain producer and execute some given action for
-- each block.
listen
    :: NetworkLayer IO
    -> ([Block] -> IO ())
    -> IO a
listen network action =
    tick (getNextBlocks network) action 5000 (SlotId 0 0)

getNextBlocks
    :: NetworkLayer IO
    -> SlotId
    -> IO (TickResult [Block], SlotId)
getNextBlocks network current = do
    res <- runExceptT $ nextBlocks network current
    -- NOTE
    -- In order to avoid having to perform some slotting arithmetic, we only
    -- process blocks if we receive more than one, such that we can use the
    -- last block as the starting point for the next batch.
    -- The trade-off is is that we'll be fetching this last block twice,
    -- which is fair price to pay in order NOT to have to do any slotting
    -- arithmetic nor track how many blocks are present per epochs.
    case res of
        Left err -> do
            -- FIXME Log the actual error with a proper logger
            print err
            pure (Sleep, current)
        Right bs | length bs < 2 ->
            pure (Sleep, current)
        Right blocks ->
            let next = slotId . header . last $ blocks
            in pure (GotChunk (init blocks), next)

-- | Process all available blocks from the network layer with the given action.
-- When the network layer reports no more blocks, stop.
drain
    :: NetworkLayer IO
    -> (SlotId -> [Block] -> IO ())
    -> IO ()
drain network action = go (SlotId 0 0)
  where
    go :: SlotId -> IO ()
    go start = do
        (res, start') <- getNextBlocks network start
        case res of
            GotChunk blocks -> do
                action start' blocks
                go start'
            Sleep -> pure ()
