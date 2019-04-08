{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    ( NetworkLayer (..)
    , tick
    , TickResult(..)
    , listen
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SignedTx, SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Time.Units
    ( Millisecond, toMicroseconds )


data NetworkLayer m e0 e1 = NetworkLayer
    { nextBlocks :: SlotId -> ExceptT e0 m [Block]
        -- ^ Gets some blocks from the node. It will not necessarily return all
        -- the blocks that the node has, but will receive a reasonable-sized
        -- chunk. It will never return blocks from before the given slot. It
        -- may return an empty list if the node does not have any blocks from
        -- after the starting slot.

    , networkTip
        :: ExceptT e1 m (Hash "BlockHeader", BlockHeader)
    , postTx
        :: SignedTx -> ExceptT e1 m ()
    }

-- | Repeatedly fetch data from a given source function, and call an action for
-- each element retrieved.
--
-- If the data source indicates that it has no more data at present ('Sleep'),
-- then sleep for the interval @delay@, and then try the fetch again.
tick
    :: forall st m a. (MonadIO m)
    => (st -> m (TickResult a, st))
    -- ^ A way to get a new elements
    -> (a -> m ())
    -- ^ Action to be taken on new elements
    -> Millisecond
    -- ^ Tick time
    -> st
    -- ^ Initial state.
    -> m ()
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
    :: forall e0 e1. (Exception e0)
    => NetworkLayer IO e0 e1
    -> ([Block] -> IO ())
    -> IO ()
listen network action = do
    tick getNextBlocks action 5000 (SlotId 0 0)
  where
    getNextBlocks :: SlotId -> IO (TickResult [Block], SlotId)
    getNextBlocks current = do
        res <- runExceptT $ nextBlocks network current
        -- NOTE
        -- In order to avoid having to perform some slotting arithmetic, we only
        -- process blocks if we receive more than one, such that we can use the
        -- last block as the starting point for the next batch.
        -- The trade-off is is that we'll be fetching this last block twice,
        -- which is fair price to pay in order NOT to have to do any slotting
        -- arithmetic nor track how many blocks are present per epochs.
        case res of
            Left err -> throwIO err
            Right bs | length bs < 2 ->
                pure (Sleep, current)
            Right blocks ->
                let next = slotId . header . last $ blocks
                in pure (GotChunk (init blocks), next)
