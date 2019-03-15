{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.NetworkLayer
    ( NetworkLayer (..)
    , tick
    , TickResult(..)
    , listen
    ) where

import Prelude

import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Monad.Except
    ( ExceptT, runExceptT )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Time.Units
    ( Millisecond, toMicroseconds )
import Fmt
    ( fmt, (+||), (||+) )
import System.Exit
    ( die )


data NetworkLayer m e0 e1 = NetworkLayer
    { nextBlocks :: SlotId -> ExceptT e0 m [Block]
        -- ^ Gets some blocks from the node. It will not necessarily return all
        -- the blocks that the node has, but will receive a reasonable-sized
        -- chunk. It will never return blocks from before the given slot. It
        -- may return an empty list if the node does not have any blocks from
        -- after the starting slot.

    , networkTip
        :: ExceptT e1 m (Hash "BlockHeader", BlockHeader)
    }

-- | Repeatedly fetch data from a given source function, and call an action for
-- each element retrieved.
--
-- If the data source indicates that it has no more data at present ('Sleep'),
-- then sleep for the interval @delay@, and then try the fetch again.
tick
    :: forall st m b. (MonadIO m)
    => (st -> m (TickResult [b], st))
    -- ^ A way to get a new elements
    -> (b -> m ())
    -- ^ Action to be taken on new elements
    -> Millisecond
    -- ^ Tick time
    -> st
    -- ^ Initial state.
    -> m ()
tick next action delay !st = do
    (res, !st') <- next st
    case res of
        GotChunk bs -> mapM_ action bs
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
    :: forall e0 e1. (Show e0)
    => NetworkLayer IO e0 e1
    -> (Block -> IO ())
    -> IO ()
listen network action = do
    tick getNextBlocks action 5000 (SlotId 0 0)
  where
    getNextBlocks :: SlotId -> IO (TickResult [Block], SlotId)
    getNextBlocks current = do
        res <- runExceptT $ nextBlocks network current
        case res of
            Left err ->
                die $ fmt $ "Chain producer error: "+||err||+""
            Right [] ->
                pure (Sleep, current)
            Right blocks ->
                let next = succ . slotId . header . last $ blocks
                in pure (GotChunk blocks, next)
