{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.NetworkLayer
    ( NetworkLayer (..)
    , tick
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

-- | Every interval @delay@, fetches some data from a given source, and call
-- an action for each elements retrieved.
tick
    :: forall st m b. (MonadIO m)
    => (st -> m ([b], st))
    -- ^ A way to get a new elements
    -> (b -> m ())
    -- ^ Action to be taken on new elements
    -> Millisecond
    -- ^ tick time
    -> st
    -> m ()
tick next action delay !st = do
    (bs, !st') <- next st
    mapM_ action bs
    liftIO $ threadDelay $ (fromIntegral . toMicroseconds) delay
    tick next action delay st'

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
    getNextBlocks :: SlotId -> IO ([Block], SlotId)
    getNextBlocks current = do
        res <- runExceptT $ nextBlocks network current
        case res of
            Left err ->
                die $ fmt $ "Chain producer error: "+||err||+""
            Right [] ->
                pure ([], current)
            Right blocks ->
                -- fixme: there are more blocks available, so we need not
                -- wait for an interval to pass before getting more blocks.
                let next = succ . slotId . header . last $ blocks
                in pure (blocks, next)
