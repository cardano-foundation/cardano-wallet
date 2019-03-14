{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the ticking function that is responsible for invoking
-- block acquisition functionality and executing it in periodic fashion.
--
-- Known limitations:
-- - Blocks are produced by the network layer. They are not expected to produce
--   any duplicates and to rollback.
--
-- -

module Cardano.Wallet.BlockSyncer
  ( tick
  , listen
  ) where


import Prelude

import Cardano.NetworkLayer
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Monad.Except
    ( runExceptT )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Time.Units
    ( Millisecond, toMicroseconds )
import Fmt
    ( fmt, (+||), (||+) )
import System.Exit
    ( die )


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
