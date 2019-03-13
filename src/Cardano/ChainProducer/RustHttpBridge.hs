{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.ChainProducer.RustHttpBridge
    ( RustBackend
    , runRustBackend
    ) where

import Control.Monad.Except
    ( ExceptT (..), mapExceptT, runExceptT )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Reader
    ( MonadReader, ReaderT (..), ask, lift )
import Data.Bifunctor
    ( first )
import Data.Maybe
    ( fromMaybe )
import Prelude

import Cardano.ChainProducer
    ( ErrGetNextBlocks (..), MonadChainProducer (..) )
import Cardano.ChainProducer.RustHttpBridge.NetworkLayer
    ( NetworkLayer (..), NetworkLayerError )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )
import Cardano.Wallet.Slotting
    ( EpochIndex
    , LocalSlotIndex (..)
    , SlotCount
    , SlotId (..)
    , addSlots
    , slotNext
    , slotsPerEpoch
    )

newtype RustBackend a = RustBackend {
    runRB :: ReaderT (NetworkLayer IO) IO a
    } deriving (Monad, Applicative, Functor,
                MonadReader (NetworkLayer IO), MonadIO)

runRustBackend :: NetworkLayer IO -> RustBackend a -> IO a
runRustBackend network action = runReaderT (runRB action) network

getNetwork :: RustBackend (NetworkLayer IO)
getNetwork = ask

instance MonadChainProducer RustBackend where
    nextBlocks = rbNextBlocks

-- Note: This will be quite inefficient for at least two reasons.
-- 1. If the number of blocks requested is small, it will fetch the same epoch
--    pack file repeatedly.
-- 2. Fetching the tip block and working backwards is not ideal.
-- We will keep it for now, and it can be improved later.
rbNextBlocks
    :: SlotCount -- ^ Number of blocks to retrieve
    -> SlotId    -- ^ Starting point
    -> ExceptT ErrGetNextBlocks RustBackend [Block]
rbNextBlocks numBlocks start = do
    net <- lift getNetwork
    (tipHash, tip) <- fmap headerSlot <$> runNetworkLayer (getNetworkTip net)
    epochBlocks <- blocksFromPacks net tip
    lastBlocks <- unstableBlocks net tipHash tip epochBlocks
    pure (epochBlocks ++ lastBlocks)

    where
        end = addSlots numBlocks start

        -- Grab blocks from epoch pack files
        blocksFromPacks network tip = do
            let epochs = epochRange numBlocks start tip
            epochBlocks <- runNetworkLayer (getEpochs network epochs)
            pure $ filter (blockIsBetween start end) (concat epochBlocks)

        -- The next slot after the last block.
        slotAfter [] = Nothing
        slotAfter bs = Just . slotNext . headerSlot . header . last $ bs

        -- Grab the remaining blocks which aren't packed in epoch files,
        -- starting from the tip.
        unstableBlocks network tipHash tip epochBlocks = do
            let start' = fromMaybe start (slotAfter epochBlocks)

            lastBlocks <- if end > start' && start' <= tip
                then runNetworkLayer $ fetchBlocksFromTip network start' tipHash
                else pure []

            pure $ filter (blockIsBefore end) lastBlocks

-- | Fetch epoch blocks until one fails.
getEpochs
    :: Monad m
    => NetworkLayer m
    -> [EpochIndex]
    -> ExceptT NetworkLayerError m [[Block]]
getEpochs network = mapUntilError (getEpoch network)

-- Fetch blocks which are not in epoch pack files.
fetchBlocksFromTip
    :: Monad m
    => NetworkLayer m
    -> SlotId
    -> Hash "BlockHeader"
    -> ExceptT NetworkLayerError m [Block]
fetchBlocksFromTip network start tipHash = reverse <$> workBackwards tipHash
    where
        workBackwards headerHash = do
            block <- getBlock network headerHash
            if blockIsAfter start block
                then do
                    blocks <- workBackwards $ prevBlockHash $ header block
                    pure (block:blocks)
                else pure [block]

runNetworkLayer
    :: ExceptT NetworkLayerError IO a
    -> ExceptT ErrGetNextBlocks RustBackend a
runNetworkLayer = mapExceptT (fmap handle . liftIO)
    where
        handle = first (GetNextBlocksError . show)

-- * Utility functions for monadic loops

-- | Apply an action to each element of a list, until an action fails, or there
-- are no more elements. This is like mapM, except that it always succeeds and
-- the resulting list might be smaller than the given list.
mapUntilError
    :: Monad m
    => (a -> ExceptT e m b)
       -- ^ Action to run
    -> [a]
       -- ^ Elements
    -> ExceptT e m [b]
       -- ^ Results
mapUntilError action (x:xs) = ExceptT $ runExceptT (action x) >>= \case
    Left _ -> pure $ Right []
    Right r -> runExceptT $ do
        rs <- mapUntilError action xs
        pure (r:rs)
mapUntilError _ [] = pure []

-- * Slotting calculation utilities

-- | Gets the slot from a block header.
headerSlot :: BlockHeader -> SlotId
headerSlot bh = SlotId
    (epochIndex (bh :: BlockHeader)) (slotNumber (bh :: BlockHeader))

-- | Calculates which epochs to fetch, given a number of slots, and the start
-- point. It takes into account the latest block available, and that the most
-- recent epoch is never available in a pack file.
epochRange
    :: SlotCount
        -- ^ Number of slots
    -> SlotId
        -- ^ Start point
    -> SlotId
        -- ^ Latest block available
    -> [EpochIndex]
epochRange
    numBlocks
    (SlotId startEpoch (LocalSlotIndex startSlot)) (SlotId tipEpoch _)
    = [startEpoch .. min (tipEpoch - 1) (startEpoch + fromIntegral numEpochs)]
    where
        numEpochs = (numBlocks + fromIntegral startSlot) `div` slotsPerEpoch

-- | Predicate returns true iff the block is from the given slot or a later one.
blockIsSameOrAfter :: SlotId -> Block -> Bool
blockIsSameOrAfter s = (>= s) . headerSlot . header

-- | Predicate returns true iff the block is after then given slot
blockIsAfter :: SlotId -> Block -> Bool
blockIsAfter s = (> s) . headerSlot . header

-- | Predicate returns true iff the block is before the given slot.
blockIsBefore :: SlotId -> Block -> Bool
blockIsBefore s = (< s) . headerSlot . header

-- | @blockIsBetween start end@ Returns true if the block is in within the
-- interval @[start, end)@.
blockIsBetween :: SlotId -> SlotId -> Block -> Bool
blockIsBetween start end b = blockIsSameOrAfter start b && blockIsBefore end b
