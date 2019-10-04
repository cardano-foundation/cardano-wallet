{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module provides a data structure and functions for operating on a
-- sequence of block headers.
--
-- The 'BlockHeaders' sequence is used for maintaining the global unstable
-- blocks state for the network layer. The unstable blocks state can be compared
-- with the block headers state of chain consumers to determine the intersection
-- point.
--

module Cardano.Wallet.Jormungandr.BlockHeaders
    (
    -- * Types
      BlockHeaders(..)
    , emptyBlockHeaders

    -- * Managing the global unstable blocks state
    , updateUnstableBlocks

    -- * Operations
    , blockHeadersTip
    , appendBlockHeaders
    , dropStartingFromSlotId
    , dropAfterSlotId
    , greatestCommonBlockHeader
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )
import Control.DeepSeq
    ( NFData, ($!!) )
import Data.Quantity
    ( Quantity (..) )
import Data.Sequence
    ( Seq (..), (><) )
import Data.Tuple.Extra
    ( thd3 )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Safe
    ( lastMay )

import qualified Data.Sequence as Seq

{-------------------------------------------------------------------------------
                                     Types
-------------------------------------------------------------------------------}

-- | A list of block headers and their hashes.
-- The last block in this sequence is the network tip.
-- The first block in this sequence is the block of depth /k/,
-- which is the last unstable block.
data BlockHeaders = BlockHeaders
    { getBlockHeaders :: !(Seq (Hash "BlockHeader", BlockHeader))
    -- ^ Double-ended queue of block headers, and their IDs.
    , getBlockHeight :: !(Quantity "block" Word32)
    -- ^ The block height of the tip of the sequence.
    } deriving stock (Show, Eq, Generic)

instance NFData BlockHeaders

-- | Constuct an empty unstable blocks sequence.
emptyBlockHeaders :: BlockHeaders
emptyBlockHeaders = BlockHeaders mempty (Quantity 0)

{-------------------------------------------------------------------------------
                   Managing the global unstable blocks state
-------------------------------------------------------------------------------}

-- | Updates the unstable blocks state using the given "fetch" functions.
--
-- This attempts to synchronise the local state with that of the node. The node
-- may be on a different chain to the current unstable blocks, so this function
-- handles switching of chains.
--
-- For example, this is what it would do when the local tip is @a13@, but the
-- node's tip is @b15@, on a different chain.
--
-- @
--                                    local tip ↴
--                  ┌───┬───  ───┬───┬───┬───┬───┐
--  Unstable blocks │a03│..    ..│a10│a11│a12│a13│
--                  └───┴───  ───┴───┴───┴───┴───┘
--                            ───┬───┬───┬───┬───┬───┬───┐
--  Node backend chain        ...│a10│a11│b12│b13│b14│b15│
--                            ───┴───┴───┴───┴───┴───┴───┘
--                        rollback point ⬏     node tip ⬏
-- @
--
-- To start with, the node says the tip hash is @b15@.
--
-- Work backwards from tip, fetching blocks and adding them to @ac@, and
-- removing overlapping blocks from @ubs@. Overlapping blocks occur when there
-- has been a rollback.
--
-- @
--     ubs                     ac
-- 1.  ───┬───┬───┬───┬───┐    ┌───┐
--     ...│a10│a11│a12│a13│    │b15│
--     ───┴───┴───┴───┴───┘    └───┘
-- 2.  ───┬───┬───┬───┬───┐┌───┬───┐
--     ...│a10│a11│a12│a13││b14│b15│
--     ───┴───┴───┴───┴───┘└───┴───┘
-- 3.  ───┬───┬───┬───┐┌───┬───┬───┐
--     ...│a10│a11│a12││b13│b14│b15│
--     ───┴───┴───┴───┘└───┴───┴───┘
-- 4.  ───┬───┬───┐┌───┬───┬───┬───┐
--     ...│a10│a11││b12│b13│b14│b15│
--     ───┴───┴───┘└───┴───┴───┴───┘
-- @
--
-- Stop once @ubs@ and @ac@ meet with a block which has the same hash.
-- If they never meet, stop cleanupConfig fetching /k/ blocks.
--
-- Finally, to get the new 'BlockHeaders', append @ac@ to @ubs@, and limit the
-- length to /k/.
--
-- The new block height is the height of the first block that was fetched.
--
-- If any errors occur while this process is running (for example, fetching a
-- block which has been rolled back and lost from the node's state), it will
-- immediately terminate.
updateUnstableBlocks
    :: forall m. Monad m
    => Quantity "block" Word32
    -- ^ Maximum number of unstable blocks (/k/).
    -> m (Hash "BlockHeader")
    -- ^ Network Tip
    -> (Hash "BlockHeader" -> m (BlockHeader, Quantity "block" Word32))
    -- ^ Fetches block header and its chain height.
    -> BlockHeaders
    -- ^ Current unstable blocks state.
    -> m BlockHeaders
updateUnstableBlocks (Quantity k) getTip getBlockHeader lbhs = do
    tip0 <- getTip
    -- Trace backwards from the tip, accumulating new block headers, and
    -- removing overlapped unstable block headers.
    (lbhs', nbhs) <- fetchBackwards lbhs [] 0 tip0
    -- The new unstable blocks is the current local blocks up to where they
    -- meet the blocks fetched starting from the tip, with the fetched
    -- blocks appended.
    pure $!! appendBlockHeaders (Quantity k) lbhs' nbhs
  where
    -- | Fetch blocks backwards starting from the given id. If fetched blocks
    -- overlap the local blocks, the excess local blocks will be dropped.
    fetchBackwards
        :: BlockHeaders
        -- ^ Current local unstable blocks
        -> [(Hash "BlockHeader", BlockHeader, Quantity "block" Word32)]
        -- ^ Accumulator of fetched blocks
        -> Word32
        -- ^ Accumulator for number of blocks fetched
        -> Hash "BlockHeader"
        -- ^ Starting point for block fetch
        -> m (BlockHeaders, [(Hash "BlockHeader", BlockHeader, Quantity "block" Word32)])
    fetchBackwards ubs ac len tip = do
        (tipHeader, tipHeight) <- getBlockHeader tip
        -- Push the remote block.
        let ac' = ((tip, tipHeader, tipHeight):ac)
         -- Pop off any overlap.
        let ubs' = dropStartingFromSlotId (slotId tipHeader) ubs
        -- If remote blocks have met local blocks, or if more than k have been
        -- fetched, or we are at the genesis, then stop.
        -- Otherwise, continue from the parent of the current tip.
        let intersected = blockHeadersTipId ubs' == Just (prevBlockHash tipHeader)
        let bufferFull = len + 1 >= k
        let atGenesis = slotId tipHeader == SlotId 0 0
        if intersected || bufferFull || atGenesis
            then pure (ubs', ac')
            else fetchBackwards ubs' ac' (len + 1) (prevBlockHash tipHeader)

-- | The tip block header of the unstable blocks, if it exists.
blockHeadersTip :: BlockHeaders -> Maybe BlockHeader
blockHeadersTip (BlockHeaders Empty _) = Nothing
blockHeadersTip (BlockHeaders (_ubs :|> (_, bh)) _) = Just bh

-- | The base block header is the oldest block header in the unstable blocks,
blockHeadersBase :: BlockHeaders -> Maybe BlockHeader
blockHeadersBase (BlockHeaders Empty _) = Nothing
blockHeadersBase (BlockHeaders ((_, bh) :<| _ubs) _) = Just bh

-- | The tip block id of the unstable blocks, if it exists.
blockHeadersTipId :: BlockHeaders -> Maybe (Hash "BlockHeader")
blockHeadersTipId (BlockHeaders Empty _) = Nothing
blockHeadersTipId (BlockHeaders (_ubs :|> (t, _)) _) = Just t

-- | Add recently fetched block headers to the unstable blocks. This will drop
-- the oldest block headers to ensure that there are at most /k/ items in the
-- sequence.
appendBlockHeaders
    :: Quantity "block" Word32
    -- ^ Maximum length of sequence.
    -> BlockHeaders
    -- ^ Current unstable block headers, with rolled back blocks removed.
    -> [(Hash "BlockHeader", BlockHeader, Quantity "block" Word32)]
    -- ^ Newly fetched block headers to add.
    -> BlockHeaders
appendBlockHeaders (Quantity k) (BlockHeaders ubs h) bs =
    BlockHeaders (ubs `appendBounded` more) h'
  where
    more = Seq.fromList [(a, b) | (a, b, _) <- bs]
    -- New block height is the height of the new tip block.
    h' = maybe h thd3 (lastMay bs)

    -- Concatenate sequences, ensuring that the result is no longer than k.
    appendBounded :: Seq a -> Seq a -> Seq a
    appendBounded a b = Seq.drop excess (a >< b)
        where excess = max 0 (Seq.length a + Seq.length b - fromIntegral k)

-- | Remove unstable blocks which have a slot greater than or equal to the given
-- slot.
dropStartingFromSlotId :: SlotId -> BlockHeaders -> BlockHeaders
dropStartingFromSlotId sl (BlockHeaders bs (Quantity h)) =
    BlockHeaders bs' (Quantity h')
  where
    isAfter = (>= sl) . slotId . snd
    bs' = Seq.dropWhileR isAfter bs
    h' = h + fromIntegral (max 0 $ Seq.length bs' - Seq.length bs)

-- | Drop any headers that are (strictly) after the given slot id.
dropAfterSlotId :: SlotId -> BlockHeaders -> BlockHeaders
dropAfterSlotId sl (BlockHeaders bs (Quantity h)) =
    BlockHeaders bs' (Quantity h')
  where
    isAfter = (> sl) . slotId . snd
    bs' = Seq.dropWhileR isAfter bs
    h' = h + fromIntegral (max 0 $ Seq.length bs' - Seq.length bs)

takeUntilSlotId :: SlotId -> BlockHeaders -> BlockHeaders
takeUntilSlotId sl (BlockHeaders bs (Quantity h)) =
    BlockHeaders bs' (Quantity h')
  where
    isBefore = (< sl) . slotId . snd
    bs' = Seq.dropWhileL isBefore bs
    h' = h + fromIntegral (max 0 $ Seq.length bs' - Seq.length bs)

-- | If the two sequences overlap in terms of slots, return the block header of
-- the last block that is common between the two. Otherwise return Nothing.
--
-- For example:
-- @
--        | (1)         | (2)         | (3)         | (4)         | (5)
-- Node   | abcdefg     |     efg     | abcdefg     | abcde       | abc
-- Local  |   cdpqrst   | abc         | abcdefg     |             |     efg
-- GCBH   | Just d      | Nothing     | Just g      | Nothing     | Nothing
-- @
greatestCommonBlockHeader
    :: BlockHeaders
    -- ^ Node's unstable blocks
    -> BlockHeaders
    -- ^ Local wallet unstable blocks
    -> Maybe BlockHeader
greatestCommonBlockHeader ubs lbs = case (minSlot, maxSlot) of
    (Just start, Just end) -> let
          (BlockHeaders ubs' _) = trimRange start end ubs
          (BlockHeaders lbs' _) = trimRange start end lbs
          pairs = Seq.zip ubs' lbs'
        in case Seq.dropWhileR (uncurry (/=)) pairs of
               _same :|> ((_, bh), _) -> Just bh
               Empty -> Nothing
    _ -> Nothing
  where
    trimRange start end = takeUntilSlotId start . dropAfterSlotId end
    minSlot = max (baseSlot ubs) (baseSlot lbs)
    maxSlot = min (tipSlot ubs) (tipSlot lbs)
    tipSlot = fmap slotId . blockHeadersTip
    baseSlot = fmap slotId . blockHeadersBase
