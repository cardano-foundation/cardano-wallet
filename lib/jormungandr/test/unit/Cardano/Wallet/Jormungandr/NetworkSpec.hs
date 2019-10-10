{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.NetworkSpec where

import Prelude

import Cardano.Wallet.Jormungandr.Api.Client
    ( JormungandrClient (..) )
import Cardano.Wallet.Jormungandr.BlockHeaders
    ( emptyBlockHeaders )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (Testnet) )
import Cardano.Wallet.Jormungandr.Network
    ( ErrGetDescendants (..), mkRawNetworkLayer )
import Cardano.Wallet.Network
    ( Cursor, ErrGetBlock (..), NetworkLayer (..), NextBlocksResult (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent.MVar.Lifted
    ( newMVar )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( except, runExceptT )
import Control.Monad.Trans.State.Strict
    ( StateT (..), evalStateT, get, gets, modify', put, runStateT )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List
    ( foldl', intersect, nub, zip4, (\\) )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Fmt
    ( fmt, hexF, padLeftF )
import Safe
    ( headMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , frequency
    , property
    , shrinkList
    , shrinkNothing
    , sized
    , sublistOf
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Chain" $ do
        it "Always generate valid chains" $
            property prop_generator

    describe "Chain sync" $ do
        it "Syncs with mock node" $
            withMaxSuccess 100000 prop_sync

{-------------------------------------------------------------------------------
                Syncing of network layer in presence of rollback
                          and concurrent node updates.
-------------------------------------------------------------------------------}

fromList = Map.fromList
test1 = S {node = N {nodeDb = fromList [], nodeChainIds = [], nodeNextBlockId = 0}, operations = [[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}, mockBlockContent = 0}]],[],[],[],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}, mockBlockContent = 1},MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}, mockBlockContent = 2},MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}, mockBlockContent = 3}]],[],[],[NodeGarbageCollect [Hash {getHash = "0000"}]],[],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}, mockBlockContent = 4},MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0004"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}, mockBlockContent = 5},MockBlock {mockBlockId = Hash {getHash = "0006"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}, mockBlockContent = 6}]],[],[],[],[],[],[],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0007"}, mockBlockPrev = Hash {getHash = "0006"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}, mockBlockContent = 7}]],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0008"}, mockBlockPrev = Hash {getHash = "0007"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}, mockBlockContent = 8}]],[NodeGarbageCollect [Hash {getHash = "0001"},Hash {getHash = "0002"},Hash {getHash = "0003"}]],[],[],[],[],[],[],[],[],[],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0009"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}, mockBlockContent = 9}]],[],[],[],[],[],[NodeRewind 1,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000a"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}, mockBlockContent = 10}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000b"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}, mockBlockContent = 11}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000c"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}, mockBlockContent = 12}]],[NodeRewind 2,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000d"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}, mockBlockContent = 13},MockBlock {mockBlockId = Hash {getHash = "000e"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}, mockBlockContent = 14}]],[],[],[NodeGarbageCollect [Hash {getHash = "000b"}]],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000f"}, mockBlockPrev = Hash {getHash = "000e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}, mockBlockContent = 15}]],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0010"}, mockBlockPrev = Hash {getHash = "000f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}, mockBlockContent = 16}]],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0011"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}, mockBlockContent = 17},MockBlock {mockBlockId = Hash {getHash = "0012"}, mockBlockPrev = Hash {getHash = "0011"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}, mockBlockContent = 18},MockBlock {mockBlockId = Hash {getHash = "0013"}, mockBlockPrev = Hash {getHash = "0012"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}, mockBlockContent = 19}]],[],[],[],[NodeGarbageCollect [Hash {getHash = "0009"},Hash {getHash = "000c"},Hash {getHash = "000e"}]],[],[],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0014"}, mockBlockPrev = Hash {getHash = "0013"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 10}, mockBlockContent = 20}]],[],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0015"}, mockBlockPrev = Hash {getHash = "0011"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}, mockBlockContent = 21},MockBlock {mockBlockId = Hash {getHash = "0016"}, mockBlockPrev = Hash {getHash = "0015"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}, mockBlockContent = 22},MockBlock {mockBlockId = Hash {getHash = "0017"}, mockBlockPrev = Hash {getHash = "0016"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 10}, mockBlockContent = 23}]],[],[],[NodeGarbageCollect [Hash {getHash = "0010"}]],[NodeGarbageCollect [Hash {getHash = "000f"},Hash {getHash = "0012"},Hash {getHash = "0013"},Hash {getHash = "0014"}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0018"}, mockBlockPrev = Hash {getHash = "0017"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 11}, mockBlockContent = 24}]],[],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0019"}, mockBlockPrev = Hash {getHash = "0015"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}, mockBlockContent = 25},MockBlock {mockBlockId = Hash {getHash = "001a"}, mockBlockPrev = Hash {getHash = "0019"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 11}, mockBlockContent = 26}]],[],[],[],[],[NodeGarbageCollect [Hash {getHash = "0018"}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001b"}, mockBlockPrev = Hash {getHash = "001a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 12}, mockBlockContent = 27}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001c"}, mockBlockPrev = Hash {getHash = "001b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 13}, mockBlockContent = 28}]],[NodeGarbageCollect [Hash {getHash = "0016"},Hash {getHash = "0017"}]],[],[],[NodeRewind 1,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001d"}, mockBlockPrev = Hash {getHash = "001b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 13}, mockBlockContent = 29}]],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001e"}, mockBlockPrev = Hash {getHash = "001d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 14}, mockBlockContent = 30}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001f"}, mockBlockPrev = Hash {getHash = "001e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 15}, mockBlockContent = 31}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0020"}, mockBlockPrev = Hash {getHash = "001f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 16}, mockBlockContent = 32}]],[],[],[],[],[],[],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0021"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}, mockBlockContent = 33}]],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0022"}, mockBlockPrev = Hash {getHash = "0021"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}, mockBlockContent = 34}]],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0023"}, mockBlockPrev = Hash {getHash = "0022"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}, mockBlockContent = 35}]],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0024"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}, mockBlockContent = 36},MockBlock {mockBlockId = Hash {getHash = "0025"}, mockBlockPrev = Hash {getHash = "0024"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}, mockBlockContent = 37},MockBlock {mockBlockId = Hash {getHash = "0026"}, mockBlockPrev = Hash {getHash = "0025"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}, mockBlockContent = 38}]],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0027"}, mockBlockPrev = Hash {getHash = "0026"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 20}, mockBlockContent = 39}]],[],[],[],[],[],[],[NodeRewind 4,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0028"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}, mockBlockContent = 40},MockBlock {mockBlockId = Hash {getHash = "0029"}, mockBlockPrev = Hash {getHash = "0028"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}, mockBlockContent = 41},MockBlock {mockBlockId = Hash {getHash = "002a"}, mockBlockPrev = Hash {getHash = "0029"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}, mockBlockContent = 42},MockBlock {mockBlockId = Hash {getHash = "002b"}, mockBlockPrev = Hash {getHash = "002a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 20}, mockBlockContent = 43}]],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002c"}, mockBlockPrev = Hash {getHash = "002b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 21}, mockBlockContent = 44}]],[],[],[],[],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002d"}, mockBlockPrev = Hash {getHash = "002c"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 22}, mockBlockContent = 45}]],[],[],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002e"}, mockBlockPrev = Hash {getHash = "002d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 23}, mockBlockContent = 46}]],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002f"}, mockBlockPrev = Hash {getHash = "002e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 24}, mockBlockContent = 47}]],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0030"}, mockBlockPrev = Hash {getHash = "002f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 25}, mockBlockContent = 48}]],[],[],[],[],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0031"}, mockBlockPrev = Hash {getHash = "0030"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 26}, mockBlockContent = 49}]]], mockNodeK = Quantity {getQuantity = 15}, logs = []}
test2 = S {node = N {nodeDb = fromList [], nodeChainIds = [], nodeNextBlockId = 0}, operations = [[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}, mockBlockContent = 0}]],[NodeRewind 1,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}, mockBlockContent = 1}]],[NodeGarbageCollect [Hash {getHash = "0000"}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}, mockBlockContent = 2}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}, mockBlockContent = 3}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}, mockBlockContent = 4}]],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}, mockBlockContent = 5}]]], mockNodeK = Quantity {getQuantity = 15}, logs = []}
test3 = S {node = N {nodeDb = fromList [], nodeChainIds = [], nodeNextBlockId = 0}, operations = [[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}, mockBlockContent = 0}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "0000"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}, mockBlockContent = 1},MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}, mockBlockContent = 2},MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}, mockBlockContent = 3}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}, mockBlockContent = 4}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0004"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}, mockBlockContent = 5}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0006"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}, mockBlockContent = 6}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0007"}, mockBlockPrev = Hash {getHash = "0006"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}, mockBlockContent = 7}]],[NodeRewind 4],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0008"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}, mockBlockContent = 8}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0009"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}, mockBlockContent = 9}]],[NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000a"}, mockBlockPrev = Hash {getHash = "0009"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}, mockBlockContent = 10}]],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000b"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}, mockBlockContent = 11},MockBlock {mockBlockId = Hash {getHash = "000c"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}, mockBlockContent = 12}]],[NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000d"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}, mockBlockContent = 13},MockBlock {mockBlockId = Hash {getHash = "000e"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}, mockBlockContent = 14}]]], mockNodeK = Quantity {getQuantity = 16}, logs = []}


prop_sync :: S -> Property
prop_sync s0 = monadicIO $ do
    let logLineC msg = logLine $ "[CONSUMER] " <> msg

    (consumer, s) <- run $ flip runStateT s0 $ do
        -- Set up network layer with mock Jormungandr
        nl <- mockNetworkLayer logLine
        -- Run a model chain consumer on the mock network layer.
        let limit = max 2 (length $ operations s0)
        let initialConsumer = C [] (initCursor nl fakeBlock) limit
        consumerRestoreStep logLineC nl initialConsumer

    -- NOTE: We never apply the first block 'block0', as this one is given as a
    -- parameter when starting the wallet. The network layer will never yield
    -- it.
    let nodeChain = drop 1 (getNodeChain (node s))

    monitor $ counterexample $ unlines
        [ "Applied blocks:  " <> showChain (consumerApplied consumer)
        , "Node chain:      " <> showChain nodeChain
        , "Steps remaining: " <> show (consumerStepsRemaining consumer)
        , "k =              " <> show (mockNodeK s0)
        , "Logs:          \n" <> unlines (reverse (logs s))
        ]

    -- tagging:
    --   advanced more than k blocks
    --   started with more than k blocks
    --   rollback full k
    --   switch chain longer
    --   switch chain shorter
    --   rewind without switch
    --   consumer starts ahead of node


    -- Consumer chain should (eventually) be in sync with node chain.
    assert (consumerApplied consumer == nodeChain || null nodeChain)
  where
    logLine msg = modify' (\(S a0 a1 a2 logs) -> S a0 a1 a2 (msg:logs))

showChain :: [MockBlock] -> String
showChain [] = "∅"
showChain chain = unwords . map showSlot . addGaps $ chain
  where
    showSlot = maybe "_" (showHash . mockBlockId)
    addGaps [] = []
    addGaps [b] = [Just b]
    addGaps (b1:b2:bs) = [Just b1] ++ replicate gap Nothing ++ addGaps (b2:bs)
      where
        gap = slotNum b2 - slotNum b1 - 1
        slotNum = fromIntegral . slotNumber . mockBlockSlot

showBlock :: MockBlock -> String
showBlock  (MockBlock ownId parentId sl _) = mconcat $
    [ show $ epochNumber sl
    , "."
    , show $ slotNumber sl
    , " "
    , B8.unpack (getHash ownId)
    ]
    ++
    if ownId == Hash "genesis"
    then []
    else
        [ "->"
        , B8.unpack (getHash parentId)
        ]

-- | This isn't actually the genesis block, but rather, a fake block that
-- allow us to bootstrap everything else. Only its hash matters here. The
-- solution is really not ideal though and we should rely instead on the current
-- block hashes instead of their parent.
fakeBlock :: BlockHeader
fakeBlock = BlockHeader (SlotId 0 0) (Quantity 0) (Hash "genesis")

----------------------------------------------------------------------------
-- Model consumer

-- | Model of consumer state.
data Consumer = C
    { consumerApplied :: [MockBlock]
    -- ^ Blocks which have been received.
    , _consumerCursor :: Cursor (Jormungandr 'Testnet)
    -- ^ Consumer state -- the cursor given by network layer.
    , consumerStepsRemaining :: Int
    -- ^ Counter which prevents infinite loops in tests.
    }

-- | A model consumer that repeatedly fetches blocks and "applies" them by
-- appending to its chain. Returns the new 'Consumer' state.
-- This is similar to the Cardano.Wallet worker restore loop.
consumerRestoreStep
    :: (Monad m)
    => (String -> StateT S m ())
    -- ^ logger function
    -> TestNetworkLayer m
    -- ^ Network layer.
    -> Consumer
    -- ^ Current consumer state.
    -> StateT S m Consumer
consumerRestoreStep logLine nw c@(C bs cur iLimit)
    | iLimit > 0 = do
        logLine $ "nextBlocks " <> show (cursorSlotId nw cur)
        runExceptT (nextBlocks nw cur) >>= \case
            Left e -> do
                logLine ("Failed to get next blocks: " ++ show e)
                consumerRestoreStep logLine nw (C bs cur i)
            Right AwaitReply -> do
                logLine "AwaitReply"
                consumerRestoreStep logLine nw (C bs cur i)
            Right (RollForward cur' _ bs') -> do
                logLine $ "RollForward " <> unwords (showBlock <$> bs')
                consumerRestoreStep logLine nw (C (bs ++ bs') cur' i)
            Right (RollBackward cur') -> do
                logLine $ "RollBackward " <> show (cursorSlotId nw cur')
                let sl = cursorSlotId nw cur'
                let bs' = takeWhile (\b -> mockBlockSlot b <= sl) bs
                consumerRestoreStep logLine nw (C bs' cur' i)
            Right Recover -> do
                logLine "Recover"
                let cur0 = initCursor nw fakeBlock
                consumerRestoreStep logLine nw (C [] cur0 i)

    | otherwise = pure c -- don't loop forever
  where
    i = iLimit - 1

----------------------------------------------------------------------------
-- Network layer with mock jormungandr node

type TestNetworkLayer m =
    NetworkLayer (StateT S m) (Jormungandr 'Testnet) MockBlock

-- | Instantiate new network layer with mock jormungandr.
mockNetworkLayer
    :: forall m. (MonadFail m, MonadBaseControl IO m)
    => (String -> StateT S m ()) -- ^ logger function
    -> StateT S m (TestNetworkLayer m)
mockNetworkLayer logLine = do
    let jm = mockJormungandrClient logLine
    st <- newMVar emptyBlockHeaders
    Right g0 <- runExceptT $ getInitialBlockchainParameters jm (Hash "genesis")
    pure $ fromJBlock <$> mkRawNetworkLayer g0 st jm

-- | A network layer which returns mock blocks and mutates its state according
-- to the generated operations.
mockJormungandrClient
    :: forall m. (Monad m)
    => (String -> StateT S m ())
        -- ^ logger function
    -> JormungandrClient (StateT S m)
mockJormungandrClient logLine = JormungandrClient
    { getTipId = do
        ch <- lift $ gets (nodeChainIds . node)
        let tip = fromMaybe (Hash "genesis") $ headMay ch
        lift . logLineP $ "getTipId" <> returns tip
        lift applyOps
        pure tip

    , getBlock = \blockId -> do
        bs <- lift $ gets (nodeDb . node)
        let block = case Map.lookup blockId bs of
                Just b -> pure $ toJBlock b
                Nothing -> Left $ ErrGetBlockNotFound blockId
        lift . logLineP $ "getBlock " <> show blockId
            <> returns (fmap (showBlock . fromJBlock) block)
        lift applyOps
        except block

    , getDescendantIds = \parentId count -> do
        ch <- lift $ gets (nodeChainIds . node)
        let res = fmap (take $ fromIntegral count) $ if parentId == Hash "genesis"
                then pure (reverse ch)
                else if parentId `elem` ch then
                    pure $ reverse (takeWhile (/= parentId) ch)
                else
                    Left $ ErrGetDescendantsParentNotFound parentId
        lift . logLineP $ "getDescendentIds " <> show parentId <> " " <> show count
            <> returns (show res)
        lift applyOps
        except res

    , getInitialBlockchainParameters = \blockId -> do
        Quantity k <- lift $ gets mockNodeK
        let block0 = MockBlock (coerce blockId) (Hash "") (SlotId 0 0) 0
        pure (toJBlock block0, BlockchainParameters
            { getGenesisBlockHash = blockId
            , getGenesisBlockDate = error "mock bp"
            , getFeePolicy = error "mock bp"
            , getSlotLength = error "mock bp"
            , getEpochLength = error "mock bp"
            , getTxMaxSize = error "mock bp"
            , getEpochStability = Quantity (fromIntegral k)
            })

    , postMessage = \_ -> error "mock postMessage"
    , getStakeDistribution = error "mock getStakeDistribution"
    }
  where
    returns :: Show a => a -> String
    returns a = "\n    ↳  " <> show a

    applyOps :: StateT S m ()
    applyOps = do
        s <- gets operations
        case s of
            [] -> return ()
            ops:rest -> do
                mapM_ (logLineN . show) ops
                modify' (\(S n _ k logs) -> S (applyNodeOps ops n) rest k logs)

    logLineP msg = logLine $ "[PRODUCER] " <> msg
    logLineN msg = logLine $ "[NETWORK ] " <> msg

-------------------------------------------------------------------------
-- Model node

-- | State of the mock node which is used by 'mockJormungandrClient'.
--
-- Jormungandr is a concurrent process to the wallet. Its state can change in
-- arbitrary ways between any REST API call.
data S = S
    { node :: Node
    -- ^ Node's current state.
    , operations :: [[NodeOp]]
    -- ^ Future mutations to apply to the node.
    -- They are consumed and applied in batches after each REST API call.
    , mockNodeK :: Quantity "block" Word32
    -- ^ Maximum number of unstable blocks.
    , logs :: [String]
    -- ^ Logs from the test execution
    } deriving (Show, Eq)

-- | Models the jormungandr node state.
data Node = N
    { nodeDb :: Map (Hash "BlockHeader") MockBlock
    -- ^ Blocks indexed by id.
    , nodeChainIds :: [Hash "BlockHeader"]
    -- ^ List of ids for the chain, newest first.
    , nodeNextBlockId :: Int
    -- ^ Counter which provides nice small IDs when forging blocks.
    } deriving (Show, Eq)

emptyNode :: Node
emptyNode = N mempty mempty 0

-- | Gets the mock node's chain as a list of blocks starting from genesis.
getNodeChain :: Node -> [MockBlock]
getNodeChain (N db ch _) = reverse $ catMaybes [Map.lookup b db | b <- ch]

getNodeTip :: Node -> Maybe MockBlock
getNodeTip (N db ch _) = headMay ch >>= flip Map.lookup db

nodeFromChain :: [MockBlock] -> Node
nodeFromChain bs = nodeAddBlocks bs emptyNode

-- | Mutation of the node state.
data NodeOp
    = NodeAddBlocks [MockBlock]
    | NodeRewind Int
    | NodeGarbageCollect [Hash "BlockHeader"]
    deriving (Show, Eq)

applyNodeOp :: NodeOp -> Node -> Node
applyNodeOp (NodeAddBlocks bs) = nodeAddBlocks bs
applyNodeOp (NodeRewind i) = nodeRewind i
applyNodeOp (NodeGarbageCollect hs) = nodeGarbageCollect hs

-- | apply one batch of operations
applyNodeOps :: [NodeOp] -> Node -> Node
applyNodeOps ops n = foldl' (flip applyNodeOp) n ops

-- | Add blocks to the node chain.
nodeAddBlocks :: [MockBlock] -> Node -> Node
nodeAddBlocks bs n = N db chain nextBlockId
  where
    chain = reverse (map mockBlockId bs) ++ nodeChainIds n
    db = nodeDb n <> Map.fromList [(mockBlockId b, b) | b <- bs]
    nextBlockId = nodeNextBlockId n + length bs

-- | Roll back, or remove some blocks from the node chain. They are not removed
-- from the DB yet (see 'nodeGarbageCollect').
nodeRewind :: Int -> Node -> Node
nodeRewind i n =
    n { nodeChainIds = drop i (nodeChainIds n) }

-- | "Garbage collect" models the potential that getting rolled back blocks will
-- still succeed after the chain is switched, but that it will stop succeeding
-- at some unknown point in the future.
nodeGarbageCollect :: [Hash "BlockHeader"] -> Node -> Node
nodeGarbageCollect hs (N bs c nid) = N bs' c nid
    where bs' = foldr Map.delete bs hs

-- | Take an existing operation and, tweak it a bit in order to make it a valid
-- operation of the same nature for the given, such that applying it still
-- preserves the invariant we try to maintain:
--
-- - NodeAddBlocks still make for a valid contiguous chain
-- - NodeRewind does not rewind for more than the chain length
-- - NodeGarbageCollect actually collects unused ids
shiftOp :: Node -> NodeOp -> (Node, NodeOp)
shiftOp n = (\op -> (applyNodeOp op n, op)) . \case
    NodeAddBlocks bs ->
        NodeAddBlocks $ genBlocksWith n (repeat False) (length bs)
    NodeRewind rw ->
        NodeRewind $ min rw (length $ nodeChainIds n)
    NodeGarbageCollect ids ->
        let (ch, db) = (nodeChainIds n, nodeDb n) in
        NodeGarbageCollect $ (ids \\ ch) `intersect` (Map.keys db)

----------------------------------------------------------------------------
-- Mock block

-- | A block which can be easily generated, shown, and used to assert
-- properties.
data MockBlock = MockBlock
    { mockBlockId :: Hash "BlockHeader"
    , mockBlockPrev :: Hash "BlockHeader"
    , mockBlockSlot :: SlotId
    , mockBlockContent :: Int -- fixme: remove
    } deriving (Show, Eq)

-- | Stuffs a mock block into a real block.
toJBlock :: MockBlock -> J.Block
toJBlock (MockBlock bid prev sl content) = J.Block hdr []
    where hdr = J.BlockHeader 0 0 sl (fromIntegral content) (coerce bid) prev Nothing

-- | Extract a mock block out of a real block.
fromJBlock :: J.Block -> MockBlock
fromJBlock (J.Block (J.BlockHeader _ _ sl content bid prev _) _) =
    MockBlock (coerce bid) prev sl (fromIntegral content)

----------------------------------------------------------------------------
-- Checks that generated mock node test cases are valid

prop_generator :: S -> Property
prop_generator (S n0 ops _ _) = continuous .&&. uniqueIds
  where
    continuous = conjoin (follow <$> scanl (flip applyNodeOp) n0 (concat ops))

    follow :: Node -> Property
    follow (N db chain _) = case catMaybes [Map.lookup b db | b <- chain] of
        [] ->
            property True
        [b] ->
            mockBlockPrev b === Hash "genesis"
        (p:b:_) ->
            counterexample ("Chain: " <> showBlock p <> " ==> " <> showBlock b)
            $ conjoin
                [ mockBlockPrev p === mockBlockId b
                , follow $ N db (drop 1 chain) 0
                ]

    uniqueIds = counterexample ("Non-unique ID: " ++ show bids) $
        length (nub bids) === length bids
      where
        bids = concat [map mockBlockId bs | NodeAddBlocks bs <- concat ops]

----------------------------------------------------------------------------
-- Generation of mock node test cases.
--

-- | Remove non-effectful NodeOps from a list which has been shrunk.
removeNoOp :: [NodeOp] -> [NodeOp]
removeNoOp = filter isUseful
  where
    isUseful (NodeAddBlocks []) = False
    isUseful (NodeRewind 0) = False
    isUseful (NodeGarbageCollect []) = False
    isUseful _ = True

genBlocksWith :: Node -> [Bool] -> Int -> [MockBlock]
genBlocksWith n empty count =
    let
        tip = getNodeTip n
        tipSlot = maybe (-1) (fromIntegral . slotNumber . mockBlockSlot) tip
        chainLength = length $ nodeChainIds n
        slots =
            [ SlotId 0 (fromIntegral $ tipSlot + i)
            | (i, gap) <- zip [1..count] empty, tipSlot + i == 0 || not gap
            ]
        contents = [nodeNextBlockId n..]
        bids = mockBlockHash <$> contents
        prevs = maybe (Hash "genesis") mockBlockId tip : bids
    in
        [ MockBlock bid prev slot content
        | (bid, prev, slot, content) <- zip4 bids prevs slots contents
        ]

instance Arbitrary S where
    arbitrary = do
        let node = emptyNode
        mockNodeK@(Quantity k) <- arbitrary
        chainLength <- choose (0, 2 * fromIntegral k)
        operations <- genNodeOps mockNodeK chainLength node
        let logs = []
        pure $ S{..}
      where
        -- Repeatedly generate operations then update state.
        -- The state is used so that only valid operations are generated.
        -- Stops generating ops once a target chain length is reached.
        genNodeOps :: Quantity "block" Word32 -> Int -> Node -> Gen [[NodeOp]]
        genNodeOps k chainLength initialNode = go initialNode []
          where
            go n ac = do
                op <- genNodeOp k n
                let n' = applyNodeOps op n
                if length (nodeChainIds n') > chainLength
                    then pure $ reverse (op:ac)
                    else go n' (op:ac)

        -- Given a node state generate a valid mutation.
        genNodeOp :: Quantity "block" Word32 -> Node -> Gen [NodeOp]
        genNodeOp (Quantity k) n = removeNoOp <$> frequency
                [ (50, pure [])
                , (35, pure . NodeAddBlocks <$> genBlocks n)
                , (10, genSwitchChain (fromIntegral k) n)
                , (5, pure . NodeGarbageCollect <$> genGC n)
                ]

        -- Generate a new contiguous batch of blocks
        genBlocks :: Node -> Gen [MockBlock]
        genBlocks n = do
            count <- sized $ \s -> choose (1, s)
            gaps <- genGaps count
            pure $ genBlocksWith n gaps count

        -- Switching chain is rewinding then adopting the blocks from a fork.
        genSwitchChain :: Int -> Node -> Gen [NodeOp]
        genSwitchChain k n = do
            rewind <- genRewind (fromIntegral k) n
            gaps <- genGaps rewind
            let bs = genBlocksWith (nodeRewind rewind n) gaps rewind
            pure [NodeRewind rewind, NodeAddBlocks bs]

        -- Rewinds are usually small to allow the node to make progress, so that
        -- the test can stop. Sometimes the full k is rolled back.
        genRewind :: Int -> Node -> Gen Int
        genRewind k (N _ ch _) = frequency
            [ (80, choose (1, min k 3))
            , (15, choose (1, (min k (length ch `div` 3))))
            , (5, min (length ch) <$> choose ((k - 1), k))
            ]

        genGC :: Node -> Gen [Hash "BlockHeader"]
        genGC (N db ch _) = sublistOf (Map.keys db \\ ch)

        genGaps :: Int -> Gen [Bool]
        genGaps count = do
            vectorOf count $ frequency [(1, pure True), (4, pure False)]

    shrink (S n ops k logs) = nub
        [ S n (filter (not . null) (removeNoOp <$> ops')) k logs
        | oops <- shrinkList (shrinkList shrinkNodeOp) ops
        , let ops' = runIdentity $ evalStateT (tweak oops) n
        ]
      where
        shrinkNodeOp :: NodeOp -> [NodeOp]
        shrinkNodeOp = \case
            NodeAddBlocks bs -> NodeAddBlocks
                <$> shrinkList shrinkNothing bs
            NodeRewind rw -> NodeRewind
                <$> shrink rw
            NodeGarbageCollect ids -> NodeGarbageCollect
                <$> shrinkList shrinkNothing ids

        tweak :: [[NodeOp]] -> StateT Node Identity [[NodeOp]]
        tweak = mapM $ mapM $ \op -> do
            node <- get
            let (node', op') = shiftOp node op
            put node' $> op'

instance Arbitrary (Quantity "block" Word32) where
    -- k doesn't need to be large for testing this
    arbitrary =
        Quantity . fromIntegral <$> choose (2 :: Int, 20)
    shrink (Quantity k) =
        [ Quantity (fromIntegral k')
        | k' <- shrink (fromIntegral k :: Int)
        , k' >= 2
        ]

mockBlockHash :: Int -> Hash "BlockHeader"
mockBlockHash num = Hash $ fmt $ padLeftF 4 '0' $ hexF num

showHash :: Hash a -> String
showHash (Hash h) = B8.unpack h
