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

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    , S
    ) where

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
    ( StateT (..), get, modify', put, runStateT )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.List
    ( zip4, (\\) )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Safe
    ( headMay, lastMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , choose
    , counterexample
    , frequency
    , property
    , sublistOf
    , vectorOf
    , withMaxSuccess
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
        it "Always generate valid chains" $ property $ \(S (N db chain) _ _ _) ->
            let
                follow :: [Hash "BlockHeader"] -> Bool
                follow [] = True
                follow [b] = b == Hash "genesis"
                follow (h:p:q) = case (Map.lookup h db, Map.lookup p db) of
                    (Just blk, Just parent) ->
                        (mockBlockId parent == mockBlockPrev blk) && follow q
                    _  -> False
            in
                follow chain

    describe "Chain sync" $ do
        it "Syncs with mock node"
            $ withMaxSuccess 1000
            $ property prop_sync

{-------------------------------------------------------------------------------
                Syncing of network layer in presence of rollback
                          and concurrent node updates.
-------------------------------------------------------------------------------}

prop_sync :: S -> Property
prop_sync s0 = monadicIO $ do
    let logLineN msg = logLine $ "[PRODUCER] " <> msg
    let logLineC msg = logLine $ "[CONSUMER] " <> msg

    (consumer, s) <- run $ flip runStateT s0 $ do
        -- Set up network layer with mock Jormungandr
        nl <- mockNetworkLayer logLineN
        -- Run a model chain consumer on the mock network layer.
        let initialConsumer = C [] (initCursor nl block0H) 10
        consumerRestoreStep logLineC nl initialConsumer

    -- Consumer chain should (eventually) be in sync with node chain.
    monitor $ counterexample $ unlines
        [ "Applied blocks: " <> showChain (consumerApplied consumer)
        , "Node chain:     " <> showChain (getNodeChain (node s))
        , "Logs:         \n" <> unlines (reverse (logs s))
        ]
    assert (consumerApplied consumer == getNodeChain (node s))
  where
    logLine msg = modify' (\(S a0 a1 a2 logs) -> S a0 a1 a2 (msg:logs))

showChain :: [MockBlock] -> String
showChain [] = "∅"
showChain chain = unwords . map (showHash . mockBlockId) $ chain

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

-- | Test Genesis block
block0H :: BlockHeader
block0H = BlockHeader (SlotId 0 0) (Quantity 0) (Hash "genesis")

----------------------------------------------------------------------------
-- Model consumer

-- | Model of consumer state.
data Consumer = C
    { consumerApplied :: [MockBlock]
    -- ^ Blocks which have been received.
    , _consumerCursor :: Cursor (Jormungandr 'Testnet)
    -- ^ Consumer state -- the cursor given by network layer.
    , _consumerStepsRemaining :: Int
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
            Left e ->
                logLine ("Failed to get next blocks: " ++ show e) $> c
            Right AwaitReply -> do
                logLine "AwaitReply"
                consumerRestoreStep logLine nw (C bs cur (min i 3))
            Right (RollForward cur' _ bs') -> do
                logLine $ "RollForward: " <> unwords (showBlock <$> bs')
                consumerRestoreStep logLine nw (C (bs ++ bs') cur' i)
            Right (RollBackward cur') -> do
                logLine "RollBackward"
                let sl = cursorSlotId nw cur'
                let bs' = takeWhile (\b -> mockBlockSlot b <= sl) bs
                consumerRestoreStep logLine nw (C bs' cur' i)
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
        ch <- nodeChainIds <$> lift getNodeState
        let tip = fromMaybe (Hash "genesis") $ headMay ch
        lift . logLine $ "getTipId" <> returns tip
        lift applyOp $> tip

    , getBlock = \blockId -> do
        bs <- nodeDb <$> lift getNodeState
        let block = if blockId == Hash "genesis"
                then pure $ toJBlock $ MockBlock blockId (Hash "") (SlotId 0 0) 0
                else case Map.lookup blockId bs of
                    Just b -> pure $ toJBlock b
                    Nothing -> Left $ ErrGetBlockNotFound blockId
        lift . logLine $ "getBlock " <> show blockId
            <> returns (fmap (showBlock . fromJBlock) block)
        lift applyOp *> except block

    , getDescendantIds = \parentId count -> do
        ch <- nodeChainIds <$> lift getNodeState
        let res = fmap (parentId:) $ case takeWhile (/= parentId) (reverse ch) of
                [] -> Left $ ErrGetDescendantsParentNotFound parentId
                ds -> pure $ take (fromIntegral count) ds
        lift . logLine $ "getDescendentIds " <> show parentId <> " " <> show count
            <> returns (show res)
        lift applyOp *>  except res

    , getInitialBlockchainParameters = \blockId -> do
        Quantity k <- mockNodeK <$> lift get
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
    getNodeState = node <$> get

    returns :: Show a => a -> String
    returns a = "\n    ↳  " <> show a

    applyOp :: StateT S m ()
    applyOp = do
        s <- get
        case s of
            S _ [] _ _ ->
                return ()
            S n (op:ops) k logs -> do
                logLine (show op)
                put (S (applyNodeOp op n) ops k logs)

----------------------------------------------------------------------------
-- Model node

-- | State of the mock node which is used by 'mockJormungandrClient'.
--
-- Jormungandr is a concurrent process to the wallet. Its state can change in
-- arbitrary ways between any REST API call.
data S = S
    { node :: Node
    -- ^ Node's current state.
    , operations :: [NodeOp]
    -- ^ Future mutations to apply to the node.
    -- They are consumed and applied in after each REST API call.
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
    } deriving (Show, Eq)

-- | Gets the mock node's chain as a list of blocks starting from genesis.
getNodeChain :: Node -> [MockBlock]
getNodeChain (N db ch) = reverse $ catMaybes [Map.lookup b db | b <- ch]

getNodeTip :: Node -> Maybe MockBlock
getNodeTip (N db ch) = lastMay ch >>= flip Map.lookup db

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

-- | Add blocks to the node chain.
nodeAddBlocks :: [MockBlock] -> Node -> Node
nodeAddBlocks bs n = N db' chain'
  where
    chain' = reverse (map mockBlockId bs) ++ nodeChainIds n
    db' = nodeDb n <> Map.fromList [(mockBlockId b, b) | b <- bs]

-- | Roll back, or remove some blocks from the node chain. They are not removed
-- from the DB yet (see 'nodeGarbageCollect').
nodeRewind :: Int -> Node -> Node
nodeRewind i n =
    n { nodeChainIds = take (length (nodeChainIds n) - i) (nodeChainIds n) }

-- | "Garbage collect" models the potential that getting rolled back blocks will
-- still succeed after the chain is switched, but that it will stop succeeding
-- at some unknown point in the future.
nodeGarbageCollect :: [Hash "BlockHeader"] -> Node -> Node
nodeGarbageCollect hs (N bs c) = N bs' c
    where bs' = foldr Map.delete bs hs

----------------------------------------------------------------------------
-- Mock block

-- | A block which can be easily generated, shown, and used to assert
-- properties.
data MockBlock = MockBlock
    { mockBlockId :: Hash "BlockHeader"
    , mockBlockPrev :: Hash "BlockHeader"
    , mockBlockSlot :: SlotId
    , mockBlockContent :: Int
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
-- Generation of mock node test cases.

instance Arbitrary S where
    arbitrary = do
        NonNegative count <- arbitrary
        let node = N mempty mempty
        mockNodeK <- arbitrary
        operations <- genNodeOps mockNodeK count node []
        let logs = []
        pure $ S{..}
      where
        -- Repeatedly generate operations then update state.
        -- The state is used so that only valid operations are generated.
        genNodeOps :: Quantity "block" Word32 -> Int -> Node -> [NodeOp] -> Gen [NodeOp]
        genNodeOps _ 0 _ ac = pure $ reverse ac
        genNodeOps k count n ac = do
            op <- genNodeOp k n
            genNodeOps k (count - 1) (applyNodeOp op n) (op:ac)

        -- Given a node state generate a valid mutation.
        -- fixme: use 'sized' to scale rollback, etc.
        genNodeOp :: Quantity "block" Word32 -> Node -> Gen NodeOp
        genNodeOp (Quantity k) n@(N _ ch) = frequency
                [ (10, NodeAddBlocks <$> genBlocks n)
                , (3, NodeRewind <$> choose (1, (min (fromIntegral k) (length ch))))
                , (1, NodeGarbageCollect <$> genGC n)
                ]

        genBlocks :: Node -> Gen [MockBlock]
        genBlocks n = do
            count <- choose (1, 3) -- fixme: getSize would be better
            let genEmpty = frequency [(1, pure True), (4, pure False)]
            empty <- vectorOf count genEmpty
            let tip = getNodeTip n
            let tipSlot = maybe 0 (fromIntegral . slotNumber . mockBlockSlot) tip
            let chainLength = length $ nodeChainIds n
            let slots =
                    [ SlotId 0 (fromIntegral $ tipSlot + i)
                    | (i, gap) <- zip [1..count] empty, not gap
                    ]
            let contents = take count [(1+chainLength)..]
            let bids = mockBlockHash <$> contents
            let prevs = maybe (Hash "genesis") mockBlockId tip : bids
            pure
                [ MockBlock bid prev slot content
                | (bid, prev, slot, content) <- zip4 bids prevs slots contents
                ]


        genGC :: Node -> Gen [Hash "BlockHeader"]
        genGC (N db ch) = sublistOf (Map.keys db \\ ch)

    shrink (S n ops k _) =
        [ S n (take s ops) k [] | s <- shrink (length ops), s > 0]

instance Arbitrary (Quantity "block" Word32) where
    -- k doesn't need to be large for testing this
    arbitrary =
        Quantity . fromIntegral <$> choose (2 :: Int, 20)
    shrink (Quantity k) =
        [ Quantity (fromIntegral k')
        | k' <- shrink (fromIntegral k :: Int)
        , k' >= 2
        ]

instance Arbitrary MockBlock where
    arbitrary = pure $ MockBlock (Hash "") (Hash "") (SlotId 0 0) 0

instance Arbitrary (Hash "BlockHeader")  where
    arbitrary = mockBlockHash . getNonNegative <$> arbitrary

mockBlockHash :: Int -> Hash "BlockHeader"
mockBlockHash num = Hash. B8.pack $ "block" ++ show num

showHash :: Hash a -> String
showHash (Hash h) = B8.unpack h
