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
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List
    ( foldl', intersect, nub, zip3, (\\) )
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
    , classify
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
    ( assert, monadicIO, monitor, pick, run )

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
        it "Regression #1" $
            withMaxSuccess 1 $ prop_sync regression1 0

        it "Regression #2" $
            withMaxSuccess 1 $ prop_sync regression2 0

        it "Regression #3" $
            withMaxSuccess 1 $ prop_sync regression3 0

        it "Regression #4" $
            withMaxSuccess 1 $ prop_sync regression4 0

        it "Regression #5" $
            withMaxSuccess 1 $ prop_sync regression5 0

        it "Syncs with mock node" $
            withMaxSuccess 10000 prop_sync

{-------------------------------------------------------------------------------
                Syncing of network layer in presence of rollback
                          and concurrent node updates.
-------------------------------------------------------------------------------}

prop_sync :: S -> Int -> Property
prop_sync s0 nCps = monadicIO $ do
    let logLineC msg = logLine $ "[CONSUMER] " <> msg

    -- Run a model chain consumer on the mock network layer.
    (c0Chain, c0Cps) <- pick $ genConsumer s0 nCps
    (consumer, s) <- run $ flip runStateT s0 $ do
        -- Set up network layer with mock Jormungandr
        nl <- mockNetworkLayer logLine
        consumerRestoreStep logLineC nl (C c0Chain (initCursor nl c0Cps)) Nothing

    let nodeChain = getNodeChain (node s)
    monitor $ counterexample $ unlines
        [ "Initial consumer chain: " <> showChain c0Chain
        , "Initial consumer cps:   " <> showCheckpoints c0Cps
        , "Initial node chain:     " <> showChain (getNodeChain $ node s0)
        , "Applied blocks:         " <> showChain (consumerApplied consumer)
        , "Node chain:             " <> showChain nodeChain
        , "k =                     " <> show (mockNodeK s0)
        , "Logs:                 \n" <> unlines (reverse (logs s))
        ]
    monitor (classify (initialChainLength (const ( == 0))) "started with an empty chain")
    monitor (classify (initialChainLength (\k -> (> k))) "started with more than k blocks")
    monitor (classify addMoreThanK "advanced more than k blocks")
    monitor (classify rollbackK "rolled back full k")
    monitor (classify (switchChain (<)) "switched to a longer chain")
    monitor (classify (switchChain (>)) "switched to a shorter chain")
    monitor (classify (switchChain (const (== 0))) "rewinded without switch")
    monitor (classify (recoveredFromGenesis s) "recovered from genesis")
    monitor (classify (startedFromScratch c0Cps) "started from scratch")

    -- Consumer chain should (eventually) be in sync with node chain.
    assert (consumerApplied consumer == nodeChain || null nodeChain)
  where
    logLine msg = modify' (\(S a0 a1 a2 logs) -> S a0 a1 a2 (msg:logs))

    -- Build some predicate between the value of `k` and the initial chain length
    initialChainLength :: (Int -> Int -> Bool) -> Bool
    initialChainLength comp =
        let (Quantity k) = mockNodeK s0
        in node s0
            & nodeChainIds
            & length
            & comp (fromIntegral k)

    addMoreThanK :: Bool
    addMoreThanK =
        let (Quantity k) = mockNodeK s0
        in operations s0
            & filter (all isNodeAddBlocks)
            & map (concatMap getAddBlocks)
            & filter ((>= fromIntegral k) . length)
            & not . null

    rollbackK :: Bool
    rollbackK =
        let (Quantity k) = mockNodeK s0
        in operations s0
            & mconcat
            & filter (isNodeRewindOf $ Just $ fromIntegral k)
            & not . null

    switchChain :: (Int -> Int -> Bool) -> Bool
    switchChain comp = operations s0
        & filter (not . null)
        & filter (isNodeRewindOf Nothing . head)
        & any (\((NodeRewind n):q) -> n `comp` length (concatMap getAddBlocks q))

    recoveredFromGenesis :: S -> Bool
    recoveredFromGenesis s =
        "[CONSUMER] Recover" `elem` logs s

    startedFromScratch :: [BlockHeader] -> Bool
    startedFromScratch = null

showChain :: [MockBlock] -> String
showChain [] = "∅"
showChain chain = unwords . map showSlot . addGaps mockBlockSlot $ chain
  where
    showSlot = maybe "_" (showHash . mockBlockId)

showBlock :: MockBlock -> String
showBlock (MockBlock ownId parentId sl) = mconcat $
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

showCheckpoints :: [BlockHeader] -> String
showCheckpoints [] = "∅"
showCheckpoints chain = unwords . map showHeader . addGaps slotId $ chain
  where
    showHeader = maybe "_" (showHash . headerHash)

addGaps :: (a -> SlotId) -> [a] -> [Maybe a]
addGaps _ []  = []
addGaps _ [b] = [Just b]
addGaps getSlot (b1:b2:bs) =
    [Just b1] ++ replicate gap Nothing ++ addGaps getSlot (b2:bs)
  where
    gap = slotNum b2 - slotNum b1 - 1
    slotNum = fromIntegral . slotNumber . getSlot

-- | Test Genesis block
block0 :: J.Block
block0 = toJBlock $ MockBlock genesisHash parentGenesisHash (SlotId 0 0)

-- | Test Genesis block
block0H :: BlockHeader
block0H = BlockHeader (SlotId 0 0) (Quantity 0) genesisHash parentGenesisHash

genesisHash :: Hash a
genesisHash = Hash "genesis"

parentGenesisHash :: Hash a
parentGenesisHash = Hash "void"

----------------------------------------------------------------------------
-- Model consumer

-- | Model of consumer state.
data Consumer = C
    { consumerApplied :: [MockBlock]
    -- ^ Blocks which have been received.
    --
    , consumerCursor :: Cursor (Jormungandr 'Testnet)
    -- ^ Consumer state -- the cursor given by network layer.
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
    -> Maybe Int
    -- ^ A counter to limit the number of steps. Only set to `Just` when there's
    -- no more operations to apply in the node and we expect the consumer to
    -- eventually be in sync.
    -> StateT S m Consumer
consumerRestoreStep _ _ c (Just 0) = pure c
consumerRestoreStep logLine nw (C bs cur) mLimit = do
    -- We apply blocks by batch of `k`, so, when the node stabilizes, we should
    -- finish in less than the chain length divided by k steps.
    S n ops (Quantity k) _ <- get
    let limit = case mLimit of
            Just lim -> Just (lim - 1)
            Nothing -> if null ops
                then Just (2 + length (nodeChainIds n) `div` fromIntegral k)
                else Nothing
    logLine $ "nextBlocks " <> show (cursorSlotId nw cur)
    runExceptT (nextBlocks nw cur) >>= \case
        Left e -> do
            logLine ("Failed to get next blocks: " ++ show e)
            consumerRestoreStep logLine nw (C bs cur) limit
        Right AwaitReply -> do
            logLine "AwaitReply"
            consumerRestoreStep logLine nw (C bs cur) limit
        Right (RollForward cur' _ bs') -> do
            logLine $ "RollForward " <> unwords (showBlock <$> bs')
            consumerRestoreStep logLine nw (C (bs ++ bs') cur') limit
        Right (RollBackward cur') -> do
            logLine $ "RollBackward " <> show (cursorSlotId nw cur')
            let sl = cursorSlotId nw cur'
            let bs' = takeWhile (\b -> mockBlockSlot b <= sl) bs
            consumerRestoreStep logLine nw (C bs' cur') limit
        Right Recover -> do
            logLine "Recover"
            let cur0 = initCursor nw []
            consumerRestoreStep logLine nw (C [] cur0) limit

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
    Right g0 <- runExceptT $ getInitialBlockchainParameters jm genesisHash
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
        let tip = fromMaybe genesisHash $ headMay ch
        lift . logLineP $ "getTipId" <> returns tip
        lift applyOps
        pure tip

    , getBlock = \blockId -> do
        bs <- lift $ gets (nodeDb . node)
        let block = case (blockId, Map.lookup blockId bs) of
                (Hash "genesis", _) -> pure block0
                (_, Just b) -> pure $ toJBlock b
                (_, Nothing) -> Left $ ErrGetBlockNotFound blockId
        lift . logLineP $ "getBlock " <> show blockId
            <> returns (fmap (showBlock . fromJBlock) block)
        lift applyOps
        except block

    , getDescendantIds = \parentId count -> do
        ch <- lift $ gets (nodeChainIds . node)
        let res = fmap (take $ fromIntegral count) $ if parentId == headerHash block0H
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
        pure (block0, BlockchainParameters
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
    = NodeAddBlocks { getAddBlocks :: [MockBlock] }
    | NodeRewind Int
    | NodeGarbageCollect [Hash "BlockHeader"]
    deriving (Show, Eq)

isNodeAddBlocks :: NodeOp -> Bool
isNodeAddBlocks = \case
    NodeAddBlocks{} -> True
    _ -> False

isNodeRewindOf :: Maybe Int -> NodeOp -> Bool
isNodeRewindOf k = \case
    NodeRewind k' -> maybe True (== k') k
    _ -> False

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
    NodeRewind rw
        | rw < chainLength -> NodeRewind rw
        | otherwise -> NodeRewind 0
    NodeGarbageCollect ids ->
        let (ch, db) = (nodeChainIds n, nodeDb n) in
        NodeGarbageCollect $ (ids \\ ch) `intersect` (Map.keys db)
  where
    chainLength = length (nodeChainIds n)

----------------------------------------------------------------------------
-- Mock block

-- | A block which can be easily generated, shown, and used to assert
-- properties.
data MockBlock = MockBlock
    { mockBlockId :: Hash "BlockHeader"
    , mockBlockPrev :: Hash "BlockHeader"
    , mockBlockSlot :: SlotId
    } deriving (Show, Eq)

-- | Stuffs a mock block into a real block.
toJBlock :: MockBlock -> J.Block
toJBlock (MockBlock bid prev sl) = J.Block hdr []
    where hdr = J.BlockHeader 0 0 sl 0 (Hash "") (coerce bid) prev Nothing

-- | Extract a mock block out of a real block.
fromJBlock :: J.Block -> MockBlock
fromJBlock (J.Block (J.BlockHeader _ _ sl _ _ bid prev _) _) =
    MockBlock (coerce bid) prev sl

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
            mockBlockPrev b === genesisHash
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

instance Arbitrary S where
    arbitrary = do
        mockNodeK@(Quantity k) <- arbitrary
        initialChainLength <- frequency
            [ (50, pure 0)
            , (50, choose (0, 2 * fromIntegral k))
            ]
        targetChainLength <- (+ initialChainLength) <$> choose (0, 2 * fromIntegral k)
        bootstrap <- genNodeOps mockNodeK initialChainLength emptyNode
        let node = applyNodeOps (mconcat bootstrap) emptyNode
        operations <- genNodeOps mockNodeK targetChainLength node
        let logs = []
        pure $ S{..}

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

-- Repeatedly generate operations then update state.
-- The state is used so that only valid operations are generated.
-- Stops generating ops once a target chain length is reached.
genNodeOps :: Quantity "block" Word32 -> Int -> Node -> Gen [[NodeOp]]
genNodeOps _ 0 _ = pure mempty
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

genBlocksWith :: Node -> [Bool] -> Int -> [MockBlock]
genBlocksWith n empty count =
    let
        tip = getNodeTip n
        tipSlot = maybe (-1) (fromIntegral . slotNumber . mockBlockSlot) tip
        slots =
            [ SlotId 0 (fromIntegral $ tipSlot + i)
            | (i, gap) <- zip [1..count] empty, tipSlot + i == 0 || not gap
            ]
        bids = map mockBlockHash [nodeNextBlockId n..]
        prevs = maybe genesisHash mockBlockId tip : bids
    in
        [ MockBlock bid prev slot
        | (bid, prev, slot) <- zip3 bids prevs slots
        ]

-- Switching chain is rewinding then adopting the blocks from a fork.
genSwitchChain :: Int -> Node -> Gen [NodeOp]
genSwitchChain k n = do
    rewind <- genRewind (fromIntegral k) n
    gaps <- genGaps (2 * rewind)
    chainLength <- choose (0, 2*rewind)
    let bs = genBlocksWith (nodeRewind rewind n) gaps chainLength
    pure [NodeRewind rewind, NodeAddBlocks bs]

-- Rewinds are usually small to allow the node to make progress, so that
-- the test can stop. Sometimes the full k is rolled back.
genRewind :: Int -> Node -> Gen Int
genRewind k (N _ ch _) = frequency
    [ (80, choose (1, min k 3))
    , (15, choose (1, (min k (rMax `div` 3))))
    , (5, min rMax <$> choose ((k - 1), k))
    ]
  where
    rMax = length ch - 1

genGC :: Node -> Gen [Hash "BlockHeader"]
genGC (N db ch _) = sublistOf (Map.keys db \\ ch)

genGaps :: Int -> Gen [Bool]
genGaps count = do
    vectorOf count $ frequency [(1, pure True), (4, pure False)]

genConsumer :: S -> Int -> Gen ([MockBlock], [BlockHeader])
genConsumer s nCps = do
    n <- choose (0, max 0 nCps)
    let chain = take n (getNodeChain (node s))
    cps <- genCheckpoints chain
    pure (chain, cps)

genCheckpoints :: [MockBlock] -> Gen [BlockHeader]
genCheckpoints blks = do
    gaps <- (\gaps -> gaps ++ [True]) <$> genGaps (length blks - 1)
    pure [ mkBlockHeader b | (b, keep) <- zip blks gaps, keep ]
  where
    mkBlockHeader :: MockBlock -> BlockHeader
    mkBlockHeader (MockBlock hh ph sl) =
        BlockHeader sl (Quantity 0) hh ph

mockBlockHash :: Int -> Hash "BlockHeader"
mockBlockHash num = Hash $ fmt $ padLeftF 4 '0' $ hexF num

showHash :: Hash a -> String
showHash (Hash h) = B8.unpack h

--------------------------------------------------------------------------------
-- Interesting Cases Seen During Development
--

regression1 :: S
regression1 = S
    { node = N
        { nodeDb = Map.fromList []
        , nodeChainIds = []
        , nodeNextBlockId = 0
        }
    ,  mockNodeK = Quantity {getQuantity = 15}
    , logs = []
    , operations =
        [ []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}}]]
        , []
        , []
        , []
        , [NodeRewind 3, NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}},MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}},MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}}]]
        , []
        , []
        , [NodeGarbageCollect [Hash {getHash = "0000"}]]
        , []
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}},MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0004"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}},MockBlock {mockBlockId = Hash {getHash = "0006"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0007"}, mockBlockPrev = Hash {getHash = "0006"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}}]]
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0008"}, mockBlockPrev = Hash {getHash = "0007"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}}]]
        , [NodeGarbageCollect [Hash {getHash = "0001"},Hash {getHash = "0002"},Hash {getHash = "0003"}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0009"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]]
        , []
        , []
        , []
        , []
        , []
        , [NodeRewind 1,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000a"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000b"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000c"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}}]]
        , [NodeRewind 2,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000d"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}},MockBlock {mockBlockId = Hash {getHash = "000e"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}}]]
        , []
        , []
        , [NodeGarbageCollect [Hash {getHash = "000b"}]]
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000f"}, mockBlockPrev = Hash {getHash = "000e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}}]]
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0010"}, mockBlockPrev = Hash {getHash = "000f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}}]]
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0011"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}},MockBlock {mockBlockId = Hash {getHash = "0012"}, mockBlockPrev = Hash {getHash = "0011"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}},MockBlock {mockBlockId = Hash {getHash = "0013"}, mockBlockPrev = Hash {getHash = "0012"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}}]]
        , []
        , []
        , []
        , [NodeGarbageCollect [Hash {getHash = "0009"},Hash {getHash = "000c"},Hash {getHash = "000e"}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0014"}, mockBlockPrev = Hash {getHash = "0013"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 10}}]]
        , []
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0015"}, mockBlockPrev = Hash {getHash = "0011"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}},MockBlock {mockBlockId = Hash {getHash = "0016"}, mockBlockPrev = Hash {getHash = "0015"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}},MockBlock {mockBlockId = Hash {getHash = "0017"}, mockBlockPrev = Hash {getHash = "0016"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 10}}]]
        , []
        , []
        , [NodeGarbageCollect [Hash {getHash = "0010"}]]
        , [NodeGarbageCollect [Hash {getHash = "000f"},Hash {getHash = "0012"},Hash {getHash = "0013"},Hash {getHash = "0014"}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0018"}, mockBlockPrev = Hash {getHash = "0017"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 11}}]]
        , []
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0019"}, mockBlockPrev = Hash {getHash = "0015"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 9}},MockBlock {mockBlockId = Hash {getHash = "001a"}, mockBlockPrev = Hash {getHash = "0019"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 11}}]]
        , []
        , []
        , []
        , []
        , [NodeGarbageCollect [Hash {getHash = "0018"}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001b"}, mockBlockPrev = Hash {getHash = "001a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 12}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001c"}, mockBlockPrev = Hash {getHash = "001b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 13}}]]
        , [NodeGarbageCollect [Hash {getHash = "0016"},Hash {getHash = "0017"}]]
        , []
        , []
        , [NodeRewind 1,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001d"}, mockBlockPrev = Hash {getHash = "001b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 13}}]]
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001e"}, mockBlockPrev = Hash {getHash = "001d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 14}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "001f"}, mockBlockPrev = Hash {getHash = "001e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 15}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0020"}, mockBlockPrev = Hash {getHash = "001f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 16}}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0021"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}}]]
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0022"}, mockBlockPrev = Hash {getHash = "0021"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}}]]
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0023"}, mockBlockPrev = Hash {getHash = "0022"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0024"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}},MockBlock {mockBlockId = Hash {getHash = "0025"}, mockBlockPrev = Hash {getHash = "0024"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}},MockBlock {mockBlockId = Hash {getHash = "0026"}, mockBlockPrev = Hash {getHash = "0025"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}}]]
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0027"}, mockBlockPrev = Hash {getHash = "0026"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 20}}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeRewind 4,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0028"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}},MockBlock {mockBlockId = Hash {getHash = "0029"}, mockBlockPrev = Hash {getHash = "0028"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}},MockBlock {mockBlockId = Hash {getHash = "002a"}, mockBlockPrev = Hash {getHash = "0029"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}},MockBlock {mockBlockId = Hash {getHash = "002b"}, mockBlockPrev = Hash {getHash = "002a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 20}}]]
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002c"}, mockBlockPrev = Hash {getHash = "002b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 21}}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002d"}, mockBlockPrev = Hash {getHash = "002c"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 22}}]]
        , []
        , []
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002e"}, mockBlockPrev = Hash {getHash = "002d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 23}}]]
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "002f"}, mockBlockPrev = Hash {getHash = "002e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 24}}]]
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0030"}, mockBlockPrev = Hash {getHash = "002f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 25}}]]
        , []
        , []
        , []
        , []
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0031"}, mockBlockPrev = Hash {getHash = "0030"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 26}}]]
        ]
    }

regression2 :: S
regression2 = S
    { node = N
        { nodeDb = Map.fromList []
        , nodeChainIds = []
        , nodeNextBlockId = 0
        }
    , mockNodeK = Quantity {getQuantity = 15}
    , logs = []
    , operations =
        [ [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}}]]
        , [NodeRewind 1,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}}]]
        , [NodeGarbageCollect [Hash {getHash = "0000"}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}}]]
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}}]]
        ]
    }

regression3 :: S
regression3 = S
    { node = N
        { nodeDb = Map.fromList []
        , nodeChainIds = []
        , nodeNextBlockId = 0
        }
    , mockNodeK = Quantity {getQuantity = 16}
    , logs = []
    , operations =
        [ [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "0000"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}},MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}},MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0004"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0006"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0007"}, mockBlockPrev = Hash {getHash = "0006"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}}]]
        , [NodeRewind 4]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0008"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "0009"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]]
        , [NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000a"}, mockBlockPrev = Hash {getHash = "0009"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}}]]
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000b"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}},MockBlock {mockBlockId = Hash {getHash = "000c"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]]
        , [NodeRewind 3,NodeAddBlocks [MockBlock {mockBlockId = Hash {getHash = "000d"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}},MockBlock {mockBlockId = Hash {getHash = "000e"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}}]]
        ]
    }

regression4 :: S
regression4 = S
    { node = N
        { nodeDb = Map.fromList
            [ (Hash {getHash = "0000"},MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}})
            , (Hash {getHash = "0001"},MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "0000"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}})
            , (Hash {getHash = "0002"},MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "0001"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}})
            , (Hash {getHash = "0003"},MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "0002"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}})
            , (Hash {getHash = "0004"},MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 7}})
            , (Hash {getHash = "0005"},MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0004"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 8}})
            , (Hash {getHash = "0006"},MockBlock {mockBlockId = Hash {getHash = "0006"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 10}})
            , (Hash {getHash = "0007"},MockBlock {mockBlockId = Hash {getHash = "0007"}, mockBlockPrev = Hash {getHash = "0006"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 11}})
            , (Hash {getHash = "0008"},MockBlock {mockBlockId = Hash {getHash = "0008"}, mockBlockPrev = Hash {getHash = "0007"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 12}})
            , (Hash {getHash = "0009"},MockBlock {mockBlockId = Hash {getHash = "0009"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 13}})
            , (Hash {getHash = "000a"},MockBlock {mockBlockId = Hash {getHash = "000a"}, mockBlockPrev = Hash {getHash = "0009"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 15}})
            , (Hash {getHash = "000b"},MockBlock {mockBlockId = Hash {getHash = "000b"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 16}})
            , (Hash {getHash = "000c"},MockBlock {mockBlockId = Hash {getHash = "000c"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 17}})
            , (Hash {getHash = "000d"},MockBlock {mockBlockId = Hash {getHash = "000d"}, mockBlockPrev = Hash {getHash = "000c"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 18}})
            , (Hash {getHash = "000e"},MockBlock {mockBlockId = Hash {getHash = "000e"}, mockBlockPrev = Hash {getHash = "000d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 19}})
            , (Hash {getHash = "000f"},MockBlock {mockBlockId = Hash {getHash = "000f"}, mockBlockPrev = Hash {getHash = "000e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 20}})
            , (Hash {getHash = "0010"},MockBlock {mockBlockId = Hash {getHash = "0010"}, mockBlockPrev = Hash {getHash = "000f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 21}})
            , (Hash {getHash = "0011"},MockBlock {mockBlockId = Hash {getHash = "0011"}, mockBlockPrev = Hash {getHash = "0010"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 22}})
            , (Hash {getHash = "0012"},MockBlock {mockBlockId = Hash {getHash = "0012"}, mockBlockPrev = Hash {getHash = "0011"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 24}})
            , (Hash {getHash = "0013"},MockBlock {mockBlockId = Hash {getHash = "0013"}, mockBlockPrev = Hash {getHash = "0012"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 25}})
            , (Hash {getHash = "0014"},MockBlock {mockBlockId = Hash {getHash = "0014"}, mockBlockPrev = Hash {getHash = "0013"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 26}})
            , (Hash {getHash = "0015"},MockBlock {mockBlockId = Hash {getHash = "0015"}, mockBlockPrev = Hash {getHash = "0014"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 27}})
            , (Hash {getHash = "0016"},MockBlock {mockBlockId = Hash {getHash = "0016"}, mockBlockPrev = Hash {getHash = "0015"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 28}})
            , (Hash {getHash = "0017"},MockBlock {mockBlockId = Hash {getHash = "0017"}, mockBlockPrev = Hash {getHash = "0016"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 29}})
            , (Hash {getHash = "0018"},MockBlock {mockBlockId = Hash {getHash = "0018"}, mockBlockPrev = Hash {getHash = "0017"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 30}})
            , (Hash {getHash = "0019"},MockBlock {mockBlockId = Hash {getHash = "0019"}, mockBlockPrev = Hash {getHash = "0018"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 31}})
            , (Hash {getHash = "001a"},MockBlock {mockBlockId = Hash {getHash = "001a"}, mockBlockPrev = Hash {getHash = "0019"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 32}})
            , (Hash {getHash = "001b"},MockBlock {mockBlockId = Hash {getHash = "001b"}, mockBlockPrev = Hash {getHash = "001a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 33}})
            , (Hash {getHash = "001c"},MockBlock {mockBlockId = Hash {getHash = "001c"}, mockBlockPrev = Hash {getHash = "001b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 34}})
            , (Hash {getHash = "001d"},MockBlock {mockBlockId = Hash {getHash = "001d"}, mockBlockPrev = Hash {getHash = "001c"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 35}})
            , (Hash {getHash = "001e"},MockBlock {mockBlockId = Hash {getHash = "001e"}, mockBlockPrev = Hash {getHash = "001d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 36}})
            , (Hash {getHash = "001f"},MockBlock {mockBlockId = Hash {getHash = "001f"}, mockBlockPrev = Hash {getHash = "001e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 37}})
            , (Hash {getHash = "0020"},MockBlock {mockBlockId = Hash {getHash = "0020"}, mockBlockPrev = Hash {getHash = "001f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 39}})
            , (Hash {getHash = "0021"},MockBlock {mockBlockId = Hash {getHash = "0021"}, mockBlockPrev = Hash {getHash = "0020"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 40}})
            , (Hash {getHash = "0022"},MockBlock {mockBlockId = Hash {getHash = "0022"}, mockBlockPrev = Hash {getHash = "0021"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 41}})
            , (Hash {getHash = "0023"},MockBlock {mockBlockId = Hash {getHash = "0023"}, mockBlockPrev = Hash {getHash = "0022"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 42}})
            , (Hash {getHash = "0024"},MockBlock {mockBlockId = Hash {getHash = "0024"}, mockBlockPrev = Hash {getHash = "0023"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 43}})
            , (Hash {getHash = "0025"},MockBlock {mockBlockId = Hash {getHash = "0025"}, mockBlockPrev = Hash {getHash = "0024"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 45}})
            , (Hash {getHash = "0026"},MockBlock {mockBlockId = Hash {getHash = "0026"}, mockBlockPrev = Hash {getHash = "0025"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 47}})
            , (Hash {getHash = "0027"},MockBlock {mockBlockId = Hash {getHash = "0027"}, mockBlockPrev = Hash {getHash = "0026"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 49}})
            , (Hash {getHash = "0028"},MockBlock {mockBlockId = Hash {getHash = "0028"}, mockBlockPrev = Hash {getHash = "0027"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 50}})
            , (Hash {getHash = "0029"},MockBlock {mockBlockId = Hash {getHash = "0029"}, mockBlockPrev = Hash {getHash = "0028"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 52}})
            , (Hash {getHash = "002a"},MockBlock {mockBlockId = Hash {getHash = "002a"}, mockBlockPrev = Hash {getHash = "0029"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 53}})
            , (Hash {getHash = "002b"},MockBlock {mockBlockId = Hash {getHash = "002b"}, mockBlockPrev = Hash {getHash = "002a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 55}})
            , (Hash {getHash = "002c"},MockBlock {mockBlockId = Hash {getHash = "002c"}, mockBlockPrev = Hash {getHash = "002b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 56}})
            , (Hash {getHash = "002d"},MockBlock {mockBlockId = Hash {getHash = "002d"}, mockBlockPrev = Hash {getHash = "002c"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 57}})
            , (Hash {getHash = "002e"},MockBlock {mockBlockId = Hash {getHash = "002e"}, mockBlockPrev = Hash {getHash = "002d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 59}})
            , (Hash {getHash = "002f"},MockBlock {mockBlockId = Hash {getHash = "002f"}, mockBlockPrev = Hash {getHash = "002e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 60}})
            , (Hash {getHash = "0030"},MockBlock {mockBlockId = Hash {getHash = "0030"}, mockBlockPrev = Hash {getHash = "002f"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 61}})
            , (Hash {getHash = "0031"},MockBlock {mockBlockId = Hash {getHash = "0031"}, mockBlockPrev = Hash {getHash = "0030"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 63}})
            , (Hash {getHash = "0032"},MockBlock {mockBlockId = Hash {getHash = "0032"}, mockBlockPrev = Hash {getHash = "0031"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 64}})
            , (Hash {getHash = "0033"},MockBlock {mockBlockId = Hash {getHash = "0033"}, mockBlockPrev = Hash {getHash = "0032"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 65}})
            , (Hash {getHash = "0034"},MockBlock {mockBlockId = Hash {getHash = "0034"}, mockBlockPrev = Hash {getHash = "0033"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 66}})
            , (Hash {getHash = "0035"},MockBlock {mockBlockId = Hash {getHash = "0035"}, mockBlockPrev = Hash {getHash = "0034"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 70}})
            , (Hash {getHash = "0036"},MockBlock {mockBlockId = Hash {getHash = "0036"}, mockBlockPrev = Hash {getHash = "0035"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 72}})
            , (Hash {getHash = "0037"},MockBlock {mockBlockId = Hash {getHash = "0037"}, mockBlockPrev = Hash {getHash = "0036"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 73}})
            , (Hash {getHash = "0038"},MockBlock {mockBlockId = Hash {getHash = "0038"}, mockBlockPrev = Hash {getHash = "0037"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 74}})
            , (Hash {getHash = "0039"},MockBlock {mockBlockId = Hash {getHash = "0039"}, mockBlockPrev = Hash {getHash = "0038"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 75}})
            , (Hash {getHash = "003a"},MockBlock {mockBlockId = Hash {getHash = "003a"}, mockBlockPrev = Hash {getHash = "0039"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 79}})
            , (Hash {getHash = "003b"},MockBlock {mockBlockId = Hash {getHash = "003b"}, mockBlockPrev = Hash {getHash = "003a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 80}})
            , (Hash {getHash = "003c"},MockBlock {mockBlockId = Hash {getHash = "003c"}, mockBlockPrev = Hash {getHash = "003b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 81}})
            , (Hash {getHash = "003d"},MockBlock {mockBlockId = Hash {getHash = "003d"}, mockBlockPrev = Hash {getHash = "003c"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 83}})
            , (Hash {getHash = "003e"},MockBlock {mockBlockId = Hash {getHash = "003e"}, mockBlockPrev = Hash {getHash = "003d"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 84}})
            ]
        , nodeChainIds =
            [ Hash {getHash = "003e"}
            , Hash {getHash = "003d"}
            , Hash {getHash = "003c"}
            , Hash {getHash = "003b"}
            , Hash {getHash = "003a"}
            , Hash {getHash = "0039"}
            , Hash {getHash = "0038"}
            , Hash {getHash = "0037"}
            , Hash {getHash = "0036"}
            , Hash {getHash = "0035"}
            , Hash {getHash = "0034"}
            , Hash {getHash = "0033"}
            , Hash {getHash = "0032"}
            , Hash {getHash = "0031"}
            , Hash {getHash = "0030"}
            , Hash {getHash = "002f"}
            , Hash {getHash = "002e"}
            , Hash {getHash = "002d"}
            , Hash {getHash = "002c"}
            , Hash {getHash = "002b"}
            , Hash {getHash = "002a"}
            , Hash {getHash = "0029"}
            , Hash {getHash = "0028"}
            , Hash {getHash = "0027"}
            , Hash {getHash = "0026"}
            , Hash {getHash = "0025"}
            , Hash {getHash = "0024"}
            , Hash {getHash = "0023"}
            , Hash {getHash = "0022"}
            , Hash {getHash = "0021"}
            , Hash {getHash = "0020"}
            , Hash {getHash = "001f"}
            , Hash {getHash = "001e"}
            , Hash {getHash = "001d"}
            , Hash {getHash = "001c"}
            , Hash {getHash = "001b"}
            , Hash {getHash = "001a"}
            , Hash {getHash = "0019"}
            , Hash {getHash = "0018"}
            , Hash {getHash = "0017"}
            , Hash {getHash = "0016"}
            , Hash {getHash = "0015"}
            , Hash {getHash = "0014"}
            , Hash {getHash = "0013"}
            , Hash {getHash = "0012"}
            , Hash {getHash = "0011"}
            , Hash {getHash = "0010"}
            , Hash {getHash = "000f"}
            , Hash {getHash = "000e"}
            , Hash {getHash = "000d"}
            , Hash {getHash = "000c"}
            , Hash {getHash = "000b"}
            , Hash {getHash = "000a"}
            , Hash {getHash = "0009"}
            , Hash {getHash = "0008"}
            , Hash {getHash = "0007"}
            , Hash {getHash = "0006"}
            , Hash {getHash = "0005"}
            , Hash {getHash = "0004"}
            , Hash {getHash = "0003"}
            , Hash {getHash = "0002"}
            , Hash {getHash = "0001"}
            , Hash {getHash = "0000"}
            ]
        , nodeNextBlockId = 63
        }
    , operations = []
    , mockNodeK = Quantity {getQuantity = 3}
    , logs = []
    }

regression5 :: S
regression5 = S
    { node = N
        { nodeDb = Map.fromList
            [ (Hash {getHash = "0000"},MockBlock {mockBlockId = Hash {getHash = "0000"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}})
            , (Hash {getHash = "0001"},MockBlock {mockBlockId = Hash {getHash = "0001"}, mockBlockPrev = Hash {getHash = "0000"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}})
            , (Hash {getHash = "0002"},MockBlock {mockBlockId = Hash {getHash = "0002"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}})
            , (Hash {getHash = "0003"},MockBlock {mockBlockId = Hash {getHash = "0003"}, mockBlockPrev = Hash {getHash = "genesis"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 0}})
            , (Hash {getHash = "0004"},MockBlock {mockBlockId = Hash {getHash = "0004"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}})
            , (Hash {getHash = "0005"},MockBlock {mockBlockId = Hash {getHash = "0005"}, mockBlockPrev = Hash {getHash = "0004"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}})
            , (Hash {getHash = "0006"},MockBlock {mockBlockId = Hash {getHash = "0006"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}})
            , (Hash {getHash = "0007"},MockBlock {mockBlockId = Hash {getHash = "0007"}, mockBlockPrev = Hash {getHash = "0005"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}})
            , (Hash {getHash = "0008"},MockBlock {mockBlockId = Hash {getHash = "0008"}, mockBlockPrev = Hash {getHash = "0003"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 1}})
            , (Hash {getHash = "0009"},MockBlock {mockBlockId = Hash {getHash = "0009"}, mockBlockPrev = Hash {getHash = "0008"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 2}})
            , (Hash {getHash = "000a"},MockBlock {mockBlockId = Hash {getHash = "000a"}, mockBlockPrev = Hash {getHash = "0009"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 3}})
            , (Hash {getHash = "000b"},MockBlock {mockBlockId = Hash {getHash = "000b"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}})
            , (Hash {getHash = "000c"},MockBlock {mockBlockId = Hash {getHash = "000c"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}})
            ]
        , nodeChainIds =
            [ Hash {getHash = "000c"}
            , Hash {getHash = "000b"}
            , Hash {getHash = "000a"}
            , Hash {getHash = "0009"}
            , Hash {getHash = "0008"}
            , Hash {getHash = "0003"}
            ]
        ,  nodeNextBlockId = 13
        }
    ,  mockNodeK = Quantity {getQuantity = 4}
    , logs = []
    , operations =
        [[NodeRewind 1]
        , [NodeAddBlocks {getAddBlocks = [MockBlock {mockBlockId = Hash {getHash = "000d"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]}]
        , [NodeRewind 1]
        , [NodeAddBlocks {getAddBlocks = [MockBlock {mockBlockId = Hash {getHash = "000e"}, mockBlockPrev = Hash {getHash = "000b"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 5}}]}]
        , [NodeAddBlocks {getAddBlocks = [MockBlock {mockBlockId = Hash {getHash = "000f"}, mockBlockPrev = Hash {getHash = "000e"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 6}}]}]
        , [NodeRewind 3,NodeAddBlocks {getAddBlocks = [MockBlock {mockBlockId = Hash {getHash = "0010"}, mockBlockPrev = Hash {getHash = "000a"}, mockBlockSlot = SlotId {epochNumber = 0, slotNumber = 4}}]}]
        ]
    }
