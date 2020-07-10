{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    , Consumer(..)
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api.Client
    ( JormungandrClient (..) )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Network
    ( pattern Cursor, ErrGetDescendants (..), mkRawNetworkLayer )
import Cardano.Wallet.Network
    ( Cursor, ErrGetBlock (..), NetworkLayer (..), NextBlocksResult (..) )
import Cardano.Wallet.Network.BlockHeaders
    ( BlockHeaders, emptyBlockHeaders, greatestCommonBlockHeader )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , Coin (..)
    , GenesisParameters (..)
    , Hash (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SlotId (..)
    , SlotNo (unSlotNo)
    , TxParameters (..)
    )
import Control.Concurrent.MVar.Lifted
    ( MVar, newMVar, readMVar )
import Control.Concurrent.STM.TChan
    ( newBroadcastTChanIO )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
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
    ( ($>), (<&>) )
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
    , label
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
        it "Syncs with mock node" $
            withMaxSuccess 100000 prop_sync

{-------------------------------------------------------------------------------
                Syncing of network layer in presence of rollback
                          and concurrent node updates.
-------------------------------------------------------------------------------}

prop_sync :: S -> Property
prop_sync s0 = monadicIO $ do
    let logLineC msg = logLine $ "[CONSUMER] " <> msg

    -- Run a model chain consumer on the mock network layer.
    (c0Chain, c0Cps) <- pick $ genConsumer s0
    (consumer, s) <- run $ flip runStateT s0 $ do
        -- Set up network layer with mock Jormungandr
        nXl@(nl, _) <- mockNetworkLayer logLine
        cursor <- initCursor nl c0Cps
        let c0 = C c0Chain cursor 0 0
        consumerRestoreStep logLineC nXl c0 Nothing

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
    monitor (label (intersectionHitRate consumer))
    monitor (classify (initialChainLength (const (== 1))) "started with an empty chain")
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
        line `elem` logs s
      where
        line = "[CONSUMER] RollBackward SlotId {epochNumber = 0, slotNumber = 0}"

    startedFromScratch :: [BlockHeader] -> Bool
    startedFromScratch = (== 1) . length

    intersectionHitRate :: Consumer -> String
    intersectionHitRate (C _ _ n total) =
        "Intersection hit rate " <> case double n / double total of
            k | k < 0.10 -> "BAD   (0%  - 10%)"
            k | k < 0.50 -> "POOR  (10% - 50%)"
            k | k < 0.75 -> "GOOD  (50% - 75%)"
            _ -> "GREAT (75% - 100%)"
      where
        double :: Int -> Double
        double = fromRational . fromIntegral

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
    slotNum = fromIntegral . unSlotNo . slotNumber . getSlot

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
    , consumerCursor :: Cursor Jormungandr
    -- ^ Consumer state -- the cursor given by network layer.

    , consumerIntersected :: Int
    -- ^ Number of time the consumer had an intersection with the chain during
    -- its lifetime

    , consumerTotalSteps :: Int
    -- ^ Total number of steps
    }

-- | A model consumer that repeatedly fetches blocks and "applies" them by
-- appending to its chain. Returns the new 'Consumer' state.
-- This is similar to the Cardano.Wallet worker restore loop.
consumerRestoreStep
    :: (Monad m)
    => (String -> StateT S m ())
    -- ^ logger function
    -> (TestNetworkLayer m, Cursor Jormungandr -> m (Maybe BlockHeader))
    -- ^ Network layer.
    -> Consumer
    -- ^ Current consumer state.
    -> Maybe Int
    -- ^ A counter to limit the number of steps. Only set to `Just` when there's
    -- no more operations to apply in the node and we expect the consumer to
    -- eventually be in sync.
    -> StateT S m Consumer
consumerRestoreStep _ _ c (Just 0) = pure c
consumerRestoreStep logLine nXw@(nw, findIntersection) (C bs cur hit total) mLimit = do
    -- We apply blocks by batch of `k`, so, when the node stabilizes, we should
    -- finish in less than the chain length divided by k steps.
    S n ops (Quantity k) _ <- get
    let limit = case mLimit of
            Just lim -> Just (lim - 1)
            Nothing -> if null ops
                then Just (2 + length (nodeChainIds n) `div` fromIntegral k)
                else Nothing
    logLine $ "nextBlocks " <> show (cursorSlotId nw cur)
    hit' <- lift (findIntersection cur) <&> \case
        Nothing -> hit
        Just _  -> hit + 1
    let total' = total + 1
    runExceptT (nextBlocks nw cur) >>= \case
        Left e -> do
            logLine ("Failed to get next blocks: " ++ show e)
            consumerRestoreStep logLine nXw (C bs cur hit' total') limit
        Right AwaitReply -> do
            logLine "AwaitReply"
            consumerRestoreStep logLine nXw (C bs cur hit' total') limit
        Right (RollForward cur' _ bs') -> do
            logLine $ "RollForward " <> unwords (showBlock <$> bs')
            consumerRestoreStep logLine nXw (C (bs ++ bs') cur' hit' total') limit
        Right (RollBackward cur') -> do
            logLine $ "RollBackward " <> show (cursorSlotId nw cur')
            let sl = cursorSlotId nw cur'
            let bs' = takeWhile (\b -> mockBlockSlot b <= sl) bs
            consumerRestoreStep logLine nXw (C bs' cur' hit' total') limit


----------------------------------------------------------------------------
-- Network layer with mock jormungandr node

type TestNetworkLayer m =
    NetworkLayer (StateT S m) Jormungandr MockBlock

-- | Instantiate new network layer with mock jormungandr.
mockNetworkLayer
    :: forall m. (MonadFail m, MonadBaseControl IO m, MonadIO m)
    => (String -> StateT S m ()) -- ^ logger function
    -> StateT S m (TestNetworkLayer m, Cursor Jormungandr -> m (Maybe BlockHeader))
mockNetworkLayer logLine = do
    let jm = mockJormungandrClient logLine
    Quantity k <- gets mockNodeK
    st <- newMVar emptyBlockHeaders
    chan <- liftIO newBroadcastTChanIO
    Right (_b0, np) <-
        runExceptT $ getInitialBlockchainParameters jm genesisHash
    pure
        ( fromJBlock <$> mkRawNetworkLayer np (fromIntegral k) st chan jm
        , findIntersection st
        )
  where
    findIntersection :: MVar BlockHeaders -> Cursor Jormungandr -> m (Maybe BlockHeader)
    findIntersection st (Cursor localChain) = do
        nodeChain <- readMVar st
        pure (greatestCommonBlockHeader nodeChain localChain)

-- | A network layer which returns mock blocks and mutates its state according
-- to the generated operations.
mockJormungandrClient
    :: forall m. (Monad m)
    => (String -> StateT S m ())
        -- ^ logger function
    -> JormungandrClient (StateT S m)
mockJormungandrClient logLine = JormungandrClient
    { getAccountState = \_ -> error "mock getAccountState"

    , getTipId = do
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
        let res = fmap (take $ fromIntegral count) $
                if parentId == headerHash block0H
                then pure (drop 1 $ reverse ch)
                else if parentId `elem` ch then
                    pure $ reverse (takeWhile (/= parentId) ch)
                else
                    Left $ ErrGetDescendantsParentNotFound parentId
        lift . logLineP $ "getDescendentIds "
            <> show parentId
            <> " "
            <> show count
            <> returns (show res)
        lift applyOps
        except res

    , getInitialBlockchainParameters = \blockId -> do
        Quantity k <- lift $ gets mockNodeK
        pure (block0, NetworkParameters
            { genesisParameters = GenesisParameters
                { getGenesisBlockHash = blockId
                , getGenesisBlockDate = error "mock gp"
                , getSlotLength = error "mock gp"
                , getEpochLength = error "mock gp"
                , getEpochStability = Quantity (fromIntegral k)
                , getActiveSlotCoefficient = error "mock gp"
                }
            , protocolParameters = ProtocolParameters
                { decentralizationLevel = minBound
                , txParameters = TxParameters
                    { getFeePolicy = error "mock gp"
                    , getTxMaxSize = error "mock gp"
                    }
                , desiredNumberOfStakePools = 10
                , minimumUTxOvalue = Coin 0
                }
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
emptyNode = N (Map.singleton genesisHash (fromJBlock block0)) [genesisHash] 1

-- | Gets the mock node's chain as a list of blocks starting from genesis.
getNodeChain :: Node -> [MockBlock]
getNodeChain (N db ch _) = reverse $ catMaybes [Map.lookup b db | b <- ch]

getNodeTip :: Node -> Maybe MockBlock
getNodeTip (N db ch _) = headMay ch >>= flip Map.lookup db

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
            mockBlockPrev b === parentGenesisHash
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
        tipSlot = maybe
            (-1)
            (fromIntegral . unSlotNo . slotNumber . mockBlockSlot)
            tip
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
genRewind k (N _ ch _) = do
    let rMax = length ch - 1
    rw <- frequency
        [ (80, choose (1, min k 3))
        , (15, choose (1, (min k (rMax `div` 3))))
        , (5, min rMax <$> choose ((k - 1), k))
        ]
    pure (if rw > rMax then 0 else rw)

genGC :: Node -> Gen [Hash "BlockHeader"]
genGC (N db ch _) = sublistOf (Map.keys db \\ ch)

genGaps :: Int -> Gen [Bool]
genGaps count = do
    vectorOf count $ frequency [(1, pure True), (4, pure False)]

genConsumer :: S -> Gen ([MockBlock], [BlockHeader])
genConsumer s = do
    n <- choose (1, length (getNodeChain (node s)))
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
