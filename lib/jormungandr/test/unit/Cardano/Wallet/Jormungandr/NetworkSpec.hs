{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
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
    ( StateT (..), get, gets, modify', runStateT )
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
import Fmt
    ( fmt, hexF, padLeftF )
import Safe
    ( headMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , choose
    , conjoin
    , counterexample
    , frequency
    , liftShrink2
    , property
    , shrinkList
    , shrinkNothing
    , sized
    , sublistOf
    , vectorOf
    , withMaxSuccess
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
        it "Always generate valid chains" $ property $ \(S n0 ops _ _) ->
            let
                follow :: Node -> Property
                follow (N db chain) = case catMaybes [Map.lookup b db | b <- chain] of
                    [] ->
                        property True
                    [b] ->
                        mockBlockPrev b === Hash "genesis"
                    (p:b:_) ->
                        counterexample ("Chain: " <> showBlock p <> " ==> " <> showBlock b)
                        $ conjoin
                            [ mockBlockPrev p === mockBlockId b
                            , follow $ N db (drop 1 chain)
                            ]
            in
                conjoin (follow <$> scanl (flip applyNodeOp) n0 (concat ops))

        it "Always generate less than 100 operations" $ property
            $ \(S _ ops _ _) -> withMaxSuccess 100000 $ property (length ops < 100)

    describe "Chain sync" $ do
        it "Syncs with mock node" $
            withMaxSuccess 1000 prop_sync

{-------------------------------------------------------------------------------
                Syncing of network layer in presence of rollback
                          and concurrent node updates.
-------------------------------------------------------------------------------}

prop_sync :: S -> Property
prop_sync s0 = monadicIO $ do
    let logLineC msg = logLine $ "[CONSUMER] " <> msg

    (consumer, s) <- run $ flip runStateT s0 $ do
        -- Set up network layer with mock Jormungandr
        nl <- mockNetworkLayer logLine
        -- Run a model chain consumer on the mock network layer.
        let initialConsumer = C [] (initCursor nl fakeBlock) 100
        consumerRestoreStep logLineC nl initialConsumer

    -- NOTE: We never apply the first block 'block0', as this one is given as a
    -- parameter when starting the wallet. The network layer will never yield
    -- it.
    let nodeChain = drop 1 (getNodeChain (node s))

    monitor $ counterexample $ unlines
        [ "Applied blocks:  " <> showChain (consumerApplied consumer)
        , "Node chain:      " <> showChain nodeChain
        , "Steps remaining: " <> show (consumerStepsRemaining consumer)
        , "Logs:          \n" <> unlines (reverse (logs s))
        ]

    -- Consumer chain should (eventually) be in sync with node chain.
    assert (consumerApplied consumer == nodeChain)
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
    } deriving (Show, Eq)

emptyNode :: Node
emptyNode = N mempty mempty

-- | Gets the mock node's chain as a list of blocks starting from genesis.
getNodeChain :: Node -> [MockBlock]
getNodeChain (N db ch) = reverse $ catMaybes [Map.lookup b db | b <- ch]

getNodeTip :: Node -> Maybe MockBlock
getNodeTip (N db ch) = headMay ch >>= flip Map.lookup db

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
applyNodeOps ops n = foldr applyNodeOp n ops

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
    n { nodeChainIds = drop i (nodeChainIds n) }

-- | "Garbage collect" models the potential that getting rolled back blocks will
-- still succeed after the chain is switched, but that it will stop succeeding
-- at some unknown point in the future.
nodeGarbageCollect :: [Hash "BlockHeader"] -> Node -> Node
nodeGarbageCollect hs (N bs c) = N bs' c
    where bs' = foldr Map.delete bs hs

-- | Remove non-effectful NodeOps from a list which has been shrunk.
filterNodeOps :: [NodeOp] -> [NodeOp]
filterNodeOps = filter isUseful
  where
    isUseful (NodeAddBlocks []) = False
    isUseful (NodeRewind 0) = False
    isUseful (NodeGarbageCollect []) = False
    isUseful _ = True

-- | Update block prev hashes so that the chain is still continuous after
-- shrinking.
fixBlockPrevs :: S -> S
fixBlockPrevs  = id

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

-- | Fix up prev header hashes in a chain of blocks where some have been removed
-- due to shrinking.
updateBlockPrevs :: MockBlock -> [MockBlock] -> [MockBlock]
updateBlockPrevs g bs =
    [ MockBlock i1 i0 sl c
    | (MockBlock i0 _ _ _, MockBlock i1 _ sl c) <- zip (g:bs) bs ]

----------------------------------------------------------------------------
-- Generation of mock node test cases.

instance Arbitrary S where
    arbitrary = do
        -- initChainLen <- arbitrary
        -- initBlocks <- genBlocksCount emptyNode initChainLen
        -- let node = nodeFromChain initBlocks
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
        genNodeOp (Quantity k) n = filterNodeOps <$> frequency
                [ (30, pure [])
                , (10, pure . NodeAddBlocks <$> genBlocks n)
                , (3, genSwitchChain (fromIntegral k) n)
                , (1, pure . NodeGarbageCollect <$> genGC n)
                ]

        genBlocks :: Node -> Gen [MockBlock]
        genBlocks n = do
            count <- sized $ \s -> choose (1, s)
            genBlocksCount n count

        genBlocksCount :: Node -> Int -> Gen [MockBlock]
        genBlocksCount n count = do
            let genEmpty = frequency [(1, pure True), (4, pure False)]
            empty <- vectorOf count genEmpty
            let tip = getNodeTip n
            let tipSlot = maybe (-1) (fromIntegral . slotNumber . mockBlockSlot) tip
            let chainLength = length $ nodeChainIds n
            let slots =
                    [ SlotId 0 (fromIntegral $ tipSlot + i)
                    | (i, gap) <- zip [1..count] empty, tipSlot + i == 0 || not gap
                    ]
            let contents = [chainLength..]
            let bids = mockBlockHash <$> contents
            let prevs = maybe (Hash "genesis") mockBlockId tip : bids
            pure
                [ MockBlock bid prev slot content
                | (bid, prev, slot, content) <- zip4 bids prevs slots contents
                ]

        -- Switching chain is rewinding then adopting the blocks from a fork.
        genSwitchChain :: Int -> Node -> Gen [NodeOp]
        genSwitchChain k n = do
          rewind <- genRewind (fromIntegral k) n
          bs <- genBlocksCount (nodeRewind rewind n) rewind
          pure [NodeRewind rewind, NodeAddBlocks bs]

        -- Rewinds are usually small to allow the node to make progress, so that
        -- the test can stop. Sometimes the full k is rolled back.
        genRewind :: Int -> Node -> Gen Int
        genRewind k (N _ ch) = frequency
            [ (19, choose (1, (min k (length ch `div` 3))))
            , (1, choose ((k - 1), k))
            ]

        genGC :: Node -> Gen [Hash "BlockHeader"]
        genGC (N db ch) = sublistOf (Map.keys db \\ ch)

    shrink (S n ops k _) = []
        -- -- [ fixBlockPrevs (S n' ops' k [])
        -- -- |  ops' <- shrinkNodeOps ops ]
      where
        shrinkNodeOps = shrinkList (shrinkList (filterNodeOps . shrinkNodeOp))

        shrinkNodeOp :: NodeOp -> [NodeOp]
        shrinkNodeOp (NodeAddBlocks bs) =
            NodeAddBlocks <$> shrinkBlocks bs
        shrinkNodeOp (NodeRewind rw) =
            NodeRewind <$> shrink rw
        shrinkNodeOp (NodeGarbageCollect ids) =
            NodeGarbageCollect <$> shrinkList shrinkNothing ids

        shrinkBlocks = shrinkList shrinkNothing

        -- shrinkNode = fmap (nodeFromChain . updateBlockPrevs) . shrinkBlocks . getNodeChain

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
