{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Network.BlockHeadersSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Network.BlockHeaders
    ( BlockHeaders (..)
    , dropStartingFromSlotId
    , greatestCommonBlockHeader
    , updateUnstableBlocks
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochNo (..)
    , Hash (..)
    , SlotId (..)
    , SlotInEpoch (..)
    )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Writer
    ( execWriterT, tell )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft )
import Data.List
    ( find, findIndex, foldl', isPrefixOf )
import Data.Maybe
    ( fromMaybe )
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
    , Property
    , choose
    , counterexample
    , frequency
    , label
    , property
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (.||.)
    , (=/=)
    , (===)
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Test Chain" $ do
        it "Always generate valid test chains" $
            property prop_generator

    describe "Unstable block headers" $ do
        it "Are updated by fetching blocks"
            $ withMaxSuccess 10000
            $ property prop_unstableBlockHeaders
        it "Does not fetch block headers which we already have"
            $ withMaxSuccess 1000
            $ property prop_updateUnstableBlocksIsEfficient
        it "Handles failure of node"
            $ withMaxSuccess 10000
            $ property prop_updateUnstableBlocksFailure
    describe "Chain intersection" $ do
        it "Calculates GCBH"
            $ withMaxSuccess 1000
            $ property prop_greatestCommonBlockHeader

{-------------------------------------------------------------------------------
                        Unstable Block Headers Property
-------------------------------------------------------------------------------}

data TestCase = TestCase
    { k :: Quantity "block" Word32
    , nodeChain :: [BlockHeader]
    , localChain :: [BlockHeader]
    } deriving (Show, Eq)

prop_unstableBlockHeaders :: TestCase -> Property
prop_unstableBlockHeaders TestCase{k, nodeChain, localChain} =
    counterexample ce prop
  where
    prop
        | hasTip nodeChain = label lbl (bs' === Just expected)
        | otherwise = label "node has no chain" (bs' === Nothing)

    expected = mkBlockHeaders $
        modelIntersection k localChain nodeChain

    isect = intersectionPoint localChain nodeChain

    lbl = case isect of
        Just i
            | chainTip localChain == chainTip nodeChain -> "synced"
            | i == chainEnd nodeChain -> "local chain ahead"
            | i == chainEnd localChain -> "node chain ahead"
            | otherwise -> "rollback"
        Nothing
            | chainLength localChain == 0 -> "local chain empty"
            | otherwise -> "no common chain"

    ce = unlines
        [ "k = " ++ show k
        , "Local chain: " ++ showChain localChain
        , "Node chain:  " ++ showChain nodeChain
        , "Intersects:  " ++ maybe "-" showSlot isect
        , "Expected:    " ++ showBlockHeaders expected
        , "Actual:      " ++ maybe "-" showBlockHeaders bs'
        ]

    bs = mkBlockHeaders localChain
    bs' = updateUnstableBlocks (coerce k) getTip getBlockHeader bs
      where
        getTip = tipId nodeChain
        getBlockHeader hh = find ((== hh) . headerHash) nodeChain

-- | 'updateUnstableBlocks' should not fetch blocks that it already has headers
-- for.
prop_updateUnstableBlocksIsEfficient :: TestCase -> Property
prop_updateUnstableBlocksIsEfficient TestCase{k, nodeChain, localChain} =
    counterexample ce prop
  where
    prop = Set.size (Set.intersection localHashes fetchedHashes) <= 1

    localHashes = Set.fromList (map parentHeaderHash (drop 1 localChain))
    fetchedHashes = maybe mempty Set.fromList (execWriterT bs')

    ce = unlines
        [ "k = " ++ show k
        , "Local chain: " ++ showChain localChain
        , "Node chain:  " ++ showChain nodeChain
        , "Fetched:     " ++ unwords (map showHash (Set.toList fetchedHashes))
        ]

    bs = mkBlockHeaders localChain
    -- Run updateUnstableBlocks and record which blocks were fetched.
    bs' = updateUnstableBlocks (coerce k) getTip getBlockHeader bs
      where
        getTip = lift (tipId nodeChain)
        getBlockHeader h = tell [h] *> lift (find ((== h) . headerHash) nodeChain)

prop_updateUnstableBlocksFailure :: TestCase -> Property
prop_updateUnstableBlocksFailure TestCase{k, nodeChain, localChain} =
    label lbl prop
  where
    prop
        | hasTip nodeChain = either ("injected" `isPrefixOf`) (const True) res
        | otherwise = isLeft res

    lbl = case res of
        Right _ -> "success"
        Left e
            | not (hasTip nodeChain) -> "node has no chain"
            | otherwise -> e

    res = updateUnstableBlocks (coerce k) getTip getBlockHeader bs
    bs = mkBlockHeaders localChain
    getTip
        | chainLength nodeChain `mod` 5 == 0 = Left "injected getTip failed"
        | otherwise = maybe (Left "no tip") Right $ tipId nodeChain
    getBlockHeader h = case findIndex ((== h) . headerHash) nodeChain of
        Just ix
            | ix `mod` 3 == 0 -> Left "injected getBlock failed"
            | otherwise -> Right (nodeChain !! ix)
        Nothing -> Left "block not found"

{-------------------------------------------------------------------------------
                                TestCase helpers
-------------------------------------------------------------------------------}

-- | Convert a test chain to 'BlockHeaders' so that it can be compared for
-- equality.
mkBlockHeaders :: [BlockHeader] -> BlockHeaders
mkBlockHeaders bs = BlockHeaders (Seq.fromList bs)

{-------------------------------------------------------------------------------
                              Test chain functions
-------------------------------------------------------------------------------}

-- | Tip of a test chain
tipId :: [BlockHeader] -> Maybe (Hash "BlockHeader")
tipId = fmap headerHash . chainTip

-- | A test chain needs a headers to have a tip.
hasTip :: [BlockHeader] -> Bool
hasTip = not . null

-- | Length of a test chain, not including the block header after the tip.
chainLength :: [BlockHeader] -> Int
chainLength = length

chainTip :: [BlockHeader] -> Maybe BlockHeader
chainTip = lastMay

-- | Slot index of the tip of a chain.
chainEnd :: [BlockHeader] -> SlotId
chainEnd = maybe (SlotId 0 0) slotId . chainTip

-- | Limit the sequence to a certain size by removing items from the beginning.
limitChain :: Quantity "block" Word32 -> [BlockHeader] -> [BlockHeader]
limitChain (Quantity k) bs = drop (max 0 (length bs - fromIntegral k)) bs

showChain :: [BlockHeader] -> String
showChain [] = "<empty chain>"
showChain bs = unwords (map (showHash . headerHash) bs)

showSlot :: SlotId -> String
showSlot (SlotId ep sl) = show ep ++ "." ++ show sl

{-------------------------------------------------------------------------------
                     Test chain pure model of intersection
-------------------------------------------------------------------------------}

-- | Merge node chain with local unstable blocks.
modelIntersection
    :: Quantity "block" Word32
    -- ^ Maximum number of unstable blocks.
    -> [BlockHeader]
    -- ^ Local test chain.
    -> [BlockHeader]
    -- ^ Remote test chain.
    -> [BlockHeader]
    -- ^ New local test chain.
modelIntersection k localChain nodeChain =
    -- We have at most k block headers ...
    limitChain k $
    -- ... added to the end of current unstable block headers ...
    maybe id dropToSlot (min p (p >>= fixup))
    -- ... fetched from the node.
    nodeChain
  where
    p = intersectionPoint localChain nodeChain
    -- The real code under test will fetch the full k if the local chain is
    -- ahead of the node chain. It's not incorrect behaviour, and the situation
    -- should never happen anyway. But we need to add this to the model so that
    -- the tests pass.
    fixup q
        | chainEnd nodeChain > q = Just q
        | otherwise = Nothing

-- | Find the last slot index of the node chain which is the same as the local
-- chain.
intersectionPoint :: [BlockHeader] -> [BlockHeader] -> Maybe SlotId
intersectionPoint localChain nodeChain = res >>= checkAfter
  where
    res = slotId . fst <$> find (uncurry (==)) pairs

    pairs = zip localChain' nodeChain
    localChain' = spliceChains localChain nodeChain

    -- The block header *after* a block contains the hash of that block.
    -- So compare that, if it exists.
    checkAfter sl | after localChain' == after nodeChain = Just sl
                  | otherwise = Nothing
      where
        after xs = fmap snd $ find ((== sl) . slotId . fst) $ zip xs (drop 1 xs)

-- | Prepend a source chain before a local chain.
spliceChains :: [BlockHeader] -> [BlockHeader] -> [BlockHeader]
spliceChains localChain nodeChain = takeToSlot start chaff ++ localChain
  where
    start = fromMaybe (SlotId 0 0) $ firstSlot localChain
    -- chaff is the same shape as the node chain, but with different hashes
    chaff = [bh { parentHeaderHash = Hash "x" } | bh <- nodeChain]

-- | The slot index at which the local chain starts.
firstSlot :: [BlockHeader] -> Maybe SlotId
firstSlot bs
    | hasTip bs = slotId <$> headMay bs
    | otherwise = Nothing

dropToSlot :: SlotId -> [BlockHeader] -> [BlockHeader]
dropToSlot sl = dropWhile ((< sl) . slotId)

takeToSlot :: SlotId -> [BlockHeader] -> [BlockHeader]
takeToSlot sl = takeWhile ((< sl) . slotId)

{-------------------------------------------------------------------------------
                         Intersection of BlockHeaders
-------------------------------------------------------------------------------}

prop_greatestCommonBlockHeader :: TestCase -> Property
prop_greatestCommonBlockHeader TestCase{nodeChain, localChain} =
    counterexample ce prop
  where
    prop = case gcbh of
        Just bh ->
            -- The block after gcbh is different
            (nextBlock bh ubs =/= nextBlock bh lbs .||.
                nextBlock bh ubs === Nothing)
            .&&.
            -- All blocks up to and including the gcbh are the same.
            (dropStartingFromSlotId (slotId bh) ubs
                === dropStartingFromSlotId (slotId bh) lbs)
        Nothing ->
            -- No common block means that the first blocks are different, or one
            -- chain is empty.
            firstUbs ubs =/= firstUbs lbs .||. isEmpty ubs

    ce = unlines
        [ "Local chain: " ++ showChain localChain
        , "Node chain:  " ++ showChain nodeChain
        , "GCBH:        " ++ show gcbh
        ]

    gcbh = greatestCommonBlockHeader ubs lbs
    ubs = mkBlockHeaders nodeChain
    lbs = mkBlockHeaders localChain

    -- Utils for poking around BlockHeaders.
    nextBlock bh (BlockHeaders bs) = seqHead $
        Seq.drop 1 $ Seq.dropWhileL (/= bh) bs
    firstUbs (BlockHeaders bs) = seqHead bs
    seqHead = Seq.lookup 0 . Seq.take 1
    isEmpty (BlockHeaders bs) = Seq.null bs

{-------------------------------------------------------------------------------
                              Test data generation
-------------------------------------------------------------------------------}

prop_generator :: TestCase -> Property
prop_generator TestCase{nodeChain, localChain} =
    valid nodeChain .&&. valid localChain
  where
    valid c = continuous c .&&. slotsIncreasing c

    continuous c =
        counterexample ("Chain not continuous: " <> showChain c) $
        and (zipWith (==) (map headerHash c) (map parentHeaderHash (drop 1 c)))

    slotsIncreasing c = counterexample ("Slots not increasing: " ++ showChain c) $
        let sls = map slotId c
        in and (zipWith (<) sls (drop 1 sls))

-- | Generate an infinite test chain. Take a slice of the list and use 'tipId'
-- and 'headerIds' to access the tip and block headers. The tip of a test chain
-- is the penultimate block.
chain :: String -> Hash "BlockHeader" -> [BlockHeader]
chain p hash0 =
    [ BlockHeader
        (SlotId 0 (fromIntegral n))
        (mockBlockHeight n)
        (hash n)
        (hash (n - 1))
    | n <- [1..]
    ]
  where
    mockBlockHeight = Quantity . fromIntegral
    hash :: Int -> Hash "BlockHeader"
    hash n = if n == 0 then hash0 else Hash . B8.pack $ p ++ show n

-- | Filter out test chain blocks that correspond to False values, and update
-- parent hashes so that the chain is still continuous.
removeBlocks :: [Bool] -> [BlockHeader] -> [BlockHeader]
removeBlocks holes bs =
    reverse
        $ snd
        $ foldl' maybeMkHole (parentHeaderHash (head bs), [])
        $ zip holes (zip (map parentHeaderHash $ tail bs) bs)
  where
    maybeMkHole (prev, ac) (True, (h, BlockHeader sl _ hh _)) =
        (h, ((BlockHeader sl bh hh prev):ac))
    maybeMkHole pbs _ =
        pbs
    bh = Quantity 0

genChain
    :: Quantity "block" Word32
    -> String
    -> Hash "BlockHeader"
    -> Gen [BlockHeader]
genChain (Quantity k) prefix hash0 = do
    len <- choose (0, fromIntegral k)
    holes <- genHoles len
    return
        $ take len
        $ removeBlocks holes
        $ chain prefix hash0
  where
    genHoles :: Int -> Gen [Bool]
    genHoles n =
        let genOne = frequency [(1, pure False), (4, pure True)]
        in (True:) <$> vectorOf (n - 1) genOne

instance Arbitrary TestCase where
    arbitrary = do
        k <- arbitrary
        let genesis = BlockHeader (SlotId 0 0) (Quantity 0) (Hash "genesis") (Hash "void")
        base  <- genChain k "base" (headerHash genesis)
        let nextHash = maybe (headerHash genesis) headerHash (chainTip base)
        local <- genChain k "local" nextHash
        node  <- genChain k "node" nextHash
        let baseTip = chainEnd base
        return TestCase
            { k = k
            , nodeChain  = [genesis] <> base <> startFrom baseTip node
            , localChain = [genesis] <> base <> startFrom baseTip local
            }
      where
        startFrom (SlotId (EpochNo ep) (SlotInEpoch n)) xs =
            [ BlockHeader (SlotId (EpochNo ep) (sl+fromIntegral n)) bh' hh prev
            | BlockHeader (SlotId _ sl) (Quantity bh) hh prev <- xs
            , let bh' = Quantity (bh+fromIntegral n+1)
            ]

    shrink TestCase{k, nodeChain, localChain} =
        [ TestCase k' (take n nodeChain) (take l localChain)
        | (k', n, l) <- shrink (k, length nodeChain, length localChain)
        ]

instance Arbitrary (Quantity "block" Word32) where
    -- k doesn't need to be large for testing this
    arbitrary =
        Quantity . fromIntegral <$> choose (2 :: Int, 30)
    shrink (Quantity k) =
        [ Quantity (fromIntegral k')
        | k' <- shrink (fromIntegral k :: Int)
        , k' >= 2
        ]

{-------------------------------------------------------------------------------
                 Extra unstable blocks functions for properties
-------------------------------------------------------------------------------}

-- | Shows just the headers of the unstable blocks.
showBlockHeaders :: BlockHeaders -> String
showBlockHeaders = unwords . map showBlockHeader . F.toList . getBlockHeaders
    where showBlockHeader = showHash . headerHash

showHash :: Hash a -> String
showHash (Hash h) = B8.unpack h
