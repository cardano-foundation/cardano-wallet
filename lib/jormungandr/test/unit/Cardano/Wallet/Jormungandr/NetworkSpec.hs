{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Network
    ( UnstableBlocks (..), updateUnstableBlocks )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )
import Control.Monad
    ( (>=>) )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Writer
    ( execWriterT, tell )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft )
import Data.List
    ( find, findIndex, isPrefixOf )
import Data.Quantity
    ( Quantity (..) )
import Numeric.Natural
    ( Natural )
import Safe
    ( headMay, initMay, lastMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , elements
    , label
    , property
    , withMaxSuccess
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
    describe "Unstable block headers" $ do
        it "Are updated by fetching blocks" $
            withMaxSuccess 1000 $
            property prop_unstableBlockHeaders
        it "Does not fetch block headers which we already have" $
            withMaxSuccess 1000 $
            property prop_updateUnstableBlocksIsEfficient
        it "Handles failure of node" $
            withMaxSuccess 1000 $
            property prop_updateUnstableBlocksFailure

{-------------------------------------------------------------------------------
                        Unstable Block Headers Property
-------------------------------------------------------------------------------}

data TestCase = TestCase
    { k :: Quantity "block" Natural
    , nodeChain :: [BlockHeader]
    , localChain :: [BlockHeader]
    } deriving (Show, Eq)

prop_unstableBlockHeaders :: TestCase -> Property
prop_unstableBlockHeaders TestCase{..} =
    counterexample ce prop
  where
    prop
        | hasTip nodeChain = label lbl (bs' === Just expected)
        | otherwise = label "node has no chain" (bs' === Nothing)

    expected = mkUnstableBlocks $ modelIntersection k localChain nodeChain

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
        , "Intersects:  " ++ show isect
        , "Expected:    " ++ showUnstableBlocks expected
        , "Actual:      " ++ maybe "-" showUnstableBlocks bs'
        ]

    bs' = updateUnstableBlocks (coerce k) getTip getBlockHeader bs
    bs = mkUnstableBlocks localChain
    getTip = tipId nodeChain
    getBlockHeader = flip lookup blocks
    blocks = mkBlockHeaderHeights nodeChain

-- | 'updateUnstableBlocks' should not fetch blocks that it already has headers
-- for.
prop_updateUnstableBlocksIsEfficient :: TestCase -> Property
prop_updateUnstableBlocksIsEfficient TestCase{..} =
    counterexample ce prop
  where
    prop = Set.size (Set.intersection localHashes fetchedHashes) <= 1

    localHashes = Set.fromList (map prevBlockHash (drop 1 localChain))
    fetchedHashes = maybe mempty Set.fromList lookups

    ce = unlines
        [ "k = " ++ show k
        , "Local chain: " ++ showChain localChain
        , "Node chain:  " ++ showChain nodeChain
        , "Fetched:     " ++ unwords (map showHash (Set.toList fetchedHashes))
        ]

    -- Run updateUnstableBlocks and record which blocks were fetched.
    lookups = execWriterT bs'
    bs' = updateUnstableBlocks (coerce k) getTip getBlockHeader bs
    bs = mkUnstableBlocks localChain
    getTip = lift (tipId nodeChain)
    getBlockHeader h = tell [h] *> lift (lookup h blocks)
    blocks = mkBlockHeaderHeights nodeChain

prop_updateUnstableBlocksFailure :: TestCase -> Property
prop_updateUnstableBlocksFailure TestCase{..} =
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
    bs = mkUnstableBlocks localChain
    getTip
        | chainLength nodeChain `mod` 5 == 0 = Left "injected getTip failed"
        | otherwise = maybe (Left "no tip") Right $ tipId nodeChain
    getBlockHeader h = case findIndex ((== h) . fst) blocks of
        Just ix
            | ix `mod` 3 == 0 -> Left "injected getBlock failed"
            | otherwise -> Right (snd (blocks !! ix))
        Nothing -> Left "block not found"
    blocks = mkBlockHeaderHeights nodeChain

{-------------------------------------------------------------------------------
                                TestCase helpers
-------------------------------------------------------------------------------}

-- | Convert a test chain to 'UnstableBlocks' so that it can be compared for
-- equality.
mkUnstableBlocks :: [BlockHeader] -> UnstableBlocks
mkUnstableBlocks bs = UnstableBlocks (Seq.fromList $ headerIds bs) h
  where
    h = Quantity $ fromIntegral (chainLength bs + fromIntegral start)
    start = maybe 0 (slotNumber . slotId) (headMay bs)

-- | Convert a test chain into an assoc list of block ids, their headers, and
-- chain heights.
mkBlockHeaderHeights
    :: [BlockHeader]
    -> [(Hash "BlockHeader", (BlockHeader, Quantity "block" Natural))]
mkBlockHeaderHeights nodeChain =
    [ (hash, (hdr, height))
    | ((hash, hdr), height) <- zip (headerIds nodeChain) [Quantity 1..] ]

{-------------------------------------------------------------------------------
                              Test chain functions
-------------------------------------------------------------------------------}

-- | Create a mapping from test chain block ids to block headers.
headerIds :: [BlockHeader] -> [(Hash "BlockHeader", BlockHeader)]
headerIds bs = [(prevBlockHash b, a) | (a, b) <- zip bs (tail bs)]

-- | Tip of a test chain is the penultimate block.
tipId :: [BlockHeader] -> Maybe (Hash "BlockHeader")
tipId bs = prevBlockHash <$> lastMay bs

-- | A test chain needs at least two headers to have a tip.
hasTip :: [BlockHeader] -> Bool
hasTip = (> 1) . length

-- | Length of a test chain, not including the block header after the tip.
chainLength :: [BlockHeader] -> Int
chainLength bs = max 0 (length bs - 1)

-- | Index of the tip of a chain.
chainEnd :: [BlockHeader] -> Int
chainEnd = maybe 0 (fromIntegral . slotNumber . slotId) . chainTip

chainTip :: [BlockHeader] -> Maybe BlockHeader
chainTip = initMay >=> lastMay

-- | Limit the sequence to a certain size by removing items from the beginning.
limitChain :: Quantity "block" Natural -> [BlockHeader] -> [BlockHeader]
limitChain (Quantity k) bs = drop (max 0 (length bs - fromIntegral k - 1)) bs

-- | Prepend a source chain before a local chain.
spliceChains :: [BlockHeader] -> [BlockHeader] -> [BlockHeader]
spliceChains source localChain = take start source ++ localChain
  where
    start = chainStart localChain

firstSlot :: [BlockHeader] -> Maybe SlotId
firstSlot bs
    | hasTip bs = slotId <$> headMay bs
    | otherwise = Nothing

-- | The slot index at which the local chain starts.
chainStart :: [BlockHeader] -> Int
chainStart = maybe 0 (fromIntegral . slotNumber) . firstSlot

showChain :: [BlockHeader] -> String
showChain [] = ""
showChain bs = mconcat
    [ show ep, ".", show sl, " | "
    , unwords (map showHeaderHash (tail bs)) ]
  where
    SlotId ep sl = slotId (head bs)
    showHeaderHash (BlockHeader _ (Hash h)) = B8.unpack h

{-------------------------------------------------------------------------------
                     Test chain pure model of intersection
-------------------------------------------------------------------------------}

-- | Merge node chain with local unstable blocks.
modelIntersection
    :: Quantity "block" Natural
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
    maybe id drop (min p (p >>= fixup))
    -- ... fetched from the node.
    nodeChain
  where
    p = intersectionPoint localChain nodeChain
    -- The real code under test will fetch the full k if the local chain is
    -- ahead of the node chain. It's not incorrect behaviour, and the situation
    -- should never happen anyway. But we need to add this to the model so that
    -- the tests pass.
    fixup q
        | length nodeChain - 2 > q = Just q
        | otherwise = Nothing

-- | Find the last index of the node chain which is the same as the local chain.
intersectionPoint :: [BlockHeader] -> [BlockHeader] -> Maybe Int
intersectionPoint localChain nodeChain = res >>= checkAfter
  where
    res = fst <$> find (uncurry (==) . snd) pairs

    pairs = zip [0..] $ zip localChain' nodeChain
    localChain' = spliceChains chaff localChain
    chaff = repeat $ BlockHeader (SlotId 0 0) (Hash "x")

    -- The block header *after* a block contains the hash of that block.
    -- So compare that, if it exists.
    checkAfter i | after localChain' == after nodeChain = Just i
                 | otherwise = Nothing
        where after xs | i + 1 < length xs = Just (xs !! (i + 1))
                       | otherwise = Nothing

{-------------------------------------------------------------------------------
                              Test data generation
-------------------------------------------------------------------------------}

-- | Generate an infinite test chain. Take a slice of the list and use 'tipId'
-- and 'headerIds' to access the tip and block headers. The tip of a test chain
-- is the penultimate block.
chain :: String -> [BlockHeader]
chain p = [BlockHeader (SlotId 0 n) (Hash . B8.pack $ p ++ hash n) | n <- [0..]]
  where
    hash 0 = ""
    hash n = show (n - 1)

instance Arbitrary TestCase where
    arbitrary = do
        k <- arbitrary
        let kk = fromIntegral k' where Quantity k' = k
        -- Choose an arbitrary local length proportional to k
        localChainLength <- choose (0, kk * 4)
        -- Choose where the local unstable block headers start from
        localChainStart <- (localChainLength -) <$> choose (0, kk)
        -- Choose an arbitrary node chain length proportional to k
        nodeChainLength <- choose (0, kk * 4)
        -- At some point the node chain changes from a to b
        forkPoint <- choose (localChainStart, localChainLength - 1)
        let chainA = chain "a"
            chainAB = take forkPoint chainA ++ drop forkPoint (chain "b")
        chainB <- elements [ chainA, chainAB ]

        let localChain = take localChainLength $ drop localChainStart chainA
            nodeChain = take nodeChainLength chainB

        pure TestCase{..}

    shrink TestCase{..} =
        [ TestCase k' (take n nodeChain) (take l localChain)
        | (k', n, l) <- shrink (k, length nodeChain, length localChain)
        , n >= 1 ]

instance Arbitrary (Quantity "block" Natural) where
    -- k doesn't need to be large for testing this
    arbitrary = Quantity . fromIntegral <$> choose (2 :: Int, 30)
    shrink (Quantity k) =
        [ Quantity (fromIntegral k')
        | k' <- shrink (fromIntegral k :: Int)
        , k' >= 2 ]

{-------------------------------------------------------------------------------
                 Extra unstable blocks functions for properties
-------------------------------------------------------------------------------}

-- | Shows just the headers of the unstable blocks.
showUnstableBlocks :: UnstableBlocks -> String
showUnstableBlocks ubs = showHeaders ubs ++ " " ++ showHeight ubs
  where
    showHeaders = unwords . map (showHash . fst) . F.toList . getUnstableBlocks
    showHeight (UnstableBlocks _ (Quantity h)) = "height=" ++ show h

showHash :: Hash a -> String
showHash (Hash h) = B8.unpack h
