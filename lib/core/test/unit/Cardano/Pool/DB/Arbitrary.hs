{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..)
    ) where

import Prelude

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( genesisParameters )
import Cardano.Wallet.Primitive.Types
    ( EpochLength (..)
    , PoolId (..)
    , SlotId (..)
    , SlotParameters (..)
    , slotSucc
    )
import Control.Monad
    ( foldM )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Ord
    ( Down (..) )
import Data.Word
    ( Word32 )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , choose
    , elements
    , shuffle
    , vectorOf
    )

import qualified Data.ByteString as BS
import qualified Data.List as L

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

data StakePoolsFixture = StakePoolsFixture
    { poolSlots :: [(PoolId, SlotId)]
    , rollbackSlots :: [SlotId] }
    deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
                                 Stake Pools
-------------------------------------------------------------------------------}

instance Arbitrary SlotId where
    shrink (SlotId ep sl) =
        uncurry SlotId <$> shrink (ep, sl)
    arbitrary = SlotId
        <$> choose (0, fromIntegral arbitraryEpochLength)
        <*> choose (0, fromIntegral arbitraryChainLength)

arbitraryEpochLength :: Word32
arbitraryEpochLength = 100

arbitraryChainLength :: Word32
arbitraryChainLength = 10

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 32 bytes
    shrink x = [zeros | x /= zeros]
      where
        zeros = PoolId $ BS.pack $ replicate 32 0

instance Arbitrary StakePoolsFixture where
    arbitrary = do
        poolsNumber <- choose (1, 100)
        pools <- vectorOf poolsNumber arbitrary
        slotsNumber <- choose (0, 200)
        firstSlot <- arbitrary
        slotsGenerated <-
            foldM (appendPair pools) [] (generateNextSlots [firstSlot] slotsNumber)
        rNum <- choose (1, slotsNumber + 1)
        rSlots <-
            (L.sortOn Down . take rNum) <$> shuffle (map snd slotsGenerated)
        pure $ StakePoolsFixture slotsGenerated rSlots

      where
        epochLength :: EpochLength
        epochLength = genesisParameters ^. #getEpochLength

        sp :: SlotParameters
        sp = SlotParameters
            epochLength
            (genesisParameters ^. #getSlotLength)
            (genesisParameters ^. #getGenesisBlockDate)

        generateNextSlots :: [SlotId] -> Int -> [SlotId]
        generateNextSlots slots@(s:_) num =
            if (num < 1) then
                reverse slots
            else
                generateNextSlots ((slotSucc sp s):slots) (num - 1)
        generateNextSlots [] _ = []

        appendPair
            :: [PoolId]
            -> [(PoolId, SlotId)]
            -> SlotId
            -> Gen [(PoolId, SlotId)]
        appendPair pools pairs slot = do
            pool <- elements pools
            return $ (pool,slot):pairs
