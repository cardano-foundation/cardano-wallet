{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Primitive.SyncProgressSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Gen
    ( genActiveSlotCoefficient
    , genBlockHeader
    , genSlotId
    , shrinkActiveSlotCoefficient
    )
import Cardano.Wallet.Primitive.Slotting
    ( SlotParameters (..), unsafeEpochNo )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance (..), syncProgress )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SlotLength (..)
    , StartTime (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( SomeException (..), evaluate, try )
import Control.Monad
    ( forM_, when )
import Data.Either
    ( isRight )
import Data.Quantity
    ( Quantity (..) )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , choose
    , counterexample
    , forAll
    , property
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

spec :: Spec
spec = do
    let sp = SlotParameters
            { getEpochLength = EpochLength 21600
            , getSlotLength  = SlotLength 10
            , getGenesisBlockDate = StartTime (read "2019-11-09 16:43:02 UTC")
            , getActiveSlotCoefficient = 1
            }
    let st = SyncTolerance 10
    describe "syncProgress" $ do
        it "works for any two slots" $ property $ \sl0 sl1 ->
            syncProgress st sp sl0 sl1 `deepseq` ()

        let mockBlockHeader sl h = BlockHeader
                { slotId = sl
                , blockHeight = Quantity h
                , headerHash = Hash "-"
                , parentHeaderHash = Hash "-"
                }

        let mockSlotParams f = SlotParameters
                { getEpochLength = EpochLength 10
                , getSlotLength = SlotLength 10
                , getGenesisBlockDate = StartTime $
                    read "2019-01-01 00:00:00 UTC"
                , getActiveSlotCoefficient = f
                }

        let syncTolerance = SyncTolerance 0

        it "unit test #1 - 0/10   0%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 0 0) 0
            let ntwkTip = SlotId 1 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0)

        it "unit test #2 - 10/20 50%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 1 0) 10
            let ntwkTip = SlotId 2 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0.5)

        it "unit test #3 - 12/15 80% " $ do
            let slotParams = mockSlotParams 0.5
            let nodeTip = mockBlockHeader (SlotId 2 2) 12
            let ntwkTip = SlotId 2 8
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0.8)

        it "unit test #4 - 10/10 100%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 1 0) 10
            let ntwkTip = SlotId 1 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Ready

        it "unit test #5 - 11/10 100%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 1 1) 11
            let ntwkTip = SlotId 1 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Ready

        --   100 |                          +  +
        --       |
        --       |                       +
        --       |                    +
        --       |              +  +
        --       |
        --       |           +
        --       |     +  +
        --       |  +
        --       |
        --       |--|--|--|--|--|--|--|--|--|--|
        --       0  1  2  3  4  5  6  7  8  9  10
        --
        it "unit test #5 - distribution over several slots" $ do
            let slotParams = mockSlotParams 0.5
            let ntwkTip = SlotId 1 0
            let plots =
                    [ (mockBlockHeader (SlotId 0 0) 0, 0.0)
                    , (mockBlockHeader (SlotId 0 1) 1, 0.2)
                    , (mockBlockHeader (SlotId 0 2) 2, 1 % 3)
                    , (mockBlockHeader (SlotId 0 3) 2, 1 % 3)
                    , (mockBlockHeader (SlotId 0 4) 2, 0.40)
                    , (mockBlockHeader (SlotId 0 5) 3, 0.60)
                    , (mockBlockHeader (SlotId 0 6) 3, 0.60)
                    , (mockBlockHeader (SlotId 0 7) 4, 2 % 3)
                    , (mockBlockHeader (SlotId 0 8) 5, 5 % 6)
                    , (mockBlockHeader (SlotId 0 9) 5, 1.00)
                    , (mockBlockHeader (SlotId 1 0) 5, 1.00)
                    ]
            forM_ plots $ \(nodeTip, p) -> do
                let progress = if p == 1
                        then Ready
                        else Syncing (Quantity $ unsafeMkPercentage p)
                syncProgress syncTolerance slotParams nodeTip ntwkTip
                    `shouldBe` progress

        it "unit test #6 - block height 0" $ do
            let slotParams = mockSlotParams 0.5
            let ntwkTip = SlotId 1 0
            let plots =
                    [ (mockBlockHeader (SlotId 0 8) 0, 0)
                    , (mockBlockHeader (SlotId 0 9) 0, 0)
                    , (mockBlockHeader (SlotId 1 0) 0, 1)
                    ]
            forM_ plots $ \(nodeTip, p) -> do
                let progress = if p == 1
                        then Ready
                        else Syncing (Quantity $ unsafeMkPercentage p)
                syncProgress syncTolerance slotParams nodeTip ntwkTip
                    `shouldBe` progress

        it "syncProgress should never crash" $ withMaxSuccess 10000
            $ property $ \f bh -> do
                let slotParams = mockSlotParams f
                let el = getEpochLength slotParams
                let genSlotIdPair = (,) <$> (genSlotId el) <*> (genSlotId el)
                forAll genSlotIdPair $ \(walSlot, netSlot) -> monadicIO $ do
                    let nodeTip = mockBlockHeader walSlot bh
                    let x = syncProgress syncTolerance slotParams nodeTip netSlot
                    when (netSlot > walSlot) $ do
                        res <- run (try @SomeException $ evaluate x)
                        monitor (counterexample $ "Result: " ++ show res)
                        assert (isRight res)


instance Arbitrary BlockHeader where
    shrink _ = []
    arbitrary = arbitrary >>= genBlockHeader

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = do
        ep <- choose (0, 10)
        sl <- choose (0, 100)
        return (SlotId (unsafeEpochNo ep) (SlotInEpoch sl))

instance Arbitrary ActiveSlotCoefficient where
    shrink = shrinkActiveSlotCoefficient
    arbitrary = genActiveSlotCoefficient
