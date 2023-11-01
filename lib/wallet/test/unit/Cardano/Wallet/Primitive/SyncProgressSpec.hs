{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Primitive.SyncProgressSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Gen
    ( genActiveSlotCoefficient
    , genBlockHeader
    , genSlotNo
    , shrinkActiveSlotCoefficient
    , shrinkSlotNo
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , interpretQuery
    , mkSingleEraInterpreter
    , slotToRelTime
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    , SyncTolerance (..)
    , syncProgress
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , EpochLength (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage
    )
import Control.DeepSeq
    ( deepseq
    )
import Control.Monad
    ( forM_
    )
import Data.Either
    ( isRight
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , counterexample
    , property
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )
import UnliftIO.Exception
    ( SomeException (..)
    , evaluate
    , try
    )

spec :: Spec
spec = do
    let t0 = read "2019-11-09 16:43:02 UTC"
    let sp = SlottingParameters
            { getEpochLength = EpochLength 21_600
            , getSlotLength  = SlotLength 10
            , getActiveSlotCoefficient = 1
            , getSecurityParameter = Quantity 2_160
            }
    let st = SyncTolerance 10

    let ti = (mkSingleEraInterpreter (StartTime t0) sp :: TimeInterpreter Identity)
    let runQry = runIdentity . interpretQuery ti
    describe "syncProgress" $ do
        it "works for any two slots" $ property $ \tip (dt :: RelativeTime) ->
            runIdentity (syncProgress st ti tip dt) `deepseq` ()
        let tolerance = SyncTolerance 0

        it "unit test #1 - 0/10   0%" $ do
            let tip = (SlotNo 0)
            let ntwkTime = runQry $ slotToRelTime $ SlotNo 10
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0)

        it "unit test #2 - 10/20 50%" $ do
            let tip = SlotNo 10
            let ntwkTime = runQry $ slotToRelTime $ SlotNo 20
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0.5)

        it "unit test #4 - 10/10 100%" $ do
            let tip = SlotNo 10
            let ntwkTime = runQry $ slotToRelTime $ SlotNo 10
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Ready

        it "unit test #4 - 11/10 100%" $ do
            let tip = SlotNo 11
            let ntwkTime = runQry $ slotToRelTime $ SlotNo 10
            runIdentity (syncProgress tolerance ti tip ntwkTime)
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
            let plots =
                    [ (SlotNo 0, 0.0)
                    , (SlotNo 1, 0.1)
                    , (SlotNo 2, 0.2)
                    , (SlotNo 3, 0.3)
                    , (SlotNo 4, 0.4)
                    , (SlotNo 5, 0.5)
                    , (SlotNo 6, 0.6)
                    , (SlotNo 7, 0.7)
                    , (SlotNo 8, 0.8)
                    , (SlotNo 9, 0.9)
                    , (SlotNo 10, 1.0)
                    ]
            forM_ plots $ \(nodeTip, p) -> do
                let ntwkTime = runQry $ slotToRelTime $ SlotNo 10
                let progress = if p == 1
                        then Ready
                        else Syncing (Quantity $ unsafeMkPercentage p)
                runIdentity
                    (syncProgress tolerance ti nodeTip ntwkTime)
                    `shouldBe` progress

        it "unit test #7 - 1k/2M 0.5% (regression for overflow issue)" $ do
            let tip = SlotNo 1_000
            let ntwkTime = runQry $ slotToRelTime $ SlotNo 2_000_000
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0.000_5)

        it "syncProgress should never crash" $ withMaxSuccess 10_000
            $ property $ \tip dt -> monadicIO $ do
                let x = runIdentity $ syncProgress tolerance ti tip dt
                res <- run (try @IO @SomeException $ evaluate x)
                monitor (counterexample $ "Result: " ++ show res)
                assert (isRight res)

instance Arbitrary BlockHeader where
    shrink _ = []
    arbitrary = arbitrary >>= genBlockHeader

instance Arbitrary SlotNo where
    arbitrary = genSlotNo
    shrink = shrinkSlotNo

-- Arbitrary instance with whole second values.
instance Arbitrary RelativeTime where
    arbitrary = RelativeTime . fromIntegral <$> arbitrary @Int
    shrink (RelativeTime t) =
        RelativeTime . fromIntegral <$> shrink (floor @_ @Int t)

instance Arbitrary ActiveSlotCoefficient where
    shrink = shrinkActiveSlotCoefficient
    arbitrary = genActiveSlotCoefficient
