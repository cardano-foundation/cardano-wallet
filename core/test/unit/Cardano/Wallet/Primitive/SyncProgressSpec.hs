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
    ( TimeInterpreter, singleEraInterpreter, startTime )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance (..), syncProgress )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , EpochLength (..)
    , GenesisParameters (..)
    , Hash (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlotNo (..)
    , StartTime (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( SomeException (..), evaluate, try )
import Control.Monad
    ( forM_ )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( NominalDiffTime, addUTCTime )
import Test.Hspec
    ( Spec, describe, it, pendingWith, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), counterexample, property, withMaxSuccess )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

spec :: Spec
spec = do
    let gp = GenesisParameters
            { getEpochLength = EpochLength 21600
            , getSlotLength  = SlotLength 10
            , getGenesisBlockDate = StartTime (read "2019-11-09 16:43:02 UTC")
            , getGenesisBlockHash = Hash ""
            , getActiveSlotCoefficient = 1
            , getEpochStability = Quantity 10
            }
    let st = SyncTolerance 10


    let ti = (singleEraInterpreter gp :: TimeInterpreter Identity)
    describe "syncProgress" $ do
        it "works for any two slots" $ property $ \tip (dt :: NominalDiffTime) ->
            let
                StartTime t0 = getGenesisBlockDate gp
                t = dt `addUTCTime` t0
            in
                runIdentity (syncProgress st ti tip t) `deepseq` ()

        let mkTip sl = BlockHeader
                { slotNo = sl
                , blockHeight = Quantity 0 -- Not needed
                , headerHash = Hash "-"
                , parentHeaderHash = Hash "-"
                }
        let tolerance = SyncTolerance 0

        it "unit test #1 - 0/10   0%" $ do
            let tip = mkTip (SlotNo 0)
            let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 10
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0)

        it "unit test #2 - 10/20 50%" $ do
            let tip = mkTip (SlotNo 10)
            let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 20
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0.5)

        it "unit test #4 - 10/10 100%" $ do
            let tip = mkTip (SlotNo 10)
            let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 10
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Ready

        it "unit test #4 - 11/10 100%" $ do
            let tip = mkTip (SlotNo 11)
            let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 10
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
                    [ (mkTip (SlotNo 0), 0.0)
                    , (mkTip (SlotNo 1), 0.1)
                    , (mkTip (SlotNo 2), 0.2)
                    , (mkTip (SlotNo 3), 0.3)
                    , (mkTip (SlotNo 4), 0.4)
                    , (mkTip (SlotNo 5), 0.5)
                    , (mkTip (SlotNo 6), 0.6)
                    , (mkTip (SlotNo 7), 0.7)
                    , (mkTip (SlotNo 8), 0.8)
                    , (mkTip (SlotNo 9), 0.9)
                    , (mkTip (SlotNo 10), 1.0)
                    ]
            forM_ plots $ \(nodeTip, p) -> do
                let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 10
                let progress = if p == 1
                        then Ready
                        else Syncing (Quantity $ unsafeMkPercentage p)
                runIdentity
                    (syncProgress tolerance ti nodeTip ntwkTime)
                    `shouldBe` progress

        it "unit test #6 - block height 0" $ do
            pendingWith "new calculation was needed for shelley"
            -- Very short chains on jormungandr will probably see the
            -- syncProgress immediately jump to 90%, and then slowly continue,
            -- but seems like an acceptable sacrifice. The ITN is long anyway.
            let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 10
            let plots =
                    [ (mkTip (SlotNo 8), 0)
                    , (mkTip (SlotNo 9), 0)
                    , (mkTip (SlotNo 10), 1)
                    ]
            forM_ plots $ \(nodeTip, p) -> do
                let progress = if p == 1
                        then Ready
                        else Syncing (Quantity $ unsafeMkPercentage p)
                runIdentity
                    (syncProgress tolerance ti nodeTip ntwkTime)
                    `shouldBe` progress


        it "unit test #7 - 1k/2M 0.5% (regression for overflow issue)" $ do
            let tip = mkTip (SlotNo 1_000)
            let ntwkTime = runIdentity $ ti $ startTime $ SlotNo 2_000_000
            runIdentity (syncProgress tolerance ti tip ntwkTime)
                `shouldBe` Syncing (Quantity $ unsafeMkPercentage 0.0005)

        it "syncProgress should never crash" $ withMaxSuccess 10000
            $ property $ \tip dt -> monadicIO $ do
                let StartTime t0 = getGenesisBlockDate gp
                let t = dt `addUTCTime` t0
                let x = runIdentity $
                        syncProgress tolerance ti tip t
                res <- run (try @SomeException $ evaluate x)
                monitor (counterexample $ "Result: " ++ show res)
                assert (isRight res)


instance Arbitrary BlockHeader where
    shrink _ = []
    arbitrary = arbitrary >>= genBlockHeader

instance Arbitrary SlotNo where
    arbitrary = genSlotNo
    shrink = shrinkSlotNo

instance Arbitrary NominalDiffTime where
    arbitrary = toEnum <$> arbitrary
    shrink x = map toEnum . shrink $ fromEnum x

instance Arbitrary ActiveSlotCoefficient where
    shrink = shrinkActiveSlotCoefficient
    arbitrary = genActiveSlotCoefficient
