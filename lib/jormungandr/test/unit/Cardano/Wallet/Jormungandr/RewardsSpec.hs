{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.RewardsSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Rewards
    ( Ratio (..)
    , RewardFormula (..)
    , RewardLimit (..)
    , RewardParams (..)
    , TaxParameters (..)
    , rewardsAt
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), choose, property, withMaxSuccess, (===) )

spec :: Spec
spec = describe "rewardsAt" $ do
    it "try-out ITN parameters" $ property $ withMaxSuccess 100000 $ \epochNo ->
        let
            drawingLimit =
                ( RewardLimitByAbsoluteStake (Ratio 4109589 10000000000)
                , Quantity 10548000000000000
                )
            treasuryTax = TaxParameters
                { taxFixed = 0
                , taxRatio = Ratio 1 10
                , taxLimit = Nothing
                }
            epochStart = EpochNo 1
            rewardFormula = LinearFormula $ RewardParams
                { rFixed = 3835616440000
                , rEpochRate = 1
                , rEpochStart = fromIntegral $ unEpochNo epochStart
                , rRatio = Ratio 0 1
                }
        in
            rewardsAt drawingLimit treasuryTax epochNo rewardFormula
                ===
            if epochNo < epochStart
            then Quantity 0
            else Quantity 3452054796000

    describe "Halving, C = 10000, ratio = 1/1, estart=10, rate=1" $ do
        let rewardFormula = HalvingFormula $ RewardParams
                { rFixed = 10000
                , rEpochRate = 1
                , rEpochStart = 10
                , rRatio = Ratio 1 1
                }
        mapM_ (testReward noDrawingLimit noTreasuryTax rewardFormula) $ mconcat
            [ [ (e, 0) | e <- [0..9]  ]
            , [ (10, 10000) ]
            , [ (11, 10000) ]
            , [ (12, 10000) ]
            , [ (13, 10000) ]
            , [ (14, 10000) ]
            ]

    describe "Linear, C = 10000, ratio = 1000/1, estart=10, rate=2" $ do
        let rewardFormula = LinearFormula $ RewardParams
                { rFixed = 10000
                , rEpochRate = 2
                , rEpochStart = 10
                , rRatio = Ratio 1000 1
                }
        mapM_ (testReward noDrawingLimit noTreasuryTax rewardFormula) $ mconcat
            [ [ (e, 0) | e <- [0..9]  ]
            , [ (10, 10000) ]
            , [ (11, 10000) ]
            , [ (12, 9000) ]
            , [ (13, 9000) ]
            , [ (14, 8000) ]
            ]

    describe "Halving, C = 10000, ratio = 1/2, estart=10, rate=2" $ do
        let rewardFormula = HalvingFormula $ RewardParams
                { rFixed = 10000
                , rEpochRate = 2
                , rEpochStart = 10
                , rRatio = Ratio 1 2
                }
        mapM_ (testReward noDrawingLimit noTreasuryTax rewardFormula) $ mconcat
            [ [ (e, 0) | e <- [0..9]  ]
            , [ (10, 10000) ]
            , [ (11, 10000) ]
            , [ (12, 5000) ]
            , [ (13, 5000) ]
            , [ (14, 2500) ]
            ]
  where
    testReward drawingLimit treasuryTax rewardFormula (epochNo, reward) =
        it title $
            rewardsAt drawingLimit treasuryTax epochNo rewardFormula
                `shouldBe` Quantity reward
      where
        title = "epoch #" <> show (unEpochNo epochNo) <> " --> " <> show reward

    noDrawingLimit =
        ( RewardLimitNone
        , Quantity 0
        )

    noTreasuryTax = TaxParameters
        { taxFixed = 0
        , taxRatio = Ratio 0 1
        , taxLimit = Nothing
        }

instance Arbitrary EpochNo where
    arbitrary = EpochNo . fromIntegral @Int <$> choose (0, 1000)
