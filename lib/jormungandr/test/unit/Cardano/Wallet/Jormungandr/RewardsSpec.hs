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
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), choose, property, (===) )

spec :: Spec
spec = describe "rewardsAt" $ do
    it "try-out ITN parameters" $ property $ \epochNo ->
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
            rewardFormula = LinearFormula $ RewardParams
                { rFixed = 3835616440000
                , rEpochRate = 1
                , rEpochStart = 1
                , rRatio = Ratio 0 1
                }
        in
            rewardsAt drawingLimit treasuryTax epochNo rewardFormula
                === Quantity 3452054796000

instance Arbitrary EpochNo where
    arbitrary = EpochNo . fromIntegral @Int <$> choose (0, 1000)
