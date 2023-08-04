{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Use camelCase" -}
module Cardano.Pool.RankSpec
  ( spec
  )
where

import Cardano.Pool.Rank
  ( RedelegationWarning (..)
  , RewardInfoPool (..)
  , RewardParams (..)
  , StakePoolsSummary (..)
  , redelegationWarning
  )
import Cardano.Pool.Types
  ( PoolId (..)
  )
import Cardano.Wallet.Gen
  ( genPercentage
  )
import Cardano.Wallet.Primitive.Types
  ( EpochNo (..)
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..)
  )
import Cardano.Wallet.Primitive.Types.Coin.Gen
  ( genCoin
  )
import Data.ByteArray.Encoding
  ( Base (Base16)
  , convertToBase
  )
import Data.ByteString qualified as BS
import Data.Map.Strict as Map
import Data.Quantity
  ( clipToPercentage
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , arbitrarySizedBoundedIntegral
  , choose
  , forAll
  , frequency
  , getPositive
  , liftArbitrary
  , oneof
  , property
  , vector
  , (=/=)
  )
import Prelude

spec :: Spec
spec = do
  describe "Relegation warning" $ do
    it "Fresh delegation never yields 'OtherPoolsBetter'"
      $ property prop_freshDelegation
    it "'performanceEstimate' in 'StakePoolsSummary' is ignored"
      $ property prop_ignorePerformanceEstimate

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
prop_freshDelegation :: RewardParams -> EpochNo -> Coin -> Property
prop_freshDelegation rp now user =
  forAll (genRewardInfoPool rp) $ \info ->
    forAll (genStakePoolsSummary rp) $ \ps ->
      redelegationWarning now (info, user) ps now =/= OtherPoolsBetter

prop_ignorePerformanceEstimate :: RewardParams -> EpochNo -> Coin -> Property
prop_ignorePerformanceEstimate rp now user =
  forAll (genRewardInfoPool rp) $ \info ->
    forAll (genStakePoolsSummary rp) $ \ps ->
      let
        ps' =
          ps
            { pools =
                Map.map
                  (\i -> i {performanceEstimate = undefined})
                  (pools ps)
            }
      in
        property
          $ (redelegationWarning (EpochNo 0) (info, user) ps' now)
          `seq` True

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
genRewardInfoPool :: RewardParams -> Gen RewardInfoPool
genRewardInfoPool RewardParams {totalStake} = do
  stake <- chooseCoin (Coin 0, totalStake)
  owner <- chooseCoin (Coin 0, stake)
  let
    stakeRelative = clipToPercentage (stake `proportionTo` totalStake)
    ownerStakeRelative = clipToPercentage (owner `proportionTo` totalStake)
    ownerStake = owner
  ownerPledge <-
    oneof
      [pure (Coin 0), chooseCoin (Coin 0, owner), chooseCoin (owner, stake)]
  cost <- genCoin
  margin <- genPercentage
  performanceEstimate <- choose (0, 1)
  pure $ RewardInfoPool {..}

proportionTo :: Coin -> Coin -> Rational
proportionTo _ (Coin 0) = 0
proportionTo (Coin x) (Coin y) = fromIntegral x / fromIntegral y

genStakePoolsSummary :: RewardParams -> Gen StakePoolsSummary
genStakePoolsSummary rp =
  StakePoolsSummary rp <$> liftArbitrary (genRewardInfoPool rp)

instance Arbitrary PoolId where
  arbitrary = PoolId . convertToBase Base16 . BS.pack <$> vector 16

chooseCoin :: (Coin, Coin) -> Gen Coin
chooseCoin (Coin a', Coin b') =
  Coin . fromIntegral
    <$> frequency [(1, pure a), (1, pure b), (5, choose (a, b))]
  where
    a = fromIntegral a' :: Integer
    b = fromIntegral b' :: Integer

instance Arbitrary Coin where
  arbitrary = genCoin

instance Arbitrary RewardParams where
  arbitrary =
    RewardParams
      <$> (getPositive <$> arbitrary)
      <*> (getPositive <$> arbitrary)
      <*> arbitrary
      <*> oneof [pure $ Coin 0, arbitrary]

instance Arbitrary EpochNo where
  arbitrary = EpochNo <$> arbitrarySizedBoundedIntegral
