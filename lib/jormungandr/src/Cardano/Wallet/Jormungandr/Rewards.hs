{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.Jormungandr.Rewards
    (
      rewardParamsFromBlock0
    , calculateRewards
    , applyLimit
    , rewardsAt
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( EpochNo (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )

import Cardano.Wallet.Jormungandr.Binary

-- Rewards
--
-- Based on the information in:
-- https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/incentives.md

rewardParamsFromBlock0
    :: Block
    -> Maybe (RewardFormula, TreasuryTax, Maybe Ratio)
rewardParamsFromBlock0 block =
    let
        params = (fragments block) >>= \case
                Initial ps -> ps
                _ -> []

        rewardParams = flip mapMaybe params $ \case
                ConfigRewardFormula x -> Just x
                _ -> Nothing

        treasuryParams = flip mapMaybe params $ \case
                ConfigTreasuryTax x -> Just x
                _ -> Nothing

        rewardLimitByAbsStake = flip mapMaybe params $ \case
                RewardLimitByAbsoluteStake x -> Just x
                _ -> Nothing
    in
        case (rewardParams, treasuryParams) of
            ([rp], [tp]) ->
                let
                    mrl = case rewardLimitByAbsStake of
                        [rl] -> Just rl
                        _ -> Nothing
                in
                    return (rp, tp, mrl)
            _ -> Nothing

calculateRewards
    :: (RewardFormula, TreasuryTax, Maybe Ratio)
    -> EpochNo
       -- ^
    -> Quantity "lovelace" Double
       -- ^ The total active stake
    -> Double
calculateRewards (rewardParams, treasury, limit) epoch totalStake =
    subtractTreasuryTax treasury
    $ applyLimit limit totalStake
    $ rewardsAt epoch rewardParams

-- | Limit rewards by a fraction of the total active stake
applyLimit
    :: Maybe Ratio
    -> Quantity "lovelace" Double
    -> Double
    -> Double
applyLimit (Just (Ratio a b)) (Quantity totalStake) = min (fromIntegral a * totalStake / fromIntegral b)
applyLimit Nothing _ = id

rewardsAt
    :: EpochNo
    -> RewardFormula
    -> Double
rewardsAt
    (EpochNo currentEpoch)
    (LinearFormula (RewardParams initial ratio epochStart epochRate)) =
    max 0 $
    (fromIntegral initial) - (ratio' * (fromIntegral @Int . floor $ (elapsed / epochRate')))
  where
    ratio' = let Ratio a b = ratio in fromIntegral a / fromIntegral b
    elapsed :: Double
    elapsed = fromIntegral $ max 0 (fromIntegral currentEpoch - epochStart)
    epochRate' = fromIntegral epochRate
rewardsAt _ (HalvingFormula _) = error "rewardsAt: to be implemented"

-- * Linear formula: `C - ratio * (#epoch after epoch_start / epoch_rate)`
-- | NOTE: Care to make sure this calculation has "full precision" were NOT
-- taken. To be used for an estimate.
subtractTreasuryTax :: TreasuryTax -> Double -> Double
subtractTreasuryTax (TreasuryTax fixed ratio) x =
    (x - fromIntegral fixed) * (1 - (rNum / rDen))
  where
    Ratio rNum' rDen' = ratio
    rNum = fromIntegral rNum'
    rDen = fromIntegral rDen'
