{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Rewards
    (
      RewardFormula (..)
    , RewardParams (..)
    , TaxParameters (..)
    , RewardLimit (..)
    , PoolCapping (..)

    , rewardsAt
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( EpochNo (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Ratio
    ( Ratio )
import Data.Word
    ( Word32, Word64 )
import GHC.Generics
    ( Generic )

data RewardParams = RewardParams
    { rFixed :: Word64
        -- ^ In the linear formula, it represents the starting point of the
        -- contribution at #epoch=0, whereas in halving formula is used as starting
        -- constant for the calculation.

    , rRatio :: Ratio Word64
        -- ^ In the halving formula, an effective value between 0.0 to 1.0
        -- indicates a reducing contribution, whereas above 1.0 it indicate an
        -- acceleration of contribution.
        --
        -- However in linear formula the meaning is just a scaling factor for the
        -- epoch zone (current_epoch - start_epoch / epoch_rate). Further
        -- requirement is that this ratio is expressed in fractional form
        -- (e.g. 1/2), which allow calculation in integer form.

    , rEpochStart :: Word32
        -- ^ indicates when this contribution start. note that if the epoch is
        -- not the same or after the epoch_start, the overall contribution is zero.

    , rEpochRate :: Word32
        -- ^ the rate at which the contribution is tweaked related to epoch.

    } deriving (Show, Eq, Generic)

instance NFData RewardParams

ratioToDouble :: Ratio Word64 -> Double
ratioToDouble = fromRational . toRational

data RewardFormula
    = HalvingFormula RewardParams
    | LinearFormula RewardParams
    deriving (Show, Eq, Generic)

instance NFData RewardFormula

data TaxParameters = TaxParameters
    { taxFixed :: Word64
        -- ^ A fix value taken from the total
    , taxRatio :: Ratio Word64
        -- ^ An extra percentage taken from the total
    , taxLimit :: Maybe Word64
        -- ^ It is possible to add a max bound to the total value taken at each
        -- cut.
    } deriving (Generic, Eq, Show)

instance NFData TaxParameters

-- | limit the epoch total reward drawing limit to a portion of the total
-- active stake of the system.
--
-- for example, if set to 10%, the reward drawn will be bounded by the
-- 10% of the total active stake.
--
-- this value is optional, the default is no reward drawing limit
data RewardLimit
    = RewardLimitNone
    | RewardLimitByAbsoluteStake (Ratio Word64)
    deriving (Show, Eq, Generic)

instance NFData RewardLimit

-- | settings to incentivize the numbers of stake pool to be registered
-- on the blockchain.
--
-- These settings does not prevent more stake pool to be added. For example
-- if there is already 1000 stake pools, someone can still register a new
-- stake pool and affect the rewards of everyone else too.
--
-- if the threshold is reached, the pool doesn't really have incentive to
-- create more blocks than 1 / set-value-of-pools % of stake.
--
-- this value is optional, the default is no pool participation capping
data PoolCapping = PoolCapping
    { minParticipation :: Word32
    , maxParticipation :: Word32
    } deriving (Show, Eq, Generic)

instance NFData PoolCapping

rewardsAt
    :: (RewardLimit, Quantity "lovelace" Word64)
    -> TaxParameters
    -> EpochNo
    -> RewardFormula
    -> Quantity "lovelace" Word64
rewardsAt limit tax epochNo = q . taxCut tax . capDrawing limit . \case
    LinearFormula  params | ep < rEpochStart params -> 0
    LinearFormula  params -> linearAbsorption params
    HalvingFormula params | ep < rEpochStart params -> 0
    HalvingFormula params -> halvingAbsorption params
  where
    q :: Double -> Quantity "lovelace" Word64
    q = Quantity . round

    ep = fromIntegral (unEpochNo epochNo)

    linearAbsorption RewardParams{rFixed,rRatio,rEpochStart,rEpochRate} =
        if a > c then 0 else c - a
      where
        a = r * fromIntegral (floor @Double @Integer (n / e))
        c = fromIntegral rFixed
        r = ratioToDouble rRatio
        n = fromIntegral (ep - rEpochStart)
        e = fromIntegral rEpochRate

    halvingAbsorption RewardParams{rFixed,rRatio,rEpochStart,rEpochRate} =
        c * a
      where
        a = r ** fromIntegral (floor @Double @Integer (n / e))
        c = fromIntegral rFixed
        r = ratioToDouble rRatio
        n = fromIntegral (ep - rEpochStart)
        e = fromIntegral rEpochRate

    taxCut TaxParameters{taxFixed,taxRatio,taxLimit} x =
        if cut > x then 0 else x - cut
      where
        cut = maybe id max lim $ c + r * x
        lim = fromIntegral <$> taxLimit
        r = ratioToDouble taxRatio
        c = fromIntegral taxFixed

    capDrawing = \case
        (RewardLimitNone, _) -> id
        (RewardLimitByAbsoluteStake lim, Quantity totalStake) ->
            let
                r  = ratioToDouble lim
                _S = fromIntegral totalStake
            in
                min (r * _S)
