{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A self contained module for ranking pools according to the delegation design
-- spec /Design Specification for Delegation and Incentives in Cardano/
-- [(Kant et al, 2019)](https://hydra.iohk.io/build/1389333/download/1/delegation_design_spec.pdf).
--
-- The module currently implements the non-myopic desirability, and might later
-- support calculating the full non-myopic pool member rewards. The latter being
-- the recomended way to rank stake-pools (see section 4.3).
--
-- The term non-myopic is explained on page 37:
--
-- @
-- It would be short-sighted (“myopic”) for stakeholders to directly use the
-- reward splitting formulas from Section 6.5. They should instead take the
-- long-term (“non-myopic”) view. To this end, the system will calculate and
-- display the “non-myopic” rewards that pool leaders and pool members can
-- expect, thus supporting stakeholders in their decision whether to create a
-- pool and to which pool to delegate their stake.
--
-- The idea is to first rank all pools by “desirability”, to then assume that
-- the k most desirable pools will eventually be saturated, whereas all other
-- pools will lose all their members, then to finally base all reward
-- calculations on these assumptions.
-- @
--
-- == Relevant identifiers
--
-- Epoch Parameters
--
-- +------------+-------------+------------------------------------------------+
-- |identifier  | domain      | description                                    |
-- +============+=============+================================================+
-- + R          | @ [0, ∞) @  | total available rewards for the epoch (in ada).|
-- +------------+-------------+------------------------------------------------+
-- + a0         | @ [0, ∞) @  | owner-stake influence on pool rewards.         |
-- +------------+-------------+------------------------------------------------+
-- + k          | @ [0, ∞) @  | desired number of pools                        |
-- +------------+-------------+------------------------------------------------+
-- + z0         | 1/k         | relative stake of a saturated pool             |
-- +------------+-------------+------------------------------------------------+
--
-- Pool's Parameters
--
-- +------------+-------------+------------------------------------------------+
-- |identifier  | domain      | description                                    |
-- +============+=============+================================================+
-- | c          | @ [0, ∞)@   | costs                                          |
-- +------------+-------------+------------------------------------------------+
-- | f          | @ [0, ∞) @  | rewards                                        |
-- +------------+-------------+------------------------------------------------+
-- | m          | @ [0, 1] @  | margin                                         |
-- +------------+-------------+------------------------------------------------+
-- | p_apparent | @ [0, 1] @  | apparent performance                           |
-- +------------+-------------+------------------------------------------------+
-- | s          | @ [0, ∞) @  | relative stake of the pool leader (i.e pledge) |
-- +------------+-------------+------------------------------------------------+
-- | σ          | @ [0, 1] @  | total relative stake of the pool               |
-- +------------+-------------+------------------------------------------------+
module Cardano.Pool.Jormungandr.Ranking
    (
      -- * Formulas
      desirability
    , saturation

    , saturatedPoolRewards
    , saturatedPoolSize

      -- * Types
    , EpochConstants (..)
    , Pool (..)
    , NonNegative (..)
    , Positive (..)
    , unsafeMkPositive
    , unsafeMkNonNegative
    )
    where

import Prelude

import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Data.Quantity
    ( Percentage (..), Quantity (..), percentageToDouble )
import Data.Ratio
    ( (%) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), blockListF', fmt )
import GHC.Generics
    ( Generic )

--------------------------------------------------------------------------------
-- Formulas from spec
--------------------------------------------------------------------------------

-- | Non-myopic pool desirability according to section 5.6.1.
--
-- Is /not/ affected by oversaturation nor pool stake in general.
desirability
    :: EpochConstants
    -> Pool
    -> Double
desirability constants pool
    | f_saturated <= c = 0
    | otherwise        = (f_saturated - c) * (1 - m)
  where
    f_saturated = saturatedPoolRewards constants pool
    m = percentageToDouble $ margin pool
    c = fromIntegral $ getQuantity $ cost pool

-- | The saturation-level of a pool indicate how far a pool is from the
-- desired relative stake fixed by the network. A saturation level above 1
-- means that the pool is satured. A level of 0.5 / 50% means that the pool owns
-- half the ideal stake.
--
-- The saturation corresponds therefore to the ratio between the ideal relative
-- stake of a pool on the actual relative stake of that pool.
--
-- The ideal relative stake is given by:
--
--              1     S     1
--   σ_ideal = --- * --- = --- = z0 (where S stands for the total available
--              S     k     k        stake in Ada)
--
-- which gives us the saturation as:
--
--             σ         σ
--    sat = --------- = ----
--           σ_ideal     z0
--
saturation
    :: EpochConstants
    -> Quantity "lovelace" Word64
        -- ^ Total delegated stake
    -> Quantity "lovelace" Word64
        -- ^ Pool's stake
    -> Double
saturation constants total own =
    σ / z0
  where
    z0 = percentageToDouble $ saturatedPoolSize constants
    _S = fromIntegral $ getQuantity total
    s  = fromIntegral $ getQuantity own
    σ  = s / _S

-- | Total rewards for a pool if it were saturated.
--
-- When a0 = 0 this reduces to just p*R*z0 (tested
-- by @prop_saturatedPoolRewardsReduces@)
saturatedPoolRewards :: EpochConstants -> Pool -> Double
saturatedPoolRewards constants pool =
    let
        a0 = getNonNegative $ leaderStakeInfluence constants
        z0 = percentageToDouble $ saturatedPoolSize constants
        s = percentageToDouble $ leaderStake pool
        _R = fromIntegral $ getQuantity $ totalRewards constants
        p = getNonNegative $ recentAvgPerformance pool
        -- ^ technically \hat{p} in the spec
    in
        (p * _R) / (1 + a0) * (z0 + ((min s z0) * a0))

-- | Determines z0, i.e 1 / k
saturatedPoolSize :: EpochConstants -> Percentage
saturatedPoolSize constants =
    unsafeMkPercentage $
        1 % fromIntegral (getPositive $ desiredNumberOfPools constants)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data EpochConstants = EpochConstants
    { leaderStakeInfluence :: NonNegative Double
      -- ^ a_0
    , desiredNumberOfPools :: Positive Int
      -- ^ k
    , totalRewards :: Quantity "lovelace" Word64
      -- ^ Total rewards in an epoch. "R" in the spec.
    } deriving (Show, Eq, Generic)

instance Buildable EpochConstants where
    build ec = fmt "\n" <> blockListF' "" id
        [ "leaderStakeInfluence (a0) =  "
            <> build (getNonNegative $ leaderStakeInfluence ec)
        , "desiredNumberOfPool (k) = "
            <> build (getPositive $ desiredNumberOfPools ec)
        , "totalRewards (R) = "
            <> build (getQuantity $ totalRewards ec)
        ]

data Pool = Pool
    { leaderStake :: Percentage
      -- ^ s
    , cost :: Quantity "lovelace" Word64
      -- ^ c
    , margin :: Percentage
      -- ^ m
    , recentAvgPerformance :: NonNegative Double
      -- ^ \hat{p}, an already averaged (apparent) performance-value.
      --
      -- Should mostly be in the range [0, 1]. May be higher than 1 due to
      -- randomness.
    } deriving (Show, Eq, Generic)

newtype Positive a = Positive { getPositive :: a }
    deriving (Generic, Eq, Show)
    deriving newtype (Ord, Num)

unsafeMkPositive :: (Ord a, Show a, Num a) => a -> (Positive a)
unsafeMkPositive x
    | x > 0    = Positive x
    | otherwise = error $ "unsafeMkPositive: " ++ show x ++ " > 0 does not hold"

newtype NonNegative a = NonNegative { getNonNegative :: a }
    deriving (Generic, Eq, Show)
    deriving newtype (Ord, Num)

unsafeMkNonNegative :: (Ord a, Show a, Num a) => a -> (NonNegative a)
unsafeMkNonNegative x
    | x >= 0    = NonNegative x
    | otherwise = error $ "unsafeMkNegative: " ++ show x ++ " >= 0 does not hold"
