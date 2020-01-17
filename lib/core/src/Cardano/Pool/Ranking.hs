{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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
module Cardano.Pool.Ranking
    (
      -- * Formulas
      desirability

    , saturatedPoolRewards
    , saturatedPoolSize

      -- * Types
    , EpochConstants (..)
    , Pool (..)
    , Lovelace (..)
    , Ratio
    , unsafeToRatio
    , getRatio
    , unsafeMkRelativeStake
    , NonNegative (..)
    , Positive (..)
    , unsafeToPositive
    , unsafeToNonNegative
    )
    where

import Prelude

import Data.Word
    ( Word64 )
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
    | otherwise    = (f_saturated - c) * (1 - m)
  where
    f_saturated = saturatedPoolRewards constants pool
    m = getRatio $ margin pool
    c = fromIntegral $ getLovelace $ cost pool

-- | Total rewards for a pool if it were saturated.
--
-- When a0 = 0 this reduces to just p*R*z0 (tested
-- by @prop_saturatedPoolRewardsReduces@)
saturatedPoolRewards :: EpochConstants -> Pool -> Double
saturatedPoolRewards constants pool =
    let
        a0 = getNonNegative $ leaderStakeInfluence constants
        z0 = getRatio $ saturatedPoolSize constants
        s = getRatio $ leaderStake pool
        _R = fromIntegral $ getLovelace $ totalRewards constants
        p = getNonNegative $ recentAvgPerformance pool
        -- ^ technically \hat{p} in the spec
    in
        (p * _R) / (1 + a0)
        * (z0 + ((min s z0) * a0))

-- | Determines z0, i.e 1 / k
saturatedPoolSize :: EpochConstants -> Ratio
saturatedPoolSize constants =
    Ratio $ 1 / fromIntegral (getPositive $ desiredNumberOfPools constants)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data EpochConstants = EpochConstants
    { leaderStakeInfluence :: NonNegative Double
      -- ^ a_0
    , desiredNumberOfPools :: Positive Int
      -- ^ k
    , totalRewards :: Lovelace
      -- ^ Total rewards in an epoch. "R" in the spec.
    } deriving (Show, Eq, Generic)

data Pool = Pool
    { leaderStake :: Ratio
      -- ^ s
    , cost :: Lovelace
      -- ^ c
    , margin :: Ratio
      -- ^ m
    , recentAvgPerformance :: NonNegative Double
      -- ^ \hat{p}, an already averaged performance-value.
      --
      -- Should mostly be in the range [0, 1], but lucky pools may exceed 1.
    } deriving (Show, Eq, Generic)

newtype Lovelace = Lovelace { getLovelace :: Word64 }
    deriving (Show, Eq)
    deriving newtype (Ord, Num)

newtype Ratio = Ratio { getRatio :: Double }
    deriving (Show, Eq)
    deriving newtype Ord

unsafeToRatio :: Double -> Ratio
unsafeToRatio x
    | x >= 0 && x <= 1  = Ratio x
    | otherwise         = error $ "unsafeToRatio: " ++ show x
                          ++ "not in range [0, 1]"

unsafeMkRelativeStake :: Lovelace -> EpochConstants -> Ratio
unsafeMkRelativeStake (Lovelace stake) constants =
    unsafeToRatio $ (fromIntegral stake) / total
  where
    total = fromIntegral . getLovelace . totalRewards $ constants

newtype Positive a = Positive { getPositive :: a }
    deriving (Generic, Eq, Show)
    deriving newtype (Ord, Num)

unsafeToPositive :: (Ord a, Show a, Num a) => a -> (Positive a)
unsafeToPositive x
    | x > 0    = Positive x
    | otherwise = error $ "unsafeToPositive: " ++ show x ++ " > 0 does not hold"

newtype NonNegative a = NonNegative { getNonNegative :: a }
    deriving (Generic, Eq, Show)
    deriving newtype (Ord, Num)

unsafeToNonNegative :: (Ord a, Show a, Num a) => a -> (NonNegative a)
unsafeToNonNegative x
    | x >= 0    = NonNegative x
    | otherwise = error $ "unsafeToNegative: " ++ show x ++ " >= 0 does not hold"
