{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides a maximum likelhood estimator for pool performance.
--
-- Copy & paste of the original implementation in
--
-- @cardano-ledger/eras/shelley/impl/src/Cardano/Ledger/Shelley/PoolRank.hs@
--
-- The copy was made in order to reduce (transitive) dependencies.
module Cardano.Pool.Rank.Likelihood
    ( -- * Pool performance estimate from historical block production
      BlockProduction (..)
    , PerformanceEstimate (..)
    , estimatePoolPerformance

    -- * Likelihood computations
    , LogWeight (..)
    , Likelihood (..)
    , likelihood
    , applyDecay
    , Histogram (..)
    , percentile'
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , DecentralizationLevel
    , EpochLength (..)
    , SlottingParameters (..)
    , getFederationPercentage
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Foldable
    ( find
    )
import Data.Function
    ( on
    )
import Data.List
    ( foldl'
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Sequence
    ( Seq
    )
import Data.Sequence.Strict
    ( StrictSeq
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Numeric.Natural
    ( Natural
    )
import Quiet

import qualified Data.Percentage as Percentage
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq

{-------------------------------------------------------------------------------
    Estimating pool performance
-------------------------------------------------------------------------------}
-- | Information about block production of a pool in one epoch.
data BlockProduction = BlockProduction
    { blocksProduced :: !Natural
        -- ^ Blocks produced in the given epoch.
    , stakeRelative :: !Rational
        -- ^ Relative stake of the pool that was relevant for block production.
        -- (i.e. from the "set" snapshot).
    }

-- | Estimate the performance of a pool from historical block production data.
--
-- Assumes that the 'SlottingParameters' are constant through the given
-- history.
estimatePoolPerformance
    :: SlottingParameters
    -> DecentralizationLevel
    -> Seq BlockProduction
        -- ^ Historical block production data. Most recent data comes /first/.
        -- Recent performance weighs more than past performance:
        --
        -- * Block production from > 25 epochs ago has less than 10% influence
        -- on the likelihoods.
        -- * Block production from > 50 epochs ago has less than 1% influence
        -- on the likelihoods and can be ignored.
    -> PerformanceEstimate
estimatePoolPerformance sp d history =
    percentile' $ foldl' considerEpoch mempty (Seq.reverse history)
  where
    considerEpoch li perf = applyDecay decayFactor li <> likelihood' perf

    prob perf = leaderProbability
        (toActiveSlotCoeff $ getActiveSlotCoefficient sp)
        (stakeRelative perf)
        (Percentage.toRational $ getFederationPercentage d)
    likelihood' perf = likelihood
        (blocksProduced perf)
        (prob perf)
        (fromIntegral $ unEpochLength $ getEpochLength sp)

decayFactor :: Float
decayFactor = 0.9

{-------------------------------------------------------------------------------
    Support types
    for copy, to avoid changing it too much
-------------------------------------------------------------------------------}
type EpochSize = Integer
type UnitInterval = Rational
type PositiveUnitInterval = Rational

unboundRational :: Rational -> Rational
unboundRational = id

toActiveSlotCoeff :: ActiveSlotCoefficient -> ActiveSlotCoeff
toActiveSlotCoeff (ActiveSlotCoefficient x) = ActiveSlotCoeff (realToFrac x)

newtype ActiveSlotCoeff = ActiveSlotCoeff
    { activeSlotVal :: PositiveUnitInterval }

{-------------------------------------------------------------------------------
    Copied material
    Almost exact copy, except for
    * missing from/toCBOR instances
    * reimannSum -> riemannSum 🤓
-------------------------------------------------------------------------------}
-- ---- begin copy ----
newtype LogWeight = LogWeight {unLogWeight :: Float}
  deriving (Eq, Generic, Ord, Num, NFData, NoThunks)
  deriving (Show) via Quiet LogWeight

toLogWeight :: Double -> LogWeight
toLogWeight d = LogWeight (realToFrac $ log d)

fromLogWeight :: LogWeight -> Double
fromLogWeight (LogWeight l) = exp (realToFrac l)

newtype Histogram = Histogram {unHistogram :: StrictSeq LogWeight}
  deriving (Eq, Show, Generic)

newtype Likelihood = Likelihood {unLikelihood :: StrictSeq LogWeight}
  -- TODO: replace with small data structure
  deriving (Show, Ord, Generic, NFData)

instance NoThunks Likelihood

instance Eq Likelihood where
  (==) = (==) `on` unLikelihood . normalizeLikelihood

instance Semigroup Likelihood where
  (Likelihood x) <> (Likelihood y) =
    normalizeLikelihood $ Likelihood (StrictSeq.zipWith (+) x y)

instance Monoid Likelihood where
  mempty = Likelihood $ StrictSeq.forceToStrict $ Seq.replicate (length samplePositions) (LogWeight 0)

normalizeLikelihood :: Likelihood -> Likelihood
normalizeLikelihood (Likelihood xs) = Likelihood $ (\x -> x - m) <$> xs
  where
    m = minimum xs

leaderProbability :: ActiveSlotCoeff -> Rational -> UnitInterval -> Double
leaderProbability activeSlotCoeff relativeStake decentralizationParameter =
  (1 - (1 - asc) ** s) * (1 - d')
  where
    d' = realToFrac . unboundRational $ decentralizationParameter
    asc = realToFrac . unboundRational . activeSlotVal $ activeSlotCoeff
    s = realToFrac relativeStake

samplePositions :: StrictSeq Double
samplePositions = (\x -> (x + 0.5) / 100.0) <$> StrictSeq.fromList [0.0 .. 99.0]

likelihood ::
  Natural -> -- number of blocks produced this epoch
  Double -> -- chance we're allowed to produce a block in this slot
  EpochSize ->
  Likelihood
likelihood blocks t slotsPerEpoch =
  Likelihood $
    sample <$> samplePositions
  where
    -- The likelihood function L(x) is the probability of observing the data we got
    -- under the assumption that the underlying pool performance is equal to x.
    -- L(x) = C(n,m) * (tx)^n * (1-tx)^m
    -- where
    -- t is the chance we're allowed to produce a block
    -- n is the number of slots in which a block was produced
    -- m is the number of slots in which a block was not produced
    --      (slots per epoch minus n)
    -- C(n,m) is a coefficient that will be irrelevant
    -- Since the likelihood function only matters up to a scalar multiple, we will
    -- will divide out C(n,m) t^n and use the following instead:
    -- L(x) = x^n * (1-tx)^m
    -- We represent this function using 100 sample points, but to avoid very
    -- large exponents, we store the log of the value instead of the value itself.
    -- log(L(x)) = log [ x^n * (1-tx)^m ]
    --           = n * log(x) + m * log(1 - tx)
    -- TODO: worry more about loss of floating point precision
    --
    -- example:
    -- a pool has relative stake of 1 / 1,000,000 (~ 30k ada of 35b ada)
    -- f = active slot coefficient = 1/20
    -- t = 1 - (1-f)^(1/1,000,000)
    n = fromIntegral blocks
    m = fromIntegral $ slotsPerEpoch - fromIntegral blocks
    l :: Double -> Double
    l x = n * log x + m * log (1 - t * x)
    sample position = LogWeight (realToFrac $ l position)

-- | Decay previous likelihood
applyDecay :: Float -> Likelihood -> Likelihood
applyDecay decay (Likelihood logWeights) = Likelihood $ mul decay <$> logWeights
  where
    mul x (LogWeight f) = LogWeight (x * f)

posteriorDistribution :: Histogram -> Likelihood -> Histogram
posteriorDistribution (Histogram points) (Likelihood likelihoods) =
  normalize $
    Histogram $ StrictSeq.zipWith (+) points likelihoods

-- | Normalize the histogram so that the total area is 1
normalize :: Histogram -> Histogram
normalize (Histogram values) = Histogram $ (\x -> x - logArea) <$> values'
  where
    m = maximum values
    values' = (\x -> x - m) <$> values
    logArea = toLogWeight area
    area = riemannSum 0.01 (fromLogWeight <$> values')

-- | Calculate the k percentile for this distribution.
-- k is a value between 0 and 1. The 0 percentile is 0 and the 1 percentile is 1
percentile :: Double -> Histogram -> Likelihood -> PerformanceEstimate
percentile p prior likelihoods =
  PerformanceEstimate . fst $
    fromMaybe (1, 1) $
      find (\(_x, fx) -> fx > p) cdf
  where
    (Histogram values) = posteriorDistribution prior likelihoods
    cdf =
      Seq.zip
        (StrictSeq.fromStrict samplePositions)
        (StrictSeq.fromStrict (StrictSeq.scanl (+) 0 (fromLogWeight <$> values)))

percentile' :: Likelihood -> PerformanceEstimate
percentile' = percentile 0.5 h
  where
    h = normalize . Histogram $ logBeta 40 1 <$> samplePositions
    -- Beta(n,m)(x) = C * x^(n-1)*(1-x)^(m-1)
    -- log( Beta(n,m)(x) ) = (n-1) * log x + (m-1) * log (1-x)
    logBeta n m x = LogWeight . realToFrac $ (n - 1) * log x + (m - 1) * log (1 - x)

riemannSum :: (Functor f, Foldable f) => Double -> f Double -> Double
riemannSum width heights = sum $ fmap (width *) heights

-- | This is a estimate of the proportion of allowed blocks a pool will
-- make in the future. It is used for ranking pools in delegation.
newtype PerformanceEstimate = PerformanceEstimate {unPerformanceEstimate :: Double}
  deriving (Show, Eq, Generic, NoThunks)
-- ---- end copy ----
