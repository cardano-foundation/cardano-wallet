{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module defines necessary bits to compute an averaged apparent
-- performance for stake pools. The definition is inspired from the "Design
-- Specification for Delegation and Incentives in Cardano" by Kant et Al, yet
-- deviates slightly from it because of different real constraints:
--
--     a) The wallet software doesn't have a reliable access to various epoch
--     details such as the stake distribution for past epochs.
--
--     b) The wallet software makes therefore a significant use of the current
--     epoch's data which represents unfinished snapshots of epochs which, because
--     of their non-deterministic nature, may favor leaders elected early in the
--     process more than those not yet elected.
--
-- To cope with these two issues, we try to harmonize past known epochs with the
-- ongoing one by averaging over a whole range of epochs, simultaneously. This
-- means computing a ratio of the total blocks produced by a leader on the total
-- number of blocks we could reasonably expect a leader to have produced, across
-- many epochs at once (i.e., doing a ratio of sums, instead of a sum of
-- ratios).
--
-- This should be less punitive for leaders that have not yet been elected as
-- part of the ongoing epoch while having still performed reasonably okay in the
-- past.
module Cardano.Pool.Performance
    ( EpochStats (..)
    , apparentPerformance
    , readPoolsPerformances

    -- * Helpers
    , count
    ) where


import Prelude

import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , EpochNo (..)
    , PoolId
    , SlotId (..)
    , SlotNo (..)
    )
import Control.Monad
    ( forM )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map

data EpochStats = EpochStats
    { poolProduction :: !Natural
    , poolStake :: !Natural
    , totalStake :: !Natural
    , epochHeight :: !Natural
    } deriving (Generic, Show)

-- We consider the following variables, indexed by a given epoch
--
-- - n(e) = number of blocks produced in an epoch e
-- - N(e) = number of slots in e
-- - s(e) = stake owned by the pool in e
-- - S(e) = total stake delegated to pools in e
-- - ε(e) = active slot coeff, i.e. % of slots for which a leader can be elected
--
-- From this, we define `μ` as the _reasonably expectable block production_:
--
--     μ(e) =
--       ε(e) * N(e) * s(e) / S(e)
--
-- Intuitively, @μ@ corresponds to the number of blocks a particular pool should
-- have produced provided the election was exactly proportional to its relative
-- stake.
--
-- This gives us the apparent performance @p@ across many epochs as:
--
--          i
--          Σ n(i)
--     p =  -----
--          i
--          Σ μ(i)
apparentPerformance
    :: Double
        -- ^ ε, considered constant across epochs
    -> [EpochStats]
        -- ^ Epoch statistics, for a given pool
    -> Double
        -- ^ Average performance
apparentPerformance _     [] = 0
apparentPerformance ε epochs =
    sum (n <$> epochs) / sum (μ <$> epochs)
  where
    n = double . poolProduction
    μ e = ε * _N * s / _S
      where
        _N = double (epochHeight e)
        _S = double (totalStake e)
        s  = double (poolStake e)

-- | Read pool performances of many epochs from the database
readPoolsPerformances
    :: Monad m
    => DBLayer m
    -> ActiveSlotCoefficient
    -> EpochLength
    -> SlotId
    -> m (Map PoolId Double)
readPoolsPerformances DBLayer{..} (ActiveSlotCoefficient ε) epLen tip = do
    stats <- atomically $ forM historicalEpochs $ \ep -> mkEpochStats
        <$> (count <$> readPoolProduction ep)
        <*> (Map.fromList <$> readStakeDistribution ep)
        <*> pure (slotsInEpoch ep)
    pure $ apparentPerformance ε <$> Map.unionsWith (<>) stats
  where
    mkEpochStats
        :: Map PoolId (Quantity "block" Word64)
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Natural
        -> Map PoolId [EpochStats]
    mkEpochStats mProduction mStake epHeight =
        let
            productionButNoStake =
                dropMissing
            stakeButNoProduction =
                traverseMissing (\_ s -> pure [mkEpochStats_ (Quantity 0) s])
            stakeAndProduction =
                zipWithMatched (\_ p s -> [mkEpochStats_ p s])
        in Map.merge
            productionButNoStake
            stakeButNoProduction
            stakeAndProduction
            mProduction
            mStake
      where
        epTotalStake = sumQ mStake

        mkEpochStats_
            :: Quantity "block" Word64
            -> Quantity "lovelace" Word64
            -> EpochStats
        mkEpochStats_ (Quantity epProduction) (Quantity epStake) = EpochStats
            { poolProduction = fromIntegral epProduction
            , poolStake = fromIntegral epStake
            , totalStake = epTotalStake
            , epochHeight = epHeight
            }

    sumQ :: Integral a => Map k (Quantity "lovelace" a) -> Natural
    sumQ = fromIntegral . Map.foldl' (\a (Quantity b) -> a + b) 0

    historicalEpochs :: [EpochNo]
    historicalEpochs
        | currentEpoch > window = [currentEpoch - window .. currentEpoch]
        | otherwise             = [0..currentEpoch]
      where
        window = 14
        currentEpoch = tip ^. #epochNumber

    slotsInEpoch :: EpochNo -> Natural
    slotsInEpoch e =
        if e == tip ^. #epochNumber
        then fromIntegral $ unSlotNo $ tip ^. #slotNumber
        else fromIntegral $ unEpochLength epLen

--------------------------------------------------------------------------------
-- Internals / Helpers
--------------------------------------------------------------------------------

-- | Count elements inside a 'Map'
count :: Map k [a] -> Map k (Quantity any Word64)
count = Map.map (Quantity . fromIntegral . length)

double :: Integral a => a -> Double
double = fromIntegral
