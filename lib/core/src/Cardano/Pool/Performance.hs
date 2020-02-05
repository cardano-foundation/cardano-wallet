{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
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

    -- * Logging
    , PerformanceLog (..)

    -- * Helpers
    , count
    ) where


import Prelude

import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..), PoolId (..))
import Control.Monad
    ( forM )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Cardano.Wallet.Unsafe (unsafeFromHex)
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..), Tracer (..), traceWith )
import Data.Quantity
    ( Quantity (..) )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Data.Text.Class (ToText (..))
import Fmt
    ( pretty, Buildable (..), fmt, blockListF' )
import Numeric.Natural
    ( Natural )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data EpochStats = EpochStats
    { poolProduction :: !Natural
    , poolStake :: !Natural
    , totalStake :: !Natural
    , totalProduction :: !Natural
    } deriving (Generic, Show, Eq)

instance Buildable EpochStats where
    build es = fmt "\n" <> blockListF' "" id
        [ "poolProduction = "
            <> build (showT $ poolProduction es)
        , "poolStake = "
            <> build (showT $ poolStake es)
        , "totalStake = "
            <> build (showT $ totalStake es)
        , "resulting relativeStake = "
            <> build (a / b)
        , "totalProduction = "
            <> build (showT $ totalProduction es)
        ]
      where
        showT = T.pack . show
        a :: Double
        a = (d (poolStake es))
        b :: Double
        b = d (totalStake es)

        d = fromIntegral

-- We consider the following variables, indexed by a given epoch
--
-- - n(e) = number of blocks produced by the pool in e
-- - N(e) = total number of blocks produced in e
-- - s(e) = stake owned by the pool in e
-- - S(e) = total stake delegated to pools in e
--
-- From this, we define `μ` as the _reasonably expectable block production_:
--
--     μ(e) =
--       N(e) * s(e) / S(e)
--
-- Intuitively, @μ@ corresponds to the number of blocks a particular pool should
-- have produced provided the election was exactly proportional to its relative
-- stake.
--
-- This gives us the apparent performance @p@ across many epochs as:
--
--              Σ n(i)
--     p =  --------------
--          max 1 (Σ μ(i))
--
-- Note that we take the maximum between @μ@ and 1 in order to cope with very
-- small pools that would look like they're over performing if their chance of
-- producing at least one block in the observed window (14 epochs here) is far
-- smaller than one.
apparentPerformance
    :: [EpochStats]
        -- ^ Epoch statistics, for a given pool
    -> Double
        -- ^ Average performance
apparentPerformance     [] = 0
apparentPerformance epochs =
    sum (n <$> epochs) / max 1 (sum (μ <$> epochs))
  where
    n = double . poolProduction
    μ e = _N * s / _S
      where
        _N = double (totalProduction e)
        _S = double (totalStake e)
        s  = double (poolStake e)

-- | Read pool performances of many epochs from the database
readPoolsPerformances
    :: MonadIO m
    => DBLayer m
    -> EpochNo
    -> Tracer IO PerformanceLog
    -> m (Map PoolId Double)
readPoolsPerformances DBLayer{..} currentEpoch tr = do
    stats <- atomically $ forM historicalEpochs $ \ep -> do
        prod <- count <$> readPoolProduction ep
        stake <- Map.fromList <$> readStakeDistribution ep
        let stats = mkEpochStats prod stake

        -- FOR DEBUGGING
        let p = PoolId $ unsafeFromHex "08b46d4bc5d8110c76dde2cc7a4c9dda0830c4434dedd7fcd32b1a4815c1a4f3"
        case Map.lookup p stats of
            Just x -> do
                let debugPerf = apparentPerformance x
                liftIO $ traceWith tr (MsgUsingEpochStats x debugPerf)
            Nothing -> return ()

        return stats
    pure $ apparentPerformance <$> Map.unionsWith (<>) stats
  where
    mkEpochStats
        :: Map PoolId (Quantity "block" Word64)
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Map PoolId [EpochStats]
    mkEpochStats mProduction mStake =
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
        epTotalProduction = sumQ mProduction

        mkEpochStats_
            :: Quantity "block" Word64
            -> Quantity "lovelace" Word64
            -> EpochStats
        mkEpochStats_ (Quantity epProduction) (Quantity epStake) = EpochStats
            { poolProduction = fromIntegral epProduction
            , poolStake = fromIntegral epStake
            , totalStake = epTotalStake
            , totalProduction  = epTotalProduction
            }

    sumQ :: Integral a => Map k (Quantity any a) -> Natural
    sumQ = fromIntegral . Map.foldl' (\a (Quantity b) -> a + b) 0

    historicalEpochs :: [EpochNo]
    historicalEpochs
        | currentEpoch > window = [currentEpoch - window .. currentEpoch]
        | otherwise             = [0..currentEpoch]
      where
        window = 14

--------------------------------------------------------------------------------
-- Internals / Helpers
--------------------------------------------------------------------------------

-- | Count elements inside a 'Map'
count :: Map k [a] -> Map k (Quantity any Word64)
count = Map.map (Quantity . fromIntegral . length)

double :: Integral a => a -> Double
double = fromIntegral

---
--- Logging
---

data PerformanceLog = MsgUsingEpochStats [EpochStats] Double
    deriving (Show, Eq)

instance DefinePrivacyAnnotation PerformanceLog
instance DefineSeverity PerformanceLog where
    defineSeverity = \case
        MsgUsingEpochStats _ _ -> Info

instance ToText PerformanceLog where
    toText = \case
        MsgUsingEpochStats s p -> mconcat
            [ "Epoch stats"
            , pretty s
            , ", perf: "
            , pretty p
            ]

