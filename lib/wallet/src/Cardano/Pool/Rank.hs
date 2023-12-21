{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides tools to estimate pool rewards
-- for the purpose of ranking pools.
module Cardano.Pool.Rank
    ( -- * Pool information
      -- $RewardEpochs
      RewardInfoPool (..)
    , RewardParams (..)
    , StakePoolsSummary (..)

    -- * Ranking formulas
    , poolSaturation
    , optimalRewards
    , currentROS
    , saturationROS

    -- * Redelegation warning
    , RedelegationWarning(..)
    , redelegationWarning

    -- * Legacy metrics
    , nonMyopicMemberReward
    , desirability
    , PoolScore (..)
    , scorePools
    )
    where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Data.Map
    ( Map
    )
import Data.Ord
    ( Down (..)
    )
import Data.Percentage
    ( Percentage (..)
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    , listF'
    , mapF
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Percentage as Percentage

{-------------------------------------------------------------------------------
    Pool information necessary to compute rewards
-------------------------------------------------------------------------------}

-- | Information need for the computation of rewards, such as the
-- stake currently delegated to a pool, or the pool cost and margin.
data RewardInfoPool = RewardInfoPool
    { stakeRelative :: Percentage -- ^ sigma = pool stake / total stake
    , ownerPledge :: Coin -- ^ pledge of pool owner(s)
    , ownerStake :: Coin -- ^ absolute stake delegated by pool owner(s)
    , ownerStakeRelative :: Percentage -- ^ s = owner stake / total stake
    , cost :: Coin
    , margin :: Percentage
    , performanceEstimate :: Double
    } deriving (Show, Eq)

instance Buildable RewardInfoPool where
    build RewardInfoPool
            {stakeRelative,ownerPledge,ownerStake,ownerStakeRelative
            ,cost,margin,performanceEstimate
            }
      = listF' id
        [ "Stake (relative): " <> build stakeRelative
        , "Pledge: " <> build ownerPledge
        , "Owner stake: " <> build ownerStake
        , "Owner stake (relative): " <> build ownerStakeRelative
        , "Pool cost: " <> build cost
        , "Pool margin: " <> build margin
        , "Pool performance: " <> build performanceEstimate
        ]

-- | Global parameters used for computing rewards
data RewardParams = RewardParams
    { nOpt :: Int -- ^ desired number of stake pools
    , a0   :: Rational -- ^ influence of the pool owner's pledge on rewards
    , r    :: Coin -- ^ Total rewards available for the given epoch
    , totalStake :: Coin -- ^ Maximum lovelace supply minus treasury
    } deriving (Show, Eq)
    -- NOTE: In the ledger, @a0@ has type 'NonNegativeInterval'.

instance Buildable RewardParams where
    build RewardParams{nOpt,a0,r,totalStake} = blockListF' "" id
        [ "Desired number of stake pools: " <> build nOpt
        , "Pledge influence parameter, a0: " <> build a0
        , "Total rewards for this epoch: " <> build r
        , "Total stake: " <> build totalStake
        ]

{- $RewardEpochs

NOTE [RewardEpochs]

We need to be careful to show the right information at the right time
in order prevent manipulation of the proof-of-stake (PoS) protocol.

In particular, we need to show those pool costs, margins, and owner stakes
that will affect the rewards for a delegation choice made at the present moment.

The reward cycle is illustrated in Section 11.2 of SL-D1, here a brief sketch.

@
                    mark   set     go
                  +------.------.------
                  |
    |e0----|e1----|e2----|e3----|e4----|e5----|
              ^
          we are here
@

We imagine that we are in epoch /e1/, and choose to delegate to a pool.
At the end of epoch /e1/, a snapshot of the stake distribution will be taken.
This snapshot will be labeled "mark" during epoch /e2/, "set" during epoch /e3/,
and "go" during epoch /e4/.
Blocks will be produced randomly according to this stake distribution when
it is labeled "set", i.e. in epoch /e3/.
Rewards for this block production will be computed when this stake
distribution is labeled "go", i.e. during epoch /e4/, and these rewards will
paid out at the beginning of epoch /e5/.

The owner stake is part of the snapshot taken at the end of epoch /e1/.
If the pool is newly registered, its cost, margin and pledge are also
immediately available in epoch /e1/. However, if a pool re-registers,
the changes to its cost, margin and pledge will not be visible until the
next epoch; put differently, the rewards for the stake snapshot taken
at the end of epoch /e1/ will only depend on changes to cost, margin,
and pledge that the pool owner initiated in epoch /e0/.
This prevents pool owners from duping delegators by changing pool costs
during an epoch. However, the pool owner could still choose to undelegate
his stake, and fail to meet his pledge at the end of epoch /e1/,
which results in zero rewards paid out at the beginning of /e5/.

To summarize, in order to make an informed delegation choice
during epoch /e1/, the delegator needs to know 'RewardInfoPool' where

* 'stakeRelative', 'ownerStake', and 'ownerStakeRelative' are
   taken at the time of decision (in epoch /e1/).
* 'ownerPledge', 'margin', 'cost' are the values of the last pool
   registration certificate
   from epoch /e0/ in case of an update,
   or from epoch /e1/ in case of a newly created pool.

For the 'performanceEstimate', it's best to estimate it from recent pool
block production using functions provided here.

-}

-- | Summary of stake distribution and stake pools obtained from network.
data StakePoolsSummary = StakePoolsSummary
    { rewardParams :: RewardParams
    , pools :: Map PoolId RewardInfoPool
    } deriving (Show, Eq)

instance Buildable StakePoolsSummary where
    build StakePoolsSummary{rewardParams,pools} = blockListF' "" id
        [ "Global reward parameters: " <> build rewardParams
        , "Individual pools: " <> mapF (Map.toList pools)
        ]

{-------------------------------------------------------------------------------
    Reward formulas
-------------------------------------------------------------------------------}
fractionOf :: RealFrac r => r -> Coin -> Coin
fractionOf r (Coin x) = Coin . floor $ r * fromIntegral x

proportionTo :: Coin -> Coin -> Rational
proportionTo _        (Coin 0) = 0
proportionTo (Coin x) (Coin y) = fromIntegral x / fromIntegral y

z0 :: RewardParams -> Rational
z0 RewardParams{nOpt} = 1 / fromIntegral nOpt

epochsPerYear :: Int
epochsPerYear = 73

-- | The yearly rate of return per unit of stake,
-- assuming that the pool's stake remains at the same level.
-- Rewards compound every epoch.
currentROS :: RewardParams -> RewardInfoPool -> Coin -> Percentage
currentROS rp RewardInfoPool{..} x
    | ownerStake < ownerPledge = Percentage.fromRationalClipped 0
    | otherwise =
        Percentage.fromRationalClipped $ (1 + astar) ^ epochsPerYear - 1
  where
    s = Percentage.fromRationalClipped $
        ownerPledge `proportionTo` (totalStake rp)
    sigma
        = Percentage.toRational stakeRelative
        + (x `proportionTo` totalStake rp)

    astar
        | sigma == 0 = 0
        | otherwise  = shareAfterFees (1/sigma) cost margin fhat
            `proportionTo` totalStake rp
    fhat  = optimalRewards rp s sigma

-- | The (yearly) return per unit of stake
-- for a pool that has reached saturation.
saturationROS :: RewardParams -> RewardInfoPool -> Percentage
saturationROS rp RewardInfoPool{..}
    | ownerStake < ownerPledge = Percentage.fromRationalClipped 0
    | otherwise = Percentage.fromRationalClipped $
        (1 + bstar) ^ epochsPerYear - 1
  where
    s = Percentage.fromRationalClipped $
        ownerPledge `proportionTo` (totalStake rp)
    sigma = z0 rp -- saturation, never = 0

    bstar = shareAfterFees (1/sigma) cost margin fhat
        `proportionTo` totalStake rp
    fhat  = optimalRewards rp s sigma

-- | Non-Myopic Pool Member Rewards
-- according to Eq.(3) of Section 5.6.4 in SL-D1.
nonMyopicMemberReward
    :: RewardParams
    -> RewardInfoPool
    -> Bool -- ^ The pool ranks in the top @nOpt@ pools
    -> Coin -- ^ stake that the member wants to delegate
    -> Coin
nonMyopicMemberReward rp RewardInfoPool{..} isTop tcoin
    | ownerStake < ownerPledge = Coin 0
    | otherwise
        = shareAfterFees memberShare cost margin
        $ (performanceEstimate `fractionOf`)
        $ optimalRewards rp s sigma_nonmyopic
  where
    s = Percentage.fromRationalClipped $
        ownerPledge `proportionTo` (totalStake rp)
    sigma = stakeRelative
    t = tcoin `proportionTo` (totalStake rp)

    memberShare = t / sigma_nonmyopic

    sigma_nonmyopic
        | isTop = max (Percentage.toRational sigma + t) (z0 rp)
        | otherwise = Percentage.toRational s + t

-- | Compute share of 'Coin' after subtracting fixed cost and
-- percentage margin.
shareAfterFees :: Rational -> Coin -> Percentage -> Coin -> Coin
shareAfterFees share cost margin x = case x `Coin.subtract` cost of
    Just y  -> (share * (1 - Percentage.toRational margin)) `fractionOf` y
    Nothing -> Coin 0

-- | Optimal rewards for a stake pool
-- according to Eq.(2) of Section 5.5.3 in SL-D1.
--
-- > optimalRewards s sigma
--
-- NOTE: This computation uses 'Double' internally
-- and is only suitable for the purpose of ranking,
-- not for computing actual monetary rewards.
optimalRewards :: RewardParams -> Percentage -> Rational -> Coin
optimalRewards params s sigma = factor `fractionOf` r params
  where
    factor = 1 / (1 + a0_)
        * ( sigma' + s' * a0_ * (sigma' - s'*(z0_-sigma')/z0_) / z0_ )

    z0_, a0_, sigma', s' :: Double
    z0_    = fromRational (z0 params)
    a0_    = fromRational (a0 params)
    sigma' = min (fromRational sigma) z0_
    s'     = min (fromRational $ Percentage.toRational s) z0_

-- | The desirabilty of a pool is equal to the total
-- member rewards at saturation
-- IF the owner meets their pledge.
desirability :: RewardParams -> RewardInfoPool -> Coin
desirability rp RewardInfoPool{..}
    = shareAfterFees 1 cost margin
    $ (performanceEstimate `fractionOf`)
    $ optimalRewards rp s (z0 rp)
  where
    s = Percentage.fromRationalClipped $
        ownerPledge `proportionTo` (totalStake rp)

-- | The saturation of a pool is the ratio of the current pool stake
-- to the fully saturated stake.
poolSaturation :: RewardParams -> RewardInfoPool -> Double
poolSaturation rp RewardInfoPool{stakeRelative}
    = fromRational (Percentage.toRational stakeRelative) / fromRational (z0 rp)

data PoolScore = PoolScore
    { _desirability :: Coin
    , _nonMyopicMemberReward :: Coin
    }

-- | Compute the desirability and non-myopic rewards for all pools.
--
-- To compute the non-myopic rewards, we need to know all pools
-- in order to rank them by desirability,
-- and we need to know the stake that the user wants to delegate.
scorePools
    :: Ord poolId
    => RewardParams
    -> Map poolId (RewardInfoPool, a)
    -> Coin -- ^ Stake that the user wants to delegate
    -> Map poolId (PoolScore, RewardInfoPool, a)
scorePools params pools t
    = Map.fromList $ zipWith doScore sortedByDesirability areTop
  where
    RewardParams{nOpt} = params
    areTop = replicate nOpt True ++ repeat False

    doScore (d, (pid, pool, a)) isTop = (pid, (score, pool, a))
      where
        score = PoolScore
            { _nonMyopicMemberReward
                = nonMyopicMemberReward params pool isTop t
            , _desirability = d
            }

    sortedByDesirability
        = L.sortOn (Down . fst)
        . map (\(pid,(pool, a)) -> (desirability params pool, (pid, pool, a)))
        $ Map.toList pools

{-------------------------------------------------------------------------------
    Redelegation warning
-------------------------------------------------------------------------------}
data RedelegationWarning
    = AllGood
    | TooFewBlocks
    | OtherPoolsBetter
    deriving (Eq, Show)

-- FIXME: Adapt message to take care of previous epoch.

instance Buildable RedelegationWarning where
    build AllGood = build $ unwords
        [ "The pool to which you had delegated your stake"
        , "gives rewards within expectations."
        ]
    build TooFewBlocks = build $ unwords
        [ "The pool to which you have delegated your stake"
        , "may have not performed as expected,"
        , "please check your delegation choice."
        ]
    build OtherPoolsBetter = build $ unwords
        [ "Other pools may offer higher rewards,"
        , "please check your delegation choice."
        ]

-- | Compute redelegation warning from current pool performance.
--
-- Note: This function uses the 'performanceEstimate' for the pool
-- that we delegate to, but ignores this fields for the argument
-- 'StakePoolsSummary'.
redelegationWarning
    :: EpochNo
        -- ^ Epoch when delegation was made
    -> (RewardInfoPool, Coin)
        -- ^ ( Info about the pool that we delegate to
        --   , absolute stake that we delegate )
    -> StakePoolsSummary
        -- ^ Current summary of all stake pools (for comparison)
    -> EpochNo
        -- ^ Current epoch
    -> RedelegationWarning
redelegationWarning timeOfDelegation (info,user) StakePoolsSummary{..} now
    | (sigma <= 0.6 * s && p < 0.85) || (sigma > 0.6 * s && p < 0.9)
        = TooFewBlocks
    | Percentage.toRational mr < Percentage.toRational mrstar * w
        = OtherPoolsBetter
    | otherwise
        = AllGood
  where
    sigma = Percentage.toRational $ stakeRelative info
    s = 1 / fromIntegral (nOpt rewardParams)
    p = performanceEstimate info

    mr = currentROS rewardParams info user
    mrstar = maximum (mr:returns)
    returns = map (\i -> currentROS rewardParams i user) $ Map.elems pools

    w = dt*dt / (25 + dt*dt) :: Rational
    dt = fromIntegral $ fromEnum now - fromEnum timeOfDelegation
        -- time different in number of epochs.
