module Light.StakePools where

import Data.Map
    ( Map
    )
import Light.Types

-- | Summary of stake distribution and stake pools obtained from network
data StakePoolsSummary = StakePoolsSummary
    { rewardParams :: RewardParams -- ^ implementable, see 'RewardParams'
    , pools :: Map PoolId RewardInfoPool -- ^ implementable, See 'RewardInfoPool'
    }

-- | Global parameters used for computing rewards
data RewardParams = RewardParams
    { nOpt :: Int -- ^ yes, '_protocolParamsNOpt'
    , a0   :: Rational -- ^ yes, '_protocolParamsA0'
    , r    :: Coin
    -- ^ implementable, '_epochInfoFees', '_accountInfoReservesSum'?
    , totalStake :: Coin -- ^ yes, '_genesisMaxLovelaceSupply'
    }

-- | Information need for the computation of rewards, such as the'
-- stake currently delegated to a pool, or the pool cost and margin.'
data RewardInfoPool = RewardInfoPool
    { stakeRelative :: Rational -- ^ yes, '_poolStakeDistributionAmount'
    , ownerPledge :: Coin -- ^ yes, '_poolInfoDeclaredPledge'
    , ownerStake :: Coin -- ^ yes (?), '_poolInfoLivePledge'
    , ownerStakeRelative :: Rational -- ^ yes, redundant
    , cost :: Coin -- ^ yes, '_poolInfoFixedCost'
    , margin :: Rational -- ^ yes, '_poolInfoMarginCost'
    , performanceEstimate :: Double
    -- ^ implementable, 'getPoolHistory', 'PoolHistory', '_poolHistoryBlocks'
    }
