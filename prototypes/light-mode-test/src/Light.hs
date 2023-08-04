module Light
  ( -- * Overview
    -- $overview

    -- * NetworkLayer — for wallet
    NetworkLayer (..)
  , mkNetworkLayerBlockfrost

    -- ** Blocks and transaction data
  , discoverTransactions
  , lightSync
  , BlockSummary (..)
  , ChainFollower (..)

    -- ** Stake pools
  , StakePoolsSummary (..)
  , RewardParams (..)
  , RewardInfoPool (..)

    -- ** Submitting transactions
  , SealedTx

    -- ** Reward accounts
  , RewardAccount

    -- ** Network information
  , ProtocolParameters (..)

    -- * NetworkLayer — for stake pools

    -- | implementable
  , getPoolBlocks
  , getPoolHistory
  , _blockBlockVrf

    -- * Types exported for readability
  , Coin
  , ChainPoint (..)
  , SyncProgress
  , Pool
  , Light.Types.PoolId
  )
where

import Blockfrost.Client as BF
import Control.Monad
  ( void
  )
import Light.ReadBlocks
import Light.StakePools
import Light.SubmitTx
import Light.Types

-- $overview
--
-- This modules presents various data types used in @cardano-wallet@
-- and checks whether they can implemented / filled with
-- data provied by http://blockfrost.io
--
-- Possible statuses of implementability are:
--
-- * done — this function is implemented here, in this prototype.
-- * yes — this function can be implemented,
--   but was skipped as the implementation seemed straightforward.
-- * implementable — this function can be implemented,
--  though may require additional thought or queries in cardano-graphql.
-- * __no__ — this function cannot be implemented as is.
-- * (internal) — this type is shown here for readability.
--
-- __Caveats__ are highlighted in bold.

{-----------------------------------------------------------------------------
    NetworkLayer
------------------------------------------------------------------------------}

-- | Interface for network capabilities.
--
-- Fields are listed in order of perceived difficulty.
-- Some types have been changed or removed for clarity.
data NetworkLayer m = NetworkLayer
  { chainSync
      :: ChainFollower m ChainPoint
      -> m ()
  -- ^ done, but only for __sequential address derivation__.
  -- Also, __payment address__ needs to be queriable directly.
  , stakeDistribution
      :: m StakePoolsSummary
  -- ^ implementable.
  -- Pool performance estimation can be copied from ledger code.
  , postTx
      :: SealedTx
      -> m ()
  -- ^ implementable, but it is unclear whether we can be __notified__
  -- if the transaction is rejected by the node or times out.
  , getCachedRewardAccountBalance
      :: RewardAccount
      -> m Coin
  -- ^ implementable, redundant.
  , fetchRewardAccountBalances
      :: RewardAccount
      -> m Coin
  -- ^ implementable.
  , currentNetworkTip
      :: m ChainPoint
  -- ^ yes
  , currentNodeEra
      :: m ()
  -- ^ implementable
  , currentProtocolParameters
      :: m ProtocolParams
  -- ^ yes
  , currentNodeProtocolParameters
      :: m ProtocolParams
  -- ^ yes, redundant
  , currentSlottingParameters
      :: m ()
  -- ^ implementable
  , watchNodeTip
      :: (ChainPoint -> m ())
      -> m ()
  -- ^ implementable using __polling__.
  , timeInterpreter
      :: (
         )
  -- ^ implementable. Converts slot numbers to wall clock time.
  , syncProgress
      :: ChainPoint
      -> m SyncProgress
  -- ^ __no__, but fortunately only important for UX.
  -- We can track the number of addresses
  -- discovered so far, but we cannot know the total number of
  -- addresses associated with a wallet in advance.
  }

mkNetworkLayerBlockfrost :: NetworkLayer BlockfrostClient
mkNetworkLayerBlockfrost = nl
  where
    nl =
      NetworkLayer
        { chainSync = Light.ReadBlocks.lightSync
        , stakeDistribution = undefined
        , postTx = void . BF.submitTx
        , getCachedRewardAccountBalance =
            fetchRewardAccountBalances nl
        , fetchRewardAccountBalances =
            fmap BF._accountInfoRewardsSum . BF.getAccount
        , currentNodeEra = pure ()
        , currentNetworkTip = fromBlock <$> BF.getLatestBlock
        , currentProtocolParameters = BF.getLatestEpochProtocolParams
        , currentNodeProtocolParameters = currentProtocolParameters nl
        , currentSlottingParameters = pure ()
        , watchNodeTip = \_ -> pure ()
        , timeInterpreter = ()
        , syncProgress = \_ -> pure 0
        }

{-----------------------------------------------------------------------------
    ProtocolParameters
------------------------------------------------------------------------------}

-- | Protocol parameters.
--
-- Some types have been changed or removed for clarity.
data ProtocolParameters = ProtocolParameters
  { decentralizationLevel
      :: (
         )
  -- ^ yes, '_protocolParamsDecentralisationParam'
  , txParameters
      :: (
         )
  -- ^ yes, '_protocolParamsMaxTxSize', …
  , desiredNumberOfStakePools
      :: Integer
  -- ^ yes, '_protocolParamsNOpt'
  , minimumUTxOvalue
      :: Lovelaces
  -- ^ yes, '_protocolParamsMinUtxo'
  , stakeKeyDeposit
      :: Lovelaces
  -- ^ yes, '_protocolParamsKeyDeposit'
  , eras
      :: Int
  -- ^ implementable, '_protocolParamsEpoch'
  , maximumCollateralInputCount
      :: Integer
  -- ^ yes, '_protocolParamsMaxCollateralInputs'
  , minimumCollateralPercentage
      :: Integer
  -- ^ yes, '_protocolParamsCollateralPercent'
  , executionUnitPrices
      :: Maybe Double
  -- ^ implementable, '_protocolParamsPriceStep', '_protocolParamsPriceMem'
  }
