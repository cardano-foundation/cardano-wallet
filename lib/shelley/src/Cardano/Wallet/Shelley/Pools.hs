{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Haskell-node "shelley" implementation of the @StakePoolLayer@ abstraction,
-- i.e. some boring glue.
module Cardano.Wallet.Shelley.Pools where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiT (..) )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable, NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), GenesisParameters (..), PoolId )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley, toPoint )
import Cardano.Wallet.Shelley.Network
    ( NodePoolLsqData (..) )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage, unsafeRunExceptT )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Data.Map
    ( Map )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Sort
    ( sortOn )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map

-- | Stake Pool Data fields fetched from the node via LSQ
data PoolLsqMetrics = PoolLsqMetrics
    { nonMyopicMemberRewards :: Quantity "lovelace" Word64
    , relativeStake :: Percentage
    , saturation :: Double
    } deriving (Eq, Show, Generic)

combineLsqData
    :: NodePoolLsqData
    -> Map PoolId PoolLsqMetrics
combineLsqData NodePoolLsqData{nOpt, rewards, stake} =
    Map.merge stakeButNoRewards rewardsButNoStake bothPresent stake rewards
  where
    -- calculate the saturation from the relative stake
    sat s = fromRational $ (getPercentage s) / (1 / fromIntegral nOpt)

    -- If we fetch non-myopic member rewards of pools using the wallet
    -- balance of 0, the resulting map will be empty. So we set the rewards
    -- to 0 here:
    stakeButNoRewards = traverseMissing $ \_k s -> pure $ PoolLsqMetrics
        { nonMyopicMemberRewards = Quantity 0
        , relativeStake = s
        , saturation = (sat s)
        }

    rewardsButNoStake = dropMissing

    bothPresent       = zipWithMatched  $ \_k s r -> PoolLsqMetrics r s (sat s)

readBlockProductions :: IO (Map PoolId Int)
readBlockProductions = return Map.empty

--
-- Api Server Handler
--

data StakePoolLayer = StakePoolLayer
    { knownPools :: IO [PoolId]
    , listStakePools :: Coin -> ExceptT ErrNetworkUnavailable IO [Api.ApiStakePool]
    }

newStakePoolLayer
    :: GenesisParameters
    -> NetworkLayer IO (IO Shelley) b
    -> StakePoolLayer
newStakePoolLayer gp nl = StakePoolLayer
    { knownPools = _knownPools
    , listStakePools = _listPools
    }
  where
    dummyCoin = Coin 0

    -- Note: We shouldn't have to do this conversion.
    el = getEpochLength gp
    gh = getGenesisBlockHash gp
    getTip = fmap (toPoint gh el) . liftIO $ unsafeRunExceptT $ currentNodeTip nl

    _knownPools
        :: IO [PoolId]
    _knownPools = do
        pt <- getTip
        res <- runExceptT $ map fst . Map.toList
            . combineLsqData <$> stakeDistribution nl pt dummyCoin
        case res of
            Right x -> return x
            Left _e -> return []


    _listPools
        :: Coin
        -- ^ The amount of stake the user intends to delegate, which may affect the
        -- ranking of the pools.
        -> ExceptT ErrNetworkUnavailable IO [Api.ApiStakePool]
    _listPools s = do
            pt <- liftIO getTip
            map mkApiPool
                . sortOn (Down . nonMyopicMemberRewards . snd)
                . Map.toList
                . combineLsqData
                <$> stakeDistribution nl pt s
      where
        mkApiPool (pid, PoolLsqMetrics prew pstk psat) = Api.ApiStakePool
            { Api.id = (ApiT pid)
            , Api.metrics = Api.ApiStakePoolMetrics
                { Api.nonMyopicMemberRewards = (mapQ fromIntegral prew)
                , Api.relativeStake = Quantity pstk
                , Api.saturation = psat
                , Api.producedBlocks = Quantity 0 -- TODO: Implement
                }
            , Api.metadata = Nothing -- TODO: Implement
            , Api.cost = Quantity 0 -- TODO: Implement
            , Api.margin = Quantity $ unsafeMkPercentage 0 -- TODO: Implement
            }

        mapQ f (Quantity x) = Quantity $ f x
