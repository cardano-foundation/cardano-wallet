{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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

import Cardano.Wallet.Api.Server
    ( LiftHandler (..), apiError )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (..), ApiT (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), GenesisParameters (..), PoolId )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , ShelleyBlock
    , fromPoolDistr
    , fromRewards
    , optimumNumberOfPools
    , toPoint
    , toShelleyCoin
    )
import Cardano.Wallet.Shelley.Network
    ( pattern Cursor )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage, unsafeRunExceptT )
import Control.Monad.Class.MonadSTM
    ( MonadSTM, TQueue )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
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
import Ouroboros.Network.Block
    ( Point )
import Ouroboros.Network.Client.Wallet
    ( LocalStateQueryCmd (..), send )
import Servant
    ( err500 )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Shelley.Ledger as OC

-- | Stake Pool Data fields fetched from the node via LSQ
data PoolLsqMetrics = PoolLsqMetrics
    { nonMyopicMemberRewards :: Quantity "lovelace" Word64
    , stake :: Percentage
    , saturation :: Double
    } deriving (Eq, Show, Generic)


data ErrFetchMetrics = ErrFetchMetrics
  deriving Show

askNode
    :: MonadSTM m
    => TQueue m (LocalStateQueryCmd ShelleyBlock m)
    -> Point ShelleyBlock
    -> Coin
    -> ExceptT ErrFetchMetrics m (Map PoolId PoolLsqMetrics)
askNode queue pt coin = do
    stakeMap <- fromPoolDistr <$> handleQueryFailure
        (queue `send` CmdQueryLocalState pt OC.GetStakeDistribution)
    let toStake = Set.singleton $ Left $ toShelleyCoin coin
    rewardMap <- fromRewards <$> handleQueryFailure
        (queue `send` CmdQueryLocalState pt (OC.GetNonMyopicMemberRewards toStake))
    pparams <- handleQueryFailure
        (queue `send` CmdQueryLocalState pt OC.GetCurrentPParams)

    return $ combine
        (optimumNumberOfPools pparams)
        stakeMap
        rewardMap
  where
    handleQueryFailure = withExceptT (const ErrFetchMetrics) . ExceptT

    combine
        :: Int -- ^ Desired number of pools
        -> Map PoolId Percentage
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Map PoolId PoolLsqMetrics
    combine nOpt =
        Map.merge stakeButNoRew rewardsButNoStake bothPresent
      where
        -- calculate the saturation from the relative stake
        sat s = fromRational $ (getPercentage s) / (1 / fromIntegral nOpt)

        -- Haven't figured out how to fetch non-myopic member rewards properly yet.
        -- Let's provide a default value of 0, at least for now.
        stakeButNoRew     = traverseMissing $ \_k s -> pure $ PoolLsqMetrics
            { nonMyopicMemberRewards = Quantity 0
            , stake = s
            , saturation = (sat s)
            }

        rewardsButNoStake = dropMissing

        bothPresent       = zipWithMatched  $ \_k s r -> PoolLsqMetrics r s (sat s)

readBlockProductions :: IO (Map PoolId Int)
readBlockProductions = return Map.empty

--
-- Api Server Handler
--

instance LiftHandler ErrFetchMetrics where
    handler = \case
        ErrFetchMetrics ->
            apiError err500 NotSynced $ mconcat
                [ "There was a problem fetching metrics from the node."
                ]

data StakePoolLayer = StakePoolLayer
    { knownPools :: IO [PoolId]
    , listStakePools :: Coin -> ExceptT ErrFetchMetrics IO [Api.ApiStakePool]
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
        Cursor _workerTip _ lsqQ <- initCursor nl []
        pt <- getTip
        res <- runExceptT $ map fst . Map.toList <$> askNode lsqQ pt dummyCoin
        case res of
            Right x -> return x
            Left _e -> return []


    _listPools
        :: Coin
        -- ^ The amount of stake the user intends to delegate, which may affect the
        -- ranking of the pools.
        -> ExceptT ErrFetchMetrics IO [Api.ApiStakePool]
    _listPools s = do
            Cursor _workerTip _ lsqQ <- liftIO $ initCursor nl []
            pt <- liftIO getTip
            map mkApiPool
                . sortOn (Down . nonMyopicMemberRewards . snd)
                . Map.toList <$> askNode lsqQ pt s
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
