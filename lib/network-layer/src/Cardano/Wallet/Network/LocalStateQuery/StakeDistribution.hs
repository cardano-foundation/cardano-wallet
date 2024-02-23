{-# LANGUAGE GADTs #-}
-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- A local state query that retrieves the stake distribution.
--
module Cardano.Wallet.Network.LocalStateQuery.StakeDistribution
    ( stakeDistribution
    ) where

import Prelude

import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( onAnyEra
    , shelleyBased
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromNonMyopicMemberRewards
    , fromPoolDistr
    , optimumNumberOfPools
    , toShelleyCoin
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId
    )
import Cardano.Wallet.Primitive.Types.StakePoolSummary
    ( StakePoolsSummary (..)
    )
import Control.Applicative
    ( liftA3
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Percentage
    ( Percentage
    )
import Data.Set
    ( Set
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{-----------------------------------------------------------------------------
    Local State Query for the Stake Distribution
------------------------------------------------------------------------------}
type LSQ' = LSQ (CardanoBlock StandardCrypto) IO

stakeDistribution :: W.Coin -> LSQ' (Maybe StakePoolsSummary)
stakeDistribution coin =
    liftA3
        (liftA3 StakePoolsSummary)
        getNOpt
        (queryNonMyopicMemberRewards coin)
        stakeDistr

stakeDistr :: LSQ' (Maybe (Map PoolId Percentage))
stakeDistr =
    shelleyBased
        (fromPoolDistr <$> LSQry Shelley.GetStakeDistribution)

getNOpt :: LSQ' (Maybe Int)
getNOpt =
    onAnyEra
        (pure Nothing)
        (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
        (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
        (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
        (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
        (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)
        (Just . optimumNumberOfPools <$> LSQry Shelley.GetCurrentPParams)

queryNonMyopicMemberRewards :: W.Coin -> LSQ' (Maybe (Map PoolId W.Coin))
queryNonMyopicMemberRewards coin =
    shelleyBased
        $ (getRewardMap . fromNonMyopicMemberRewards)
            <$> LSQry (Shelley.GetNonMyopicMemberRewards coins)
  where
    coins :: Set (Either Coin a)
    coins = Set.singleton $ Left $ toShelleyCoin coin

    fromJustRewards =
        fromMaybe
            ( error
                "stakeDistribution: requested rewards\
                \ not included in response"
            )

    getRewardMap
        :: Map (Either W.Coin W.RewardAccount) (Map PoolId W.Coin)
        -> Map PoolId W.Coin
    getRewardMap =
        fromJustRewards . Map.lookup (Left coin)
