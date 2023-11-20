{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.StakePoolSummary
    ( StakePoolsSummary (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.PoolId
    ( PoolId (..)
    )
import Data.Map
    ( Map
    )
import Data.Quantity
    ( Percentage
    )
import Fmt
    ( Buildable (..)
    , listF'
    , mapF
    , pretty
    )

import qualified Data.Map as Map

data StakePoolsSummary = StakePoolsSummary
    { nOpt :: Int
    , rewards :: Map PoolId Coin
    , stake :: Map PoolId Percentage
    }
    deriving (Show, Eq)

instance Buildable StakePoolsSummary where
    build StakePoolsSummary{nOpt, rewards, stake} =
        listF'
            id
            [ "Stake: " <> mapF (Map.toList stake)
            , "Non-myopic member rewards: " <> mapF (Map.toList rewards)
            , "Optimum number of pools: " <> pretty nOpt
            ]
