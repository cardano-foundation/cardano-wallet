{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.

module Cardano.Pool.DB.Sqlite.TH where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( sqlSettings' )
import Data.Word
    ( Word32, Word64 )
import Database.Persist.Class
    ( AtLeastOneUniqueKey (..), OnlyOneUniqueKey (..) )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )

import qualified Cardano.Wallet.DB.Sqlite.Types as W
import qualified Cardano.Wallet.Primitive.Types as W

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- The set of stake pools that produced a given block
PoolProduction sql=pool_production
    poolProductionPoolId         W.PoolId     sql=pool_id
    poolProductionSlot           W.SlotId     sql=slot
    poolProductionHeaderHash     W.BlockId    sql=header_hash
    poolProductionParentHash     W.BlockId    sql=parent_header_hash
    poolProductionBlockHeight    Word32       sql=block_height

    Primary poolProductionSlot
    deriving Show Generic

-- Stake distribution for each stake pool
StakeDistribution sql=stake_distribution
    stakeDistributionPoolId     W.PoolId     sql=pool_id
    stakeDistributionEpoch      Word64       sql=epoch
    stakeDistributionStake      Word64       sql=stake

    Primary stakeDistributionPoolId stakeDistributionEpoch
    deriving Show Generic
|]
