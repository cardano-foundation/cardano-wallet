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
-- Copyright: © 2018-2020 IOHK
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
    ( Word32, Word64, Word8 )
import Database.Persist.Class
    ( AtLeastOneUniqueKey (..), OnlyOneUniqueKey (..) )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )
import System.Random
    ( StdGen )

import qualified Cardano.Wallet.DB.Sqlite.Types as W
import qualified Cardano.Wallet.Primitive.Types as W

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- A unique, but arbitrary, value for this particular device
ArbitrarySeed sql=arbitrary_seed
    seedSeed                    StdGen       sql=seed

    deriving Show Generic

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

-- Mapping from pool id to owner
PoolOwner sql=pool_owner
    poolOwnerPoolId     W.PoolId     sql=pool_id
    poolOwnerOwner      W.PoolOwner  sql=pool_owner
    poolOwnerIndex      Word8        sql=pool_owner_index

    Primary poolOwnerPoolId poolOwnerOwner poolOwnerIndex
    Foreign PoolRegistration fk_registration_pool_id poolOwnerPoolId ! ON DELETE CASCADE
    deriving Show Generic

-- Mapping of registration certificate to pool
PoolRegistration sql=pool_registration
    poolRegistrationPoolId  W.PoolId  sql=pool_id
    poolRegistrationSlot    W.SlotId  sql=slot
    poolRegistrationMargin  Word8     sql=margin
    poolRegistrationCost    Word64    sql=cost

    Primary poolRegistrationPoolId
    deriving Show Generic
|]
