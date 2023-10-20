{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Cardano.Pool.Types
    ( PoolId (..)
    , StakePoolTicker
    )
import Cardano.Slotting.Slot
    ( SlotNo
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( sqlSettings'
    )
import Cardano.Wallet.Primitive.Types
    ()
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Data.Word
    ( Word32
    , Word64
    , Word8
    )
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    )
import GHC.Generics
    ( Generic (..)
    )
import System.Random
    ( StdGen
    )

import qualified Cardano.Pool.Types as P
import qualified Cardano.Wallet.DB.Sqlite.Types as W
import qualified Cardano.Wallet.Primitive.Types as W

share
    [ mkPersist sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

InternalState sql=internal_state
    lastGCMetadata                   POSIXTime Maybe   sql=last_gc_metadata

    deriving Show Generic

Settings sql=settings
    settingsPoolMetadataSource       W.PoolMetadataSource   sql=metadata_source

    deriving Show Generic

-- A unique, but arbitrary, value for this particular device
ArbitrarySeed sql=arbitrary_seed
    seedSeed                    StdGen       sql=seed

    deriving Show Generic

-- The set of stake pools that produced a given block
PoolProduction sql=pool_production
    poolProductionPoolId         PoolId       sql=pool_id
    poolProductionSlot           SlotNo       sql=slot
    poolProductionHeaderHash     W.BlockId    sql=header_hash
    poolProductionParentHash     W.BlockId    sql=parent_header_hash
    poolProductionBlockHeight    Word32       sql=block_height

    Primary poolProductionSlot
    deriving Show Generic

-- A block header
BlockHeader sql=block_headers
    blockSlot           SlotNo       sql=slot
    blockHeaderHash     W.BlockId    sql=header_hash
    blockParentHash     W.BlockId    sql=parent_header_hash
    blockHeight         Word32       sql=block_height

    Primary blockHeight
    deriving Show Generic

-- Stake distribution for each stake pool
StakeDistribution sql=stake_distribution
    stakeDistributionPoolId     PoolId       sql=pool_id
    stakeDistributionEpoch      Word64       sql=epoch
    stakeDistributionStake      Word64       sql=stake

    Primary stakeDistributionPoolId stakeDistributionEpoch
    deriving Show Generic

-- Mapping from pool id to owner.
PoolOwner sql=pool_owner
    poolOwnerPoolId             PoolId              sql=pool_id
    poolOwnerSlot               W.SlotNo            sql=slot
    poolOwnerSlotInternalIndex  Word64              sql=slot_internal_index
    poolOwnerOwner              P.PoolOwner         sql=pool_owner
    poolOwnerIndex              Word8               sql=pool_owner_index

    Primary poolOwnerPoolId poolOwnerSlot poolOwnerSlotInternalIndex poolOwnerOwner poolOwnerIndex
    Foreign PoolRegistration OnDeleteCascade fk_registration_pool_id poolOwnerPoolId poolOwnerSlot poolOwnerSlotInternalIndex
    deriving Show Generic

-- Mapping of registration certificate to pool
PoolRegistration sql=pool_registration
    poolRegistrationPoolId            PoolId                        sql=pool_id
    poolRegistrationSlot              W.SlotNo                      sql=slot
    poolRegistrationSlotInternalIndex Word64                        sql=slot_internal_index
    poolRegistrationMarginNumerator   Word64                        sql=margin_numerator
    poolRegistrationMarginDenominator Word64                        sql=margin_denominator
    poolRegistrationCost              Word64                        sql=cost
    poolRegistrationPledge            Word64                        sql=pledge
    poolRegistrationMetadataUrl       StakePoolMetadataUrl  Maybe sql=metadata_url
    poolRegistrationMetadataHash      StakePoolMetadataHash Maybe sql=metadata_hash

    Primary poolRegistrationPoolId poolRegistrationSlot poolRegistrationSlotInternalIndex
    deriving Show Generic

PoolDelistment sql=pool_delistment
    delistedPoolId                PoolId                      sql=pool_id
    Primary delistedPoolId
    deriving Show Generic

-- Mapping of retirement certificates to pools
PoolRetirement sql=pool_retirement
    poolRetirementPoolId              PoolId              sql=pool_id
    poolRetirementSlot                W.SlotNo            sql=slot
    poolRetirementSlotInternalIndex   Word64              sql=slot_internal_index
    poolRetirementEpoch               Word64              sql=epoch

    Primary poolRetirementPoolId poolRetirementSlot poolRetirementSlotInternalIndex
    deriving Show Generic

-- Cached metadata after they've been fetched from a remote server.
PoolMetadata sql=pool_metadata
    poolMetadataHash                   StakePoolMetadataHash sql=metadata_hash
    poolMetadataName                   Text                    sql=name
    poolMetadataTicker                 StakePoolTicker         sql=ticker
    poolMetadataDescription            Text Maybe              sql=description
    poolMetadataHomepage               Text                    sql=homepage

    Primary poolMetadataHash
    deriving Show Generic

PoolMetadataFetchAttempts sql=pool_metadata_fetch_attempts
    poolFetchAttemptsMetadataHash    StakePoolMetadataHash sql=metadata_hash
    poolFetchAttemptsMetadataUrl     StakePoolMetadataUrl  sql=metadata_url
    poolFetchAttemptsRetryAfter      UTCTime                 sql=retry_after
    poolFetchAttemptsRetryCount      Word8                   sql=retry_count

    Primary poolFetchAttemptsMetadataHash poolFetchAttemptsMetadataUrl
    deriving Show Generic
|]
