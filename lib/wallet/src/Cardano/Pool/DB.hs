{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Database / Persistence layer for the pool production.
module Cardano.Pool.DB
    ( -- * Interface
      DBLayer (..)

      -- * Utilities
    , determinePoolLifeCycleStatus

      -- * Errors
    , ErrPointAlreadyExists (..)
    ) where

import Prelude

import Cardano.Pool.Metadata.Types
    ( StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader
    , CertificatePublicationTime (..)
    , EpochNo (..)
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate
    , PoolRetirementCertificate
    , Settings
    , SlotNo (..)
    )
import Control.Monad.IO.Class
    ( MonadIO
    )
import Control.Monad.Trans.Except
    ( ExceptT
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Map.Strict
    ( Map
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Data.Word
    ( Word64
    )
import System.Random
    ( StdGen
    )

-- | A Database interface for storing pool production in DB.
--
-- To use it, you will need the NamedFieldPuns extension and wrap operations
-- with @atomically@:
--
-- Example:
--
-- >>> :set -XNamedFieldPuns
-- >>> DBLayer{atomically,putPoolProduction} = db
-- >>> atomically $ putPoolProduction blockHeader pool
--
-- This gives you the power to also run /multiple/ operations atomically.
--
-- FIXME: Allowing 'MonadIO' to enable logging also within db transactions.
-- Ideally, we should lower than constraint to only allow logging effects and
-- not any dragons in IO.
data DBLayer m = forall stm.
      (MonadFail stm, MonadIO stm) =>
    DBLayer
    { putPoolProduction
        :: BlockHeader
        -> PoolId
        -> ExceptT ErrPointAlreadyExists stm ()
    -- ^ Write for a given slot id the id of stake pool that produced a
    -- a corresponding block
    , readPoolProduction
        :: EpochNo
        -> stm (Map PoolId [BlockHeader])
    -- ^ Read the all stake pools together with corresponding slot ids
    -- for a given epoch.
    , readTotalProduction
        :: stm (Map PoolId (Quantity "block" Word64))
    -- ^ Read the total pool production since the pool was first registered.
    , putStakeDistribution
        :: EpochNo
        -> [(PoolId, Quantity "lovelace" Word64)]
        -> stm ()
    -- ^ Replace an existing distribution for the given epoch by the one
    -- given as argument.
    --
    -- If there's no existing distribution, simply inserts it.
    , readStakeDistribution
        :: EpochNo
        -> stm [(PoolId, Quantity "lovelace" Word64)]
    , readPoolProductionCursor
        :: Int
        -> stm [BlockHeader]
    -- ^ Read the latest @k@ blockheaders in ascending order. The tip will
    -- be the last element in the list.
    --
    -- This is useful for the @NetworkLayer@ to know how far we have synced.
    -- Returns all headers if limit is <= 0.
    , readPoolLifeCycleStatus
        :: PoolId
        -> stm PoolLifeCycleStatus
    -- ^ Read the current life cycle status of the given pool.
    , putPoolRegistration
        :: CertificatePublicationTime
        -> PoolRegistrationCertificate
        -> stm ()
    -- ^ Add a mapping between stake pools and their corresponding
    -- certificate. If the mapping already exists, data are replaced with
    -- the latest version.
    , readPoolRegistration
        :: PoolId
        -> stm (Maybe (CertificatePublicationTime, PoolRegistrationCertificate))
    -- ^ Find the /latest/ registration certificate for the given pool,
    -- together with the point in time that the certificate was added.
    --
    -- Note that a pool may also have other certificates associated with it
    -- that affect its current lifecycle status.
    --
    -- See 'readPoolLifeCycleStatus' for a complete picture.
    , putPoolRetirement
        :: CertificatePublicationTime
        -> PoolRetirementCertificate
        -> stm ()
    -- ^ Add a retirement certificate for a particular pool.
    , readPoolRetirement
        :: PoolId
        -> stm (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
    -- ^ Find the /latest/ retirement certificate for the given pool,
    -- together with the point in time that the certificate was added.
    --
    -- Note that a pool may also have other certificates associated with it
    -- that affect its current lifecycle status.
    --
    -- See 'readPoolLifeCycleStatus' for a complete picture.
    , unfetchedPoolMetadataRefs
        :: Int
        -> stm [(PoolId, StakePoolMetadataUrl, StakePoolMetadataHash)]
    -- ^ Read the list of metadata remaining to fetch from remote server,
    -- possibly empty if every pool already has an associated metadata
    -- cached.
    --
    -- It returns at most `n` results, where `n` is the first argument.
    , putFetchAttempt
        :: (StakePoolMetadataUrl, StakePoolMetadataHash)
        -> stm ()
    -- ^ Store a fetch attempt for a given hash, so that it isn't retried
    -- too often.
    , listRegisteredPools
        :: stm [PoolId]
    -- ^ List the list of known pools, based on their registration
    -- certificate. This list doesn't necessarily match the keys of the
    -- map we would get from 'readPoolProduction' because not all registered
    -- pools have necessarily produced any block yet!
    , listRetiredPools
        :: EpochNo
        -> stm [PoolRetirementCertificate]
    -- ^ List all pools with an active retirement epoch that is earlier
    -- than or equal to the specified epoch.
    , listPoolLifeCycleData
        :: EpochNo
        -> stm [PoolLifeCycleStatus]
    -- ^ List the lifecycle data of all non-retired pools: pools that
    -- either don't have an active retirement epoch or pools that have
    -- an active retirement epoch that is later than the given epoch.
    , putPoolMetadata
        :: StakePoolMetadataHash
        -> StakePoolMetadata
        -> stm ()
    -- ^ Store metadata fetched from a remote server.
    , removePoolMetadata
        :: stm ()
    -- ^ Delete all pool metadata.
    , readPoolMetadata
        :: stm (Map StakePoolMetadataHash StakePoolMetadata)
    , readSystemSeed
        :: stm StdGen
    -- ^ Read the seed assigned to this particular database. The seed is
    -- created with the database and is "unique" for each database. This
    -- however allow to have a seed that can be used to produce consistent
    -- results across requests.
    , rollbackTo
        :: SlotNo
        -> stm ()
    -- ^ Remove all entries of slot ids newer than the argument
    , putDelistedPools
        :: [PoolId]
        -> stm ()
    -- ^ Overwrite the set of delisted pools with a completely new set.
    -- Pools may be delisted for reasons such as non-compliance.
    , readDelistedPools
        :: stm [PoolId]
    -- ^ Fetch the set of delisted pools.
    , removePools
        :: [PoolId]
        -> stm ()
    -- ^ Remove all data relating to the specified pools.
    , removeRetiredPools
        :: EpochNo
        -> stm [PoolRetirementCertificate]
    -- ^ Remove all pools with an active retirement epoch that is earlier
    --   than or equal to the specified epoch.
    --
    -- Returns the retirement certificates of the pools that were removed.
    --
    -- See also:
    --
    --    - 'listRetiredPools'.
    --    - 'removePools'.
    , putHeader
        :: BlockHeader
        -> stm ()
    -- ^ Add a block header
    , listHeaders
        :: Int -- limit
        -> stm [BlockHeader]
    -- ^ List headers, usually stored during syncing.
    -- Returns all headers if limit is <= 0.
    , readSettings
        :: stm Settings
    -- ^ Get the settings.
    , putSettings
        :: Settings
        -> stm ()
    -- ^ Modify the settings.
    , readLastMetadataGC
        :: stm (Maybe POSIXTime)
    -- ^ Get the last metadata GC time.
    , putLastMetadataGC
        :: POSIXTime
        -> stm ()
    -- ^ Set the last metadata GC time.
    , cleanDB
        :: stm ()
    -- ^ Clean a database
    , atomically
        :: forall a
         . stm a
        -> m a
    -- ^ Run an operation.
    --
    -- For a Sqlite DB, this would be "run a query inside a transaction".
    }

-- | Given the /latest/ registration and retirement certificates for a pool,
--   determine the pool's current life cycle status, based on the relative
--   order in which the certificates were published.
--
-- If two certificates are supplied, then:
--
--   * the certificates must be from the same pool.
--   * the publication times must be non-equal.
--
-- Violating either of the above pre-conditions is a programming error.
--
-- This function determines order of precedence according to the "pool
-- inference rule", as described in "A Formal Specification of the Cardano
-- Ledger":
--
-- https://hydra.iohk.io/build/3202141/download/1/ledger-spec.pdf
determinePoolLifeCycleStatus
    :: (Ord publicationTime, Show publicationTime)
    => Maybe (publicationTime, PoolRegistrationCertificate)
    -> Maybe (publicationTime, PoolRetirementCertificate)
    -> PoolLifeCycleStatus
determinePoolLifeCycleStatus mReg mRet = case (mReg, mRet) of
    (Nothing, _) -> PoolNotRegistered
    (Just (_, regCert), Nothing) -> PoolRegistered regCert
    (Just (regTime, regCert), Just (retTime, retCert))
        | regPoolId /= retPoolId ->
            differentPoolsError
        | regTime > retTime ->
            -- A re-registration always /supersedes/ a prior retirement.
            PoolRegistered regCert
        | regTime < retTime ->
            -- A retirement always /augments/ the latest known registration.
            PoolRegisteredAndRetired regCert retCert
        | otherwise ->
            timeCollisionError
      where
        regPoolId = view #poolId regCert
        retPoolId = view #poolId retCert

        differentPoolsError =
            error
                $ mconcat
                    [ "programming error:"
                    , " determinePoolLifeCycleStatus:"
                    , " called with certificates for different pools:"
                    , " pool id of registration certificate: "
                    , show regPoolId
                    , " pool id of retirement certificate: "
                    , show retPoolId
                    ]

        timeCollisionError =
            error
                $ mconcat
                    [ "programming error:"
                    , " determinePoolLifeCycleStatus:"
                    , " called with identical certificate publication times:"
                    , " pool id of registration certificate: "
                    , show regPoolId
                    , " pool id of retirement certificate: "
                    , show retPoolId
                    , " publication time of registration certificate: "
                    , show regTime
                    , " publication time of retirement certificate: "
                    , show retTime
                    ]

-- | Forbidden operation was executed on an already existing slot
newtype ErrPointAlreadyExists
    = ErrPointAlreadyExists BlockHeader -- Point already exists in db
    deriving (Eq, Show)
