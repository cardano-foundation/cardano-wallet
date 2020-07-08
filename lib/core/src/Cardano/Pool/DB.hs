{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
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

    , CertificatePublicationTime (..)
    , PoolRegistrationStatus (..)
    , determinePoolRegistrationStatus
    , readPoolRegistrationStatus

      -- * Errors
    , ErrPointAlreadyExists (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader
    , EpochNo (..)
    , PoolId
    , PoolRegistrationCertificate
    , PoolRetirementCertificate
    , SlotId (..)
    , SlotInternalIndex (..)
    , StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen )

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
data DBLayer m = forall stm. (MonadFail stm, MonadIO stm) => DBLayer
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
        -- along with the point in time that the certificate was added.

    , putPoolRetirement
        :: CertificatePublicationTime
        -> PoolRetirementCertificate
        -> stm ()
        -- ^ Add a retirement certificate for a particular pool.

    , readPoolRetirement
        :: PoolId
        -> stm (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
        -- ^ Find the /latest/ retirement certificate for the given pool,
        -- along with the point in time that the certificate was added.

    , unfetchedPoolMetadataRefs
        :: Int
        -> stm [(StakePoolMetadataUrl, StakePoolMetadataHash)]
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

    , putPoolMetadata
        :: StakePoolMetadataHash
        -> StakePoolMetadata
        -> stm ()
        -- ^ Store metadata fetched from a remote server.

    , readPoolMetadata
        :: stm (Map StakePoolMetadataHash StakePoolMetadata)

    , readSystemSeed
        :: stm StdGen
        -- ^ Read the seed assigned to this particular database. The seed is
        -- created with the database and is "unique" for each database. This
        -- however allow to have a seed that can be used to produce consistent
        -- results across requests.

    , rollbackTo
        :: SlotId
        -> stm ()
        -- ^ Remove all entries of slot ids newer than the argument

    , cleanDB
        :: stm ()
        -- ^ Clean a database

    , atomically
        :: forall a. stm a -> m a
        -- ^ Run an operation.
        --
        -- For a Sqlite DB, this would be "run a query inside a transaction".
    }

-- | Represents an abstract notion of a certificate publication time.
--
-- Certificates published at later times take precedence over certificates
-- published at earlier times.
--
data CertificatePublicationTime = CertificatePublicationTime
    { slotId
        :: SlotId
    , slotInternalIndex
        :: SlotInternalIndex
    }
    deriving (Eq, Generic, Ord, Show)

-- | Indicates the current registration status of a pool.
--
-- Use the 'readPoolRegistrationStatus' function to query the registration
-- status for a particular pool and database backend.
--
data PoolRegistrationStatus
    = PoolNotRegistered
        -- ^ Indicates that a pool is not registered.
    | PoolRegistered
        PoolRegistrationCertificate
        -- ^ Indicates that a pool is registered BUT NOT marked for retirement.
        -- Records the latest registration certificate.
    | PoolRegisteredAndRetired
        PoolRegistrationCertificate
        PoolRetirementCertificate
        -- ^ Indicates that a pool is registered AND ALSO marked for retirement.
        -- Records the latest registration and retirement certificates.
    deriving (Eq, Show)

-- | Given the /latest/ registration and retirement certificates for a pool,
--   determine the pool's current registration status based on the relative
--   order in which the certificates were published.
--
-- If two certificates are supplied, then:
--
--   * the certificates must be from the same pool.
--   * the publication times must be non-equal.
--
-- This function determines order of precedence according to the "pool
-- inference rule", as described in "A Formal Specification of the Cardano
-- Ledger":
--
-- https://hydra.iohk.io/build/3202141/download/1/ledger-spec.pdf
--
determinePoolRegistrationStatus
    :: Ord certificatePublicationTime
    => Maybe (certificatePublicationTime, PoolRegistrationCertificate)
    -> Maybe (certificatePublicationTime, PoolRetirementCertificate)
    -> PoolRegistrationStatus
determinePoolRegistrationStatus mReg mRet = case (mReg, mRet) of
    (Nothing, _) ->
        PoolNotRegistered
    (Just (_, regCert), Nothing) ->
        PoolRegistered regCert
    (Just (regTime, regCert), Just (retTime, retCert))
        | regPoolId /= retPoolId ->
            -- Comparing certificates from different pools is a programming
            -- error.
            differentPoolsError
        | regTime > retTime ->
            -- A re-registration always /supercedes/ a prior retirement.
            PoolRegistered regCert
        | regTime < retTime ->
            -- A retirement always /augments/ the latest known registration.
            PoolRegisteredAndRetired regCert retCert
        | otherwise ->
            -- If a registration certificate and a retirement certificate
            -- for the same pool appear to have been published at exactly
            -- the same time, this indicates a programming error.
            timeCollisionError
      where
        regPoolId = view #poolId regCert
        retPoolId = view #poolId retCert

        differentPoolsError = error $ mconcat
            [ "determinePoolRegistrationStatus:"
            , " called with certificates for different pools:"
            , " pool id of registration certificate: "
            , show regPoolId
            , " pool id of retirement certificate: "
            , show retPoolId
            ]

        timeCollisionError = error $ mconcat
            [ "determinePoolRegistrationStatus:"
            , " called with identical certificate publication times:"
            , " pool id of registration certificate: "
            , show regPoolId
            , " pool id of retirement certificate: "
            , show retPoolId
            , " publication time of registration certificate: "
            , show regPoolId
            , " publication time of retirement certificate: "
            , show retPoolId
            ]

-- | Reads the current registration status of a pool.
--
-- See 'PoolRegistrationStatus' for more details.
--
readPoolRegistrationStatus
    :: DBLayer m
    -> PoolId
    -> m PoolRegistrationStatus
readPoolRegistrationStatus
    DBLayer {atomically, readPoolRegistration, readPoolRetirement} poolId =
        atomically $ determinePoolRegistrationStatus
            <$> readPoolRegistration poolId
            <*> readPoolRetirement poolId

-- | Forbidden operation was executed on an already existing slot
newtype ErrPointAlreadyExists
    = ErrPointAlreadyExists BlockHeader -- Point already exists in db
    deriving (Eq, Show)
