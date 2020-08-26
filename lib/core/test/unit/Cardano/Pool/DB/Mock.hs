{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Functions for mocking the pool database layer.
--
-- This module is meant to be imported in a qualified fashion. For example:
--
-- >>> import qualified Cardano.Pool.DB.Mock as Mock
--
module Cardano.Pool.DB.Mock
    ( DBLayer (..)
    , dbLayer
    , toReal
    ) where

import Prelude hiding
    ( fail )

import Cardano.Pool.DB
    ( ErrPointAlreadyExists (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochNo (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , SlotNo (..)
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
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import System.Random
    ( StdGen )

import qualified Cardano.Pool.DB as Real

-- | A simplified version of 'Real.DBLayer' without existential types.
--
-- Since this type does not use existential quantification, member functions
-- can be overridden with the standard record update syntax.
--
data DBLayer m = DBLayer
    { putPoolProduction
        :: BlockHeader
        -> PoolId
        -> ExceptT ErrPointAlreadyExists m ()
    , readPoolProduction
        :: EpochNo
        -> m (Map PoolId [BlockHeader])
    , readTotalProduction
        :: m (Map PoolId (Quantity "block" Word64))
    , putStakeDistribution
        :: EpochNo
        -> [(PoolId, Quantity "lovelace" Word64)]
        -> m ()
    , readStakeDistribution
        :: EpochNo
        -> m [(PoolId, Quantity "lovelace" Word64)]
    , readPoolProductionCursor
        :: Int
        -> m [BlockHeader]
    , readPoolLifeCycleStatus
        :: PoolId
        -> m PoolLifeCycleStatus
    , putPoolRegistration
        :: CertificatePublicationTime
        -> PoolRegistrationCertificate
        -> m ()
    , readPoolRegistration
        :: PoolId
        -> m (Maybe (CertificatePublicationTime, PoolRegistrationCertificate))
    , putPoolRetirement
        :: CertificatePublicationTime
        -> PoolRetirementCertificate
        -> m ()
    , readPoolRetirement
        :: PoolId
        -> m (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
    , unfetchedPoolMetadataRefs
        :: Int
        -> m [(PoolId, StakePoolMetadataUrl, StakePoolMetadataHash)]
    , putFetchAttempt
        :: (StakePoolMetadataUrl, StakePoolMetadataHash)
        -> m ()
    , listRegisteredPools
        :: m [PoolId]
    , listRetiredPools
        :: EpochNo
        -> m [PoolRetirementCertificate]
    , putPoolMetadata
        :: StakePoolMetadataHash
        -> StakePoolMetadata
        -> m ()
    , readPoolMetadata
        :: m (Map StakePoolMetadataHash StakePoolMetadata)
    , readSystemSeed
        :: m StdGen
    , rollbackTo
        :: SlotNo
        -> m ()
    , removePools
        :: [PoolId]
        -> m ()
    , cleanDB
        :: m ()
    }

-- | An empty DB layer where none of the operations can be called.
--
dbLayer :: DBLayer m
dbLayer = DBLayer
    { cleanDB =
        fail "cleanDB"
    , listRegisteredPools =
        fail "listRegisteredPools"
    , listRetiredPools = \_ ->
        fail "listRetiredPools"
    , putFetchAttempt =
        fail "putFetchAttempt"
    , putPoolMetadata =
        fail "putPoolMetadata"
    , putPoolProduction =
        fail "putPoolProduction"
    , putPoolRegistration =
        fail "putPoolRegistration"
    , putPoolRetirement =
        fail "putPoolRetirement"
    , putStakeDistribution =
        fail "putStakeDistribution"
    , readPoolLifeCycleStatus =
        fail "readPoolLifeCycleStatus"
    , readPoolMetadata =
        fail "readPoolMetadata"
    , readPoolProduction =
        fail "readPoolProduction"
    , readPoolProductionCursor =
        fail "readPoolProductionCursor"
    , readPoolRegistration =
        fail "readPoolRegistration"
    , readPoolRetirement =
        fail "readPoolRetirement"
    , readStakeDistribution =
        fail "readStakeDistribution"
    , readSystemSeed =
        fail "readSystemSeed"
    , readTotalProduction =
        fail "readTotalProduction"
    , removePools =
        fail "removePools"
    , rollbackTo =
        fail "rollbackTo"
    , unfetchedPoolMetadataRefs =
        fail "unfetchedPoolMetadataRefs"
    }
  where
    fail op = error $ "mock DB layer: unexpected call to: " <> op

-- | Converts a 'DBLayer' into a 'Real.DBLayer'.
--
toReal
    :: (MonadFail m, MonadIO m)
    => DBLayer m
    -> Real.DBLayer m
toReal DBLayer {..} = Real.DBLayer {..}
  where
    atomically = id
