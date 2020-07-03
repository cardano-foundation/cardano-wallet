{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the production pool database using only pure functions.
--
-- These functions and types model the behaviour of the SQLite database backend,
-- and are used for QuickCheck state machine testing, and the MVar database
-- backend.

module Cardano.Pool.DB.Model
    (
    -- * Model Types
      PoolDatabase (..)
    , emptyPoolDatabase
    -- * Model Operation Types
    , ModelPoolOp
    , PoolErr (..)
    -- * Model pool database functions
    , mCleanPoolProduction
    , mPutPoolProduction
    , mReadPoolProduction
    , mReadTotalProduction
    , mPutStakeDistribution
    , mReadStakeDistribution
    , mReadPoolMetadata
    , mPutPoolRegistration
    , mReadPoolRegistration
    , mPutPoolRetirement
    , mReadPoolRetirement
    , mUnfetchedPoolMetadataRefs
    , mPutFetchAttempt
    , mPutPoolMetadata
    , mListRegisteredPools
    , mReadSystemSeed
    , mRollbackTo
    , mReadCursor
    ) where

import Prelude

import Cardano.Pool.DB
    ( CertificatePublicationTime )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochNo (..)
    , PoolId
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , SlotId (..)
    , StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Data.Bifunctor
    ( first )
import Data.Foldable
    ( fold )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen, newStdGen )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                            Model Database Types
-------------------------------------------------------------------------------}

data PoolDatabase = PoolDatabase
    { pools :: !(Map PoolId [BlockHeader])
    -- ^ Information of what blocks were produced by which stake pools

    , distributions :: !(Map EpochNo [(PoolId, Quantity "lovelace" Word64)])
    -- ^ Store known stake distributions for epochs

    , owners :: !(Map PoolId [PoolOwner])
    -- ^ Mapping between pool ids and owners

    , registrations ::
        !(Map (CertificatePublicationTime, PoolId) PoolRegistrationCertificate)
    -- ^ On-chain registrations associated with pools

    , retirements ::
        !(Map (CertificatePublicationTime, PoolId) PoolRetirementCertificate)
    -- ^ On-chain retirements associated with pools

    , metadata :: !(Map StakePoolMetadataHash StakePoolMetadata)
    -- ^ Off-chain metadata cached in database

    , fetchAttempts :: !(Map (StakePoolMetadataUrl, StakePoolMetadataHash) Int)
    -- ^ Metadata (failed) fetch attempts

    , seed :: !SystemSeed
    -- ^ Store an arbitrary random generator seed
    } deriving (Generic, Show, Eq)

data SystemSeed
    = SystemSeed StdGen
    | NotSeededYet
    deriving (Generic, Show)

-- | Shallow / weak equality on seeds.
instance Eq SystemSeed where
    (SystemSeed _) == (SystemSeed _) = True
    NotSeededYet == NotSeededYet = True
    _ == _ = False

-- | Produces an empty model pool production database.
emptyPoolDatabase :: PoolDatabase
emptyPoolDatabase =
    PoolDatabase mempty mempty mempty mempty mempty mempty mempty NotSeededYet

{-------------------------------------------------------------------------------
                                  Model Operation Types
-------------------------------------------------------------------------------}

type ModelPoolOp a = PoolDatabase -> (Either PoolErr a, PoolDatabase)

newtype PoolErr = PointAlreadyExists BlockHeader
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                            Model Pool Database Functions
-------------------------------------------------------------------------------}

mCleanPoolProduction :: ModelPoolOp ()
mCleanPoolProduction _ = (Right (), emptyPoolDatabase)

mPutPoolProduction :: BlockHeader -> PoolId -> ModelPoolOp ()
mPutPoolProduction point poolId db@PoolDatabase{pools} =
    let alter slot = \case
            Nothing -> Just [slot]
            Just slots -> Just $ sortDesc (slot:slots)
        sortDesc = L.sortBy (flip compare)
    in if point `elem` concat (Map.elems pools) then
        (Left (PointAlreadyExists point), db)
    else
        ( Right ()
        , db { pools = Map.alter (alter point) poolId pools }
        )

mReadPoolProduction :: EpochNo -> ModelPoolOp (Map PoolId [BlockHeader])
mReadPoolProduction epoch db@PoolDatabase{pools} =
    let updateSlots e = Map.map (filter (\x -> epochNumber (slotId x) == e))
        updatePools = Map.filter (not . L.null)
    in (Right (updatePools $ (updateSlots epoch) pools), db)

mReadTotalProduction :: ModelPoolOp (Map PoolId (Quantity "block" Word64))
mReadTotalProduction db@PoolDatabase{pools} =
    ( Right (Map.map (Quantity . fromIntegral . length) pools), db )

mPutStakeDistribution
    :: EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> ModelPoolOp ()
mPutStakeDistribution epoch distrib db@PoolDatabase{distributions} =
    ( Right ()
    , db { distributions = Map.insert epoch distrib distributions }
    )

mReadStakeDistribution
    :: EpochNo
    -> ModelPoolOp [(PoolId, Quantity "lovelace" Word64)]
mReadStakeDistribution epoch db@PoolDatabase{distributions} =
    ( Right $ Map.findWithDefault mempty epoch distributions
    , db
    )

mPutPoolRegistration
    :: CertificatePublicationTime
    -> PoolRegistrationCertificate
    -> ModelPoolOp ()
mPutPoolRegistration sp registration db =
    ( Right ()
    , db { owners = Map.insert poolId poolOwners owners
         , registrations = Map.insert (sp, poolId) registration registrations
         }
    )
  where
    PoolDatabase {owners, registrations} = db
    PoolRegistrationCertificate {poolId, poolOwners} = registration

mReadPoolRegistration
    :: PoolId
    -> ModelPoolOp
        (Maybe (CertificatePublicationTime, PoolRegistrationCertificate))
mReadPoolRegistration poolId db =
    ( Right
        $ fmap (first fst)
        $ Map.lookupMax
        $ Map.filterWithKey (only poolId) registrations
    , db
    )
  where
    PoolDatabase {registrations} = db
    only k (_, k') _ = k == k'

mPutPoolRetirement
    :: CertificatePublicationTime
    -> PoolRetirementCertificate
    -> ModelPoolOp ()
mPutPoolRetirement sp retirement db =
    ( Right ()
    , db { retirements = Map.insert (sp, poolId) retirement retirements }
    )
  where
    PoolDatabase {retirements} = db
    PoolRetirementCertificate poolId _retiredIn = retirement

mReadPoolRetirement
    :: PoolId
    -> ModelPoolOp
        (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
mReadPoolRetirement poolId db =
    ( Right
        $ fmap (first fst)
        $ Map.lookupMax
        $ Map.filterWithKey (only poolId) retirements
    , db
    )
  where
    PoolDatabase {retirements} = db
    only k (_, k') _ = k == k'

mListRegisteredPools :: PoolDatabase -> ([PoolId], PoolDatabase)
mListRegisteredPools db@PoolDatabase{registrations} =
    ( snd <$> Map.keys registrations, db )

mUnfetchedPoolMetadataRefs
    :: Int
    -> ModelPoolOp [(StakePoolMetadataUrl, StakePoolMetadataHash)]
mUnfetchedPoolMetadataRefs n db@PoolDatabase{registrations,metadata} =
    ( Right (toTuple <$> take n (Map.elems unfetched))
    , db
    )
  where
    unfetched
        :: Map (CertificatePublicationTime, PoolId) PoolRegistrationCertificate
    unfetched = flip Map.filter registrations $ \r ->
        case poolMetadata r of
            Nothing -> False
            Just (_, hash) -> hash `notElem` Map.keys metadata

    toTuple
        :: PoolRegistrationCertificate
        -> (StakePoolMetadataUrl, StakePoolMetadataHash)
    toTuple PoolRegistrationCertificate{poolMetadata} =
        (metadataUrl, metadataHash)
      where
        Just (metadataUrl, metadataHash) = poolMetadata

mPutFetchAttempt
    :: (StakePoolMetadataUrl, StakePoolMetadataHash)
    -> ModelPoolOp ()
mPutFetchAttempt key db@PoolDatabase{fetchAttempts} =
    ( Right ()
    , db { fetchAttempts = Map.insertWith (+) key 1 fetchAttempts }
    )

mPutPoolMetadata
    :: StakePoolMetadataHash
    -> StakePoolMetadata
    -> ModelPoolOp ()
mPutPoolMetadata hash meta db@PoolDatabase{metadata,fetchAttempts} =
    ( Right ()
    , db { metadata = Map.insert hash meta metadata
         , fetchAttempts = Map.filterWithKey (\k _ -> snd k /= hash) fetchAttempts
         }
    )

mReadPoolMetadata
    :: ModelPoolOp (Map StakePoolMetadataHash StakePoolMetadata)
mReadPoolMetadata db@PoolDatabase{metadata} = (Right metadata, db)

mReadSystemSeed
    :: PoolDatabase
    -> IO (StdGen, PoolDatabase)
mReadSystemSeed db@PoolDatabase{seed} =
    case seed of
        NotSeededYet -> do
            seed' <- newStdGen
            return ( seed', db { seed = SystemSeed seed' })
        SystemSeed s ->
            return ( s, db )

mReadCursor :: Int -> ModelPoolOp [BlockHeader]
mReadCursor k db@PoolDatabase{pools} =
    let allHeaders = fold pools
        sortDesc = L.sortOn (Down . slotId)
        limit = take k
    in (Right $ reverse $ limit $ sortDesc allHeaders, db)

mRollbackTo :: SlotId -> ModelPoolOp ()
mRollbackTo point PoolDatabase { pools
                               , distributions
                               , owners
                               , registrations
                               , retirements
                               , metadata
                               , seed
                               , fetchAttempts
                               } =
    let
        registrations' =
            Map.mapMaybeWithKey (discardBy id . fst . fst) registrations
        retirements' =
            Map.mapMaybeWithKey (discardBy id . fst . fst) retirements
        owners' = Map.restrictKeys owners
            $ Set.fromList
            $ snd <$> Map.keys registrations'
    in
        ( Right ()
        , PoolDatabase
            { pools = updatePools $ updateSlots pools
            , distributions =
                Map.mapMaybeWithKey (discardBy epochNumber) distributions
            , owners = owners'
            , registrations = registrations'
            , retirements = retirements'
            , metadata
            , fetchAttempts
            , seed
            }
        )

  where
    updateSlots = Map.map (filter ((<= point) . slotId))
    updatePools = Map.filter (not . L.null)

    discardBy :: Ord point => (SlotId -> point) -> point -> a -> Maybe a
    discardBy get point' v
        | point' <= get point = Just v
        | otherwise = Nothing
