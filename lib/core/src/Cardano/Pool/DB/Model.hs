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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    , mCleanDatabase
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
    , mListPoolLifeCycleData
    , mListRegisteredPools
    , mListRetiredPools
    , mReadPoolLifeCycleStatus
    , mReadSystemSeed
    , mRollbackTo
    , mReadCursor
    , mRemovePools
    , mRemoveRetiredPools
    ) where

import Prelude

import Cardano.Pool.DB
    ( determinePoolLifeCycleStatus )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime
    , EpochNo (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , SlotNo (..)
    , StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    , getPoolRetirementCertificate
    )
import Control.Monad.Trans.State.Strict
    ( runState, state )
import Data.Bifunctor
    ( first )
import Data.Foldable
    ( fold )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
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

mCleanDatabase :: ModelPoolOp ()
mCleanDatabase _ = (Right (), emptyPoolDatabase)

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

mReadPoolProduction
    :: TimeInterpreter Identity
    -> EpochNo
    -> ModelPoolOp (Map PoolId [BlockHeader])
mReadPoolProduction timeInterpreter epoch db@PoolDatabase{pools} =
    let epochOf' = runIdentity . timeInterpreter . epochOf
        updateSlots e = Map.map (filter (\x -> epochOf' (slotNo x) == e))
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
mPutPoolRegistration cpt cert = (pure (),)
    . over #owners
        (Map.insert poolId poolOwners)
    . over #registrations
        (Map.insert (cpt, poolId) cert)
  where
    PoolRegistrationCertificate {poolId, poolOwners} = cert

mReadPoolRegistration
    :: PoolId
    -> ModelPoolOp
        (Maybe (CertificatePublicationTime, PoolRegistrationCertificate))
mReadPoolRegistration poolId db = (, db)
    $ pure
    $ fmap (first fst)
    $ Map.lookupMax
    $ Map.filterWithKey (only poolId) registrations
  where
    PoolDatabase {registrations} = db
    only k (_, k') _ = k == k'

mPutPoolRetirement
    :: CertificatePublicationTime
    -> PoolRetirementCertificate
    -> ModelPoolOp ()
mPutPoolRetirement cpt cert = (pure (),)
    . over #retirements
        (Map.insert (cpt, poolId) cert)
  where
    PoolRetirementCertificate poolId _retirementEpoch = cert

mReadPoolRetirement
    :: PoolId
    -> ModelPoolOp
        (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
mReadPoolRetirement poolId db = (, db)
    $ pure
    $ fmap (first fst)
    $ Map.lookupMax
    $ Map.filterWithKey (only poolId) retirements
  where
    PoolDatabase {retirements} = db
    only k (_, k') _ = k == k'

mListPoolLifeCycleData :: EpochNo -> ModelPoolOp [PoolLifeCycleStatus]
mListPoolLifeCycleData epoch db = flip runState db $ do
    let registeredPools = listRegisteredPools db
    let retiredPools = fmap (view #poolId) (listRetiredPools epoch db)
    let nonRetiredPools = Set.toList $ Set.difference
            (Set.fromList registeredPools)
            (Set.fromList retiredPools)
    sequence <$> mapM (state . mReadPoolLifeCycleStatus) nonRetiredPools

mListRegisteredPools :: PoolDatabase -> ([PoolId], PoolDatabase)
mListRegisteredPools db = (listRegisteredPools db, db)

listRegisteredPools :: PoolDatabase -> [PoolId]
listRegisteredPools PoolDatabase {registrations} =
    snd <$> Map.keys registrations

mListRetiredPools
    :: EpochNo
    -> PoolDatabase
    -> ([PoolRetirementCertificate], PoolDatabase)
mListRetiredPools epochNo db = (listRetiredPools epochNo db, db)

listRetiredPools :: EpochNo -> PoolDatabase -> [PoolRetirementCertificate]
listRetiredPools epochNo db = retiredPools
  where
    allKnownPoolIds :: [PoolId]
    allKnownPoolIds =
        L.nub $ snd <$> Map.keys registrations

    retiredPools :: [PoolRetirementCertificate]
    retiredPools = activeRetirementCertificates
        & filter ((<= epochNo) . view #retirementEpoch)

    activeRetirementCertificates :: [PoolRetirementCertificate]
    activeRetirementCertificates =
        allKnownPoolIds
        & fmap (`lookupLifeCycleStatus` db)
        & fmap getPoolRetirementCertificate
        & catMaybes

    PoolDatabase {registrations} = db

mReadPoolLifeCycleStatus :: PoolId -> ModelPoolOp PoolLifeCycleStatus
mReadPoolLifeCycleStatus poolId db = (, db) $ pure $
    lookupLifeCycleStatus poolId db

lookupLifeCycleStatus :: PoolId -> PoolDatabase -> PoolLifeCycleStatus
lookupLifeCycleStatus poolId PoolDatabase {registrations, retirements} =
    determinePoolLifeCycleStatus
        (lookupLatestCertificate registrations)
        (lookupLatestCertificate retirements)
  where
    lookupLatestCertificate
        :: Map (publicationTime, PoolId) certificate
        -> Maybe (publicationTime, certificate)
    lookupLatestCertificate certMap =
        fmap (first fst)
        $ Map.lookupMax
        $ Map.filterWithKey (\(_, k) _ -> k == poolId) certMap

mUnfetchedPoolMetadataRefs
    :: Int
    -> ModelPoolOp [(PoolId, StakePoolMetadataUrl, StakePoolMetadataHash)]
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
        -> (PoolId, StakePoolMetadataUrl, StakePoolMetadataHash)
    toTuple PoolRegistrationCertificate{poolId,poolMetadata} =
        (poolId, metadataUrl, metadataHash)
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
mPutPoolMetadata hash meta = (pure (), )
    . over #metadata
        (Map.insert hash meta)
    . over #fetchAttempts
         (Map.filterWithKey $ \k _ -> snd k /= hash)

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
        sortDesc = L.sortOn (Down . slotNo)
        limit = take k
    in (Right $ reverse $ limit $ sortDesc allHeaders, db)

mRollbackTo :: TimeInterpreter Identity -> SlotNo -> ModelPoolOp ()
mRollbackTo ti point db = (pure (), ) $
    dbFiltered
        & over #owners (`Map.restrictKeys` poolIdsFiltered)
  where
    dbFiltered = db
        & over #distributions
            (Map.mapMaybeWithKey $ discardBy $ runIdentity . ti . epochOf)
        & over #pools
            (Map.filter (not . L.null) . fmap (filter ((<= point) . slotNo)))
        & over #registrations
            (Map.mapMaybeWithKey $ discardBy id . view #slotNo . fst)
        & over #retirements
            (Map.mapMaybeWithKey $ discardBy id . view #slotNo . fst)

    poolIdsFiltered =
        Set.map snd $ Map.keysSet $ view #registrations dbFiltered

    discardBy :: Ord point => (SlotNo -> point) -> point -> a -> Maybe a
    discardBy get point' v
        | point' <= get point = Just v
        | otherwise = Nothing

mRemovePools :: [PoolId] -> ModelPoolOp ()
mRemovePools poolsToRemove = (pure (), )
    . over #distributions
        (Map.map $ L.filter $ \(p, _) -> retain p)
    . over #pools
        (Map.filterWithKey $ \p _ -> retain p)
    . over #owners
        (Map.filterWithKey $ \p _ -> retain p)
    . over #registrations
        (Map.filterWithKey $ \(_, p) _ -> retain p)
    . over #retirements
        (Map.filterWithKey $ \(_, p) _ -> retain p)
  where
    retain p = p `Set.notMember` poolsToRemoveSet
    poolsToRemoveSet = Set.fromList poolsToRemove

mRemoveRetiredPools :: EpochNo -> ModelPoolOp [PoolRetirementCertificate]
mRemoveRetiredPools epoch db =
    first
        (fmap $ const certificates)
        (mRemovePools (view #poolId <$> certificates) db)
  where
    certificates = listRetiredPools epoch db
