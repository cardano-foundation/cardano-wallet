{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
    ( -- * Model Types
      PoolDatabase (..)
    , emptyPoolDatabase

      -- * Model Operation Types
    , ModelOp
    , PoolErr (..)

      -- * Model pool database functions
    , mCleanDatabase
    , mCleanPoolMetadata
    , mPutPoolProduction
    , mPutHeader
    , mListHeaders
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
    , mPutDelistedPools
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
    , mReadDelistedPools
    , mRemoveRetiredPools
    , mReadSettings
    , mPutSettings
    , mPutLastMetadataGC
    , mReadLastMetadataGC
    ) where

import Prelude

import Cardano.Pool.DB
    ( determinePoolLifeCycleStatus
    )
import Cardano.Pool.Types
    ( PoolId
    , PoolOwner
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , epochOf
    , interpretQuery
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime
    , EpochNo (..)
    , InternalState (..)
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , Settings
    , SlotNo (..)
    , defaultInternalState
    , defaultSettings
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.State.Strict
    ( StateT
    )
import Data.Bifunctor
    ( first
    )
import Data.Foldable
    ( fold
    )
import Data.Function
    ( (&)
    )
import Data.Functor.Const
    ( Const (..)
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , view
    )
import Data.Map.Strict
    ( Map
    )
import Data.Ord
    ( Down (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Set
    ( Set
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import System.Random
    ( StdGen
    , newStdGen
    )

import Cardano.Pool.Metadata.Types
    ( StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import qualified Control.Monad.Trans.State.Strict as State
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
    , registrations
        :: !(Map (CertificatePublicationTime, PoolId) PoolRegistrationCertificate)
    -- ^ On-chain registrations associated with pools
    , retirements
        :: !(Map (CertificatePublicationTime, PoolId) PoolRetirementCertificate)
    -- ^ On-chain retirements associated with pools
    , delisted :: !(Set PoolId)
    , metadata :: !(Map StakePoolMetadataHash StakePoolMetadata)
    -- ^ Off-chain metadata cached in database
    , fetchAttempts :: !(Map (StakePoolMetadataUrl, StakePoolMetadataHash) Int)
    -- ^ Metadata (failed) fetch attempts
    , seed :: !SystemSeed
    -- ^ Store an arbitrary random generator seed
    , blockHeaders :: [BlockHeader]
    -- ^ Store headers during syncing
    , settings :: Settings
    , internalState :: InternalState
    -- ^ Various internal states that need to persist across
    -- wallet restarts.
    }
    deriving (Generic, Show, Eq)

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
    PoolDatabase
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        NotSeededYet
        mempty
        defaultSettings
        defaultInternalState

{-------------------------------------------------------------------------------
                                  Model Operation Types
-------------------------------------------------------------------------------}

type ModelOp a = StateT PoolDatabase (Either PoolErr) a

newtype PoolErr = PointAlreadyExists BlockHeader
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                            Model Pool Database Functions
-------------------------------------------------------------------------------}

mCleanDatabase :: ModelOp ()
mCleanDatabase = State.put emptyPoolDatabase

mCleanPoolMetadata :: ModelOp ()
mCleanPoolMetadata = do
    modify #metadata
        $ const mempty
    mPutDelistedPools []

mPutPoolProduction :: BlockHeader -> PoolId -> ModelOp ()
mPutPoolProduction point poolId =
    getPoints >>= \points ->
        if
            | point `elem` points ->
                lift $ Left $ PointAlreadyExists point
            | otherwise ->
                modify #pools $ Map.alter (alter point) poolId
  where
    alter slot = \case
        Nothing -> Just [slot]
        Just slots -> Just $ sortDesc $ slot : slots
    sortDesc = L.sortBy (flip compare)

    getPoints :: ModelOp [BlockHeader]
    getPoints = concat . Map.elems <$> get #pools

mReadPoolProduction
    :: TimeInterpreter Identity
    -> EpochNo
    -> ModelOp (Map PoolId [BlockHeader])
mReadPoolProduction timeInterpreter epoch =
    updatePools . updateSlots epoch <$> get #pools
  where
    epochOf' = runIdentity . interpretQuery timeInterpreter . epochOf
    updatePools = Map.filter (not . L.null)
    updateSlots e = Map.map (filter (\x -> epochOf' (slotNo x) == e))

mReadTotalProduction :: ModelOp (Map PoolId (Quantity "block" Word64))
mReadTotalProduction =
    Map.map (Quantity . fromIntegral . length) <$> get #pools

mPutStakeDistribution
    :: EpochNo -> [(PoolId, Quantity "lovelace" Word64)] -> ModelOp ()
mPutStakeDistribution epoch distribution =
    modify #distributions $ Map.insert epoch distribution

mReadStakeDistribution
    :: EpochNo -> ModelOp [(PoolId, Quantity "lovelace" Word64)]
mReadStakeDistribution epoch =
    Map.findWithDefault mempty epoch <$> get #distributions

mPutPoolRegistration
    :: CertificatePublicationTime
    -> PoolRegistrationCertificate
    -> ModelOp ()
mPutPoolRegistration cpt cert = do
    modify #owners
        $ Map.insert poolId poolOwners
    modify #registrations
        $ Map.insert (cpt, poolId) cert
  where
    PoolRegistrationCertificate{poolId, poolOwners} = cert

mReadPoolRegistration
    :: PoolId
    -> ModelOp
        (Maybe (CertificatePublicationTime, PoolRegistrationCertificate))
mReadPoolRegistration poolId =
    fmap (first fst) . Map.lookupMax . Map.filterWithKey (only poolId)
        <$> get #registrations
  where
    only k (_, k') _ = k == k'

mPutPoolRetirement
    :: CertificatePublicationTime
    -> PoolRetirementCertificate
    -> ModelOp ()
mPutPoolRetirement cpt cert =
    modify #retirements $ Map.insert (cpt, poolId) cert
  where
    PoolRetirementCertificate poolId _retirementEpoch = cert

mReadPoolRetirement
    :: PoolId
    -> ModelOp
        (Maybe (CertificatePublicationTime, PoolRetirementCertificate))
mReadPoolRetirement poolId =
    fmap (first fst) . Map.lookupMax . Map.filterWithKey (only poolId)
        <$> get #retirements
  where
    only k (_, k') _ = k == k'

mListPoolLifeCycleData :: EpochNo -> ModelOp [PoolLifeCycleStatus]
mListPoolLifeCycleData epoch = do
    registeredPools <- mListRegisteredPools
    retiredPools <- fmap (view #poolId) <$> mListRetiredPools epoch
    let nonRetiredPools =
            Set.toList
                $ Set.difference
                    (Set.fromList registeredPools)
                    (Set.fromList retiredPools)
    mapM mReadPoolLifeCycleStatus nonRetiredPools

mListRegisteredPools :: ModelOp [PoolId]
mListRegisteredPools =
    Set.toList . Set.map snd . Map.keysSet <$> get #registrations

mListRetiredPools :: EpochNo -> ModelOp [PoolRetirementCertificate]
mListRetiredPools epochNo = do
    retirements <- fmap (Just . view #retirementEpoch) <$> get #retirements
    retirementCancellations <- fmap (const Nothing) <$> get #registrations
    let retiredPools =
            -- First, merge the retirements map with the cancellations map.
            -- A retirement is represented as a 'Just retirementEpoch' value.
            -- A retirement cancellation is represented as a 'Nothing' value.
            Map.union retirements retirementCancellations
                -- Keep only the most-recently published retirement epoch for each
                -- pool (which will be 'Nothing' in the case of a cancellation):
                & retainOnlyMostRecent
                -- Remove pools that have had their retirements cancelled:
                & pruneEmptyValues
                -- Remove pools that have not yet retired:
                & Map.filter (<= epochNo)
    retiredPools
        & Map.toList
        & fmap (uncurry PoolRetirementCertificate)
        & pure
  where
    pruneEmptyValues :: Map k (Maybe v) -> Map k v
    pruneEmptyValues = Map.mapMaybe id

    retainOnlyMostRecent :: Ord k => Map (publicationTime, k) v -> Map k v
    retainOnlyMostRecent =
        -- If more than one key from the original map is mapped to the same key
        -- in the result map, 'Map.mapKeys' guarantees to retain only the value
        -- corresponding to the greatest of the original keys.
        Map.mapKeys snd

mReadPoolLifeCycleStatus :: PoolId -> ModelOp PoolLifeCycleStatus
mReadPoolLifeCycleStatus poolId =
    determinePoolLifeCycleStatus
        <$> (lookupLatestCertificate <$> get #registrations)
        <*> (lookupLatestCertificate <$> get #retirements)
  where
    lookupLatestCertificate
        :: Map (publicationTime, PoolId) certificate
        -> Maybe (publicationTime, certificate)
    lookupLatestCertificate =
        fmap (first fst)
            . Map.lookupMax
            . Map.filterWithKey (\(_, k) _ -> k == poolId)

mUnfetchedPoolMetadataRefs
    :: Int
    -> ModelOp [(PoolId, StakePoolMetadataUrl, StakePoolMetadataHash)]
mUnfetchedPoolMetadataRefs n =
    inner
        <$> get #registrations
        <*> get #metadata
        <*> get #fetchAttempts
  where
    inner registrations metadata fetchAttempts =
        toTuple <$> take n (Map.elems unfetched)
      where
        unfetched = flip Map.filter registrations $ \r ->
            case poolMetadata r of
                Nothing -> False
                Just fkey@(_, hash) ->
                    (&&)
                        (hash `notElem` Map.keys metadata)
                        (fkey `notElem` Map.keys fetchAttempts)
        toTuple = \case
            PoolRegistrationCertificate
                { poolId
                , poolMetadata = Just (metadataUrl, metadataHash)
                } -> (poolId, metadataUrl, metadataHash)
            _ -> error "mUnfetchedPoolMetadataRefs: poolMetadata is Nothing"

mPutFetchAttempt
    :: (StakePoolMetadataUrl, StakePoolMetadataHash)
    -> ModelOp ()
mPutFetchAttempt key =
    modify #fetchAttempts $ Map.insertWith (+) key 1

mPutPoolMetadata
    :: StakePoolMetadataHash
    -> StakePoolMetadata
    -> ModelOp ()
mPutPoolMetadata hash meta = do
    modify #metadata
        $ Map.insert hash meta
    modify #fetchAttempts
        $ Map.filterWithKey
        $ \k _ -> snd k /= hash

mReadPoolMetadata
    :: ModelOp (Map StakePoolMetadataHash StakePoolMetadata)
mReadPoolMetadata = get #metadata

mReadSystemSeed
    :: PoolDatabase
    -> IO (StdGen, PoolDatabase)
mReadSystemSeed db@PoolDatabase{seed} =
    case seed of
        NotSeededYet -> do
            seed' <- newStdGen
            return (seed', db{seed = SystemSeed seed'})
        SystemSeed s ->
            return (s, db)

mReadCursor :: Int -> ModelOp [BlockHeader]
mReadCursor k = do
    allHeaders <- fold <$> get #pools
    pure $ reverse $ limit $ sortDesc allHeaders
  where
    sortDesc = L.sortOn (Down . slotNo)
    limit = take k

mRollbackTo :: TimeInterpreter Identity -> SlotNo -> ModelOp ()
mRollbackTo ti point = do
    modify #distributions
        $ Map.mapMaybeWithKey
        $ discardBy
        $ runIdentity . interpretQuery ti . epochOf
    modify #pools
        $ Map.filter (not . L.null) . fmap (filter ((<= point) . slotNo))
    modify #registrations
        $ Map.mapMaybeWithKey
        $ discardBy id . view #slotNo . fst
    modify #retirements
        $ Map.mapMaybeWithKey
        $ discardBy id . view #slotNo . fst
    modify #owners
        . flip Map.restrictKeys
        . Set.fromList
        =<< mListRegisteredPools
  where
    discardBy :: Ord point => (SlotNo -> point) -> point -> a -> Maybe a
    discardBy getPoint point' v
        | point' <= getPoint point = Just v
        | otherwise = Nothing

mPutDelistedPools :: [PoolId] -> ModelOp ()
mPutDelistedPools = modify #delisted . const . Set.fromList

mReadDelistedPools :: ModelOp [PoolId]
mReadDelistedPools = Set.toList <$> get #delisted

mRemovePools :: [PoolId] -> ModelOp ()
mRemovePools poolsToRemove = do
    modify #distributions
        $ Map.map
        $ L.filter
        $ \(p, _) -> retain p
    modify #pools
        $ Map.filterWithKey
        $ \p _ -> retain p
    modify #owners
        $ Map.filterWithKey
        $ \p _ -> retain p
    modify #registrations
        $ Map.filterWithKey
        $ \(_, p) _ -> retain p
    modify #retirements
        $ Map.filterWithKey
        $ \(_, p) _ -> retain p
  where
    retain p = p `Set.notMember` poolsToRemoveSet
    poolsToRemoveSet = Set.fromList poolsToRemove

mRemoveRetiredPools :: EpochNo -> ModelOp [PoolRetirementCertificate]
mRemoveRetiredPools epoch = do
    certificates <- mListRetiredPools epoch
    mRemovePools (view #poolId <$> certificates)
    pure certificates

mPutHeader :: BlockHeader -> ModelOp ()
mPutHeader header = modify #blockHeaders (header :)

mListHeaders :: Int -> ModelOp [BlockHeader]
mListHeaders k
    | k > 0 = reverse . take k <$> get #blockHeaders
    | otherwise = reverse <$> get #blockHeaders

mReadSettings
    :: ModelOp Settings
mReadSettings = get #settings

mPutSettings
    :: Settings
    -> ModelOp ()
mPutSettings s = modify #settings (\_ -> s)

mReadLastMetadataGC
    :: ModelOp (Maybe POSIXTime)
mReadLastMetadataGC = get (#internalState . #lastMetadataGC)

mPutLastMetadataGC
    :: POSIXTime
    -> ModelOp ()
mPutLastMetadataGC t = modify (#internalState . #lastMetadataGC) (\_ -> Just t)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Get the value of a particular field from the database.
--
get
    :: ((a -> Const a a) -> PoolDatabase -> Const a PoolDatabase)
    -- ^ Database field label.
    -> ModelOp a
get label = State.gets $ view label

-- Modify the value of a particular field within the database.
--
modify
    :: ((a -> Identity b) -> PoolDatabase -> Identity PoolDatabase)
    -- ^ Database field label.
    -> (a -> b)
    -- ^ Modification function.
    -> ModelOp ()
modify label = State.modify . over label
