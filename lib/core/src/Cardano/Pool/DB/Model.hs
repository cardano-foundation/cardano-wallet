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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
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
    , mPutStakeDistribution
    , mReadStakeDistribution
    , mPutPoolRegistration
    , mReadPoolRegistration
    , mListRegisteredPools
    , mReadSystemSeed
    , mRollbackTo
    , mReadCursor
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochNo (..)
    , PoolId
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , SlotId (..)
    )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Text.Class
    ( toText )
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

    , metadata :: !(Map (EpochNo, PoolId) (Percentage, Quantity "lovelace" Word64))
    -- ^ On-chain metadata associated with pools

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
emptyPoolDatabase = PoolDatabase mempty mempty mempty mempty NotSeededYet

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
    ( Right $ fromMaybe mempty $ Map.lookup epoch distributions
    , db
    )

mPutPoolRegistration :: EpochNo -> PoolRegistrationCertificate -> ModelPoolOp ()
mPutPoolRegistration ep registration db@PoolDatabase{owners,metadata} =
    ( Right ()
    , db { owners = Map.insert poolId poolOwners owners
         , metadata = Map.insert (ep, poolId) (poolMargin, poolCost) metadata
         }
    )
  where
    PoolRegistrationCertificate
        { poolId
        , poolOwners
        , poolCost
        , poolMargin
        } = registration

mReadPoolRegistration :: PoolId -> ModelPoolOp (Maybe PoolRegistrationCertificate)
mReadPoolRegistration poolId db@PoolDatabase{owners, metadata} =
    ( Right $
        case Map.lookupMax $ Map.filterWithKey (only poolId) metadata of
            Nothing -> Nothing
            Just (_, (poolMargin, poolCost)) ->
                let poolOwners = maybe [] (L.sortOn toText) $ Map.lookup poolId owners
                in Just PoolRegistrationCertificate
                    { poolId
                    , poolOwners
                    , poolMargin
                    , poolCost
                    }
    , db
    )
  where
    only k (_, k') _ = k == k'

mListRegisteredPools :: PoolDatabase -> ([PoolId], PoolDatabase)
mListRegisteredPools db@PoolDatabase{metadata} =
    ( snd <$> Map.keys metadata, db )

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
    let allHeaders = foldMap snd $ Map.toList pools
        sortDesc = L.sortOn (Down . slotId)
        limit = take k
    in (Right $ reverse $ limit $ sortDesc allHeaders, db)

mRollbackTo :: SlotId -> ModelPoolOp ()
mRollbackTo point PoolDatabase{pools, distributions, owners, metadata, seed} =
    let
        metadata' = Map.mapMaybeWithKey (\(ep, _) -> discardEpoch ep) metadata
        owners' = Map.restrictKeys owners
            $ Set.fromList
            $ snd <$> Map.keys metadata'
    in
        ( Right ()
        , PoolDatabase
            { pools = updatePools $ updateSlots pools
            , distributions = Map.mapMaybeWithKey discardEpoch distributions
            , owners = owners'
            , metadata = metadata'
            , seed
            }
        )

  where
    updateSlots = Map.map (filter ((<= point) . slotId))
    updatePools = Map.filter (not . L.null)

    discardEpoch :: EpochNo -> a -> Maybe a
    discardEpoch ep v
        | ep <= epochNumber point = Just v
        | otherwise = Nothing
