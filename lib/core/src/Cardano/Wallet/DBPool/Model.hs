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

module Cardano.Wallet.DBPool.Model
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
    , mRollbackTo
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( PoolId, SlotId (..) )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )

import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                            Model Database Types
-------------------------------------------------------------------------------}

newtype PoolDatabase = PoolDatabase
    { pools :: (Map PoolId [SlotId])
    -- ^ Information of what blocks were produced by which stake pools
    } deriving (Generic, Show, Eq)

-- | Produces an empty model pool production database.
emptyPoolDatabase :: PoolDatabase
emptyPoolDatabase = PoolDatabase mempty

{-------------------------------------------------------------------------------
                                  Model Operation Types
-------------------------------------------------------------------------------}

type ModelPoolOp a = PoolDatabase -> (Either PoolErr a, PoolDatabase)

newtype PoolErr = SlotAlreadyExists SlotId
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                            Model Pool Database Functions
-------------------------------------------------------------------------------}

mCleanPoolProduction :: ModelPoolOp ()
mCleanPoolProduction _ = (Right (), emptyPoolDatabase)

mPutPoolProduction :: SlotId -> PoolId -> ModelPoolOp ()
mPutPoolProduction point poolId db@(PoolDatabase pools) =
    let alter slot = \case
            Nothing -> Just [slot]
            Just slots -> Just (slot:slots)
    in if point `elem` concat (Map.elems pools) then
        (Left (SlotAlreadyExists point), db)
    else
        (Right (), PoolDatabase (Map.alter (alter point) poolId pools))

mReadPoolProduction :: Word64 -> ModelPoolOp (Map PoolId [SlotId])
mReadPoolProduction epoch db@(PoolDatabase pools) =
    let updateSlots e = Map.map (filter (\(SlotId e' _) -> e' == e))
        updatePools = Map.filter (not . L.null)
    in (Right (updatePools $ (updateSlots epoch) pools), db)

mRollbackTo :: SlotId -> ModelPoolOp ()
mRollbackTo point (PoolDatabase pools) =
    let updateSlots = Map.map (filter (<= point))
        updatePools = Map.filter (not . L.null)
    in (Right (), PoolDatabase (updatePools $ updateSlots pools))
