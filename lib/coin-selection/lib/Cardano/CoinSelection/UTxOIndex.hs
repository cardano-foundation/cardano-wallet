-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Provides the 'UTxOIndex' type, which indexes a UTxO set by asset identifier.
--
-- The index makes it possible to efficiently compute the subset of a UTxO set
-- containing a particular asset, or to select just a single UTxO containing a
-- particular asset, without having to search linearly through the entire UTxO
-- set.
--
-- See the documentation for 'UTxOIndex' for more details.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
--
module Cardano.CoinSelection.UTxOIndex
    (
    -- * Type
      UTxOIndex

    -- * Construction
    , empty
    , singleton
    , fromMap
    , fromSequence

    -- * Deconstruction
    , toList
    , toMap

    -- * Folding
    , fold

    -- * Modification
    , insert
    , insertMany
    , delete
    , deleteMany

    -- * Filtering and partitioning
    , filter
    , partition

    -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

    -- * Set operations
    , difference
    , disjoint

    -- * Selection
    , Asset (..)
    , SelectionFilter (..)
    , selectRandom
    , selectRandomWithPriority

    ) where

import Cardano.CoinSelection.UTxOIndex.Internal
