-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Provides the 'UTxOIndex' type, which stores a UTxO set indexed by asset ID.
--
-- See the documentation for 'UTxOIndex' for more details.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
--
module Cardano.Wallet.Primitive.Types.UTxOIndex
    (
    -- * Type
      UTxOIndex

    -- * Construction
    , empty
    , singleton
    , fromSequence
    , fromUTxO

    -- * Deconstruction
    , toList
    , toUTxO

    -- * Folding
    , fold

    -- * Modification
    , insert
    , insertMany
    , delete
    , deleteMany

    -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

    -- * Selection
    , SelectionFilter (..)
    , selectRandom

    ) where

import Cardano.Wallet.Primitive.Types.UTxOIndex.Internal
