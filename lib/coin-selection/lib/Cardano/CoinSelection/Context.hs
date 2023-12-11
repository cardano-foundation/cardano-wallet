{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides the 'SelectionContext' class, which provides a shared
-- context for types used by coin selection.
--
module Cardano.CoinSelection.Context
    (
    -- * Selection contexts
      SelectionContext (..)
    )
    where

import Prelude

-- | Provides a shared context for types used by coin selection.
--
class
    ( Ord (Address c)
    , Ord (UTxO c)
    , Show (Address c)
    , Show (UTxO c)
    ) =>
    SelectionContext c
  where

    -- | A target address to which payments can be made.
    type Address c

    -- | A unique identifier for an individual UTxO.
    type UTxO c
