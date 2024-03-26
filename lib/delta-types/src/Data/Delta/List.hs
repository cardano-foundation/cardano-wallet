{-# LANGUAGE TypeFamilies #-}

{-|
Copyright: © 2021-2023 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Delta types for lists.
-}
module Data.Delta.List
    ( DeltaList (..)
    ) where

import Prelude

import Data.Delta.Core
    ( Delta (..)
    )

{-------------------------------------------------------------------------------
    Delta type for lists
-------------------------------------------------------------------------------}

-- | Delta type for lists where a list of elements is prepended.
newtype DeltaList a = Append [a]
    deriving (Eq, Ord, Show)

-- |
-- prop> apply (Append xs) ys = xs ++ ys
instance Delta (DeltaList a) where
    type Base (DeltaList a) = [a]
    apply (Append xs) ys = xs ++ ys
