{-|
Copyright: © 2021-2023 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Delta types are instances of the type class 'Delta'.
An instance 'Delta'@ delta@ indicates that the type @delta@
is a delta type of the corresponding base type 'Base'@ delta@.
The 'apply' function applies a delta to the base type.

Delta types can be transformed into each other using an 'Embedding'.

Examples:

prop> apply (Replace 7) 3 = 7
prop> apply [Append [1], Append [2]] [3] = [1,2,3]
prop> apply [Insert 'c', Delete 'b'] (Set.fromList "ab") = Set.fromList "ac"
prop> ∀(x :: Set Char). apply [Delete 'b', Insert 'b'] x = apply (Delete 'b') x
-}
module Data.Delta (
    -- * Modules
    -- | Core definitions:
    -- 'Delta' class, 'apply' function, 'Base' type family.
    --
    -- Core instances:
    -- 'Replace', 'Data.Monoid.Endo', 'Data.Maybe.Maybe', '[]'.
      module Data.Delta.Core

    -- | Delta types for common containers.
    , module Data.Delta.List
    , module Data.Delta.Set

    -- | Embeddings of delta types and helper functions.
    , module Data.Delta.Embedding
    , module Data.Semigroupoid
    ) where

import Data.Delta.Core
import Data.Delta.Embedding
import Data.Delta.List
import Data.Delta.Set
import Data.Semigroupoid
