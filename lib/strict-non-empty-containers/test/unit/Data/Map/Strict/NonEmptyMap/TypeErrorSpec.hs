{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fno-warn-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Map.Strict.NonEmptyMap.TypeErrorSpec
    ( spec
    ) where

import Prelude

import Data.Map.Strict.NonEmptyMap.Internal
    ( NonEmptyMap (..) )
import Test.Hspec
    ( Expectation, Spec, describe, it )
import Test.ShouldNotTypecheck
    ( shouldNotTypecheck )

spec :: Spec
spec = describe "Strict non-empty map type error tests" $

    it "Default data constructor is not exported"
        testDefaultDataConstructorNotExported

-- Test that the default data constructor for 'NonEmptyMap' is not exported.
--
-- The default data constructor for 'NonEmptyMap' is not exported, by design,
-- as the internal data structure has an invariant that must be preserved
-- across all operations.
--
-- Exporting the default data constructor would make it possible for functions
-- in external modules to break the invariant, opening the door to subtle
-- regressions.
--
-- See the definition of 'NonEmptyMap' for more details of the invariant.
--
testDefaultDataConstructorNotExported :: Expectation
testDefaultDataConstructorNotExported =
    shouldNotTypecheck $ NonEmptyMap ((), ()) mempty
