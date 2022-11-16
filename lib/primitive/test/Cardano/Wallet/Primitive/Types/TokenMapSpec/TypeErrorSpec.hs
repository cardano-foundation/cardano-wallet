{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fno-warn-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.Primitive.Types.TokenMapSpec.TypeErrorSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap (..) )
import Test.Hspec
    ( Expectation, Spec, describe, it )
import Test.ShouldNotTypecheck
    ( shouldNotTypecheck )

spec :: Spec
spec = describe "Token map type error tests" $

    it "Default data constructor is not exported"
        testDefaultDataConstructorNotExported

-- Test that the default data constructor for 'TokenMap' is not exported.
--
-- The default data constructor for 'TokenMap' is not exported, by design, as
-- the internal data structure has an invariant that must be preserved across
-- all operations.
--
-- Exporting the default constructor would make it possible for functions
-- outside the 'TokenMap' module to break the invariant, opening the door to
-- subtle regressions.
--
-- See the definition of 'TokenMap' for more details of the invariant.
--
testDefaultDataConstructorNotExported :: Expectation
testDefaultDataConstructorNotExported = shouldNotTypecheck (TokenMap mempty)
