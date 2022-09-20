{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fno-warn-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.Primitive.Types.UTxOSelectionSpec.TypeErrorSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection (..), UTxOSelectionNonEmpty (..) )
import Test.Hspec
    ( Expectation, Spec, describe, it )
import Test.ShouldNotTypecheck
    ( shouldNotTypecheck )

-- The data constructors for 'UTxOSelection' and 'UTxOSelectionNonEmpty' are
-- not exported, by design, as their internal data structures have invariants
-- that must be preserved across all operations.
--
-- Exporting these constructors would make it possible for functions outside
-- the 'UTxOSelection' module to break these invariants.
--
-- Therefore, we test here that they are not exported.
--
spec :: Spec
spec = describe "UTxOSelection type error tests" $ do

    it "Data constructor is not exported for UTxOSelection"
        testDataConstructorNotExportedForUTxOSelection
    it "Data constructor is not exported for UTxOSelectionNonEmpty"
        testDataConstructorNotExportedForUTxOSelectionNonEmpty

testDataConstructorNotExportedForUTxOSelection :: Expectation
testDataConstructorNotExportedForUTxOSelection =
    shouldNotTypecheck UTxOSelection

testDataConstructorNotExportedForUTxOSelectionNonEmpty :: Expectation
testDataConstructorNotExportedForUTxOSelectionNonEmpty =
    shouldNotTypecheck UTxOSelectionNonEmpty
