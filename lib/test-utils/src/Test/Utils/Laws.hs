{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides testing functions to check that type class instances obey laws.
module Test.Utils.Laws
    ( testLaws
    , testLawsMany
    ) where

import Prelude

import Control.Monad
    ( forM_
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , parallel
    )
import Test.QuickCheck.Classes
    ( Laws (..)
    )

-- | Constructs a test to check that the given type class instance obeys the
--   given set of laws.
--
-- Example usage:
--
-- >>> testLaws @Natural ordLaws
-- >>> testLaws @(Map Int) functorLaws
testLaws
    :: forall a
     . Typeable a
    => (Proxy a -> Laws)
    -> Spec
testLaws getLaws =
    parallel
        $ describe description
        $ forM_ (lawsProperties laws)
        $ uncurry it
  where
    description =
        mconcat
            [ "Testing "
            , lawsTypeclass laws
            , " laws for type "
            , show (typeRep $ Proxy @a)
            ]
    laws = getLaws $ Proxy @a

-- | Calls `testLaws` with multiple sets of laws.
--
-- Example usage:
--
-- >>> testLawsMany @Natural [eqLaws, ordLaws]
-- >>> testLawsMany @(Map Int) [foldableLaws, functorLaws]
testLawsMany
    :: forall a
     . Typeable a
    => [Proxy a -> Laws]
    -> Spec
testLawsMany getLawsMany =
    testLaws @a `mapM_` getLawsMany
