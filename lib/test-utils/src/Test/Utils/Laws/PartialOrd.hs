{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides laws for the 'PartialOrd' class.
module Test.Utils.Laws.PartialOrd
  ( partialOrdLaws
  )
where

import Algebra.PartialOrd
  ( PartialOrd (..)
  )
import Data.Proxy
  ( Proxy
  )
import Test.QuickCheck
  ( Arbitrary
  , Property
  , property
  )
import Test.QuickCheck.Classes
  ( Laws (..)
  )
import Prelude

partialOrdLaws :: (PartialOrd a, Arbitrary a, Show a) => Proxy a -> Laws
partialOrdLaws p =
  Laws
    "PartialOrd"
    [
      ( "Antisymmetry"
      , partialOrdAntisymmetric p
      )
    ,
      ( "Reflexivity"
      , partialOrdReflexive p
      )
    ,
      ( "Transitivity"
      , partialOrdTransitive p
      )
    ]

partialOrdAntisymmetric
  :: forall a. (Show a, PartialOrd a, Arbitrary a) => Proxy a -> Property
partialOrdAntisymmetric _ = property
  $ \(a :: a) b -> ((a `leq` b) && (b `leq` a)) == (a == b)

partialOrdReflexive
  :: forall a. (Show a, PartialOrd a, Arbitrary a) => Proxy a -> Property
partialOrdReflexive _ = property
  $ \(a :: a) -> a `leq` a

partialOrdTransitive
  :: forall a. (Show a, PartialOrd a, Arbitrary a) => Proxy a -> Property
partialOrdTransitive _ = property test
  where
    test (a :: a) b c
      | a `leq` b && b `leq` c = a `leq` c
      | a `leq` c && c `leq` b = a `leq` b
      | b `leq` a && a `leq` c = b `leq` c
      | b `leq` c && c `leq` a = b `leq` a
      | c `leq` a && a `leq` b = c `leq` b
      | c `leq` b && b `leq` a = c `leq` a
      | otherwise = True
