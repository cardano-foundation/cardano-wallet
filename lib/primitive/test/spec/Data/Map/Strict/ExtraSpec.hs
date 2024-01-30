{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Copyright: © 2023 Cardano Foundation
--
module Data.Map.Strict.ExtraSpec
    ( spec
    ) where

import Prelude hiding
    ( filter
    )

import Data.Function
    ( (&)
    )
import Data.Map.Strict
    ( Map
    , filter
    , fromList
    , intersectionWith
    )
import Data.Map.Strict.Extra
    ( conflictsWith
    )
import Data.String
    ( IsString
    )
import Data.Tuple
    ( swap
    )
import GHC.Generics
    ( Generic
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary
    , CoArbitrary
    , Fun
    , Function (..)
    , Property
    , pattern Fn2
    , property
    , (===)
    )
import Test.QuickCheck.Property
    ( checkCoverage
    , cover
    )
import Test.QuickCheck.Quid
    ( Latin (..)
    , Quid
    , Size (..)
    )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "conflictsWith" $ do
        it "example_conflictsWith" $
            example_conflictsWith
                & property
        it "prop_conflictsWith_intersectionWith_filter" $
            prop_conflictsWith_intersectionWith_filter
                & property
        it "prop_conflictsWith_flip_swap" $
            prop_conflictsWith_flip_swap
                & property

--------------------------------------------------------------------------------
-- Test key and value types
--------------------------------------------------------------------------------

newtype TestKey = TestKey Quid
    deriving stock (Eq, Generic, Ord)
    deriving (IsString, Show) via Latin Quid
    deriving Arbitrary via Size 4 Quid
    deriving CoArbitrary via Quid
    deriving anyclass Function

newtype TestValue = TestValue Int
    deriving stock (Eq, Generic, Ord)
    deriving newtype (Arbitrary, CoArbitrary, Show, Num)
    deriving anyclass Function

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

example_conflictsWith :: Property
example_conflictsWith =
    conflictsWith @TestKey @TestValue (/=)
        (fromList [("A", 1), ("B", 1), ("C", 1), ("D", 1)          ])
        (fromList [          ("B", 1), ("C", 2), ("D", 1), ("E", 1)])
    ===
        (fromList [("C", (1, 2))])

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_conflictsWith_intersectionWith_filter
    :: Fun (TestValue, TestValue) Bool
    -> Map TestKey TestValue
    -> Map TestKey TestValue
    -> Property
prop_conflictsWith_intersectionWith_filter (Fn2 f) m1 m2 =
    conflictsWith f m1 m2 === filter (uncurry f) (intersectionWith (,) m1 m2)
    & cover 50
        (Map.size (conflictsWith f m1 m2) > 1)
        "Map.size (conflictsWith f m1 m2) > 1"
    & checkCoverage

prop_conflictsWith_flip_swap
    :: Fun (TestValue, TestValue) Bool
    -> Map TestKey TestValue
    -> Map TestKey TestValue
    -> Property
prop_conflictsWith_flip_swap (Fn2 f) m1 m2 =
    conflictsWith f m1 m2 === fmap swap (conflictsWith (flip f) m2 m1)
    & cover 50
        (Map.size (conflictsWith f m1 m2) > 1)
        "Map.size (conflictsWith f m1 m2) > 1"
    & checkCoverage
