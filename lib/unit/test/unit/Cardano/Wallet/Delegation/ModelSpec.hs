{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Test properties of the delegations-history model
module Cardano.Wallet.Delegation.ModelSpec
    ( spec
    , genDelta
    , Config (..)
    )
where

import Prelude

import Cardano.Wallet.Delegation.Model
    ( History
    , Operation (..)
    , Status (Active)
    , Transition (..)
    )
import Cardano.Wallet.Delegation.Properties
    ( Step (Step)
    , properties
    )
import Control.Arrow
    ( (&&&)
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Foldable
    ( toList
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , frequency
    , getSize
    , suchThat
    )
import Test.QuickCheck.Property
    ( Property
    , Testable (property)
    , conjoin
    , forAllShrinkShow
    , mapSize
    )
import Text.Pretty.Simple
    ( pShow
    )

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T

spec :: Spec
spec =
    describe "delegations-history operations"
        $ it "respects specification invariants"
        $ property prop_delegation_history

-- | Support for polymorphic generation of slot and pool
data Config slot drep pool = Config
    { genSlotBefore :: slot -> Gen slot
    , genSlotAfter :: slot -> Gen slot
    , genSlotNew :: Gen slot
    , genNewPool :: [pool] -> Gen pool
    , genNewDRep :: [drep] -> Gen drep
    }

-- | Crafted 'Operation' generation based on 'History'
genDelta
    :: Config slot drep pool
    -> History slot drep pool
    -> Gen (Operation slot drep pool)
genDelta c h = do
    slot <- genSlot c h
    pool <- genPool c h
    drep <- genRep c h
    elements
        [ ApplyTransition (VoteAndDelegate (Just drep) (Just pool)) slot
        , ApplyTransition (VoteAndDelegate Nothing (Just pool)) slot
        , ApplyTransition (VoteAndDelegate (Just drep) Nothing) slot
        , Rollback slot
        ]

genPool :: Config slot drep pool -> History slot drep pool -> Gen pool
genPool c h =
    let
        ps = toList h >>= poolOf
    in
        case ps of
            [] -> genNewPool c []
            pss ->
                frequency
                    [ (1, elements pss)
                    , (4, genNewPool c pss)
                    ]

genRep :: Config slot drep pool -> History slot drep pool -> Gen drep
genRep c h =
    let
        ds = toList h >>= repOf
    in
        case ds of
            [] -> genNewDRep c []
            dss ->
                frequency
                    [ (1, elements dss)
                    , (4, genNewDRep c dss)
                    ]

poolOf :: Status drep pool -> [pool]
poolOf (Active _ (Just p)) = [p]
poolOf _ = []

repOf :: Status drep pool -> [drep]
repOf (Active (Just d) _) = [d]
repOf _ = []

genSlot :: Config slot drep pool -> History slot drep pool -> Gen slot
genSlot c h =
    let
        mm = Map.lookupMax h
    in
        case mm of
            Just (maxslot, _) ->
                frequency
                    [ (1, genSlotBefore c maxslot)
                    , (4, genSlotAfter c maxslot)
                    ]
            Nothing -> genSlotNew c

genStatus
    :: (Ord slot, Eq pool, Eq drep)
    => Config slot drep pool
    -> Gen [Step slot drep pool]
genStatus c = getSize >>= go mempty
  where
    go _ 0 = pure []
    go x n = do
        d <- genDelta c x
        let x' = apply d x
        (Step x x' d :) <$> go x' (pred n)

validatedHistory
    :: ( Show pool
       , Show slot
       , Ord slot
       , Eq pool
       , Show drep
       , Eq drep
       )
    => Config slot drep pool
    -> Gen [(Property, Step slot drep pool)]
validatedHistory c =
    fmap (properties (genSlot c) &&& id)
        <$> genStatus c

shrinkByInit :: [a] -> [[a]]
shrinkByInit [] = []
shrinkByInit xs = [init xs]

naive :: Config Int Int Int
naive =
    Config
        { genSlotBefore = \x -> choose (0, x)
        , genSlotAfter = \x -> choose (x + 1, x + 10)
        , genSlotNew = choose (0, 9)
        , genNewPool = \xs -> arbitrary `suchThat` (`notElem` xs)
        , genNewDRep = \xs -> arbitrary `suchThat` (`notElem` xs)
        }

prop_delegation_history :: Property
prop_delegation_history = mapSize (* 30)
    $ forAllShrinkShow
        (validatedHistory @Int @Int naive)
        shrinkByInit
        (\xs -> T.unpack $ pShow $ last $ snd <$> xs)
    $ \xs -> conjoin . fmap fst $ xs
