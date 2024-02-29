{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Test properties of the delegations-history 'Store'.
module Cardano.Wallet.DB.Store.Delegations.StoreSpec
    ( spec
    )
where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..)
    , runQuery
    )
import Cardano.Pool.Types
    ( PoolId (..)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , logScale
    , withDBInMemory
    , withInitializedWalletProp
    )
import Cardano.Wallet.DB.Store.Delegations.Store
    ( mkStoreDelegations
    )
import Cardano.Wallet.Delegation.Model
    ( Operation (..)
    , Status (..)
    , Transition (..)
    , status
    )
import Cardano.Wallet.Delegation.ModelSpec
    ( Config (..)
    , genDelta
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.List
    ( nub
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , choose
    , counterexample
    , generate
    , property
    , suchThat
    , vectorOf
    , (===)
    )
import Test.Store
    ( applyS
    , checkLaw
    , context
    , ignore
    , observe
    , prop_StoreUpdate
    , reset
    , unitTestStore
    )

spec :: Spec
spec =
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "delegations-history single wallet store" $ do
            it "probably respects store laws"
                $ property . prop_StoreDelegationsLaws
            it "unit pass store laws" $ property . units

prop_StoreDelegationsLaws :: WalletProperty
prop_StoreDelegationsLaws db _ =
    prop_StoreUpdate
        (runQuery db)
        (pure mkStoreDelegations)
        (pure mempty)
        (logScale . genDelta conf)

conf :: Config SlotNo DRep PoolId
conf =
    Config
        { genSlotBefore = \t -> toEnum <$> choose (0, fromEnum t)
        , genSlotAfter = \t ->
            toEnum <$> choose (fromEnum t + 1, fromEnum t + 10)
        , genSlotNew = toEnum <$> choose (0, 9)
        , genNewPool = \xs -> arbitrary `suchThat` (not . (`elem` xs))
        , genNewDRep = \xs -> arbitrary `suchThat` (not . (`elem` xs))
        }

pattern Register :: slot -> Operation slot drep pool
pattern Register i = ApplyTransition (VoteAndDelegate Nothing Nothing) i

pattern Delegate :: pool -> slot -> Operation slot drep pool
pattern Delegate p i = ApplyTransition (VoteAndDelegate Nothing (Just p)) i

pattern Vote :: drep -> slot -> Operation slot drep pool
pattern Vote v i = ApplyTransition (VoteAndDelegate (Just v) Nothing) i

pattern Deregister' :: slot -> Operation slot drep pool
pattern Deregister' i = ApplyTransition Deregister i

pattern DelegateAndVote :: pool -> drep -> slot -> Operation slot drep pool
pattern DelegateAndVote p v i
    = ApplyTransition (VoteAndDelegate (Just v) (Just p)) i

pattern Registered :: Status drep pool
pattern Registered = Active Nothing Nothing

pattern Delegating :: pool -> Status drep pool
pattern Delegating p = Active Nothing (Just p)

pattern Voting :: drep -> Status drep pool
pattern Voting v = Active (Just v) Nothing

pattern DelegatingAndVoting :: pool -> drep -> Status drep pool
pattern DelegatingAndVoting p v = Active (Just v) (Just p)

units :: WalletProperty
units = withInitializedWalletProp $ \_ runQ -> do
    [p0 :: PoolId, p1, _p2] <-
        liftIO
            $ generate
            $ vectorOf 3 arbitrary `suchThat` (\xs -> xs == nub xs)
    [v0 :: DRep , v1, _v2] <-
        liftIO
            $ generate
            $ vectorOf 3 arbitrary `suchThat` (\xs -> xs == nub xs)
    let
        unit x f = context (counterexample x) $ reset >> f >> checkLaw
        observeStatus s x = observe $ \h -> status s h === x
    runQ $ unitTestStore mempty mkStoreDelegations $ do
        unit "reg-dereg" $ do
            applyS $ Register 0
            applyS $ Deregister' 0
            observeStatus 0 Inactive
        unit "reg-dereg, different time" $ do
            applyS $ Register 0
            observeStatus 0 Registered
            applyS $ Deregister' 1
            observeStatus 0 Registered
            observeStatus 1 Inactive
        unit "dereg-reg" $ do
            applyS $ Deregister' 0
            applyS $ Register 0
            observeStatus 0 Registered
        unit "dereg-reg different time" $ do
            applyS $ Deregister' 0
            applyS $ Register 1
            observeStatus 1 Registered
        unit "reg-deleg" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            observeStatus 0 $ Delegating p0
        unit "reg-deleg 2" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            applyS $ Delegate p1 0
            observeStatus 0 $ Delegating p1
        unit "reg-deleg different time" $ do
            applyS $ Register 0
            applyS $ Delegate p0 1
            applyS $ Delegate p1 2
            observeStatus 2 $ Delegating p1
        unit "reg-deleg-dereg" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            applyS $ Deregister' 1
            observeStatus 2 Inactive
        unit "reg-vote" $ do
            applyS $ Register 0
            applyS $ Vote v0 0
            observeStatus 0 $ Voting v0
        unit "reg-vote 2" $ do
            applyS $ Register 0
            applyS $ Vote v0 0
            applyS $ Vote v1 0
            observeStatus 0 $ Voting v1
        unit "reg-vote different time" $ do
            applyS $ Register 0
            applyS $ Vote v0 1
            applyS $ Vote v1 2
            observeStatus 2 $ Voting v1
        unit "reg-vote-dereg" $ do
            applyS $ Register 0
            applyS $ Vote v0 0
            applyS $ Deregister' 1
            observeStatus 2 Inactive
        unit "reg-deleg-and-vote" $ do
            applyS $ Register 0
            applyS $ DelegateAndVote p0 v0 0
            observeStatus 0 $ DelegatingAndVoting p0 v0
        unit "reg-deleg-and-vote 2" $ do
            applyS $ Register 0
            applyS $ DelegateAndVote p0 v0 0
            applyS $ DelegateAndVote p1 v1 0
            observeStatus 0 $ DelegatingAndVoting p1 v1
        unit "reg-deleg-and-vote different time" $ do
            applyS $ Register 0
            applyS $ DelegateAndVote p0 v0 1
            applyS $ DelegateAndVote p1 v1 2
            observeStatus 2 $ DelegatingAndVoting p1 v1
        unit "reg-deleg-and-vote-dereg" $ do
            applyS $ Register 0
            applyS $ DelegateAndVote p0 v0 0
            applyS $ Deregister' 1
            observeStatus 2 Inactive
        unit "reg-deleg-then-vote" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            applyS $ Vote v0 0
            observeStatus 0 $ DelegatingAndVoting p0 v0
        unit "reg-deleg-then-vote different time" $ do
            applyS $ Register 0
            applyS $ Delegate p0 1
            applyS $ Vote v1 2
            observeStatus 2 $ DelegatingAndVoting p0 v1
        unit "reg-vote-then-deleg" $ do
            applyS $ Register 0
            applyS $ Vote v0 0
            applyS $ Delegate p0 0
            observeStatus 0 $ DelegatingAndVoting p0 v0
        unit "reg-vote-then-deleg different time" $ do
            applyS $ Register 0
            applyS $ Vote v0 1
            applyS $ Delegate p0 2
            observeStatus 2 $ DelegatingAndVoting p0 v0
        unit "reg-deleg-then-deleg-and-vote" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            applyS $ DelegateAndVote p1 v0 0
            observeStatus 0 $ DelegatingAndVoting p1 v0
        unit "reg-deleg-then-deleg-and-vote different time" $ do
            applyS $ Register 0
            applyS $ Delegate p0 1
            applyS $ DelegateAndVote p1 v1 2
            observeStatus 2 $ DelegatingAndVoting p1 v1
        unit "reg-vote-then-deleg-and-vote" $ do
            applyS $ Register 0
            applyS $ Vote v0 0
            applyS $ DelegateAndVote p0 v1 0
            observeStatus 0 $ DelegatingAndVoting p0 v1
        unit "reg-vote-then-deleg-and-vote different time" $ do
            applyS $ Register 0
            applyS $ Vote v0 1
            applyS $ DelegateAndVote p0 v1 2
            observeStatus 2 $ DelegatingAndVoting p0 v1
        ignore
            $ unit "xxxx"
            $ observeStatus 2 Registered
