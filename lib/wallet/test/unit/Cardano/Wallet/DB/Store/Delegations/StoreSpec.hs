{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    (
    )
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
    , status
    )
import Cardano.Wallet.Delegation.ModelSpec
    ( Config (..)
    , genDelta
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.List
    ( nub
    )
import Fmt
    ( Buildable (..)
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

conf :: Config SlotNo PoolId
conf =
    Config
        { genSlotBefore = \t -> toEnum <$> choose (0, fromEnum t)
        , genSlotAfter = \t ->
            toEnum <$> choose (fromEnum t + 1, fromEnum t + 10)
        , genSlotNew = toEnum <$> choose (0, 9)
        , genNewPool = \xs -> arbitrary `suchThat` (not . (`elem` xs))
        }

instance Buildable (Operation SlotNo PoolId) where
    build = build . show

units :: WalletProperty
units = withInitializedWalletProp $ \_ runQ -> do
    [p0, p1, _p2] <-
        liftIO
            $ generate
            $ vectorOf 3 arbitrary `suchThat` (\xs -> xs == nub xs)
    let
        unit x f = context (counterexample x) $ reset >> f >> checkLaw
        observeStatus s x = observe $ \h -> status s h === x
    runQ $ unitTestStore mempty mkStoreDelegations $ do
        unit "reg-dereg" $ do
            applyS $ Register 0
            applyS $ Deregister 0
            observeStatus 0 Inactive
        unit "reg-dereg, different time" $ do
            applyS $ Register 0
            observeStatus 0 Registered
            applyS $ Deregister 1
            observeStatus 0 Registered
            observeStatus 1 Inactive
        unit "dereg-reg" $ do
            applyS $ Deregister 0
            applyS $ Register 0
            observeStatus 0 Registered
        unit "dereg-reg different time" $ do
            applyS $ Deregister 0
            applyS $ Register 1
            observeStatus 1 Registered
        unit "reg-deleg" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            observeStatus 0 $ Active p0
        unit "reg-deleg 2" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            applyS $ Delegate p1 0
            observeStatus 0 $ Active p1
        unit "reg-deleg different time" $ do
            applyS $ Register 0
            applyS $ Delegate p0 1
            applyS $ Delegate p1 2
            observeStatus 2 $ Active p1
        unit "reg-deleg-dereg" $ do
            applyS $ Register 0
            applyS $ Delegate p0 0
            applyS $ Deregister 1
            observeStatus 2 Inactive
        ignore
            $ unit "xxxx"
            $ observeStatus 2 Registered
