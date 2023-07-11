{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Test properties of the rewards-history 'Store'.
module Cardano.Wallet.DB.Store.Rewards.StoreSpec (
    spec,
) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..), SqliteContext, runQuery )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( logScale, withDBInMemory )
import Cardano.Wallet.DB.Store.Rewards.Store
    ( mkStoreRewards )
import Data.Delta
    ( Replace (..) )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, property )
import Test.Store
    ( prop_StoreUpdate )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W

spec :: Spec
spec =
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "latest-rewards of a wallet store" $ do
            it "probably respects store laws" $
                property . prop_StoreRewardsLaws

prop_StoreRewardsLaws :: SqliteContext -> W.WalletId -> Property
prop_StoreRewardsLaws db wid =
    prop_StoreUpdate
        (runQuery db)
        (pure $ mkStoreRewards wid)
        (pure $ W.Coin 0)
        (logScale . genDelta)

genDelta :: p -> Gen (Replace W.Coin)
genDelta _ = Replace <$> arbitrary
