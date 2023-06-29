{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Test properties of the privatekey 'Store'.
module Cardano.Wallet.DB.Store.PrivateKey.StoreSpec
    ( spec
    )
where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    )
import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..)
    , runQuery
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK)
    )
import Cardano.Wallet.DB.Arbitrary
    (
    )
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , logScale
    , withDBInMemory
    )
import Cardano.Wallet.DB.Store.PrivateKey.Store
    ( DeltaPrivateKey
    , mkStorePrivateKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials
    , RootCredentials (RootCredentials)
    )
import Data.Delta
    ( Replace (..)
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
    ( Arbitrary
    , Gen
    , arbitrary
    , property
    )
import Test.Store
    ( prop_StoreUpdate
    )

spec :: Spec
spec =
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "private-key store" $ do
            it "respects store laws for ShelleyKeyS"
                $ property . prop_StorePrivateKeyLaws ShelleyKeyS
            it "respects store laws for ByronKeyS"
                $ property . prop_StorePrivateKeyLaws ByronKeyS

prop_StorePrivateKeyLaws
    :: (Eq (k 'RootK XPrv), Show (k 'RootK XPrv), Arbitrary (k 'RootK XPrv))
    => KeyFlavorS k
    -> WalletProperty
prop_StorePrivateKeyLaws kF db wid = do
    prop_StoreUpdate
        (runQuery db)
        (pure $ mkStorePrivateKey kF wid)
        genPrivateKey
        (logScale . genDelta)

genPrivateKey
    :: Arbitrary (k 'RootK XPrv) => Gen (Maybe (HashedCredentials k))
genPrivateKey = fmap Just $ RootCredentials <$> arbitrary <*> arbitrary

instance Buildable (DeltaPrivateKey k) where
    build _ = "DeltaPrivateKey"

genDelta
    :: Arbitrary (k 'RootK XPrv)
    => (Maybe (HashedCredentials k))
    -> Gen (DeltaPrivateKey k)
genDelta _ = Replace <$> genPrivateKey
