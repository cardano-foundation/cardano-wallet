{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}

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
    ( XPrv )
import Cardano.DB.Sqlite
    ( ForeignKeysSetting (ForeignKeysDisabled) )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( RunQuery
    , WalletProperty
    , logScale
    , withDBInMemory
    , withInitializedWalletProp
    )
import Cardano.Wallet.DB.Store.PrivateKey.Store
    ( DeltaPrivateKey, mkStorePrivateKey )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Credentials
    ( Credentials (Credentials), HashedCredentials )
import Data.Delta
    ( Replace (..) )
import Fmt
    ( Buildable (..) )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary, Gen, arbitrary, property )
import Test.QuickCheck.Monadic
    ( PropertyM )
import Test.Store
    ( prop_StoreUpdates )

spec :: Spec
spec =
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "private-key store" $ do
            it "respects store laws for ShelleyKeyS"
                $ property . prop_SingleWalletStoreLaws ShelleyKeyS
            it "respects store laws for ByronKeyS"
                $ property . prop_SingleWalletStoreLaws ByronKeyS

prop_SingleWalletStoreLaws
    :: (Eq (k 'RootK XPrv), Show (k 'RootK XPrv), Arbitrary (k 'RootK XPrv))
    => KeyFlavorS k
    -> WalletProperty
prop_SingleWalletStoreLaws kF = do
    withInitializedWalletProp $ \wid runQ -> do
        propStore runQ wid kF

propStore
    :: (Eq (k 'RootK XPrv), Show (k 'RootK XPrv), Arbitrary (k 'RootK XPrv))
    => RunQuery
    -> WalletId
    -> KeyFlavorS k
    -> PropertyM IO ()
propStore runQ wid kF =
    prop_StoreUpdates
        runQ
        (mkStorePrivateKey kF wid)
        genPrivateKey
        (logScale . genDelta)

genPrivateKey :: Arbitrary (k 'RootK XPrv) => Gen (Maybe (HashedCredentials k))
genPrivateKey = fmap Just $ Credentials <$> arbitrary <*> arbitrary

instance Buildable (DeltaPrivateKey k) where
    build _ = "DeltaPrivateKey"

genDelta
    :: Arbitrary (k 'RootK XPrv)
    => (Maybe (HashedCredentials k))
    -> Gen (DeltaPrivateKey k)
genDelta _ = Replace <$> genPrivateKey
