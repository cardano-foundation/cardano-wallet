{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Wallet.DB.Store.Meta.StoreSpec
    ( spec )
    where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..), TxMetaHistory (..) )
import Cardano.Wallet.DB.Store.Meta.ModelSpec
    ( genDeltasForManipulate, genExpand )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Test.DBVar
    ( prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, arbitrary, frequency, property )

spec :: Spec
spec = around withDBInMemory $ do
    describe "meta-transactions store" $ do
        it "respects store laws"
            $ property . prop_StoreMetaLaws

genDeltas :: WalletId -> TxMetaHistory -> Gen DeltaTxMetaHistory
genDeltas wid history =
    frequency $
        (10, Expand <$> genExpand wid arbitrary)
            : fmap (fmap (fmap Manipulate)) (genDeltasForManipulate history)


prop_StoreMetaLaws :: WalletProperty
prop_StoreMetaLaws = withInitializedWalletProp $ \wid runQ ->
    prop_StoreUpdates
        runQ
        (mkStoreMetaTransactions  wid)
        (pure mempty)
        (logScale . genDeltas wid)
