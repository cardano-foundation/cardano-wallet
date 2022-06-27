{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Wallets.StoreSpec ( spec ) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Store.Meta.ModelSpec
    ( genDeltasForManipulate )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( DeltaTxWalletsHistory (..), mkStoreTxWalletsHistory )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( NonEmptyList (getNonEmpty), arbitrary, frequency, property )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

spec :: Spec
spec = around withDBInMemory $ do
    describe "wallets-transactions store" $ do
        it "respects store laws" $ property . prop_StoreWalletsLaws

prop_StoreWalletsLaws :: WalletProperty
prop_StoreWalletsLaws =
    withInitializedWalletProp $ \wid runQ -> prop_StoreUpdates
        runQ
        mkStoreTxWalletsHistory
        (pure mempty)
        (logScale . genDeltaTxWallets wid)

genDeltaTxWallets :: W.WalletId -> GenDelta DeltaTxWalletsHistory
genDeltaTxWallets wid (_,wmeta) = do
    let metaGens = case Map.lookup wid wmeta of
            Nothing -> []
            Just history
                -> [( 10
                    , ChangeTxMetaWalletsHistory wid
                      <$> frequency (genDeltasForManipulate history))
                  , (5, pure GarbageCollectTxWalletsHistory)
                  , (1, pure $ RemoveWallet wid)
                   ]
    frequency
        $ (10, ExpandTxWalletsHistory wid . getNonEmpty <$> arbitrary)
        : metaGens

