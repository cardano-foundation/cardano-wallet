{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.CoinSelectionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( prepareOutputsWith )
import Cardano.Wallet.Primitive.CoinSelection.BalanceSpec
    ( MockComputeMinimumAdaQuantity, unMockComputeMinimumAdaQuantity )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..), txOutCoin )
import Data.Generics.Internal.VL.Lens
    ( view )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Property, property, (===) )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelectionSpec" $

    parallel $ describe "Preparing outputs" $ do

        it "prop_prepareOutputsWith_twice" $
            property prop_prepareOutputsWith_twice
        it "prop_prepareOutputsWith_length" $
            property prop_prepareOutputsWith_length
        it "prop_prepareOutputsWith_assetsUnchanged" $
            property prop_prepareOutputsWith_assetsUnchanged
        it "prop_prepareOutputsWith_preparedOrExistedBefore" $
            property prop_prepareOutputsWith_preparedOrExistedBefore

--------------------------------------------------------------------------------
-- Preparing outputs
--------------------------------------------------------------------------------

prop_prepareOutputsWith_twice
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_twice minCoinValueDef outs =
    once === twice
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    (_:once:twice:_) = iterate (prepareOutputsWith minCoinValueFor) outs

prop_prepareOutputsWith_length
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_length minCoinValueDef outs =
    F.length (prepareOutputsWith minCoinValueFor outs) === F.length outs
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef

prop_prepareOutputsWith_assetsUnchanged
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_assetsUnchanged minCoinValueDef outs =
    (txOutAssets <$> (prepareOutputsWith minCoinValueFor outs))
    ===
    (txOutAssets <$> outs)
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    txOutAssets = TokenBundle.getAssets . view #tokens

prop_prepareOutputsWith_preparedOrExistedBefore
    :: MockComputeMinimumAdaQuantity
    -> [TxOut]
    -> Property
prop_prepareOutputsWith_preparedOrExistedBefore minCoinValueDef outs =
    property $ F.all isPreparedOrExistedBefore (zip outs outs')
  where
    minCoinValueFor = unMockComputeMinimumAdaQuantity minCoinValueDef
    outs' = prepareOutputsWith minCoinValueFor outs

    isPreparedOrExistedBefore :: (TxOut, TxOut) -> Bool
    isPreparedOrExistedBefore (before, after)
        | txOutCoin before /= Coin 0 =
            txOutCoin after == txOutCoin before
        | otherwise =
            txOutCoin after == minCoinValueFor (view (#tokens . #tokens) before)
