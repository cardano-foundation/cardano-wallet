module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genFixedSizeTokenBundle
    , genTokenBundleSmallRange
    , genTokenBundleSmallRangePositive
    , genVariableSizedTokenBundle
    , shrinkTokenBundleSmallRange
    , shrinkTokenBundleSmallRangePositive
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin
    , genCoinFullRange
    , genCoinPositive
    , shrinkCoin
    , shrinkCoinPositive
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange, genTokenMapSmallRange, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( txOutMaxTokenQuantity, txOutMinTokenQuantity )
import Control.Monad
    ( replicateM )
import Test.QuickCheck
    ( Gen, choose, oneof )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle

--------------------------------------------------------------------------------
-- Token bundles with fixed numbers of assets.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genFixedSizeTokenBundle :: Int -> Gen TokenBundle
genFixedSizeTokenBundle fixedAssetCount
    = TokenBundle.fromFlatList
        <$> genCoinFullRange
        <*> replicateM fixedAssetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity
    genTokenQuantity = integerToTokenQuantity <$> oneof
        [ pure $ tokenQuantityToInteger txOutMinTokenQuantity
        , pure $ tokenQuantityToInteger txOutMaxTokenQuantity
        , choose
            ( tokenQuantityToInteger txOutMinTokenQuantity + 1
            , tokenQuantityToInteger txOutMaxTokenQuantity - 1
            )
        ]
      where
        tokenQuantityToInteger :: TokenQuantity -> Integer
        tokenQuantityToInteger = fromIntegral . unTokenQuantity

        integerToTokenQuantity :: Integer -> TokenQuantity
        integerToTokenQuantity = TokenQuantity . fromIntegral

--------------------------------------------------------------------------------
-- Token bundles with variable numbers of assets, with an upper bound.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genVariableSizedTokenBundle :: Int -> Gen TokenBundle
genVariableSizedTokenBundle maxAssetCount =
    genFixedSizeTokenBundle =<< choose (0, maxAssetCount)

--------------------------------------------------------------------------------
-- Token bundles with coins, assets, and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange = TokenBundle
    <$> genCoin
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoin)
        (m, shrinkTokenMap)

genTokenBundleSmallRangePositive :: Gen TokenBundle
genTokenBundleSmallRangePositive = TokenBundle
    <$> genCoinPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRangePositive :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRangePositive (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinPositive)
        (m, shrinkTokenMap)
