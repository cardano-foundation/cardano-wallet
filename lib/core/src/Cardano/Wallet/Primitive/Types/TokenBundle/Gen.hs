{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genFixedSizeTokenBundle
    , genTokenBundleSmallRange
    , genTokenBundleSmallRangePositive
    , shrinkTokenBundleSmallRange
    , shrinkTokenBundleSmallRangePositive
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinSmall
    , genCoinSmallPositive
    , shrinkCoinSmall
    , shrinkCoinSmallPositive
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange, genTokenMapSmallRange, shrinkTokenMapSmallRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( txOutMaxTokenQuantity, txOutMinTokenQuantity )
import Control.Monad
    ( replicateM )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Gen, choose )
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
        <$> genCoin
        <*> replicateM fixedAssetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity
    genCoin = Coin
        <$> choose (unCoin minBound, unCoin maxBound)
    genTokenQuantity = TokenQuantity . fromIntegral @Integer @Natural
        <$> choose
            ( fromIntegral $ unTokenQuantity txOutMinTokenQuantity
            , fromIntegral $ unTokenQuantity txOutMaxTokenQuantity
            )

--------------------------------------------------------------------------------
-- Token bundles with coins, assets, and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange = TokenBundle
    <$> genCoinSmall
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinSmall)
        (m, shrinkTokenMapSmallRange)

genTokenBundleSmallRangePositive :: Gen TokenBundle
genTokenBundleSmallRangePositive = TokenBundle
    <$> genCoinSmallPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRangePositive :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRangePositive (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinSmallPositive)
        (m, shrinkTokenMapSmallRange)
