module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , genTokenBundleSmallRangePositive
    , genTokenBundle
    , shrinkTokenBundle
    , shrinkTokenBundleSmallRange
    , shrinkTokenBundleSmallRangePositive
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, genTokenMapSmallRange, shrinkTokenMap )
import Test.QuickCheck
    ( Gen )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

--------------------------------------------------------------------------------
-- Token bundles with variable numbers of assets, the upper bound being
-- QuickCheck's size parameter.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genTokenBundle :: Gen TokenBundle
genTokenBundle = TokenBundle
    <$> genCoin
    <*> genTokenMap

shrinkTokenBundle :: TokenBundle -> [TokenBundle]
shrinkTokenBundle (TokenBundle c m)=
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoin)
        (m, shrinkTokenMap)

--------------------------------------------------------------------------------
-- Token bundles with coins, assets, and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange = TokenBundle
    <$> genCoin
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange = shrinkTokenBundle

genTokenBundleSmallRangePositive :: Gen TokenBundle
genTokenBundleSmallRangePositive = TokenBundle
    <$> genCoinPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRangePositive :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRangePositive (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinPositive)
        (m, shrinkTokenMap)
