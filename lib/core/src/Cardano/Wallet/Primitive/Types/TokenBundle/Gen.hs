module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinSmallPositive, shrinkCoinSmallPositive )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMapSmallRange, shrinkTokenMapSmallRange )
import Test.QuickCheck
    ( Gen )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

--------------------------------------------------------------------------------
-- Token bundles with coins, assets, and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange = TokenBundle
    <$> genCoinSmallPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinSmallPositive)
        (m, shrinkTokenMapSmallRange)
