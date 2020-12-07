{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinSmall, shrinkCoinSmall )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.TokenMap.Gen
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
    <$> genCoinSmall
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinSmall)
        (m, shrinkTokenMapSmallRange)
