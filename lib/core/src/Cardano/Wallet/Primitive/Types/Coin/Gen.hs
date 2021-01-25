{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinAny
    , genCoinSmall
    , genCoinSmallPositive
    , genCoinLargePositive
    , shrinkCoinAny
    , shrinkCoinSmall
    , shrinkCoinSmallPositive
    , shrinkCoinLargePositive
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Test.QuickCheck
    ( Gen, choose, shrink )

--------------------------------------------------------------------------------
-- Coins chosen from the full range available
--------------------------------------------------------------------------------

genCoinAny :: Gen Coin
genCoinAny = Coin <$> choose (unCoin minBound, unCoin maxBound)

shrinkCoinAny :: Coin -> [Coin]
shrinkCoinAny (Coin c) = Coin <$> shrink c

--------------------------------------------------------------------------------
-- Coins chosen to be small and possibly zero
--------------------------------------------------------------------------------

genCoinSmall :: Gen Coin
genCoinSmall = Coin <$> choose (0, 10)

shrinkCoinSmall :: Coin -> [Coin]
shrinkCoinSmall (Coin c) = Coin <$> shrink c

--------------------------------------------------------------------------------
-- Coins chosen to be small and strictly positive
--------------------------------------------------------------------------------

genCoinSmallPositive :: Gen Coin
genCoinSmallPositive = Coin <$> choose (1, 10)

shrinkCoinSmallPositive :: Coin -> [Coin]
shrinkCoinSmallPositive (Coin c) = Coin <$> filter (> 0) (shrink c)

--------------------------------------------------------------------------------
-- Coins chosen from a large range and strictly positive
--------------------------------------------------------------------------------

genCoinLargePositive :: Gen Coin
genCoinLargePositive = Coin <$> choose (1, 1_000_000_000_000)

shrinkCoinLargePositive :: Coin -> [Coin]
shrinkCoinLargePositive (Coin c) = Coin <$> filter (> 0) (shrink c)
