{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinFullRange
    , genCoinSmall
    , genCoinSmallPositive
    , genCoinLargePositive
    , shrinkCoinFullRange
    , shrinkCoinSmall
    , shrinkCoinSmallPositive
    , shrinkCoinLargePositive
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Test.QuickCheck
    ( Gen, choose, frequency, shrink )

--------------------------------------------------------------------------------
-- Coins chosen from the full range available.
--------------------------------------------------------------------------------

-- | Generates coins across the full range available.
--
-- This generator has a slight bias towards the limits of the range, but
-- otherwise generates values uniformly across the whole range.
--
-- This can be useful when testing roundtrip conversions between different
-- types.
--
genCoinFullRange :: Gen Coin
genCoinFullRange = frequency
    [ (1, pure (Coin 0))
    , (1, pure (maxBound :: Coin))
    , (8, Coin <$> choose (1, unCoin (maxBound :: Coin) - 1))
    ]

shrinkCoinFullRange :: Coin -> [Coin]
shrinkCoinFullRange (Coin c) = Coin <$> shrink c

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
