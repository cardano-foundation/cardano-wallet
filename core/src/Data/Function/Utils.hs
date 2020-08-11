{-# LANGUAGE BangPatterns #-}

module Data.Function.Utils
    ( applyN
    ) where

import Prelude

-- | Apply a function 'n' times to the specified input.
applyN :: Integral n => n -> (a -> a) -> a -> a
applyN !n !f !a
    | n <= 0 = a
    | otherwise = applyN (n - 1) f (f a)
