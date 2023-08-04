{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Monadic utility functions.
module Control.Monad.Util
  ( applyNM
  )
where

import Prelude

-- | Applies a monadic function @n@ times to an initial value.
--
-- Returns the final result, equivalent to the nth element of the following
-- sequence:
--
-- @
-- [ pure a
-- , f a
-- , (f <=< f) a
-- , (f <=< f <=< f) a
-- , (f <=< f <=< f <=< f) a
-- , ...
-- ]
-- @
applyNM :: forall m a. Monad m => Int -> (a -> m a) -> a -> m a
applyNM n next = loop n
  where
    loop :: Int -> a -> m a
    loop !i !a
      | i <= 0 = pure a
      | otherwise = loop (i - 1) =<< next a
