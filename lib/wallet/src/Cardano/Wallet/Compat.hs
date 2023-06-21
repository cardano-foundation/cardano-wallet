-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Temporary compatibility functions for the ghc-8.10 update.

module Cardano.Wallet.Compat
    ( (^?)
    ) where

import Prelude

import Control.Applicative
    ( Const (..) )
import Data.Monoid
    ( First (..) )
import Data.Profunctor.Unsafe
    ( (#.) )

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)
