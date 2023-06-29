{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.TypeLevel
    ( -- * Type-level functions
      Including
    , Excluding
    ) where

import Prelude

import Data.Kind
    ( Constraint
    )
import Data.Type.Equality
    ( type (==)
    )

type family Excluding xs x :: Constraint where
    Excluding '[] _ = ()
    Excluding (x ': xs) y = ((x == y) ~ 'False, Excluding xs y)

type family Difference ys xs where
    Difference ys '[] = ys
    Difference ys (x ': xs) = Difference (FilterOut x ys) xs

type family FilterOut x xs where
    FilterOut x '[] = '[]
    FilterOut x (x ': xs) = FilterOut x xs
    FilterOut x (y ': xs) = y ': FilterOut x xs

type Including ys xs x = Excluding (Difference ys xs) x
