module Cardano.Wallet.Launch.Cluster.UnsafeInterval
    ( unsafeUnitInterval
    , unsafeNonNegativeInterval
    , unsafePositiveUnitInterval
    )
where

import Prelude

import Cardano.Ledger.BaseTypes
    ( BoundedRational (boundRational)
    , NonNegativeInterval
    , PositiveUnitInterval
    , UnitInterval
    )
import Data.Maybe
    ( fromMaybe
    )
import GHC.Stack
    ( HasCallStack
    )

unsafeUnitInterval :: HasCallStack => Rational -> UnitInterval
unsafeUnitInterval x =
    fromMaybe
        (error $ "unsafeUnitInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafeNonNegativeInterval :: HasCallStack => Rational -> NonNegativeInterval
unsafeNonNegativeInterval x =
    fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafePositiveUnitInterval :: HasCallStack => Rational -> PositiveUnitInterval
unsafePositiveUnitInterval x =
    fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)
