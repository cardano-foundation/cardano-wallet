{-# LANGUAGE TypeFamilies #-}

module Data.DeltaMap
    ( DeltaMap(..)
    ) where

import Data.Delta
    ( Delta (..) )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import Prelude
    ( Ord )


{-------------------------------------------------------------------------------
    A Delta type for Maps,
    useful for handling multiple wallets.
-------------------------------------------------------------------------------}
-- | Delta type for 'Map'.
data DeltaMap key da
    = Insert key (Base da)
    | Delete key
    | Adjust key da

instance (Ord key, Delta da) => Delta (DeltaMap key da) where
    type Base (DeltaMap key da) = Map key (Base da)
    apply (Insert key a) = Map.insert key a
    apply (Delete key) = Map.delete key
    apply (Adjust key da) = Map.adjust (apply da) key
