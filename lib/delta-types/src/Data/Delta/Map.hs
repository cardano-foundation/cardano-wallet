{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright: © 2021-2023 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Delta types for 'Data.Map.Map'.
-}
module Data.Delta.Map
    ( DeltaMap(..)
    ) where

import Prelude

import Data.Delta.Core
    ( Delta (..)
    )
import Data.Map.Strict
    ( Map
    )
import Fmt
    ( Buildable (..)
    )

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    Delta type for 'Map'
-------------------------------------------------------------------------------}
-- | Delta type for 'Map'.
data DeltaMap key da
    = Insert key (Base da)
    | Delete key
    | Adjust key da

deriving instance (Show key, Show da, Show (Base da)) => Show (DeltaMap key da)
instance (Ord key, Delta da)
    => Delta (DeltaMap key da) where
    type Base (DeltaMap key da) = Map key (Base da)
    apply (Insert key a) = Map.insert key a
    apply (Delete key) = Map.delete key
    apply (Adjust key da) = Map.adjust (apply da) key

instance (Show key, Show da, Show (Base da))
    => Buildable (DeltaMap key da) where
    build = build . show
