{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit (..)
    )
where

import Control.DeepSeq
    ( NFData
    )
import Data.Quantity
    ( Quantity
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Prelude

data ValidityIntervalExplicit = ValidityIntervalExplicit
    { invalidBefore :: !(Quantity "slot" Word64)
    , invalidHereafter :: !(Quantity "slot" Word64)
    }
    deriving (Generic, Eq, Show)
    deriving anyclass (NFData)
