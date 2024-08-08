module Cardano.Wallet.UI.Signal
    ( Signal (..)
    )
where

import Prelude

-- | Signals that can be collected from the wallet.
data Signal
    = NewTip
    deriving (Show)
