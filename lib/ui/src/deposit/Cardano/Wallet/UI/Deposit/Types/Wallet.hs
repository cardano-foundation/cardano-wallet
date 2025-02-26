module Cardano.Wallet.UI.Deposit.Types.Wallet
    ( Status (..)
    )
where

import Prelude

import Cardano.Wallet.Deposit.Read
    ( ChainPoint
    , NetworkTag
    , Value
    , WithOrigin
    )
import Data.Time
    ( UTCTime
    )

data Status = Status
    { tip :: ChainPoint
    , tipTime :: Maybe (WithOrigin UTCTime)
    , balance :: Value
    , network :: NetworkTag
    }
