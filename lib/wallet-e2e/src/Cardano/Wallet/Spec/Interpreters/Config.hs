module Cardano.Wallet.Spec.Interpreters.Config where

import Data.Tagged
    ( Tagged
    )
import Path
    ( Abs
    , Dir
    , Path
    )

newtype TraceConfiguration = TraceConfiguration
    { traceConfigurationDir :: Tagged "tracing-dir" (Path Abs Dir)
    }
