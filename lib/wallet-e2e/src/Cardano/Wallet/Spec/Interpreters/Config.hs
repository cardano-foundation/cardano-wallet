module Cardano.Wallet.Spec.Interpreters.Config where

import Cardano.Wallet.Spec.Lib.Paths
    ( DirOf
    )

newtype TraceConfiguration = TraceConfiguration
    { traceConfigurationDir :: DirOf "tracing-dir"
    }
