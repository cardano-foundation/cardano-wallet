module Cardano.Wallet.Spec.Interpreters.Config where
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf
    )

newtype TraceConfiguration = TraceConfiguration
    { traceConfigurationDir :: DirOf "tracing-dir"
    }
