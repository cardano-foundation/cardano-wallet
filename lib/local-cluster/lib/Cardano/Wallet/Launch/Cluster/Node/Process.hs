module Cardano.Wallet.Launch.Cluster.Node.Process where

import Prelude

import Cardano.BM.Tracer
    ( Tracer
    )
import Cardano.Launcher
    ( ProcessHasExited
    )
import Cardano.Launcher.Node
    ( CardanoNodeConfig
    , CardanoNodeConn
    , withCardanoNode
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (MsgLauncher)
    )
import Control.Exception
    ( throwIO
    )
import Control.Monad
    ( (>=>)
    )
import Control.Tracer
    ( Contravariant (..)
    )

withCardanoNodeProcess
    :: Tracer IO ClusterLog
    -> String
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -> IO a
withCardanoNodeProcess tr name cfg = withCardanoNode tr' cfg >=> throwErrs
  where
    tr' = contramap (MsgLauncher name) tr
    throwErrs :: Either ProcessHasExited a -> IO a
    throwErrs = either throwIO pure
