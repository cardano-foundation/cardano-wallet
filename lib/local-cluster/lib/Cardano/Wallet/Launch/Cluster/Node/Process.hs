{-# LANGUAGE RecordWildCards #-}
module Cardano.Wallet.Launch.Cluster.Node.Process where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConfig
    , CardanoNodeConn
    , withCardanoNode
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (MsgLauncher)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Control.Tracer
    ( Contravariant (..)
    )

withCardanoNodeProcess
    :: String
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -> ClusterM a
withCardanoNodeProcess name cfg f = do
    Config{..} <- ask
    liftIO $ withCardanoNode (contramap (MsgLauncher name) cfgTracer) cfg f
