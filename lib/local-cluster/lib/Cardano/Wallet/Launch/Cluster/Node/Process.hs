{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.Node.Process where

import Cardano.Launcher.Node
    ( CardanoNodeConfig
    , CardanoNodeConn
    , MaybeK
    , withCardanoNode
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , UnliftClusterM (UnliftClusterM)
    , askUnliftClusterM
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
import Prelude

withCardanoNodeProcess
    :: String
    -> CardanoNodeConfig d
    -> (MaybeK d CardanoNodeConn -> ClusterM a)
    -> ClusterM a
withCardanoNodeProcess name cfg f = do
    Config{..} <- ask
    UnliftClusterM withConfig _ <- askUnliftClusterM
    liftIO
        $ withCardanoNode (contramap (MsgLauncher $ show name) cfgTracer) cfg
        $ withConfig . f
