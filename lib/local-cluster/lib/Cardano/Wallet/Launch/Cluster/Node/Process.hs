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
    , UnliftClusterM (UnliftClusterM)
    , askUnliftClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , NodeId
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Control.Tracer
    ( Contravariant (..)
    )
import Data.Functor.Contravariant
    ( (>$<)
    )

withCardanoNodeProcess
    :: NodeId
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> ClusterM a)
    -> ClusterM a
withCardanoNodeProcess name cfg f = do
    Config{..} <- ask
    UnliftClusterM withConfig _ <- askUnliftClusterM
    liftIO
        $ withCardanoNode
            (contramap (MsgLauncher $ show name) cfgTracer)
            (Just $ MsgNodeStdout name >$< cfgTracer)
            (Just $ MsgNodeStderr name >$< cfgTracer)
            cfg
        $ withConfig . f
