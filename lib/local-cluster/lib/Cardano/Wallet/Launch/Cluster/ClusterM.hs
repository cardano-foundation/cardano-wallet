{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM (..)
    , traceClusterLog
    , runClusterM
    , askRunner
    , bracketTracer'
    )
where

import Prelude

import Cardano.BM.Extra
    ( bracketTracer
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (ask)
    , ReaderT (..)
    , asks
    )
import Control.Monad.Trans.Resource
    ( MonadUnliftIO
    )
import Control.Tracer
    ( contramap
    , traceWith
    )
import Data.Text
    ( Text
    )

newtype ClusterM a = ClusterM
    { unClusterM :: ReaderT Config IO a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Config
        , MonadUnliftIO
        , MonadFail
        )

traceClusterLog :: ClusterLog -> ClusterM ()
traceClusterLog msg = do
    tracer <- asks cfgTracer
    liftIO $ traceWith tracer msg

runClusterM :: Config -> ClusterM a -> IO a
runClusterM cfg = flip runReaderT cfg . unClusterM

askRunner :: ClusterM (Config, ClusterM a -> IO a)
askRunner = do
    cfg <- ask
    return (cfg, runClusterM cfg)

bracketTracer' :: Text -> ClusterM a -> ClusterM a
bracketTracer' name f = do
    config@Config{..} <- ask
    liftIO $ bracketTracer (contramap (MsgBracket name) cfgTracer) $ do
        runClusterM config f
