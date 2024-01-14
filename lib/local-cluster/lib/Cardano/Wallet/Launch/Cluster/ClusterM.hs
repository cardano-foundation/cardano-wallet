{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM (..)
    , traceClusterLog
    , runClusterM
    , UnliftClusterM (..)
    , askUnliftClusterM
    , bracketTracer'
    , askNodeDir
    )
where

import Prelude

import Cardano.BM.Extra
    ( bracketTracer
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , NodeSegment (..)
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
import Data.Tagged
    ( untag
    )
import Data.Text
    ( Text
    )
import System.FilePath
    ( (</>)
    )

newtype ClusterM a = ClusterM
    {unClusterM :: ReaderT Config IO a}
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

data UnliftClusterM = UnliftClusterM (forall a. ClusterM a -> IO a) Config

askUnliftClusterM :: ClusterM UnliftClusterM
askUnliftClusterM = do
    cfg <- ask
    pure $ UnliftClusterM (runClusterM cfg) cfg

bracketTracer' :: Text -> ClusterM a -> ClusterM a
bracketTracer' name f = do
    UnliftClusterM withConfig Config{..} <- askUnliftClusterM
    liftIO
        $ bracketTracer (contramap (MsgBracket name) cfgTracer)
        $ withConfig f

askNodeDir :: NodeSegment -> ClusterM FilePath
askNodeDir (NodeSegment nodeSegment) = do
    Config{..} <- ask
    pure $ untag cfgClusterDir </> nodeSegment
