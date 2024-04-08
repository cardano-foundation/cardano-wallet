module Cardano.Wallet.Launch.Cluster.Monitoring.Monitor
    ( withMonitor
    )
where

import Prelude

import Cardano.BM.Tracing
    ( Tracer (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans.Resource
    ( MonadUnliftIO
    )
import Control.Monitoring
    ( MonitorState (..)
    , mkFoldingMonitor
    , mkMonitor
    , runMonitor
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Time
    ( getCurrentTime
    )

import qualified Control.Foldl as F

withMonitor
    :: (MonadUnliftIO m, Show a)
    => Int
    -- ^ Monitoring port
    -> MonitorState
    -- ^ Initial monitor state
    -> (Tracer m a -> m r)
    -- ^ Action to run with the monitor tracer
    -> m r
withMonitor monitoringPort initialState action = do
    t <- mkFoldingMonitor (liftIO getCurrentTime) F.map initialState
    c <- mkMonitor t
    runMonitor monitoringPort (fmap show . toList) c $ action . Tracer
