{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Server
    ( mkControlHandlers
    , ControlHandlers
    , mkControlServer
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( ApiT (..)
    , ControlAPI
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monitoring.Monitor
    ( Monitor (..)
    )
import Control.Monitoring.Tracing
    ( MonitorState
    )
import Data.Foldable
    ( find
    )
import Data.Functor
    ( ($>)
    )
import Data.Maybe
    ( isJust
    )
import Servant
    ( Handler
    , NoContent (..)
    , (:<|>) (..)
    )
import Servant.Server
    ( HasServer (..)
    )

isReady :: Phase -> Bool
isReady (Cluster _) = True
isReady _ = False

-- | Create handlers for the monitoring API
mkControlHandlers
    :: Monitor IO a History
    -> ControlHandlers
mkControlHandlers monitor =
    ControlHandlers
        { handleReady = do
            s <- history . fst <$> observe monitor
            pure $ isJust $ find (isReady . snd) s
        , handleStep = step monitor
        , handleSwitch = do
            switch monitor
            snd <$> observe monitor
        , handleObserve = observe monitor
        }

-- | Handlers for the monitoring API, opaque.
data ControlHandlers = ControlHandlers
    { handleReady :: IO Bool
    , handleStep :: IO ()
    , handleSwitch :: IO MonitorState
    , handleObserve :: IO (History, MonitorState)
    }

mkControlServer
    :: ControlHandlers
    -> ServerT ControlAPI Handler
mkControlServer ControlHandlers{..} =
    liftIO (handleReady)
        :<|> liftIO (handleStep $> NoContent)
        :<|> liftIO (ApiT <$> handleSwitch)
        :<|> liftIO (ApiT <$> handleObserve)
