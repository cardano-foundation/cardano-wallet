{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Server
    ( withHttpServer
    , httpServer
    , mkHandlers
    , Handlers
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    , ApiT (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Control.Monad
    ( (<=<)
    )
import Control.Monad.Cont
    ( ContT (..)
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
import Network.Socket
    ( PortNumber
    )
import Network.Wai.Handler.Warp
    ( run
    )
import Servant
    ( Application
    , Handler
    , NoContent (..)
    , Proxy (..)
    , (:<|>) (..)
    )
import Servant.Server
    ( serve
    )
import UnliftIO
    ( async
    , link
    )

isReady :: Phase -> Bool
isReady (Cluster _) = True
isReady _ = False

-- | Create handlers for the monitoring API
mkHandlers
    :: Monitor IO a History
    -> Handlers
mkHandlers monitor =
    Handlers
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
data Handlers = Handlers
    { handleReady :: IO Bool
    , handleStep :: IO ()
    , handleSwitch :: IO MonitorState
    , handleObserve :: IO (History, MonitorState)
    }

server
    :: Handlers
    -> IO Application
server handlers = do
    let lIO :: forall a. IO a -> Handler a
        lIO = liftIO
    pure
        $ serve (Proxy @API)
        $ lIO (handleReady handlers)
            :<|> lIO (handleStep handlers $> NoContent)
            :<|> lIO (ApiT <$> handleSwitch handlers)
            :<|> lIO (ApiT <$> handleObserve handlers)

-- | Run a HTTP server that serves the monitoring API
httpServer :: PortNumber -> Handlers -> IO ()
httpServer port handlers = server handlers >>= run (fromIntegral port)

-- | Start a HTTP server in a linked thread that serves the monitoring API
withHttpServer
    :: PortNumber
    -> Handlers
    -> ContT r IO ()
withHttpServer port handlers = ContT $ \k -> do
    link <=< async $ httpServer port handlers
    k ()
