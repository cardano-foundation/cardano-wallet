{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

-- | How to run a monitor
module Control.Monitoring.Server
    ( Protocol (..)
    , runMonitor
    )
where

import Prelude

import Control.Monad
    ( forever
    , (<=<)
    )
import Control.Monitoring.Monitor
    ( Monitor (..)
    )
import Network.Simple.TCP
    ( HostPreference (HostAny)
    , serve
    )
import Network.Socket
    ( socketToHandle
    )
import System.IO
    ( hGetLine
    , hPutStrLn
    )
import Text.Read
    ( readMaybe
    )
import UnliftIO
    ( IOMode (..)
    , MonadUnliftIO
    , UnliftIO (..)
    , askUnliftIO
    , async
    , liftIO
    , link
    )

-- | The command you can send to the monitor
data Protocol
    = Pull -- ^ Pull the next trace, if in pulling state
    | Switch -- ^ Switch between pulling and not-pulling or vice versa
    | Observe -- ^ Observe the current state
    | Kill -- ^ Try to kill the program on the next trace
    deriving stock (Show, Read)

-- | Run a monitor on a given port
runMonitor
    :: MonadUnliftIO m
    => Int
    -- ^ The port to listen on
    -> (b -> [String])
    -- ^ How to render the output
    -> Monitor m a b
    -- ^ The monitor
    -> ((a -> m ()) -> m c)
    -- ^ The action to run with the tracer from the monitor
    -> m c
runMonitor port renderOuptut c action = do
    UnliftIO run <- askUnliftIO
    liftIO
        $ link <=< async
        $ serve HostAny (show port)
        $ \(socket, _) -> do
            handle <- socketToHandle socket ReadWriteMode
            forever $ do
                l <- hGetLine handle
                let p = do
                        (output, state) <- run (observe c)
                        mapM_ (hPutStrLn handle) $ renderOuptut output
                        hPutStrLn handle $ "State: " <> show state

                case readMaybe l of
                    Just Pull -> run (pull c) >> p
                    Just Switch -> run (switch c)
                    Just Observe -> p
                    Just Kill -> run (kill c)
                    Nothing -> hPutStrLn handle "Invalid command"
    action $ trace c
