{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Control.Monitoring where

import Prelude

import Control.Comonad
    ( Comonad (duplicate, extract)
    , ($>)
    )
import Control.Foldl
    ( Fold
    )
import qualified Control.Foldl as F
import Control.Monad
    ( forever
    , when
    , (<=<)
    )
import Data.Bifunctor
    ( Bifunctor (..)
    )
import Data.Profunctor
    ( Profunctor (..)
    , dimap
    , lmap
    , rmap
    )
import GHC.IO.Handle
    ( hGetLine
    )
import Network.Simple.TCP
    ( HostPreference (HostAny)
    , serve
    )
import Network.Socket
    ( socketToHandle
    )
import System.IO
    ( hPutStrLn
    )
import Text.Read
    ( readMaybe
    )
import UnliftIO
    ( IOMode (..)
    , MonadIO
    , MonadUnliftIO
    , UnliftIO (..)
    , askUnliftIO
    , async
    , atomically
    , liftIO
    , link
    , modifyTVar
    , newEmptyTMVarIO
    , newTVarIO
    , putTMVar
    , readTVarIO
    , takeTMVar
    , writeTVar
    )

data Protocol = Pull | Switch | Observe | Kill
    deriving stock (Show, Read)

runMonitor
    :: MonadUnliftIO m
    => Int
    -- ^ The port to listen on
    -> (b -> [String])
    -- ^ How to render the output
    -> Monitor m a b
    -- ^ The controller
    -> ((a -> m ()) -> m c)
    -- ^ The action to run with the tracer action
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

-- | A tracing controller. It accumulates the trace of `a` in `b` and can
-- control the program via `step`.
data Tracing m a b = Tracing
    { observeTracing :: m b
    -- ^ Observe the current trace
    , traceTracing :: a -> m ()
    -- ^ The tracer
    }

instance Monad m => Functor (Tracing m a) where
    fmap = rmap

instance Monad m => Profunctor (Tracing m) where
    dimap f g Tracing{..} =
        Tracing
            { observeTracing = fmap g observeTracing
            , traceTracing = lmap f traceTracing
            }

data Pulling m a b = Pulling
    { tracingPulling :: Tracing m a b
    , notPulling :: m (NotPulling m a b)
    , pullPulling :: m ()
    }

instance Monad m => Profunctor (Pulling m) where
    dimap f g Pulling{..} =
        Pulling
            { tracingPulling = dimap f g tracingPulling
            , notPulling = dimap f g <$> notPulling
            , pullPulling
            }

data NotPulling m a b = NotPulling
    { tracingNotPulling :: Tracing m a b
    , pulling :: Pulling m a b
    }

instance Monad m => Profunctor (NotPulling m) where
    dimap f g NotPulling{..} =
        NotPulling
            { tracingNotPulling = dimap f g tracingNotPulling
            , pulling = dimap f g pulling
            }

data MonitorState = PullingState | NotPullingState
    deriving stock (Show, Read)

-- | A controller that can switch between pulling and not-pulling
-- * `trace` is tracing in both states
-- * `switch` switches between pulling and not pulling and vice versa
-- * `observe` observes the current state which is kept however we switch
-- * `pull` is a no-op when not pulling
data Monitor m a b = Monitor
    { trace :: a -> m ()
    , switch :: m ()
    , observe :: m (b, MonitorState)
    , pull :: m ()
    , kill :: m ()
    }

natMonitor :: (forall x. m x -> n x) -> Monitor m a b -> Monitor n a b
natMonitor nat Monitor{..} =
    Monitor
        { trace = nat . trace
        , switch = nat switch
        , observe = nat observe
        , pull = nat pull
        , kill = nat kill
        }

instance Monad m => Functor (Monitor m a) where
    fmap = rmap

instance Monad m => Profunctor (Monitor m) where
    dimap f g Monitor{..} =
        Monitor
            { trace = trace . f
            , switch
            , observe = fmap (first g) observe
            , pull
            , kill
            }

-- | Create an STM thread safe controller from a tracer
-- in either pulling or not-pulling state
mkMonitor
    :: MonadIO m
    => PullingOrNotPulling m a b
    -> m (Monitor m a b)
mkMonitor tracer = do
    tracerVar <- liftIO $ newTVarIO tracer
    let r = liftIO $ readTVarIO tracerVar
        w = liftIO . atomically . writeTVar tracerVar
    k <- liftIO $ newTVarIO False
    let pull' = do
            t <- r
            eitherPulling pullPulling (const $ pure ()) t
    pure
        $ Monitor
            { trace = \a -> do
                t <- r
                kill <- liftIO $ readTVarIO k
                when kill $ error "Killed"
                eitherPulling
                    (traceTracing . tracingPulling)
                    (traceTracing . tracingNotPulling)
                    t
                    a
            , switch = do
                t <- r
                case t of
                    MkPulling Pulling{notPulling} ->
                        notPulling >>= w . MkNotPulling
                    MkNotPulling NotPulling{pulling} ->
                        w $ MkPulling pulling
            , observe = do
                t <- r
                eitherPulling
                    (fmap (,PullingState) . observeTracing . tracingPulling)
                    (fmap (,NotPullingState) . observeTracing . tracingNotPulling)
                    t
            , pull = pull'
            , kill = do
                liftIO $ atomically $ writeTVar k True
                pull'
            }

data PullingOrNotPulling m a b
    = MkPulling (Pulling m a b)
    | MkNotPulling (NotPulling m a b)

eitherPulling
    :: (Pulling m a b -> c)
    -> (NotPulling m a b -> c)
    -> PullingOrNotPulling m a b
    -> c
eitherPulling f _ (MkPulling p) = f p
eitherPulling _ g (MkNotPulling p) = g p

-- | Create a NotPulling tracer from a folding function
mkFoldingMonitor
    :: MonadIO m
    => m ctx
    -- ^ Get the context after each trace
    -> Fold (ctx, a) b
    -- ^ How to fold the context and the value
    -> MonitorState
    -> m (PullingOrNotPulling m a b)
mkFoldingMonitor getContext how state = do
    resultVar <- liftIO $ newTVarIO how
    let updateResult a =
            modifyTVar resultVar $ \how' -> F.fold (duplicate how') [a]
    block <- liftIO newEmptyTMVarIO
    let
        observeTracing = liftIO $ extract <$> readTVarIO resultVar
        step =
            liftIO
                $ atomically
                $ takeTMVar block >>= updateResult
        tracePulling a = do
            ctx <- getContext
            liftIO $ atomically $ putTMVar block (ctx, a)
        traceNotPulling a = do
            ctx <- getContext
            liftIO $ atomically $ updateResult (ctx, a)
    let notPulling =
            NotPulling
                { tracingNotPulling =
                    Tracing
                        { traceTracing = traceNotPulling
                        , observeTracing
                        }
                , pulling = pulling
                }
        pulling =
            Pulling
                { tracingPulling =
                    Tracing
                        { traceTracing = tracePulling
                        , observeTracing
                        }
                , pullPulling = step
                , notPulling = step $> notPulling
                }
    pure $ case state of
        PullingState -> MkNotPulling notPulling
        NotPullingState -> MkPulling pulling
