{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Common.Layer
    ( UILayer (..)
    , withUILayer
    , SessionLayer (..)
    , Push (..)
    , State (..)
    , stateL
    , sseEnabled
    , sourceOfNewTip
    , walletTipChanges
    )
where

import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.UI.Common.Handlers.SSE
    ( Message (..)
    )
import Cardano.Wallet.UI.Cookies
    ( SessionKey
    )
import Cardano.Wallet.UI.Signal
    ( Signal (..)
    )
import Control.Lens
    ( Lens'
    , lens
    , view
    )
import Control.Monad
    ( forM_
    , forever
    , void
    , when
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Data.Functor
    ( ($>)
    )
import UnliftIO
    ( MonadIO (..)
    , dupTChan
    , newBroadcastTChanIO
    , newEmptyTMVarIO
    , putTMVar
    , withAsync
    , writeTChan
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.STM
    ( TChan
    , TVar
    , atomically
    , modifyTVar
    , newTVar
    , newTVarIO
    , orElse
    , readTVarIO
    , takeTMVar
    )
import Prelude

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

-- | The state of the UI.
data State s = State
    { _state :: s
    -- ^ The selected wallet id, if any.
    , _sseEnabled :: Bool
    -- ^ Whether server-sent events are enabled.
    }
    deriving (Eq, Show)

bootState :: s -> State s
bootState s = State s True

stateL :: Lens' (State s) s
stateL = lens _state (\s a -> s{_state = a})

sseEnabled :: Lens' (State s) Bool
sseEnabled = lens _sseEnabled (\s a -> s{_sseEnabled = a})

-- | A push message.
newtype Push = Push BL.ByteString

-- | The UI layer.
data UILayer s = UILayer
    { sessions :: SessionKey -> IO (SessionLayer s)
    -- ^ Get the session layer for a given session key. Always succeed
    , signals :: Tracer IO Signal
    -- ^ A tracer for signals.
    , oobMessages :: Tracer IO Push
    }

-- | The session layer.
data SessionLayer s = SessionLayer
    { state :: IO (State s)
    -- ^ Get the state.
    , update :: (State s -> State s) -> IO ()
    -- ^ Update the state.
    , sendSSE :: Push -> IO ()
    -- ^ Send a server-sent event.
    , sseConfig :: TChan Message
    -- ^ The server-sent events configuration.
    }

messageOfPush :: Push -> Message
messageOfPush (Push x) = Message x mempty

walletTipChanges :: () -> Push
walletTipChanges _ = Push "wallet-tip"

-- | Create a session layer giver the state and the server-sent events channel.
mkSession :: TVar (State s) -> TChan Message -> SessionLayer s
mkSession var sseChan =
    SessionLayer
        { state = readTVarIO var
        , update = atomically . modifyTVar var
        , sendSSE = \x -> do
            s <- readTVarIO var
            case (view sseEnabled s, x) of
                (True, m) -> write $ messageOfPush m
                _ -> pure ()
        , sseConfig = sseChan
        }
  where
    write :: Message -> IO ()
    write = atomically . writeTChan sseChan

-- | A throttling mechanism. When you call it, it will run the action or ignore
-- it if it's too soon.
type Throttling = IO () -> IO ()

-- | Create a throtting mechanism based on frequency. Will run the action at
-- most once every 1/freq seconds.
freqThrottle
    :: Int
    -- ^ The frequency in Hz.
    -> ContT r IO Throttling
freqThrottle freq = do
    t <- liftIO newEmptyTMVarIO
    _ <- ContT $ withAsync $ forever $ do
        atomically $ putTMVar t ()
        threadDelay $ 1_000_000 `div` freq
    pure $ \action -> do
        run <- atomically $ (takeTMVar t $> True) `orElse` pure False
        when run action

-- | Create a UI layer given the sessions map.
mkUILayer
    :: Throttling
    -> TChan Message
    -- ^ Out of band messages.
    -> TVar (Map.Map SessionKey (SessionLayer s))
    -> s
    -> UILayer s
mkUILayer throttling oobChan sessions' s0 = UILayer{..}
  where
    oobMessages =
        Tracer
            $ throttling
                . atomically
                . writeTChan oobChan
                . messageOfPush
    sessions sid = do
        sids <- readTVarIO sessions'
        case Map.lookup sid sids of
            Just session -> pure session
            Nothing -> atomically $ do
                sseChan <- dupTChan oobChan
                var <- newTVar $ bootState s0
                let session = mkSession var sseChan
                modifyTVar sessions' $ Map.insert sid session
                pure session

    signals = Tracer $ \case
        NewTip -> throttling $ do
            sessions'' <- readTVarIO sessions'
            forM_ (Map.elems sessions'') $ \s -> do
                sendSSE s $ Push "tip"

-- | Run an action with a UI layer.
withUILayer :: Int -> s -> ContT r IO (UILayer s)
withUILayer freq s0 = do
    sessions' <- liftIO $ newTVarIO mempty
    throttled <- freqThrottle freq
    oobChan <- liftIO newBroadcastTChanIO
    pure $ mkUILayer throttled oobChan sessions' s0

-- | Collect NewTip signals
sourceOfNewTip :: NetworkLayer IO block -> UILayer s -> ContT r IO ()
sourceOfNewTip netLayer ui = do
    void
        $ ContT
        $ withAsync
        $ watchNodeTip netLayer
        $ \_ -> traceWith (signals ui) NewTip
