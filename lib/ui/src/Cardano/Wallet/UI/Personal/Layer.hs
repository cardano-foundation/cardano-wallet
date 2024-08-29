{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Personal.Layer
    ( UILayer (..)
    , withUILayer
    , SessionLayer (..)
    , Push (..)
    , State (..)
    , walletId
    , sseEnabled
    , sourceOfNewTip
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Cookies
    ( SessionKey
    )
import Cardano.Wallet.UI.Common.Handlers.SSE
    ( Message (..)
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
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import UnliftIO
    ( MonadIO (..)
    , newEmptyTMVarIO
    , putTMVar
    , readTMVar
    , withAsync
    , writeTChan
    )
import UnliftIO.STM
    ( TChan
    , TVar
    , atomically
    , modifyTVar
    , newBroadcastTChan
    , newTVar
    , newTVarIO
    , orElse
    , readTVarIO
    )

import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Data.Functor
    ( ($>)
    )
import UnliftIO.Concurrent
    ( threadDelay
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

-- | The state of the UI.
data State = State
    { _walletId :: Maybe WalletId
    -- ^ The selected wallet id, if any.
    , _sseEnabled :: Bool
    -- ^ Whether server-sent events are enabled.
    }
    deriving (Eq, Show)

bootState :: State
bootState = State Nothing True

walletId :: Lens' State (Maybe WalletId)
walletId = lens _walletId (\s a -> s{_walletId = a})

sseEnabled :: Lens' State Bool
sseEnabled = lens _sseEnabled (\s a -> s{_sseEnabled = a})

-- | A push message.
newtype Push = Push BL.ByteString

-- | The UI layer.
data UILayer = UILayer
    { sessions :: SessionKey -> IO SessionLayer
    -- ^ Get the session layer for a given session key. Always succeed
    , signals :: Tracer IO Signal
    -- ^ A tracer for signals.
    }

-- | The session layer.
data SessionLayer = SessionLayer
    { state :: IO State
    -- ^ Get the state.
    , update :: (State -> State) -> IO ()
    -- ^ Update the state.
    , sendSSE :: Push -> IO ()
    -- ^ Send a server-sent event.
    , sseConfig :: TChan Message
    -- ^ The server-sent events configuration.
    }

-- | Create a session layer giver the state and the server-sent events channel.
mkSession :: TVar State -> TChan Message -> SessionLayer
mkSession var sseChan =
    SessionLayer
        { state = readTVarIO var
        , update = atomically . modifyTVar var
        , sendSSE = \x -> do
            s <- readTVarIO var
            case (view sseEnabled s, x) of
                (True, Push m) -> write $ Message m mempty
                _ -> pure ()
        , sseConfig = sseChan
        }
  where
    write :: Message -> IO ()
    write = atomically . writeTChan sseChan

type Throttling = IO () -> IO ()

throttler :: Int -> ContT r IO Throttling
throttler freq = do
    t <- liftIO newEmptyTMVarIO
    _ <- ContT $ withAsync $ forever $ do
        atomically $ putTMVar t ()
        threadDelay $ 1_000_000 `div` freq
    pure $ \action -> do
        run <- atomically $ (readTMVar t $> True) `orElse` pure False
        when run action

-- | Create a UI layer given the sessions map.
mkUILayer :: Throttling -> TVar (Map.Map SessionKey SessionLayer) -> UILayer
mkUILayer throttling sessions' = UILayer{..}
  where
    sessions sid = do
        sids <- readTVarIO sessions'
        case Map.lookup sid sids of
            Just session -> pure session
            Nothing -> atomically $ do
                sseChan <- newBroadcastTChan
                var <- newTVar bootState
                let session = mkSession var sseChan
                modifyTVar sessions' $ Map.insert sid session
                pure session

    signals = Tracer $ \case
        NewTip -> throttling $ do
            sessions'' <- readTVarIO sessions'
            forM_ (Map.elems sessions'') $ \s -> do
                sendSSE s $ Push "tip"
                sendSSE s $ Push "wallets"
                sendSSE s $ Push "wallet"

-- | Run an action with a UI layer.
withUILayer :: Int -> ContT r IO UILayer
withUILayer freq = do
    sessions' <- liftIO $ newTVarIO mempty
    throttled <- throttler freq
    pure $ mkUILayer throttled sessions'

-- | Collect NewTip signals
sourceOfNewTip :: NetworkLayer IO block -> UILayer -> ContT r IO ()
sourceOfNewTip netLayer ui = do
    void
        $ ContT
        $ withAsync
        $ watchNodeTip netLayer
        $ \_ -> traceWith (signals ui) NewTip
