{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Layer where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Cookies
    ( SessionKey
    )
import Cardano.Wallet.UI.Handlers.SSE
    ( Message (..)
    , SSEConfig (..)
    )
import Cardano.Wallet.UI.Signal
    ( Signal (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Lens
    ( Lens'
    , lens
    , view
    , (^.)
    )
import Control.Monad
    ( forM_
    , forever
    , when
    , (<=<)
    )
import Control.Tracer
    ( Tracer (..)
    )
import UnliftIO
    ( async
    , link
    , writeTChan
    )
import UnliftIO.STM
    ( TChan
    , TVar
    , atomically
    , modifyTVar
    , newTChan
    , newTVar
    , newTVarIO
    , readTVarIO
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Time
    ( UTCTime
    , getCurrentTime
    )
import Lucid
    ( toHtml
    )

data State = State
    { _walletId :: Maybe WalletId
    , _sseEnabled :: Bool
    }
    deriving (Eq, Show)

bootState :: State
bootState = State Nothing False

walletId :: Lens' State (Maybe WalletId)
walletId = lens _walletId (\s a -> s{_walletId = a})

refreshEnabled :: Lens' State Bool
refreshEnabled = lens _sseEnabled (\s a -> s{_sseEnabled = a})

data Push = Refresh | Sync BL.ByteString | Time UTCTime

data UILayer = UILayer
    { sessions :: SessionKey -> IO SessionLayer
    , signals :: Tracer IO Signal
    }

data SessionLayer = SessionLayer
    { state :: IO State
    , update :: (State -> State) -> IO ()
    , sendSSE :: Push -> IO ()
    , sseConfig :: SSEConfig
    }

mkSession :: TVar State -> TChan Message -> SessionLayer
mkSession var sseChan =
    SessionLayer
        { state = readTVarIO var
        , update = atomically . modifyTVar var
        , sendSSE = \x -> do
            s <- readTVarIO var
            case (view refreshEnabled s, x) of
                (_, Sync m) -> write $ Message m mempty
                (True, Refresh) -> write $ Message "refresh" mempty
                (True, Time t) -> write $ Message "time" (toHtml $ show t)
                _ -> pure ()
        , sseConfig =
            SSEConfig
                { sseConfigSource = sseChan
                , sseConfigQueueLength = 100
                , sseThroughput = 10
                }
        }
  where
    write :: Message -> IO ()
    write = atomically . writeTChan sseChan

mkUILayer :: TVar (Map.Map SessionKey SessionLayer) -> UILayer
mkUILayer sessions' =
    UILayer{..}
  where
    sessions sid = do
        sids <- readTVarIO sessions'
        case Map.lookup sid sids of
            Just session -> pure session
            Nothing -> do
                session <- atomically $ do
                    sseChan <- newTChan
                    var <- newTVar bootState
                    let session = mkSession var sseChan
                    modifyTVar sessions' $ Map.insert sid session
                    pure session
                link <=< async $ forever $ do
                    threadDelay 1_000_000
                    time <- getCurrentTime
                    sendSSE session $ Time time
                pure session

    signals = Tracer $ \(NewTip wid _) -> do
        sessions'' <- readTVarIO sessions'
        forM_ (Map.elems sessions'') $ \s -> do
            w <- state s
            when (w ^. walletId == Just wid)
                $ sendSSE s (Sync "wallet")

withUILayer :: (UILayer -> IO a) -> IO a
withUILayer action = do
    sessions' <- newTVarIO mempty
    action $ mkUILayer sessions'
