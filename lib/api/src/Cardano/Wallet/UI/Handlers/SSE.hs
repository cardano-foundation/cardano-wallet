{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Handlers.SSE where

import Prelude

import Control.Monad.Fix
    ( fix
    )
import Data.Typeable
    ( Typeable
    )
import Servant
    ( Accept (contentType)
    , MimeRender (..)
    , NoFraming
    , Server
    , SourceIO
    , StreamGet
    )
import Servant.Types.SourceT
    ( StepT (..)
    , fromStepT
    )
import UnliftIO
    ( MonadIO (..)
    , TBQueue
    , TChan
    , dupTChan
    , link
    , peekTBQueue
    , readTChan
    , writeTBQueue
    )
import UnliftIO.STM
    ( STM
    , atomically
    , newTBQueue
    , orElse
    , readTBQueue
    )

import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( forever
    , unless
    , (<=<)
    )
import Lucid
    ( Html
    , renderBS
    )
import Numeric.Natural
    ( Natural
    )
import UnliftIO.Async
    ( async
    )

import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Media as M

-- imitate the Servant JSON and OctetStream implementations
data EventStream deriving (Typeable)

instance Accept EventStream where
    contentType _ = "text" M.// "event-stream"

data Message = Message
    { event :: BL.ByteString
    , data_ :: Html ()
    }
instance MimeRender EventStream Message where
    mimeRender _ = toSSE

toSSE :: Message -> BL.ByteString
toSSE Message{..} =
    "event: "
        <> event
        <> "\ndata: \n"
        <> renderBS data_
        <> "\n"

type SSE = StreamGet NoFraming EventStream (SourceIO Message)

-- | Configuration for a server-sent events stream
data SSEConfig = SSEConfig
    { sseConfigSource :: TChan Message
    -- ^ Source of messages
    , sseConfigQueueLength :: Natural
    -- ^ Maximum number of messages to keep in memory
    , sseThroughput :: Double
    -- ^ Number of messages per second
    }

fairWrite :: TBQueue Message -> Message -> STM ()
fairWrite queue x = do
    let write = writeTBQueue queue x `orElse` pure ()
    mx' <- (Just <$> peekTBQueue queue) `orElse` pure Nothing
    case mx' of
        Just x' -> unless (event x == event x') write
        Nothing -> write

sse :: SSEConfig  -> Server SSE
sse SSEConfig{..} = do
    duplicate <- atomically $ dupTChan sseConfigSource
    queue <- atomically $ newTBQueue sseConfigQueueLength
    liftIO $ link <=< async $ forever $ do
        x <- atomically $ readTChan duplicate
        atomically $ writeTBQueue queue x `orElse` pure ()
    pure $ fromStepT $ fix $ \s -> Effect $ do
        x <- atomically $ readTBQueue queue
        threadDelay $ round $ 1_000_000  / sseThroughput
        pure $ Yield x s
