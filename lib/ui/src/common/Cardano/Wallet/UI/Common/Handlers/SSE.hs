{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Common.Handlers.SSE
    ( Message (..)
    , SSE
    , sse
    )
where

import Control.Monad.Fix
    ( fix
    )
import Data.Typeable
    ( Typeable
    )
import Lucid
    ( Html
    , renderBS
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
    ( TChan
    , dupTChan
    , readTChan
    )
import UnliftIO.STM
    ( atomically
    )
import Prelude

import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Media as M

-- | Imitate the Servant JSON and OctetStream implementations
data EventStream deriving (Typeable)

instance Accept EventStream where
    contentType _ = "text" M.// "event-stream"

-- | A message to be sent over the Server-Sent Events (SSE) connection.
data Message = Message
    { event :: BL.ByteString
    , data_ :: Html ()
    }
    deriving (Show)

instance MimeRender EventStream Message where
    mimeRender _ Message{..} =
        "event: "
            <> event
            <> "\ndata: "
            <> renderBS data_
            <> "\n\n"

-- | A Server-Sent Events (SSE) stream to use as an endpoint type.
type SSE = StreamGet NoFraming EventStream (SourceIO Message)

-- | Create a Server-Sent Events (SSE) stream from a 'TChan' of 'Message's.
sse :: TChan Message -> Server SSE
sse sseConfigSource = do
    duplicate <- atomically $ dupTChan sseConfigSource
    pure $ fromStepT $ fix $ \s -> Effect $ do
        x <- atomically $ readTChan duplicate
        pure $ Yield x s
