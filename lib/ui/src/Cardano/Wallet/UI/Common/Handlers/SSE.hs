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
    ( TChan
    , dupTChan
    , readTChan
    )
import UnliftIO.STM
    ( atomically
    )

import Lucid
    ( Html
    , renderBS
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
    deriving (Show)

instance MimeRender EventStream Message where
    mimeRender _ Message{..} =
        "event: "
            <> event
            <> "\ndata: "
            <> renderBS data_
            <> "\n\n"

type SSE = StreamGet NoFraming EventStream (SourceIO Message)

sse :: TChan Message -> Server SSE
sse sseConfigSource = do
    duplicate <- atomically $ dupTChan sseConfigSource
    pure $ fromStepT $ fix $ \s -> Effect $ do
        x <- atomically $ readTChan duplicate
        pure $ Yield x s
