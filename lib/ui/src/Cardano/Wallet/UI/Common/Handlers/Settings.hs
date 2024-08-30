{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Common.Handlers.Settings
    ( toggleSSE
    ) where

import Prelude

import Cardano.Wallet.UI.Common.Layer
    ( Push (..)
    , SessionLayer (..)
    , sseEnabled
    )
import Control.Lens
    ( over
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    )

-- | Toggles the Server-Sent Events (SSE) feature on and off.
toggleSSE :: SessionLayer s -> Handler ()
toggleSSE SessionLayer{..} = liftIO $ do
    update $ over sseEnabled not
    sendSSE $ Push "settings"
