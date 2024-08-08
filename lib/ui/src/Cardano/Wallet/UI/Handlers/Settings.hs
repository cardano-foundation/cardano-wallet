{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Handlers.Settings
    ( toggleSSE
    ) where

import Prelude

import Cardano.Wallet.UI.Layer
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

toggleSSE :: SessionLayer -> Handler ()
toggleSSE SessionLayer{..} = liftIO $ do
    update $ over sseEnabled not
    sendSSE $ Push "settings"
