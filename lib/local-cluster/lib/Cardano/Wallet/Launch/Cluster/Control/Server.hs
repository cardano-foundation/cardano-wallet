{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Control.Server
    ( server
    ) where

import Cardano.Wallet.Launch.Cluster.Control.API
    ( API
    )
import Cardano.Wallet.Launch.Cluster.Control.State
    ( ControlLayer (..)
    , Phase
    , phaseL
    )
import Control.Lens
    ( view
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Set
    ( Set
    )
import Prelude
import Servant
    ( Application
    , Proxy (..)
    )
import Servant.Server
    ( serve
    )
import UnliftIO
    ( MonadUnliftIO
    , UnliftIO (..)
    , withUnliftIO
    )

handlePhase :: Functor m => ControlLayer m -> m (Set Phase)
handlePhase ControlLayer{..} = view phaseL <$> controlState

server :: MonadUnliftIO m => ControlLayer m -> m Application
server l = withUnliftIO $ \(UnliftIO u) -> do
    let z = liftIO . u
    pure $ serve (Proxy @API) $ z $ handlePhase l
