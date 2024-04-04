{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Control.API
    ( API
    , proxyAPI) where

import Cardano.Wallet.Launch.Cluster.Control.State
    ( Phase
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Set
    ( Set
    )
import Servant.API
    ( Get
    , JSON
    , (:>)
    )

type API = "phase" :> Get '[JSON] (Set Phase)

proxyAPI :: Proxy API
proxyAPI = Proxy
