{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Http.API
    ( API
    )
where

import Cardano.Wallet.Launch.Cluster.Http.Faucet.API
    ( FaucetAPI
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.API
    ( ControlAPI
    )
import Servant.API
    ( (:<|>)
    )

-- | The API for the monitoring server and the query cluster application
type API n = ControlAPI :<|> FaucetAPI n
