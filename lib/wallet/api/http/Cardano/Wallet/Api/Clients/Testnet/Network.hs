{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Clients.Testnet.Network
    ( networkInformation
    , networkParameters
    , networkClock
    )
where

import Prelude

import Cardano.Wallet.Api
    ( Network
    )
import Cardano.Wallet.Api.Types
    ( ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    )
import Data.Proxy
    ( Proxy (..)
    )
import Servant.API
    ( type (:<|>) ((:<|>))
    , type (:>)
    )
import Servant.Client
    ( ClientM
    , client
    )

networkInformation
    :: ClientM ApiNetworkInformation
networkParameters
    :: ClientM ApiNetworkParameters
networkClock
    :: Bool -> ClientM ApiNetworkClock
networkInformation :<|> networkParameters :<|> networkClock =
    client (Proxy @("v2" :> Network))
