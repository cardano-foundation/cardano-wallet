{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Clients.Network
    ( networkInformation
    , networkParameters
    , networkClock
    , blocksLatestHeader
    )
where

import Prelude

import Cardano.Wallet.Api
    ( GetBlocksLatestHeader
    , Network
    )
import Cardano.Wallet.Api.Types
    ( ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    )
import Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader
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

blocksLatestHeader :: ClientM ApiBlockHeader
blocksLatestHeader = client (Proxy @("v2" :> GetBlocksLatestHeader))
