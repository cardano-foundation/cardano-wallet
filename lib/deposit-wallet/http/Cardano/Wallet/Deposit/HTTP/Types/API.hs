{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Servant Type for our HTTP API.
module Cardano.Wallet.Deposit.HTTP.Types.API
    ( API
    , NetworkAPI
    )
where

import Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( Address
    , ApiT
    , ChainPoint
    , Customer
    , CustomerList
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic
    , PostWalletViaXPub
    )
import Servant.API
    ( Capture
    , Get
    , JSON
    , Put
    , PutNoContent
    , ReqBody
    , StdMethod (..)
    , Verb
    , (:<|>)
    , (:>)
    )

{-----------------------------------------------------------------------------
    API
------------------------------------------------------------------------------}

type API =
    "customers"
        :> Get '[JSON] (ApiT CustomerList)
        :<|> "customers"
            :> Capture "customerId" (ApiT Customer)
            :> Put '[JSON] (ApiT Address)
        :<|> "mnemonics"
            :> ReqBody '[JSON] PostWalletViaMnemonic
            :> PutNoContent
        :<|> "xpub"
            :> ReqBody '[JSON] PostWalletViaXPub
            :> PutNoContent

type NetworkAPI =
    "network"
        :> "local-tip"
        :> Verb 'GET 200 '[JSON] (ApiT ChainPoint)
