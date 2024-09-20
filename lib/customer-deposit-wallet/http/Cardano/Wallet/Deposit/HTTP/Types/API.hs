{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Servant Type for our HTTP API.
module Cardano.Wallet.Deposit.HTTP.Types.API
    ( CustomerAPI
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
    ( PostWalletViaMenmonic
    , PostWalletViaXPub
    )
import Servant.API
    ( Capture
    , Get
    , JSON
    , NoContent
    , Put
    , ReqBody
    , StdMethod (..)
    , Verb
    , (:<|>)
    , (:>)
    )

{-----------------------------------------------------------------------------
    API
------------------------------------------------------------------------------}

type CustomerAPI =
    "customers"
        :> Get '[JSON] (ApiT CustomerList)
        :<|> "customers"
            :> Capture "customerId" (ApiT Customer)
            :> Put '[JSON] (ApiT Address)
        :<|> "mnemonics"
            :> ReqBody '[JSON] PostWalletViaMenmonic
            :> Put '[JSON] NoContent
        :<|> "xpub"
            :> ReqBody '[JSON] PostWalletViaXPub
            :> Put '[JSON] NoContent

type NetworkAPI =
    "network"
        :> "local-tip"
        :> Verb 'GET 200 '[JSON] (ApiT ChainPoint)
