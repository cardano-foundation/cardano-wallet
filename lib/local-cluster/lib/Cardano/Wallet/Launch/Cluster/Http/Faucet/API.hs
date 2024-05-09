{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.API
    ( FaucetAPI
    , SendFaucetAssetsAPI
    )
where

import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets
    , WithNetwork (..)
    )
import Servant
    ( JSON
    , PostNoContent
    , ReqBody
    , (:>)
    )

type SendFaucetAssetsAPI n =
    "send"
        :> "faucet-assets"
        :> ReqBody '[JSON] (WithNetwork SendFaucetAssets n)
        :> PostNoContent

type FaucetAPI n = SendFaucetAssetsAPI n
