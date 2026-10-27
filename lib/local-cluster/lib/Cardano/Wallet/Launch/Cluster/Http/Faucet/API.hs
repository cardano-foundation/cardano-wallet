{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.API
    ( FaucetAPI
    , SendFaucetAssetsAPI
    , RegisterNewDRepAPI
    , RegisterDRepAPI
    , RetireDRepAPI
    )
where

import Cardano.Wallet.Launch.Cluster.Http.Faucet.ClusterDRep
    ( ClusterDRep
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets
    , WithNetwork (..)
    )
import Servant
    ( JSON
    , Post
    , PostNoContent
    , ReqBody
    , (:<|>)
    , (:>)
    )

type SendFaucetAssetsAPI n =
    "send"
        :> "faucet-assets"
        :> ReqBody '[JSON] (WithNetwork SendFaucetAssets n)
        :> PostNoContent

type RegisterNewDRepAPI =
    "drep"
        :> "register-new"
        :> Post '[JSON] ClusterDRep

type RegisterDRepAPI =
    "drep"
        :> "register"
        :> ReqBody '[JSON] ClusterDRep
        :> PostNoContent

type RetireDRepAPI =
    "drep"
        :> "retire"
        :> ReqBody '[JSON] ClusterDRep
        :> PostNoContent

type FaucetAPI n =
    SendFaucetAssetsAPI n
        :<|> RegisterNewDRepAPI
        :<|> RegisterDRepAPI
        :<|> RetireDRepAPI
