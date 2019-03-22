{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Cardano.Wallet.Api.Types
    ( ApiT, Wallet, WalletId )
import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>), (:>), Capture, Delete, Get, JSON, NoContent )

api :: Proxy Api
api = Proxy

type Api = DeleteWallet :<|> GetWallet :<|> ListWallets

type DeleteWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Delete '[] NoContent

type GetWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] Wallet

type ListWallets = "wallets"
    :> Get '[JSON] [Wallet]
