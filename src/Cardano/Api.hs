{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api where

import Cardano.Api.Types
    ( Wallet, WalletId )
import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>), (:>), Capture, Delete, Get, JSON, NoContent )

api :: Proxy Api
api = Proxy

type Api = DeleteWallet :<|> GetWallet :<|> ListWallets

type DeleteWallet = "wallets"
    :> Capture "walletId" WalletId
    :> Delete '[JSON] NoContent

type GetWallet = "wallets"
    :> Capture "walletId" WalletId
    :> Get '[JSON] Wallet

type ListWallets = "wallets"
    :> Get '[JSON] [Wallet]
