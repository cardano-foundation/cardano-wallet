{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.V2 where

import Cardano.Wallet.Api.V2.Types.Wallet
import Cardano.Wallet.Api.V2.Types.WalletId
import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:>), (:<|>), Capture, Delete, Get, JSON )

type Api = DeleteWallet :<|> GetWallet :<|> ListWallets

api :: Proxy Api
api = Proxy

type DeleteWallet = "wallets"
    :> "deleteWallet"
    :> Capture "walletId" WalletId
    :> Delete '[] ()

type GetWallet = "wallets"
    :> "getWallet"
    :> Capture "walletId" WalletId
    :> Get '[JSON] Wallet

type ListWallets = "wallets"
    :> "listWallets"
    :> Get '[JSON] [Wallet]
