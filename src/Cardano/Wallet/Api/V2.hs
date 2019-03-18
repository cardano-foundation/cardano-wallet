{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.V2 where

import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>), (:>), Capture, Delete, Get, JSON )

import qualified Cardano.Wallet.Api.V2.Types.Wallet as T
import qualified Cardano.Wallet.Api.V2.Types.WalletId as T

type Api = DeleteWallet :<|> GetWallet :<|> ListWallets

api :: Proxy Api
api = Proxy

type DeleteWallet = "wallets"
    :> "deleteWallet"
    :> Capture "walletId" T.WalletId
    :> Delete '[] ()

type GetWallet = "wallets"
    :> "getWallet"
    :> Capture "walletId" T.WalletId
    :> Get '[JSON] T.Wallet

type ListWallets = "wallets"
    :> "listWallets"
    :> Get '[JSON] [T.Wallet]
