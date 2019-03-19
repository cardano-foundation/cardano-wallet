{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>), (:>), Capture, Delete, Get, JSON, NoContent )

import qualified Cardano.Wallet.Api.Types.Wallet as T
import qualified Cardano.Wallet.Api.Types.WalletId as T

type Api = DeleteWallet :<|> GetWallet :<|> ListWallets

api :: Proxy Api
api = Proxy

type DeleteWallet = "wallets"
    :> Capture "walletId" T.WalletId
    :> Delete '[JSON] NoContent

type GetWallet = "wallets"
    :> Capture "walletId" T.WalletId
    :> Get '[JSON] T.Wallet

type ListWallets = "wallets"
    :> Get '[JSON] [T.Wallet]
