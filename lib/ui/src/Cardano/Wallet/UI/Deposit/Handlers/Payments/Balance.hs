module Cardano.Wallet.UI.Deposit.Handlers.Payments.Balance
    ( getAvailableBalance
    )
where

import Prelude

import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , availableBalance
    )
import Cardano.Wallet.Read
    ( Coin (..)
    , Value (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy.Char8 as BL8

getAvailableBalance
    :: SessionLayer WalletResource
    -> (Coin -> html)
    -> (BL8.ByteString -> html)
    -> Handler html
getAvailableBalance layer render alert =
    catchRunWalletResourceHtml layer alert id $ do
        ValueC r _ <- availableBalance
        pure $ render r
