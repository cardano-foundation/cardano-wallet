{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.UI.Deposit.Handlers.Addresses
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , customerAddress
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    , walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Time
    ( UTCTime
    , getCurrentTime
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy.Char8 as BL

getAddresses
    :: SessionLayer WalletResource
    -> (UTCTime -> WalletPresent -> html) -- success report
    -> Handler html
getAddresses layer render = do
    now <- liftIO getCurrentTime
    render now <$> walletPresence layer

getCustomerAddress
    :: SessionLayer WalletResource
    -> (Address -> html)
    -> (BL.ByteString -> html)
    -> Customer
    -> Handler html
getCustomerAddress layer render alert customer = do
    catchRunWalletResourceHtml layer alert render'
        $ customerAddress customer
  where
    render' = \case
        Just a -> render a
        Nothing -> alert "Address not discovered"
