{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.UI.Deposit.Handlers.Wallet
where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , customerAddress
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMenmonic (..)
    , PostWalletViaXPub (..)
    , decodeXPub
    , xpubFromMnemonics
    )
import Cardano.Wallet.UI.Common.Layer
    ( Push (Push)
    , SessionLayer (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    , walletPresent
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy.Char8 as BL

getWallet
    :: SessionLayer WalletResource
    -> (WalletPresent -> html) -- success report
    -> Handler html
getWallet layer render = render <$> walletPresent layer

initWalletWithXPub
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> (WalletResourceM ())
    -> Handler html
initWalletWithXPub l@SessionLayer{sendSSE} alert render initWallet = do
    liftIO $ sendSSE $ Push "wallet"
    r <- catchRunWalletResourceHtml l alert render initWallet
    liftIO $ sendSSE $ Push "wallet"
    pure r

postMnemonicWallet
    :: SessionLayer WalletResource
    -> (XPub -> Customer -> WalletResourceM ())
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> PostWalletViaMenmonic
    -> Handler html
postMnemonicWallet
    l
    initWallet
    alert
    render
    (PostWalletViaMenmonic mnemonic customers) = do
        let xpub = xpubFromMnemonics mnemonic
        initWalletWithXPub l alert render
            $ initWallet xpub
            $ fromIntegral customers

postXPubWallet
    :: SessionLayer WalletResource
    -> (XPub -> Customer -> WalletResourceM ())
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> PostWalletViaXPub
    -> Handler html
postXPubWallet
    l
    initWallet
    alert
    render
    (PostWalletViaXPub xpubText customers) =
        case decodeXPub xpubText of
            Left e -> pure $ alert $ BL.pack $ "Invalid base64: " <> e
            Right Nothing ->
                pure
                    $ alert
                    $ BL.pack
                    $ "Invalid xpub: " <> show xpubText
            Right (Just xpub) ->
                initWalletWithXPub l alert render
                    $ initWallet xpub
                    $ fromIntegral customers

walletIsLoading
    :: SessionLayer WalletResource
    -> (WalletPresent -> html)
    -> Handler html
walletIsLoading layer render = render <$> walletPresent layer

deleteWalletHandler
    :: SessionLayer WalletResource
    -> WalletResourceM ()
    -- ^ deleteWallet
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> Handler html
deleteWalletHandler layer deleteWallet alert render =
    catchRunWalletResourceHtml layer alert render deleteWallet

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
