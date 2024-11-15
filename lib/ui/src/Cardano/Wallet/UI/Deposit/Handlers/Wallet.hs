{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.UI.Deposit.Handlers.Wallet
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Credentials
    , Customer
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( credentialsFromEncodedXPub
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic (..)
    , PostWalletViaXPub (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( Push (Push)
    , SessionLayer (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    , walletPresence
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
getWallet layer render = render <$> walletPresence layer

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
    -> (Credentials -> Customer -> WalletResourceM ())
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> PostWalletViaMnemonic
    -> Handler html
postMnemonicWallet
    l
    initWallet
    alert
    render
    (PostWalletViaMnemonic mnemonic passphrase customers) = do
        let credentials = credentialsFromMnemonics mnemonic passphrase
        initWalletWithXPub l alert render
            $ initWallet credentials
            $ fromIntegral customers

postXPubWallet
    :: SessionLayer WalletResource
    -> (Credentials -> Customer -> WalletResourceM ())
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
        case credentialsFromEncodedXPub xpubText of
            Left e -> pure $ alert $ BL.pack $ show e
            Right credentials ->
                initWalletWithXPub l alert render
                    $ initWallet credentials
                    $ fromIntegral customers

walletIsLoading
    :: SessionLayer WalletResource
    -> (WalletPresent -> html)
    -> Handler html
walletIsLoading layer render = render <$> walletPresence layer

deleteWalletHandler
    :: SessionLayer WalletResource
    -> WalletResourceM ()
    -- ^ deleteWallet
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> Handler html
deleteWalletHandler layer deleteWallet alert render =
    catchRunWalletResourceHtml layer alert render deleteWallet
