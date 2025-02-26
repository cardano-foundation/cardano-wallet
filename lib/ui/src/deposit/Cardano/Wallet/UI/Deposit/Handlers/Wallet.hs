{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Cardano.Wallet.UI.Deposit.Handlers.Wallet
where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (slotToUTCTime)
    )
import Cardano.Wallet.Deposit.Pure
    ( Credentials
    , Customer
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( createMnemonicFromWords
    , credentialsFromEncodedXPub
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.Read
    ( slotFromChainPoint
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , availableBalance
    , getWalletTip
    , networkTag
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
import Cardano.Wallet.UI.Deposit.Types.Wallet
    ( Status (..)
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
getWallet layer render = do
    presence <- walletPresence layer
    pure $ render presence

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
        case createMnemonicFromWords mnemonic of
            Left e -> pure $ alert $ BL.pack $ show e
            Right mnemonic' -> do
                let credentials = credentialsFromMnemonics mnemonic' passphrase
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

getStatusRest :: NetworkEnv IO x -> WalletResourceM Status
getStatusRest nenv = do
    tip <- getWalletTip
    slotToTime <- liftIO $ slotToUTCTime nenv
    Status
        <$> pure tip
        <*> pure (slotToTime $ slotFromChainPoint tip)
        <*> availableBalance
        <*> networkTag
getStatus
    :: NetworkEnv IO x
    -> SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (Status -> html)
    -> Handler html
getStatus nenv layer alert render = do
    catchRunWalletResourceHtml layer alert id $ do
        render <$> getStatusRest nenv
