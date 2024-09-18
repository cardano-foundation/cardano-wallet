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
import Cardano.Wallet.Deposit.REST
    ( ErrWalletResource
    , WalletResource
    , WalletResourceM
    , runWalletResourceM
    )
import Cardano.Wallet.UI.Common.Layer
    ( Push (Push)
    , SessionLayer (..)
    , stateL
    )
import Cardano.Wallet.UI.Deposit.API
    ( PostWalletViaMenmonic (..)
    , PostWalletViaXPub (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( walletPresent
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent
    )
import Control.Lens
    ( view
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString
    ( ByteString
    )
import Servant
    ( Handler
    )

import qualified Cardano.Address.Derivation as Addresses
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as T

catchRunWalletResourceM
    :: SessionLayer WalletResource
    -> WalletResourceM a
    -> IO (Either ErrWalletResource a)
catchRunWalletResourceM layer f = liftIO $ do
    s <- view stateL <$> state layer
    runWalletResourceM f s

catchRunWalletResourceHtml
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (a -> html)
    -> WalletResourceM a
    -> Handler html
catchRunWalletResourceHtml layer alert render f = liftIO $ do
    s <- view stateL <$> state layer
    r <- runWalletResourceM f s
    pure $ case r of
        Left e -> alert $ BL.pack $ show e
        Right a -> render a

getWallet
    :: SessionLayer WalletResource
    -> (WalletPresent -> html) -- success report
    -> Handler html
getWallet layer render = render <$> walletPresent layer

initWalletWithXPub
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> (WalletResourceM a)
    -> Handler html
initWalletWithXPub l@SessionLayer{sendSSE} alert render initWallet = do
    liftIO $ sendSSE $ Push "wallet"
    r <- liftIO $ catchRunWalletResourceM l initWallet
    case r of
        Left e -> pure $ alert $ BL.pack $ show e
        Right _ -> do
            liftIO $ sendSSE $ Push "wallet"
            pure $ render ()

postMnemonicWallet
    :: SessionLayer WalletResource
    -> (XPub -> Customer -> WalletResourceM a)
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> PostWalletViaMenmonic
    -> Handler html
postMnemonicWallet
    l
    initWallet
    alert
    render
    (PostWalletViaMenmonic mnemonic users) = do
        let xpub =
                Addresses.toXPub
                    $ Addresses.generate (T.encodeUtf8 mnemonic)
        initWalletWithXPub l alert render $ initWallet xpub $ fromIntegral users

unBase64 :: ByteString -> Either String ByteString
unBase64 = convertFromBase Base64

postXPubWallet
    :: SessionLayer WalletResource
    -> (XPub -> Customer -> WalletResourceM a)
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> PostWalletViaXPub
    -> Handler html
postXPubWallet l initWallet alert render (PostWalletViaXPub xpubText users) = do
    case T.encodeUtf8 xpubText of
        xpubByteString -> case unBase64 xpubByteString of
            Left e -> pure $ alert $ BL.pack $ "Invalid base64: " <> e
            Right xpubBytes -> case Addresses.xpubFromBytes xpubBytes of
                Nothing ->
                    pure
                        $ alert
                        $ BL.pack
                        $ "Invalid xpub: " <> show xpubText
                Just xpub ->
                    initWalletWithXPub l alert render
                        $ initWallet xpub
                        $ fromIntegral users

walletIsLoading
    :: SessionLayer WalletResource
    -> (WalletPresent -> html)
    -> Handler html
walletIsLoading layer render = render <$> walletPresent layer
