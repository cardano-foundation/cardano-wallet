{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.UI.Deposit.Handlers.Wallet
where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    )
import Cardano.Wallet.Deposit.REST
    ( ErrWalletResource
    , WalletResource
    , WalletResourceM
    , runWalletResourceM
    )
import Cardano.Wallet.UI.Common.Handlers.Lib
    ( handleParseRequestError
    )
import Cardano.Wallet.UI.Common.Layer
    ( Push (Push)
    , SessionLayer (..)
    , stateL
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
import Data.Aeson
    ( Value
    , withObject
    , (.:)
    )
import Data.Aeson.Types
    ( parseEither
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
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
    -> (XPub -> WalletResourceM a)
    -> XPub
    -> Handler html
initWalletWithXPub l@SessionLayer{sendSSE} alert render initWallet xpub = do
    liftIO $ sendSSE $ Push "wallet"
    r <- liftIO $ catchRunWalletResourceM l (initWallet xpub)
    case r of
        Left e -> pure $ alert $ BL.pack $ show e
        Right _ -> do
            liftIO $ sendSSE $ Push "wallet"
            pure $ render ()

postMnemonicWallet
    :: SessionLayer WalletResource
    -> (XPub -> WalletResourceM a)
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> Value
    -> Handler html
postMnemonicWallet l initWallet alert render v = do
    mnemonic <-
        handleParseRequestError
            $ parsePostWalletRequest v
    let xpub =
            Addresses.toXPub
                $ Addresses.generate (T.encodeUtf8 mnemonic)
    initWalletWithXPub l alert render initWallet xpub

parsePostWalletRequest :: Value -> Either String Text
parsePostWalletRequest = parseEither
    . withObject "create wallet request"
    $ \o -> o .: "mnemonicSentence"

unBase64 :: ByteString -> Either String ByteString
unBase64 = convertFromBase Base64

postXPubWallet
    :: SessionLayer WalletResource
    -> (XPub -> WalletResourceM a)
    -> (BL.ByteString -> html)
    -> (() -> html)
    -> Value
    -> Handler html
postXPubWallet l initWallet alert render v = do
    xpubText <-
        handleParseRequestError
            $ parsePostXPubRequest v
    case T.encodeUtf8 xpubText of
        xpubByteString -> case unBase64 xpubByteString of
            Left e -> pure $ alert $ BL.pack $ "Invalid base64: " <> e
            Right xpubBytes -> case Addresses.xpubFromBytes xpubBytes of
                Nothing ->
                    pure
                        $ alert
                        $ BL.pack
                        $ "Invalid xpub: " <> show xpubText
                Just xpub -> initWalletWithXPub l alert render initWallet xpub

parsePostXPubRequest :: Value -> Either String Text
parsePostXPubRequest = parseEither
    . withObject "create wallet from xpub request"
    $ \o -> o .: "xpub"

walletIsLoading
    :: SessionLayer WalletResource
    -> (WalletPresent -> html)
    -> Handler html
walletIsLoading layer render = render <$> walletPresent layer
