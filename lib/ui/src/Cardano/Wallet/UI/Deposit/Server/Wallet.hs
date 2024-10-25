{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Server.Wallet
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , deleteWallet
    , initXPubWallet
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    )
import Cardano.Wallet.UI.Common.Handlers.Wallet
    ( pickMnemonic
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    , rogerH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Wallet
    ( mnemonicH
    )
import Cardano.Wallet.UI.Common.Layer
    ( UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    , sessioning
    )
import Cardano.Wallet.UI.Deposit.Handlers.Wallet
    ( deleteWalletHandler
    , getWallet
    , postMnemonicWallet
    , postXPubWallet
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( deleteWalletModalH
    , walletElementH
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( alert
    , renderSmoothHtml
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer (..)
    )
import Data.Text
    ( Text
    )
import Servant
    ( Handler
    )

import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMenmonic
    , PostWalletViaXPub
    )

serveMnemonic
    :: Maybe Bool
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveMnemonic hintOrClean =
    sessioning
        $ renderSmoothHtml . mnemonicH
            <$> liftIO (pickMnemonic 15 hintOrClean)

serveWalletPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveWalletPage ul = withSessionLayer ul $ \layer -> do
    getWallet layer (renderSmoothHtml . walletElementH alertH)

servePostMnemonicWallet
    :: Tracer IO String
    -> WalletBootEnv IO
    -> FilePath
    -> UILayer WalletResource
    -> PostWalletViaMenmonic
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePostMnemonicWallet tr env dbDir ul request =
    withSessionLayer ul $ \layer -> do
        postMnemonicWallet layer initWallet alert ok request
  where
    ok _ = renderHtml . rogerH @Text $ "ok"
    initWallet = initXPubWallet tr env dbDir

servePostXPubWallet
    :: Tracer IO String
    -> WalletBootEnv IO
    -> FilePath
    -> UILayer WalletResource
    -> PostWalletViaXPub
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePostXPubWallet tr env dbDir ul request =
    withSessionLayer ul $ \layer -> do
        postXPubWallet layer initWallet alert ok request
  where
    ok _ = renderHtml . rogerH @Text $ "ok"
    initWallet = initXPubWallet tr env dbDir

serveDeleteWallet
    :: UILayer WalletResource
    -> FilePath
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDeleteWallet ul dbDir = withSessionLayer ul
    $ \l -> deleteWalletHandler l (deleteWallet dbDir) alert ok
  where
    ok _ = renderHtml . rogerH @Text $ "ok"

serveDeleteWalletModal
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDeleteWalletModal ul = withSessionLayer ul $ \_ ->
    pure $ renderSmoothHtml deleteWalletModalH

{-         :<|> (\c -> )
        :<|> wsl (\l -> deleteWalletHandler l (deleteWallet dbDir) alert ok)
        :<|> wsl (\_l -> pure $ renderSmoothHtml deleteWalletModalH)
        :<|> ( \c ->
                wsl
                    ( \l ->
                        getCustomerAddress
                            l
                            ( renderSmoothHtml
                                . customerAddressH WithCopy
                            )
                            alert
                            c
                    )
             ) -}
