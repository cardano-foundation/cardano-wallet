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
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , deleteWallet
    , initWallet
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic
    , PostWalletViaXPub
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
    , getStatus
    , getWallet
    , postMnemonicWallet
    , postXPubWallet
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( deleteWalletModalH
    , walletElementH
    , walletStatusH
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
    getWallet layer $ \presence ->
        renderSmoothHtml $ walletElementH alertH presence

servePostMnemonicWallet
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -> WalletBootEnv IO
    -> FilePath
    -> UILayer WalletResource
    -> PostWalletViaMnemonic
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePostMnemonicWallet wtc tr env dbDir ul request =
    withSessionLayer ul $ \layer -> do
        postMnemonicWallet layer initWallet' alert ok request
  where
    ok _ = renderHtml . rogerH @Text $ "ok"
    initWallet' = initWallet wtc tr env dbDir

servePostXPubWallet
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -> WalletBootEnv IO
    -> FilePath
    -> UILayer WalletResource
    -> PostWalletViaXPub
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
servePostXPubWallet wtc tr env dbDir ul request =
    withSessionLayer ul $ \layer -> do
        postXPubWallet layer initWallet' alert ok request
  where
    ok _ = renderHtml . rogerH @Text $ "ok"
    initWallet' = initWallet wtc tr env dbDir

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

serveWalletStatus
    :: NetworkEnv IO x
    -> UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveWalletStatus nenv ul = withSessionLayer ul $ \l ->
    renderHtml <$> getStatus nenv l alertH walletStatusH
