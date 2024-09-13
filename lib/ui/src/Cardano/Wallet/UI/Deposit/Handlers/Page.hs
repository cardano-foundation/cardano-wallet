{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cardano.Wallet.UI.Deposit.Handlers.Page
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , loadWallet
    , walletExists
    , walletPublicIdentity
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( PageConfig
    )
import Cardano.Wallet.UI.Common.Layer
    ( Push (..)
    , UILayer (..)
    , sendSSE
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    , walletPresent
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Servant
    ( Handler
    )

lg :: (MonadIO m, Show a) => Tracer IO String -> String -> a -> m ()
lg tr p x = liftIO $ traceWith tr $ p <> ": " <> show x

pageHandler
    :: Tracer IO String
    -> UILayer WalletResource
    -> WalletBootEnv IO
    -- ^ The deposit UI layer
    -> FilePath
    -- ^ The directory where the wallet data is stored
    -> PageConfig
    -- ^ The page configuration
    -> Page
    -- ^ How to alert
    -> Maybe RequestCookies
    -- ^ The request cookies
    -> Handler (CookieResponse RawHtml)
pageHandler tr layer env dir config x =
    withSessionLayer layer $ \session -> do
        wp <- walletPresent session
        wp' <- catchRunWalletResourceM session $ do
            case wp of
                WalletAbsent -> do
                    test <- walletExists dir
                    if test
                        then do
                            lg tr "Loading wallet from" dir
                            loadWallet env dir
                                $ Tracer
                                $ \_ -> sendSSE session $ Push "wallet-present"
                            lg tr "Wallet loaded from" dir
                            WalletPresent <$> walletPublicIdentity
                        else
                            pure WalletAbsent
                wp'' -> pure wp''

        lg tr "Rendering page" wp'
        pure $ page config x
