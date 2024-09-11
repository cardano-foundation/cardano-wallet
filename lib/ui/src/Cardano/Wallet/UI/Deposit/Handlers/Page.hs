{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cardano.Wallet.UI.Deposit.Handlers.Page
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( ResourceStatus (..)
    , readStatus
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
    ( UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Reader.Class
    ( ask
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy.Char8 as BL

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
    -- ^ The page to render
    -> (BL.ByteString -> WHtml ())
    -> Maybe RequestCookies
    -- ^ The request cookies
    -> Handler (CookieResponse RawHtml)
pageHandler tr layer env dir config x alert =
    withSessionLayer layer $ \session -> do
        w <- catchRunWalletResourceM session $ do
            s <- ask >>= liftIO . readStatus
            case s of
                NotInitialized -> do
                    test <- walletExists dir
                    if test
                        then do
                            lg tr "Loading wallet from" dir
                            loadWallet env dir
                            lg tr "Wallet loaded from" dir
                            WalletPresent <$> walletPublicIdentity
                        else
                            pure WalletAbsent
                Initialized _ -> WalletPresent <$> walletPublicIdentity
                Vanished e -> pure $ WalletVanished e
                FailedToInitialize e -> pure $ WalletFailedToInitialize e
                Initializing -> pure WalletInitializing
        lg tr "Rendering page" w
        pure $ page config x alert w
