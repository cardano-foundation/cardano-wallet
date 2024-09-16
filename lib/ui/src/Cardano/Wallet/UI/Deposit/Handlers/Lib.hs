module Cardano.Wallet.UI.Deposit.Handlers.Lib
where

import Prelude

import Cardano.Wallet.Deposit.IO.Resource
    ( ResourceStatus (..)
    , readStatus
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , runWalletResourceM
    , walletPublicIdentity
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    , stateL
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    )
import Control.Lens
    ( view
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Control.Monad.Trans.Except
    ( throwE
    )
import Servant
    ( Handler (..)
    , ServerError (..)
    , err500
    )

import qualified Data.ByteString.Lazy.Char8 as BL

catchRunWalletResourceM
    :: SessionLayer WalletResource
    -> WalletResourceM a
    -> Handler a
catchRunWalletResourceM layer f = do
    r <- liftIO $ do
        s <- view stateL <$> state layer
        runWalletResourceM f s
    case r of
        Right a -> pure a
        Left e -> Handler $ throwE $ err500{errBody = BL.pack $ show e}

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

walletPresent :: SessionLayer WalletResource -> Handler WalletPresent
walletPresent session = catchRunWalletResourceM session $ do
    s <- ask >>= liftIO . readStatus
    case s of
        Closed -> pure WalletAbsent
        Open _ -> WalletPresent <$> walletPublicIdentity
        Vanished e -> pure $ WalletVanished e
        FailedToOpen e -> pure $ WalletFailedToInitialize e
        Opening -> pure WalletInitializing
        Closing -> pure WalletClosing
