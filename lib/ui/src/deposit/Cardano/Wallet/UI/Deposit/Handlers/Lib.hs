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
import Control.Concurrent.STM
    ( atomically
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
import Servant
    ( Handler (..)
    , err500
    )

import qualified Cardano.Wallet.Deposit.REST.Catch as REST
import qualified Data.ByteString.Lazy.Char8 as BL

catchRunWalletResourceM
    :: SessionLayer WalletResource
    -> WalletResourceM a
    -> Handler a
catchRunWalletResourceM layer f = do
    r <- liftIO $ view stateL <$> state layer
    REST.catchRunWalletResourceM r err500 f

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

walletPresence :: SessionLayer WalletResource -> Handler WalletPresent
walletPresence session = catchRunWalletResourceM session $ do
    s <- ask >>= liftIO . atomically . readStatus
    case s of
        Closed -> pure WalletAbsent
        Open _ -> WalletPresent <$> walletPublicIdentity
        Vanished e -> pure $ WalletVanished e
        FailedToOpen e -> pure $ WalletFailedToInitialize e
        Opening -> pure WalletInitializing
        Closing -> pure WalletClosing
