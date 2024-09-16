module Cardano.Wallet.UI.Deposit.Handlers.Wallet
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity
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
import Control.Lens
    ( view
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy.Char8 as BL

catchRunWalletResourceM
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (a -> html)
    -> WalletResourceM a
    -> Handler html
catchRunWalletResourceM layer alert render f = liftIO $ do
    s <- view stateL <$> state layer
    r <- runWalletResourceM f s
    pure $ case r of
        Left e -> alert $ BL.pack $ show e
        Right a -> render a

getWallet
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html) -- problem report
    -> (WalletPublicIdentity -> html) -- success report
    -> Handler html
getWallet layer alert render =
    catchRunWalletResourceM layer alert render walletPublicIdentity
