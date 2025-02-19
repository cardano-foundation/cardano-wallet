module Cardano.Wallet.Deposit.REST.Catch
    ( catchRunWalletResourceM
    )
where

import Prelude

import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , runWalletResourceM
    )
import Control.Exception
    ( SomeException (..)
    , try
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans.Except
    ( throwE
    )
import Servant
    ( Handler (..)
    , ServerError (..)
    )

import qualified Data.ByteString.Lazy.Char8 as BL

-- | Catch and run a 'WalletResourceM' action, converting any exceptions to
-- 'ServerError'.
catchRunWalletResourceM
    :: WalletResource
    -> ServerError
    -> WalletResourceM a
    -> Handler a
catchRunWalletResourceM s se f = do
    er <- liftIO $ try $ runWalletResourceM f s
    case er of
        Right (Right a) ->
            pure a
        Right (Left e) ->
            Handler $ throwE $ se{errBody = BL.pack $ show e}
        Left (SomeException e) ->
            Handler $ throwE $ se{errBody = BL.pack $ show e}
