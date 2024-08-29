{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Personal.Handlers.Lib
    ( handleParseRequestError
    , alertOnServerError
    , catching
    , withWallet
    , evenWithNoWallet
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Personal.Layer
    ( SessionLayer (..)
    , walletId
    )
import Control.Lens
    ( view
    )
import Control.Monad.Catch
    ( MonadCatch (..)
    , SomeException (..)
    )
import Data.Aeson
    ( Value
    , decode
    )
import Servant
    ( Handler
    , ServerError (..)
    , err400
    , throwError
    )
import Servant.Server
    ( runHandler
    )

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Handle a parsing error by returning a 400 error with the error message.
handleParseRequestError :: Either String a -> Handler a
handleParseRequestError = \case
    Left e -> throwError $ err400{errBody = BL.pack e}
    Right a -> pure a

--
alertOnServerError
    :: (BL.ByteString -> html)
    -> (b -> html)
    -> Either ServerError b
    -> html
alertOnServerError alert render =  \case
    Left ServerError{..} ->
        case decode errBody of
            Nothing -> alert errBody
            Just je -> alert $ Aeson.encodePretty @Value je
    Right ws -> render ws

catching :: MonadCatch m => (BL.ByteString -> html) -> m html -> m html
catching alert f = catch f
    $ \(SomeException e) -> pure . alert . BL.pack . show $ e

-- | Run a handler with the current wallet, if any, or return an error message.
withWallet
    :: SessionLayer (Maybe WalletId)
    -> (BL.ByteString -> html)
    -- ^ Alert renderer
    -> (a -> html)
    -- ^ Result renderer
    -> (WalletId -> Handler a)
    -- ^ Action to run with the wallet
    -> IO html
withWallet SessionLayer{..} alert render action = catching alert $ do
    mwid <- view walletId <$> state
    case mwid of
        Nothing -> do
            pure $ alert "No wallet selected"
        Just wid -> do
            result <- runHandler $ action wid
            pure $ alertOnServerError alert render result

evenWithNoWallet
    :: (BL.ByteString -> html)
    -> (a -> html)
    -> (Handler a)
    -> IO html
evenWithNoWallet alert render action =
    catching alert $ do
        result <- runHandler action
        pure $ alertOnServerError alert render result
