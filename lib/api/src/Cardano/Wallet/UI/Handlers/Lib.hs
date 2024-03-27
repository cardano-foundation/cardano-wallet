{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Handlers.Lib where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Layer
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
import Data.Functor
    ( ($>)
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

handleParseRequestError :: Either String a -> Handler a
handleParseRequestError = \case
    Left e -> throwError $ err400{errBody = BL.pack e}
    Right a -> pure a

alertOnServerError
    :: Applicative m => (BL.ByteString -> a)
    -> Either ServerError b
    -> (b -> m a)
    -> m a
alertOnServerError alert x render = case x of
    Left ServerError{..} ->
        pure $ case decode errBody of
            Nothing -> alert errBody
            Just je -> alert $ Aeson.encodePretty @Value je
    Right ws -> render ws

catching :: MonadCatch m => (BL.ByteString -> a) -> m a -> m a
catching alert f = catch f
    $ \(SomeException e) -> pure . alert . BL.pack . show $ e

withWallet
    :: SessionLayer
    -> (BL.ByteString -> html)
    -> (a -> html)
    -> (WalletId -> Handler a)
    -> IO ((a -> IO ()) -> IO html)
withWallet SessionLayer{..} alert render action = catching alert' $ do
    mwid <- view walletId <$> state
    case mwid of
        Nothing -> do
            pure $ \_ -> pure $ alert "No wallet selected"
        Just wid -> do
            result <- runHandler $ action wid
            pure $ \f -> alertOnServerError alert result $ \x -> do
                f x $> render x
    where
        alert' x _ = pure $ alert x

evenWithNoWallet
    :: (BL.ByteString -> html)
    -> (a -> html)
    -> (Handler a)
    -> IO ((a -> IO ()) -> IO html)
evenWithNoWallet alert render action =
        catching alert' $ do
            result <- runHandler action
            pure $ \f -> alertOnServerError alert result $ \x -> do
                f x $> render x
    where
        alert' x _ = pure $ alert x
