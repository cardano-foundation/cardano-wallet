{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.UI.Handlers.Lib where

import Prelude

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

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

handleParseRequestError :: Either String a -> Handler a
handleParseRequestError = \case
    Left e -> throwError $ err400{errBody = BL.pack e}
    Right a -> pure a

alerting
    :: Applicative m
    => (BL.ByteString -> a)
    -> Either ServerError b
    -> (b -> m a)
    -> m a
alerting alert x render = case x of
    Left ServerError{..} ->
        pure $ case decode errBody of
            Nothing -> alert errBody
            Just je -> alert $ Aeson.encodePretty @Value je
    Right ws -> render ws
