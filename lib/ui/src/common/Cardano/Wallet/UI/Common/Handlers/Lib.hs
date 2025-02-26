{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Handlers.Lib
    ( handleParseRequestError
    , alertOnServerError
    , catching
    )
where

import Prelude

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

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Handle a parsing error by returning a 400 error with the error message.
handleParseRequestError :: Either String a -> Handler a
handleParseRequestError = \case
    Left e -> throwError $ err400{errBody = BL.pack e}
    Right a -> pure a

-- | Render an Either ServerError b as HTML, using the first argument to render
-- the error message in case of a server error, and the second argument to render
-- the value in case of success.
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

-- | Catch exceptions and render them as HTML using the provided function.
catching :: MonadCatch m => (BL.ByteString -> html) -> m html -> m html
catching alert f = catch f
    $ \(SomeException e) -> pure . alert . BL.pack . show $ e
