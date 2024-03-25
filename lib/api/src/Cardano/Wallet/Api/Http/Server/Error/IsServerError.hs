{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Http.Server.Error.IsServerError
    ( IsServerError (..)
    , liftHandler
    , liftE
    , handler
    , apiError
    , err425
    , showT
    )
    where

import Prelude

import Cardano.Wallet.Api.Types.Error
    ( ApiError (..)
    , ApiErrorInfo
    , ApiErrorMessage (ApiErrorMessage)
    )
import Control.Monad.Except
    ( ExceptT
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Except
    ( throwE
    , withExceptT
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Media
    ( RenderHeader (..)
    )
import Network.HTTP.Types
    ( hContentType
    )
import Servant
    ( Accept (..)
    , Handler (..)
    , JSON
    , Proxy (..)
    , ServerError (..)
    )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

-- | Maps types to servant error responses.
class IsServerError e where
    -- | A structured human-readable error code to return to API clients.
    toServerError :: e -> ServerError

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
liftHandler :: IsServerError e => ExceptT e IO a -> Handler a
liftHandler action = Handler (withExceptT toServerError action)

liftE :: IsServerError e => e -> Handler a
liftE = liftHandler . throwE

-- | Lift an IO action into servant 'Handler'
handler :: IO a -> Handler a
handler = Handler . liftIO

apiError :: ServerError -> ApiErrorInfo -> Text -> ServerError
apiError err info messageUnformatted =
    err
        { errBody = Aeson.encode ApiError{info, message}
        , errHeaders =
            (hContentType, renderHeader $ contentType $ Proxy @JSON)
                : errHeaders err
        }
  where
    message = ApiErrorMessage (T.replace "\n" " " messageUnformatted)

err425 :: ServerError
err425 = ServerError 425 "Too early" "" []

-- | Small helper to easy show things to Text
showT :: Show a => a -> Text
showT = T.pack . show
