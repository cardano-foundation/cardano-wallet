{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Application.Logging
    ( ApplicationLog (..)
    ) where

import Cardano.Wallet.Api.Http.Logging
    ( ApiApplicationLog
    )
import Cardano.Wallet.Application.Server
    ( ListenError
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    )
import Cardano.Wallet.Tracing.Data.Tracer
    ( HasPrivacyAnnotation
    , HasSeverityAnnotation (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import Prelude

data ApplicationLog
    = ApiApplicationLog ApiApplicationLog
    | MsgServerStartupError ListenError
    deriving (Generic, Show)

instance ToText ApplicationLog where
    toText = \case
        ApiApplicationLog msg -> toText msg
        MsgServerStartupError err -> toText err

instance HasPrivacyAnnotation ApplicationLog

instance HasSeverityAnnotation ApplicationLog where
    getSeverityAnnotation = \case
        ApiApplicationLog msg -> getSeverityAnnotation msg
        MsgServerStartupError _ -> Error
