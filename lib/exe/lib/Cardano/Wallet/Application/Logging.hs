{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Application.Logging
    ( ApplicationLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation
    , HasSeverityAnnotation (..)
    )
import Cardano.Wallet.Api.Http.Logging
    ( ApiApplicationLog
    )
import Cardano.Wallet.Application.Server
    ( ListenError
    )
import Data.Text.Class
    ( ToText (..)
    )
import GHC.Generics
    ( Generic
    )

data ApplicationLog
    = ApiApplicationLog ApiApplicationLog
    | MsgServerStartupError ListenError
    | DepositUIApplicationLog String
    | DepositApplicationLog String
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText = \case
        ApiApplicationLog msg -> toText msg
        MsgServerStartupError err -> toText err
        DepositUIApplicationLog msg -> toText msg
        DepositApplicationLog msg -> toText msg

instance HasPrivacyAnnotation ApplicationLog

instance HasSeverityAnnotation ApplicationLog where
    getSeverityAnnotation = \case
        ApiApplicationLog msg -> getSeverityAnnotation msg
        MsgServerStartupError _ -> Error
        DepositUIApplicationLog _ -> Info
        DepositApplicationLog _ -> Info
