{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Logging functionality for the Shelley wallet
--
module Cardano.Wallet.Api.Http.Logging
    ( ApiApplicationLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( getSeverityAnnotation
    )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation
    , HasSeverityAnnotation
    , Severity (..)
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import Network.URI
    ( URI
    , uriToString
    )

import qualified Data.Text as T

-- | Log messages related to application startup and shutdown.
data ApiApplicationLog
    = MsgStartingNode CardanoNodeConn
    | MsgNetworkName Text
    | MsgFailedConnectSMASH URI
    deriving (Generic, Show, Eq)

instance ToText ApiApplicationLog where
    toText = \case
        MsgStartingNode conn ->
            "Wallet backend server starting. Using " <> toText conn <> "."
        MsgNetworkName network ->
            "Node is Haskell Node on " <> network <> "."
        MsgFailedConnectSMASH uri -> T.unwords
            [ "Failed connect to the given smash server\
              \ or validate a healthy status."
            , "SMASH uri was: "
            , T.pack $ uriToString id uri ""
            ]

instance HasPrivacyAnnotation ApiApplicationLog
instance HasSeverityAnnotation ApiApplicationLog where
    getSeverityAnnotation = \case
        MsgStartingNode _ -> Info
        MsgNetworkName _ -> Info
        MsgFailedConnectSMASH _ -> Warning
