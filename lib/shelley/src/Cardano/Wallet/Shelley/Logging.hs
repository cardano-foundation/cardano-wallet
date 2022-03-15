{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Logging functionality for the Shelley wallet
--
module Cardano.Wallet.Shelley.Logging
    ( ApplicationLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( getSeverityAnnotation )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation, HasSeverityAnnotation, Severity (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConn )
import Cardano.Wallet.Api.Server
    ( ListenError (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )
import Network.URI
    ( URI, uriToString )

import qualified Blockfrost.Client as Blockfrost
import qualified Data.Text as T

-- | Log messages related to application startup and shutdown.
data ApplicationLog
    = MsgStartingNode CardanoNodeConn
    | MsgStartingLite Blockfrost.Project
    | MsgNetworkName Text
    | MsgServerStartupError ListenError
    | MsgFailedConnectSMASH URI
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText = \case
        MsgStartingNode conn ->
            "Wallet backend server starting. Using " <> toText conn <> "."
        MsgStartingLite Blockfrost.Project{..} ->
            "Wallet backend server starting. Using lite mode: Blockfrost, " <>
            T.pack (show projectEnv) <> "."
        MsgNetworkName network ->
            "Node is Haskell Node on " <> network <> "."
        MsgServerStartupError startupErr -> case startupErr of
            ListenErrorHostDoesNotExist host -> mempty
                <> "Can't listen on "
                <> T.pack (show host)
                <> ". It does not exist."
            ListenErrorInvalidAddress host -> mempty
                <> "Can't listen on "
                <> T.pack (show host)
                <> ". Invalid address."
            ListenErrorAddressAlreadyInUse mPort -> mempty
                <> "The API server listen port "
                <> maybe "(unknown)" (T.pack . show) mPort
                <> " is already in use."
            ListenErrorOperationNotPermitted -> mempty
                <> "Cannot listen on the given port. "
                <> "The operation is not permitted."
        MsgFailedConnectSMASH uri -> T.unwords
            [ "Failed connect to the given smash server\
              \ or validate a healthy status."
            , "SMASH uri was: "
            , T.pack $ uriToString id uri ""
            ]

instance HasPrivacyAnnotation ApplicationLog
instance HasSeverityAnnotation ApplicationLog where
    getSeverityAnnotation = \case
        MsgStartingNode _ -> Info
        MsgStartingLite _ -> Info
        MsgNetworkName _ -> Info
        MsgServerStartupError _ -> Alert
        MsgFailedConnectSMASH _ -> Warning
