{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    , ApiT (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON
    , Value (..)
    , object
    , toJSON
    , withObject
    , (.:)
    , (.=)
    )

import Servant
    ( Post
    , PostNoContent
    )
import Servant.API
    ( Get
    , JSON
    , (:<|>)
    , (:>)
    )

-- | The API for the monitoring server
type API =
    "ready" :> Get '[JSON] Bool
        :<|> "control" :> "step" :> PostNoContent
        :<|> "control" :> "switch" :> Post '[JSON] (ApiT MonitorState)
        :<|> "control" :> "observe" :> Get '[JSON] (ApiT (History, MonitorState))

-- | A newtype wrapper to avoid orphan instances
newtype ApiT a = ApiT {unApiT :: a}
    deriving newtype (Eq, Show)

instance ToJSON (ApiT MonitorState) where
    toJSON = \case
        ApiT Wait -> String "waiting"
        ApiT Step -> String "stepping"
        ApiT Run -> String "running"

instance FromJSON (ApiT MonitorState) where
    parseJSON = \case
        String "waiting" -> pure $ ApiT Wait
        String "stepping" -> pure $ ApiT Step
        String "running" -> pure $ ApiT Run
        _ -> fail "Invalid state"

instance ToJSON (ApiT (History, MonitorState)) where
    toJSON (ApiT (History{history}, state)) =
        object
            [ "phases" .=  history
            , "state" .= ApiT state
            ]

instance FromJSON (ApiT (History, MonitorState)) where
    parseJSON = withObject "ApiT (History, MonitorState)" $ \o -> do
        history <- o .: "phases"
        ApiT state <- o .: "state"
        pure $ ApiT (History{history}, state)
