{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    , ApiT (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.OpenApi
    ( monitorStateSchema
    , observationSchema
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Data.Aeson.Types
    ( FromJSON (..)
    , Parser
    , ToJSON (..)
    , Value (..)
    , object
    , withArray
    , withObject
    , (.:)
    , (.=)
    )
import Data.Foldable
    ( toList
    )
import Data.OpenApi
    ( NamedSchema (..)
    , ToSchema (..)
    )
import GHC.Generics
    ( Generic (..)
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
    deriving newtype (Eq, Show, Generic)

renderHistory :: History -> Value
renderHistory History{history} = toJSON $ do
    (time, phase) <- history
    pure
        $ object
            [ "time" .= time
            , "phase" .= phase
            ]

parseHistory :: Value -> Parser History
parseHistory = withArray "History" $ \arr -> do
    history <- traverse parsePhase (toList arr)
    pure $ History{history}
  where
    parsePhase = withObject "Phase" $ \o -> do
        time <- o .: "time"
        phase <- o .: "phase"
        pure (time, phase)

instance ToJSON (ApiT MonitorState) where
    toJSON = \case
        ApiT Wait -> String "waiting"
        ApiT Step -> String "stepping"
        ApiT Run -> String "running"

instance ToSchema (ApiT MonitorState) where
    declareNamedSchema _ = do
        pure
            $ NamedSchema
                (Just "ApiT MonitorState")
                monitorStateSchema

instance FromJSON (ApiT MonitorState) where
    parseJSON = \case
        String "waiting" -> pure $ ApiT Wait
        String "stepping" -> pure $ ApiT Step
        String "running" -> pure $ ApiT Run
        _ -> fail "Invalid state"

instance ToJSON (ApiT (History, MonitorState)) where
    toJSON (ApiT (history, state)) =
        object
            [ "phases" .= renderHistory history
            , "state" .= ApiT state
            ]

instance ToSchema (ApiT (History, MonitorState)) where
    declareNamedSchema _ =
        pure
            $ NamedSchema
                (Just "ApiT (History, MonitorState)")
                observationSchema

instance FromJSON (ApiT (History, MonitorState)) where
    parseJSON = withObject "ApiT (History, MonitorState)" $ \o -> do
        history <- o .: "phases" >>= parseHistory
        ApiT state <- o .: "state"
        pure $ ApiT (history, state)
