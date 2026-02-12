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

module Cardano.Wallet.Launch.Cluster.Http.Monitor.API
    ( ApiT (..)
    , ReadyAPI
    , StepAPI
    , SwitchAPI
    , ObserveAPI
    , ControlAPI
    , renderPhase
    )
where

import Cardano.Launcher.Node
    ( cardanoNodeConn
    , nodeSocketFile
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.OpenApi
    ( monitorStateSchema
    , observationSchema
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Applicative
    ( asum
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
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic (..)
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..)
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
import Prelude

import qualified Data.Map as Map

type ReadyAPI = "ready" :> Get '[JSON] Bool
type StepAPI = "control" :> "step" :> PostNoContent
type SwitchAPI =
    "control" :> "switch" :> Post '[JSON] (ApiT MonitorState)
type ObserveAPI =
    "control" :> "observe" :> Get '[JSON] (ApiT (History, MonitorState))

-- | The API to control the monitoring server
type ControlAPI = ReadyAPI :<|> StepAPI :<|> SwitchAPI :<|> ObserveAPI

-- | A newtype wrapper to avoid orphan instances
newtype ApiT a = ApiT {unApiT :: a}
    deriving newtype (Eq, Show, Generic)

renderHistory :: History -> Value
renderHistory History{history} = toJSON $ do
    (time, phase) <- history
    pure
        $ object
            [ "time" .= time
            , "phase" .= renderPhase phase
            ]

renderTagged :: Text -> Value
renderTagged tagName =
    object
        [ "tag" .= tagName
        ]

renderTaggedWithContent :: Text -> (Maybe Value) -> Value
renderTaggedWithContent tagName mContent =
    object
        $ [ "tag" .= tagName
          ]
            <> ["content" .= content | Just content <- [mContent]]

renderPhase :: Phase -> Value
renderPhase = \case
    RetrievingFunds -> renderTagged "retrieving-funds"
    Metadata -> renderTagged "metadata"
    Genesis -> renderTagged "genesis"
    Pool0 -> renderTagged "pool0"
    Funding -> renderTagged "funding"
    Pools -> renderTagged "pools"
    Relay -> renderTagged "relay"
    Cluster mNode -> renderTaggedWithContent "cluster" $ fmap renderNode mNode

renderNode :: RunningNode -> Value
renderNode
    RunningNode
        { runningNodeSocketPath
        , runningNodeShelleyGenesis
        , runningNodeVersionData =
            NodeToClientVersionData
                { networkMagic = NetworkMagic nm
                , query
                }
        } =
        object
            [ "socket" .= nodeSocketFile runningNodeSocketPath
            , "genesis" .= runningNodeShelleyGenesis
            , "version"
                .= object
                    [ "magic" .= nm
                    , "query" .= query
                    ]
            ]

parseHistory :: Value -> Parser History
parseHistory = withArray "History" $ \arr -> do
    history <- traverse parsePhase (toList arr)
    pure $ History{history}
  where
    parsePhase = withObject "Phase" $ \o -> do
        time <- o .: "time"
        phase <- o .: "phase" >>= parsePhase'
        pure (time, phase)

type Tags a = [(Text, [Either a (Value -> Parser a)])]

tag :: a -> b -> (a, [b])
tag a b = (a, [b])

tags :: a -> [b] -> (a, [b])
tags a bs = (a, bs)

parseTaggeds :: Tags a -> Value -> Parser a
parseTaggeds ts = withObject "Tagged" $ \o -> do
    t <- o .: "tag"
    case Map.lookup t $ Map.fromList ts of
        Just fs ->
            let g :: Either a (Value -> Parser a) -> Parser a
                g = \case
                    Left a -> pure a
                    Right f -> o .: "content" >>= f
            in  asum $ g <$> fs
        Nothing -> fail "Invalid tag"

parsePhase' :: Value -> Parser Phase
parsePhase' =
    parseTaggeds
        [ tag "retrieving-funds" $ Left RetrievingFunds
        , tag "metadata" $ Left Metadata
        , tag "genesis" $ Left Genesis
        , tag "pool0" $ Left Pool0
        , tag "funding" $ Left Funding
        , tag "pools" $ Left Pools
        , tag "relay" $ Left Relay
        , tags "cluster" [Right parseNode, Left $ Cluster Nothing]
        ]

parseNode :: Value -> Parser Phase
parseNode = withObject "RunningNode" $ \o -> do
    socket <- o .: "socket"
    genesis <- o .: "genesis"
    version <- o .: "version" >>= parseVersionData
    case cardanoNodeConn socket of
        Left e -> fail e
        Right nodeConn ->
            pure
                $ Cluster
                $ Just
                    RunningNode
                        { runningNodeSocketPath = nodeConn
                        , runningNodeShelleyGenesis = genesis
                        , runningNodeVersionData = version
                        }

parseVersionData :: Value -> Parser NodeToClientVersionData
parseVersionData = withObject "NodeToClientVersionData" $ \o -> do
    nm <- o .: "magic"
    query <- o .: "query"
    pure
        NodeToClientVersionData
            { networkMagic = NetworkMagic nm
            , query
            }

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
