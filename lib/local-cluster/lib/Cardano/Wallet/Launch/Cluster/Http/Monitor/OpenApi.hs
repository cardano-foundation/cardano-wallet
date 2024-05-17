{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Http.Monitor.OpenApi
    ( monitoringPaths
    , monitoringDefinitions
    , monitorStateSchema
    , observationSchema
    , phaseSchema
    ) where

import Prelude

import Control.Lens
    ( At (..)
    , (&)
    , (.~)
    , (?~)
    )
import Data.Aeson
import Data.HashMap.Strict.InsOrd
    ( InsOrdHashMap
    )
import Data.OpenApi
    ( AdditionalProperties (AdditionalPropertiesAllowed)
    , Definitions
    , HasAdditionalProperties (additionalProperties)
    , HasContent (..)
    , HasDescription (..)
    , HasEnum (..)
    , HasGet (..)
    , HasItems (..)
    , HasOneOf (..)
    , HasPost (..)
    , HasProperties (..)
    , HasSchema (..)
    , HasSummary (..)
    , HasType (..)
    , OpenApiItems (..)
    , OpenApiType (..)
    , Operation
    , PathItem
    , Reference (..)
    , Referenced (..)
    , Schema
    , _Inline
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Media
    ( MediaType
    )

monitoringDefinitions :: Definitions Schema
monitoringDefinitions =
    [ ("Ready", mempty & type_ ?~ OpenApiBoolean)
    , ("MonitorState", monitorStateSchema)
    , ("Observation", observationSchema)
    ]

monitorStateSchema :: Schema
monitorStateSchema =
    mempty
        & type_ ?~ OpenApiString
        & enum_ ?~ ["waiting", "stepping", "running"]

observationSchema :: Schema
observationSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("phases", Inline historySchema)
               , ("state", Inline monitorStateSchema)
               ]

historySchema :: Schema
historySchema =
    mempty
        & type_ ?~ OpenApiArray
        & items
            ?~ OpenApiItemsObject
                (Inline timedPhaseSchema)

timedPhaseSchema :: Schema
timedPhaseSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("phase", Inline phaseSchema)
               , ("time", Inline $ mempty & type_ ?~ OpenApiString)
               ]

tagged :: Text -> Referenced Schema
tagged t = taggedWithContent t Nothing

taggedWithContent :: Text -> Maybe Schema -> Referenced Schema
taggedWithContent tagName mContentSchema =
    Inline
        $ mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [
                   ( "tag"
                   , Inline
                        $ mempty
                        & type_
                            ?~ OpenApiString
                        & enum_ ?~ [String tagName]
                   )
               ]
                <> maybe [] (\s' -> [("content", Inline s')]) mContentSchema
phaseSchema :: Schema
phaseSchema =
    mempty
        & type_ ?~ OpenApiString
        & description ?~ "The different phases the cluster can be in"
        & oneOf
            ?~ [ tagged "retrieving-funds"
               , tagged "metadata"
               , tagged "genesis"
               , tagged "pool0"
               , tagged "funding"
               , tagged "pools"
               , tagged "relay"
               , taggedWithContent "cluster" $ Just runningNodeSchema
               ]

runningNodeSchema :: Schema
runningNodeSchema =
    mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A running node"
        & properties
            .~ [ ("socket", Inline $ mempty & type_ ?~ OpenApiString)
               , ("genesis", Inline genesisSchema)
               , ("version", Inline nodeToClientVersionDataSchema)
               ]

genesisSchema :: Schema
genesisSchema =
    mempty
        & type_ ?~ OpenApiObject
        & additionalProperties ?~ AdditionalPropertiesAllowed True

nodeToClientVersionDataSchema :: Schema
nodeToClientVersionDataSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("magic", Inline $ mempty & type_ ?~ OpenApiNumber)
               , ("query", Inline $ mempty & type_ ?~ OpenApiBoolean)
               ]

monitoringPaths :: InsOrdHashMap FilePath PathItem
monitoringPaths =
    [ readyPath
    , controlStepPath
    , controlSwitchPath
    , controlObservePath
    ]

controlObservePath :: (FilePath, PathItem)
controlObservePath = ("/control/observe", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & get ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 200 ?~ at200
    summary' = "Observe the local-cluster monitor state"
    at200 =
        "Ok"
            & _Inline . content . at jsonMediaType
                ?~ (mempty & schema ?~ Ref (Reference "Observation"))

controlSwitchPath :: (FilePath, PathItem)
controlSwitchPath = ("/control/switch", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & post ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 200 ?~ at200
    summary' = "Switch the local-cluster monitor"
    at200 =
        "Ok"
            & _Inline . content . at jsonMediaType
                ?~ (mempty & schema ?~ Ref (Reference "MonitorState"))

controlStepPath :: (FilePath, PathItem)
controlStepPath = ("/control/step", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & post ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 204 ?~ at204
    summary' = "Step the local-cluster monitor"
    at204 =
        "No Content"
            & _Inline . content . at jsonMediaType
                ?~ mempty

jsonMediaType :: MediaType
jsonMediaType = "application/json"

readyPath :: (FilePath, PathItem)
readyPath = ("/ready", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & get ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 200 ?~ at200
    summary' = "Check if the local-cluster is ready"
    at200 =
        "Ok"
            & _Inline . content . at jsonMediaType
                ?~ (mempty & schema ?~ Ref (Reference "Ready"))
