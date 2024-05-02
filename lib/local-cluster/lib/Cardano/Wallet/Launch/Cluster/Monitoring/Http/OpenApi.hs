{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.OpenApi
    ( generateOpenapi3
    , apiSchema
    , definitions
    , monitorStateSchema
    , observationSchema
    ) where

import Prelude

import Control.Lens
    ( At (..)
    , (&)
    , (.~)
    , (?~)
    )
import Data.Aeson
import Data.Aeson.Encode.Pretty
    ( encodePretty
    )
import Data.HashMap.Strict.InsOrd
    ( InsOrdHashMap
    )
import Data.OpenApi
    ( Definitions
    , HasComponents (..)
    , HasContent (..)
    , HasDescription (..)
    , HasEnum (..)
    , HasGet (..)
    , HasInfo (..)
    , HasItems (..)
    , HasLicense (license)
    , HasOneOf (..)
    , HasPaths (..)
    , HasPost (..)
    , HasProperties (..)
    , HasSchema (..)
    , HasSchemas (..)
    , HasSummary (..)
    , HasTitle (..)
    , HasType (..)
    , HasUrl (..)
    , HasVersion (..)
    , License
    , OpenApi
    , OpenApiItems (..)
    , OpenApiType (..)
    , Operation
    , PathItem
    , Reference (..)
    , Referenced (..)
    , Schema
    , URL (..)
    , _Inline
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Media
    ( MediaType
    )

import qualified Data.ByteString.Lazy.Char8 as BL

generateOpenapi3 :: BL.ByteString
generateOpenapi3 = encodePretty apiSchema
    -- jsonMediaType :: MediaType
    -- jsonMediaType = "application/json"

apiSchema :: OpenApi
apiSchema :: OpenApi =
        mempty
            & info . title .~ "Cardano Wallet Monitoring API"
            & info . version .~ "0.1.0.0"
            & info . description ?~ "This is the API for the monitoring server"
            & info . license ?~ license'
            & paths .~ paths'
            & components . schemas .~ definitions

definitions :: Definitions Schema
definitions = [ ("Ready", mempty & type_ ?~ OpenApiBoolean)
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
                <> maybe [] (\s' -> [("contents", Inline s')]) mContentSchema
phaseSchema :: Schema
phaseSchema =
    mempty
        & type_ ?~ OpenApiString
        & description ?~ "The different phases the cluster can be in"
        & oneOf
            ?~ [ tagged "RetrievingFunds"
               , tagged "Metadata"
               , tagged "Genesis"
               , tagged "Pool0"
               , tagged "Funding"
               , tagged "Pools"
               , tagged "Relay"
               , taggedWithContent "Cluster" $ Just relayNodeSchema
               ]

relayNodeSchema :: Schema
relayNodeSchema =
    mempty
        & type_ ?~ OpenApiString
        & description ?~ "The socket file or pipe of a relay node"

license' :: License
license' =
    "Apache 2"
        & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0.html"

paths' :: InsOrdHashMap FilePath PathItem
paths' =
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
