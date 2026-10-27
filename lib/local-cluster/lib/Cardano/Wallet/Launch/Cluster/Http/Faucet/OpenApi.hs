{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.OpenApi
    ( faucetDefinitions
    , faucetPaths
    , sendAssetsSchema
    , clusterDRepSchema
    ) where

import Control.Lens
    ( At (..)
    , (&)
    , (.~)
    , (?~)
    )
import Data.HashMap.Strict.InsOrd
    ( InsOrdHashMap
    )
import Data.OpenApi
    ( Definitions
    , HasContent (..)
    , HasIn (..)
    , HasItems (..)
    , HasName (..)
    , HasParameters (..)
    , HasPost (..)
    , HasProperties (..)
    , HasRequestBody (..)
    , HasRequired (..)
    , HasSchema (..)
    , HasSummary (..)
    , HasType (..)
    , OpenApiItems (..)
    , OpenApiType (..)
    , Operation
    , ParamLocation (..)
    , PathItem
    , Reference (..)
    , Referenced (..)
    , RequestBody
    , Response
    , Schema
    , _Inline
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Media
    ( MediaType
    )
import Prelude

faucetDefinitions :: Definitions Schema
faucetDefinitions =
    [ ("SendAssets", sendAssetsSchema)
    , ("ClusterDRep", clusterDRepSchema)
    ]

sendAssetsSchema :: Schema
sendAssetsSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("batch-size", Inline $ mempty & type_ ?~ OpenApiInteger)
               , ("assets", Inline assetsSchema)
               ]

assetsSchema :: Schema
assetsSchema =
    mempty
        & type_ ?~ OpenApiArray
        & items
            ?~ OpenApiItemsObject
                (Inline assetSchema)

assetSchema :: Schema
assetSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("address", Inline $ mempty & type_ ?~ OpenApiString)
               , ("bundle", Inline bundleSchema)
               , ("metadata", Inline metadataSchema)
               ]

metadataSchema :: Schema
metadataSchema =
    mempty
        & type_ ?~ OpenApiArray
        & items
            ?~ OpenApiItemsObject
                (Inline metadataValueSchema)

metadataValueSchema :: Schema
metadataValueSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("key", Inline $ mempty & type_ ?~ OpenApiString)
               , ("value", Inline $ mempty & type_ ?~ OpenApiString)
               ]

bundleSchema :: Schema
bundleSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("assets", Inline assetsQuantitySchema)
               , ("coin", Inline $ mempty & type_ ?~ OpenApiInteger)
               ]

assetsQuantitySchema :: Schema
assetsQuantitySchema =
    mempty
        & type_ ?~ OpenApiArray
        & items
            ?~ OpenApiItemsObject
                (Inline assetQuantitySchema)

assetQuantitySchema :: Schema
assetQuantitySchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("asset", Inline assetNameSchema)
               , ("quantity", Inline $ mempty & type_ ?~ OpenApiInteger)
               ]

assetNameSchema :: Schema
assetNameSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("name", Inline $ mempty & type_ ?~ OpenApiString)
               , ("policy", Inline $ mempty & type_ ?~ OpenApiString)
               ]

-- | A DRep whose keys the local cluster holds, named by the hash of its
-- verification key.
clusterDRepSchema :: Schema
clusterDRepSchema =
    mempty
        & type_ ?~ OpenApiObject
        & required .~ ["key_hash"]
        & properties
            .~ [ ("key_hash", Inline $ mempty & type_ ?~ OpenApiString)
               ]

faucetPaths :: InsOrdHashMap FilePath PathItem
faucetPaths =
    [ sendFaucetAssetsPath
    , registerNewDRepPath
    , registerDRepPath
    , retireDRepPath
    ]

sendFaucetAssetsPath :: (FilePath, PathItem)
sendFaucetAssetsPath = ("/send/assets", pathItem)
  where
    pathItem :: PathItem
    pathItem =
        mempty
            & post ?~ operation
            & parameters
                .~ [ Inline
                        $ mempty
                        & in_ .~ ParamPath
                        & name .~ "assets"
                        & schema ?~ Ref (Reference "SendAssets")
                   ]
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 204 ?~ at204
    summary' = "Send assets to the faucet"
    at204 :: Referenced Response
    at204 =
        "No Content"
            & _Inline . content . at ("application/json" :: MediaType)
                ?~ mempty

registerNewDRepPath :: (FilePath, PathItem)
registerNewDRepPath = ("/drep/register-new", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & post ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary
                ?~ "Generate DRep keys and register the DRep, \
                   \returning once the ledger holds the registration"
            & at 200 ?~ at200
    at200 :: Referenced Response
    at200 =
        "Ok"
            & _Inline . content . at ("application/json" :: MediaType)
                ?~ (mempty & schema ?~ Ref (Reference "ClusterDRep"))

registerDRepPath :: (FilePath, PathItem)
registerDRepPath =
    clusterDRepPath
        "/drep/register"
        "Register a DRep whose keys the cluster holds, \
        \returning once the ledger holds the registration"

retireDRepPath :: (FilePath, PathItem)
retireDRepPath =
    clusterDRepPath
        "/drep/retire"
        "Retire a DRep whose keys the cluster holds, \
        \returning once the ledger no longer holds the DRep"

clusterDRepPath :: FilePath -> Text -> (FilePath, PathItem)
clusterDRepPath path summary' = (path, pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & post ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & requestBody ?~ Inline body
            & at 204 ?~ at204
    body :: RequestBody
    body =
        mempty
            & content . at ("application/json" :: MediaType)
                ?~ (mempty & schema ?~ Ref (Reference "ClusterDRep"))
    at204 :: Referenced Response
    at204 = Inline "No Content"
