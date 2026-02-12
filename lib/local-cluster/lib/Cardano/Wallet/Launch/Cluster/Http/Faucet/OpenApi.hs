{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.OpenApi
    ( faucetDefinitions
    , faucetPaths
    , sendAssetsSchema
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
    , Response
    , Schema
    , _Inline
    )
import Network.HTTP.Media
    ( MediaType
    )
import Prelude

faucetDefinitions :: Definitions Schema
faucetDefinitions =
    [ ("SendAssets", sendAssetsSchema)
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

faucetPaths :: InsOrdHashMap FilePath PathItem
faucetPaths = [sendFaucetAssetsPath]

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
