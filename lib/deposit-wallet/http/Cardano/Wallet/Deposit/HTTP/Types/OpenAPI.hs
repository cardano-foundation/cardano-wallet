{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.HTTP.Types.OpenAPI
    ( generateOpenapi3
    , apiSchema
    , depositPaths
    , depositDefinitions

    , customerSchema
    , addressSchema
    , customerListSchema
    , chainPointSchema
    ) where

import Prelude

import Control.Lens
    ( At (..)
    , (&)
    , (.~)
    , (?~)
    )
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
    , HasFormat (..)
    , HasGet (..)
    , HasIn (..)
    , HasInfo (..)
    , HasItems (..)
    , HasLicense (license)
    , HasMaxLength (..)
    , HasMaximum (..)
    , HasMinLength (..)
    , HasMinimum (..)
    , HasName (..)
    , HasOneOf (..)
    , HasParameters (..)
    , HasPaths (..)
    , HasProperties (..)
    , HasPut (..)
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
    , ParamLocation (..)
    , PathItem
    , Reference (..)
    , Referenced (..)
    , Schema
    , URL (..)
    , _Inline
    )
import Data.Word
    ( Word64
    )
import Network.HTTP.Media
    ( MediaType
    )

import qualified Data.ByteString.Lazy.Char8 as BL

generateOpenapi3 :: BL.ByteString
generateOpenapi3 = encodePretty apiSchema

apiSchema :: OpenApi
apiSchema :: OpenApi =
    mempty
        & info . title .~ "Cardano Deposit Wallet API"
        & info . version .~ "0.0.0.1"
        & info . description ?~ "This is the API for the deposit wallet"
        & info . license ?~ license'
        & paths .~ depositPaths
        & components . schemas .~ depositDefinitions

license' :: License
license' =
    "Apache 2"
        & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0.html"

depositPaths :: InsOrdHashMap FilePath PathItem
depositPaths =
    [ getCustomersListPath
    , putCustomerPath
    , getLocalTipPath
    ]

depositDefinitions :: Definitions Schema
depositDefinitions =
    [ ("ApiT Customer", customerSchema)
    , ("ApiT Address", addressSchema)
    , ("ApiT CustomerList", customerListSchema)
    , ("ApiT ChainPoint", chainPointSchema)
    ]

-- | Paths
jsonMediaType :: MediaType
jsonMediaType = "application/json"

getCustomersListPath :: (FilePath, PathItem)
getCustomersListPath = ("/customers", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & get ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 200 ?~ at200
    summary' = "Obtain the list of customers"
    at200 =
        "Ok"
            & _Inline . content . at jsonMediaType
                ?~ (mempty & schema ?~ Ref (Reference "ApiT CustomerList"))

putCustomerPath :: (FilePath, PathItem)
putCustomerPath = ("/customers", pathItem)
  where
    pathItem :: PathItem
    pathItem =
        mempty
            & put ?~ operation
            & parameters
                .~ [ Inline
                        $ mempty
                        & in_ .~ ParamPath
                        & name .~ "customerId"
                        & schema ?~ Ref (Reference "ApiT Customer")
                   ]
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 200 ?~ at200
    summary' = "Add customer"
    at200 =
        "Ok"
            & _Inline . content . at jsonMediaType
                ?~ (mempty & schema ?~ Ref (Reference "ApiT Address"))

-- | Input/Output type schemas
customerSchema :: Schema
customerSchema =
    mempty
        & type_ ?~ OpenApiInteger
        & minimum_ ?~ 0
        & maximum_ ?~ 2147483647

addressSchema :: Schema
addressSchema =
    mempty
        & type_ ?~ OpenApiString
        & format ?~ "hex"

customerListItemSchema :: Schema
customerListItemSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~ [ ("customer", Inline customerSchema)
               , ("address", Inline addressSchema)
               ]

customerListSchema :: Schema
customerListSchema =
    mempty
        & type_ ?~ OpenApiArray
        & items
            ?~ OpenApiItemsObject
                (Inline customerListItemSchema)

getLocalTipPath :: (FilePath, PathItem)
getLocalTipPath = ("/network/local-tip", pathItem)
  where
    pathItem :: PathItem
    pathItem = mempty & get ?~ operation
    operation :: Operation
    operation =
        mempty
            & summary ?~ summary'
            & at 200 ?~ at200
    summary' = "Obtain the chain point until which the wallet is synchronized against the network"
    at200 =
        "Ok"
            & _Inline . content . at jsonMediaType
                ?~ (mempty & schema ?~ Ref (Reference "ApiT ChainPoint"))

chainPointSchema :: Schema
chainPointSchema =
    mempty
       & oneOf ?~ [Inline chainPointOriginSchema, Inline chainPointAtSlotSchema]

chainPointOriginSchema :: Schema
chainPointOriginSchema =
    mempty
        & type_ ?~ OpenApiString
        & enum_ ?~ ["genesis"]

chainPointAtSlotSchema :: Schema
chainPointAtSlotSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties
            .~  [ ("slot_no", Inline slotSchema)
                , ("header_hash", Inline headerHashSchema)
                ]

slotSchema :: Schema
slotSchema =
    mempty
        & type_ ?~ OpenApiInteger
        & minimum_ ?~ 0
        & maximum_ ?~ fromIntegral (maxBound :: Word64)

headerHashSchema :: Schema
headerHashSchema =
    blake2b_256Schema
        & description ?~ "Hash (Blake2b_256) of a block header."

blake2b_256Schema :: Schema
blake2b_256Schema =
    mempty
        & type_ ?~ OpenApiString
        & format ?~ "hex"
        & minLength ?~ (32 * hexCharactersPerByte)
        & maxLength ?~ (32 * hexCharactersPerByte)

hexCharactersPerByte :: Integer
hexCharactersPerByte = 2
