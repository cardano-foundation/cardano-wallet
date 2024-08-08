{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.HTTP.OpenAPI
    ( generateOpenapi3
    , apiSchema
    , definitions
    ) where

import Prelude

import Control.Lens
    ( (&)
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
    , HasDescription (..)
    , HasInfo (..)
    , HasLicense (license)
    , HasPaths (..)
    , HasSchemas (..)
    , HasTitle (..)
    , HasUrl (..)
    , HasVersion (..)
    , License
    , OpenApi
    , PathItem
    , Schema
    , URL (..)
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
        & components . schemas .~ definitions

definitions :: Definitions Schema
definitions = depositDefinitions

license' :: License
license' =
    "Apache 2"
        & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0.html"

depositPaths :: InsOrdHashMap FilePath PathItem
depositPaths = undefined

depositDefinitions :: Definitions Schema
depositDefinitions = undefined
