{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Http.OpenApi
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
    , Schema
    , URL (..)
    )

import Cardano.Wallet.Launch.Cluster.Http.Faucet.OpenApi
    ( faucetDefinitions
    , faucetPaths
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.OpenApi
    ( monitoringDefinitions
    , monitoringPaths
    )
import qualified Data.ByteString.Lazy.Char8 as BL

generateOpenapi3 :: BL.ByteString
generateOpenapi3 = encodePretty apiSchema

apiSchema :: OpenApi
apiSchema :: OpenApi =
    mempty
        & info . title .~ "Cardano Wallet Monitoring API"
        & info . version .~ "0.1.0.0"
        & info . description ?~ "This is the API for the monitoring server"
        & info . license ?~ license'
        & paths .~ (faucetPaths <> monitoringPaths)
        & components . schemas .~ definitions

definitions :: Definitions Schema
definitions = faucetDefinitions <> monitoringDefinitions

license' :: License
license' =
    "Apache 2"
        & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0.html"
