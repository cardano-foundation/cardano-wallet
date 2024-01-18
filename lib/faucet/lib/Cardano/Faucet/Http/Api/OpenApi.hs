{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet.Http.Api.OpenApi
    ( FaucetApi
    , Mnemonic
    , MnemonicIndex
    , IndexedMnemonic
    , FaucetAddress
    , AddressIndex
    , IndexedAddress
    , generateOpenapi3
    ) where

import Prelude

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.HashSet.InsOrd as IOHS

import Cardano.Faucet.Http.Api.OrphanInstances
    ()

import Cardano.Address
    ( NetworkTag
    )
import Cardano.Faucet.Http.Api.Servant
    ( FaucetApi
    )
import Cardano.Faucet.Http.Api.Utils
    ( typeName
    , typeRef
    )
import Cardano.Faucet.Types
    ( AddressIndex
    , AddressStyle
    , FaucetAddress
    , IndexedAddress
    , IndexedMnemonic
    , Mnemonic
    , MnemonicIndex
    )
import Control.Lens
    ( at
    , (&)
    , (.~)
    , (?~)
    )
import Data.Aeson.Encode.Pretty
    ( encodePretty
    )
import Data.Data
    ( Proxy (..)
    )
import Data.OpenApi
    ( HasComponents (components)
    , HasContent (content)
    , HasDescription (description)
    , HasGet (get)
    , HasIn (in_)
    , HasInfo (info)
    , HasLicense (license)
    , HasName (name)
    , HasParameters (parameters)
    , HasPaths (paths)
    , HasSchema (schema)
    , HasSchemas (schemas)
    , HasSummary (summary)
    , HasTags (tags)
    , HasTitle (title)
    , HasUrl (url)
    , HasVersion (version)
    , OpenApi
    , ParamLocation (ParamPath)
    , Referenced (Inline, Ref)
    , URL (URL)
    , _Inline
    , toParamSchema
    , toSchema
    )
import Network.HTTP.Media
    ( MediaType
    )
import Servant.OpenApi
    ( HasOpenApi (toOpenApi)
    )
import System.Directory
    ( getCurrentDirectory
    )
import System.FilePath
    ( (</>)
    )

--------------------------------------------------------------------------------
-- OpenAPI generation ----------------------------------------------------------

generateOpenapi3 :: IO ()
generateOpenapi3 = do
    currentDirectory <- getCurrentDirectory
    BSL8.writeFile (currentDirectory </> "openapi.json") (encodePretty api)
  where
    jsonMediaType :: MediaType
    jsonMediaType = "application/json"

    api :: OpenApi = toOpenApi (Proxy @FaucetApi)
        & info.title .~ "Faucet API"
        & info.version .~ "0.1"
        & info.description ?~
            "This API exposes functionality to manage \
            \assets within the Cardano test cluster:\n \
            \  - Get mnemonics from which various types of wallets \
            \could be restored such that they contain test funds \
            \(ADA, custom assets, rewards).\n \
            \  - Track already used mnemonics;"
        & info.license ?~ ("Apache 2"
            & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0.html")
        & paths .~ IOHM.fromList
            [ ("/mnemonics/{minIndex}/{maxIndex}", mempty
                & get ?~ ( mempty
                    & tags .~ IOHS.fromList ["Mnemonics"]
                    & summary ?~
                        "Collection of mnemonics \
                        \viewed by index range (inclusive)"
                    & at 200 ?~ (
                        "Ok" & _Inline.content.at jsonMediaType ?~ ( mempty
                                & schema ?~ Ref (typeRef @[IndexedMnemonic])
                            )
                        )
                    )
                & parameters .~
                    [ Inline $ mempty
                        & in_ .~ ParamPath
                        & name .~ "minIndex"
                        & schema ?~ Ref (typeRef @MnemonicIndex)
                    , Inline $ mempty
                        & in_ .~ ParamPath
                        & name .~ "maxIndex"
                        & schema ?~ Ref (typeRef @MnemonicIndex)
                    ]
              )
            , ("/mnemonics/{index}", mempty
                & get ?~ ( mempty
                    & tags .~ IOHS.fromList ["Mnemonics"]
                    & summary ?~ "Mnemonic by index"
                    & at 200 ?~ (
                        "Ok" & _Inline.content.at jsonMediaType ?~ ( mempty
                                & schema ?~ Ref (typeRef @Mnemonic)
                            )
                        )
                    )
                & parameters .~
                    [ Inline $ mempty
                        & in_ .~ ParamPath
                        & name .~ "index"
                        & schema ?~ Ref (typeRef @MnemonicIndex)
                    ]
              )
            , ("/mnemonics/{index}/addresses/{style}/{networkTag}/{minIndex}/{maxIndex}", mempty
                & get ?~ ( mempty
                    & tags .~ IOHS.fromList ["Addresses"]
                    & summary ?~
                        "Collection of mnemonic addresses \
                        \viewed by a range of indexes"
                    & parameters .~
                        [ Inline $ mempty
                            & in_ .~ ParamPath
                            & name .~ "index"
                            & schema ?~ Ref (typeRef @MnemonicIndex)
                        , Inline $ mempty
                            & in_ .~ ParamPath
                            & name .~ "style"
                            & schema ?~ Ref (typeRef @AddressStyle)
                        , Inline $ mempty
                            & in_ .~ ParamPath
                            & name .~ "networkTag"
                            & schema ?~ Ref (typeRef @NetworkTag)
                        , Inline $ mempty
                            & in_ .~ ParamPath
                            & name .~ "minIndex"
                            & schema ?~ Ref (typeRef @AddressIndex)
                        , Inline $ mempty
                            & in_ .~ ParamPath
                            & name .~ "maxIndex"
                            & schema ?~ Ref (typeRef @AddressIndex)
                        ]
                    & at 200 ?~ (
                        "Ok" & _Inline.content.at jsonMediaType ?~ ( mempty
                                & schema ?~ Ref (typeRef @[IndexedAddress])
                            )
                        )
                    )
              )
            ]
        & components . schemas .~ IOHM.fromList
            [ (typeName @[IndexedMnemonic], toSchema (Proxy @[IndexedMnemonic]))
            , (typeName @IndexedMnemonic, toSchema (Proxy @IndexedMnemonic))
            , (typeName @MnemonicIndex, toSchema (Proxy @MnemonicIndex))
            , (typeName @Mnemonic, toSchema (Proxy @Mnemonic))
            , (typeName @NetworkTag, toParamSchema (Proxy @NetworkTag))
            , (typeName @AddressStyle, toSchema (Proxy @AddressStyle))
            , (typeName @[IndexedAddress], toSchema (Proxy @[IndexedAddress]))
            , (typeName @IndexedAddress, toSchema (Proxy @IndexedAddress))
            , (typeName @AddressIndex, toSchema (Proxy @AddressIndex))
            , (typeName @FaucetAddress, toSchema (Proxy @FaucetAddress))
            ]
