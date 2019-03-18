module Cardano.Wallet.Api.V2.JSON
    ( defaultSumTypeOptions
    , defaultRecordTypeOptions
    ) where

import Prelude

import Data.Aeson
    ( Options
    , SumEncoding (ObjectWithSingleField)
    , camelTo2
    , constructorTagModifier
    , defaultOptions
    , fieldLabelModifier
    , omitNothingFields
    , sumEncoding
    )

defaultSumTypeOptions :: Options
defaultSumTypeOptions = defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , sumEncoding = ObjectWithSingleField }

defaultRecordTypeOptions :: Options
defaultRecordTypeOptions = defaultOptions
    { fieldLabelModifier = drop 1
    , omitNothingFields = True }
