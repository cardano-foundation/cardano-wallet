module Cardano.Wallet.Api.V2.JSON
    ( simpleEnumOptions
    , simpleRecordOptions
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

simpleEnumOptions :: Options
simpleEnumOptions = defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , sumEncoding = ObjectWithSingleField }

simpleRecordOptions :: Options
simpleRecordOptions = defaultOptions
    { fieldLabelModifier = drop 1
    , omitNothingFields = True }
