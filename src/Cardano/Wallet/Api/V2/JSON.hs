module Cardano.Wallet.Api.V2.JSON
    ( defaultSumTypeOptions
    , defaultRecordTypeOptions
    ) where

import Prelude

import Data.Aeson
    ( Options
    , camelTo2
    , constructorTagModifier
    , defaultOptions
    , fieldLabelModifier
    , omitNothingFields
    , tagSingleConstructors
    )

defaultSumTypeOptions :: Options
defaultSumTypeOptions = defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , tagSingleConstructors = True }

defaultRecordTypeOptions :: Options
defaultRecordTypeOptions = defaultOptions
    { fieldLabelModifier = drop 1
    , omitNothingFields = True }
