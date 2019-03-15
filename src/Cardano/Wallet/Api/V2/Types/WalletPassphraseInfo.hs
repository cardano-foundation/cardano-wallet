{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletPassphraseInfo where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleRecordOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import Data.Time.Clock
    ( UTCTime )
import GHC.Generics
    ( Generic )

newtype WalletPassphraseInfo = WalletPassphraseInfo
    { _lastUpdatedAt :: UTCTime
    } deriving (Eq, Generic, Show)

instance FromJSON WalletPassphraseInfo where
    parseJSON = genericParseJSON simpleRecordOptions
instance ToJSON WalletPassphraseInfo where
    toJSON = genericToJSON simpleRecordOptions
