{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletDelegation where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleRecordOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.V2.Types.WalletDelegationStatus as T

newtype WalletDelegation = WalletDelegation
    { _status :: T.WalletDelegationStatus
    } deriving (Eq, Generic, Show)

instance FromJSON WalletDelegation where
    parseJSON = genericParseJSON simpleRecordOptions
instance ToJSON WalletDelegation where
    toJSON = genericToJSON simpleRecordOptions
