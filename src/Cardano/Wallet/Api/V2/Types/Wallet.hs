{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.Wallet where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleRecordOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.V2.Types.WalletAddressPoolGap as T
import qualified Cardano.Wallet.Api.V2.Types.WalletBalance as T
import qualified Cardano.Wallet.Api.V2.Types.WalletDelegation as T
import qualified Cardano.Wallet.Api.V2.Types.WalletId as T
import qualified Cardano.Wallet.Api.V2.Types.WalletName as T
import qualified Cardano.Wallet.Api.V2.Types.WalletPassphraseInfo as T
import qualified Cardano.Wallet.Api.V2.Types.WalletState as T

data Wallet = Wallet
    { _id :: !T.WalletId
    , _addressPoolGap :: !T.WalletAddressPoolGap
    , _balance :: !T.WalletBalance
    , _delegation :: !T.WalletDelegation
    , _name :: !T.WalletName
    , _passphrase :: !T.WalletPassphraseInfo
    , _state :: !T.WalletState
    } deriving (Eq, Generic, Show)

instance FromJSON Wallet where
    parseJSON = genericParseJSON simpleRecordOptions
instance ToJSON Wallet where
    toJSON = genericToJSON simpleRecordOptions
