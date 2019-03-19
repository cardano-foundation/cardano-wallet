{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.Types.Wallet where

import Prelude

import Cardano.Wallet.Api.JSON
    ( defaultRecordTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.Types.WalletAddressPoolGap as T
import qualified Cardano.Wallet.Api.Types.WalletBalance as T
import qualified Cardano.Wallet.Api.Types.WalletDelegation as T
import qualified Cardano.Wallet.Api.Types.WalletId as T
import qualified Cardano.Wallet.Api.Types.WalletName as T
import qualified Cardano.Wallet.Api.Types.WalletPassphraseInfo as T
import qualified Cardano.Wallet.Api.Types.WalletState as T

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
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Wallet where
    toJSON = genericToJSON defaultRecordTypeOptions
