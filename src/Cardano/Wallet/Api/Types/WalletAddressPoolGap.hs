{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Api.Types.WalletAddressPoolGap
    ( WalletAddressPoolGap
    ) where

import Prelude

import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Data.Aeson
    ( FromJSON, ToJSON, Value (Number), parseJSON, toJSON )
import GHC.Generics
    ( Generic )

newtype WalletAddressPoolGap = WalletAddressPoolGap
    { getWalletAddressPoolGap :: AddressPoolGap }
    deriving stock (Generic, Show)
    deriving newtype (Bounded, Enum, Eq, Ord)

instance FromJSON WalletAddressPoolGap where
    parseJSON x = validate . mkAddressPoolGap =<< parseJSON x
        where
            validate = either
                (fail . show)
                (pure . WalletAddressPoolGap)

instance ToJSON WalletAddressPoolGap where
    toJSON = Number
        . fromIntegral
        . getAddressPoolGap
        . getWalletAddressPoolGap
