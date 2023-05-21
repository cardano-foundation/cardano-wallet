{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Address.States.Test.AddressesWithCreds
    ( isOwned
    )
where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Address.Derivation
    ( Depth (CredFromKeyK, RootK)
    , HardDerivation (deriveAccountPrivateKey, deriveAddressPrivateKey)
    , Role (UtxoExternal)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.States.Test.Types
    ( DummyStateAddressesWithCreds )
import Cardano.Wallet.Flavor
    ( TestState (TestState) )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )

import qualified Data.Map as Map

isOwned
    :: TestState DummyStateAddressesWithCreds ShelleyKey
    -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> Address
    -> Maybe (ShelleyKey 'CredFromKeyK XPrv, Passphrase "encryption")
isOwned (TestState m) (rootK, pwd) addr = do
    ix <- Map.lookup addr m
    let accXPrv = deriveAccountPrivateKey pwd rootK minBound
        addrXPrv = deriveAddressPrivateKey pwd accXPrv UtxoExternal ix
    return (addrXPrv, pwd)
