{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Address.States.Features
    ( IsOwned
    , TestFeatures (..)
    , defaultTestFeatures
    )
where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    )
import Cardano.Wallet.Address.Derivation.Shared
    (
    )
import Cardano.Wallet.Address.States.Families
    ( CredFromOf
    , KeyOf
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )

type IsOwned s =
    s
    -> (KeyOf s 'RootK XPrv, Passphrase "encryption")
    -> Address
    -> Maybe (KeyOf s (CredFromOf s) XPrv, Passphrase "encryption")

newtype TestFeatures s = TestFeatures
    { isOwnedTest :: IsOwned s
    }

defaultTestFeatures :: TestFeatures s
defaultTestFeatures =
    TestFeatures
        { isOwnedTest = error "isOwned: not implemented"
        }
