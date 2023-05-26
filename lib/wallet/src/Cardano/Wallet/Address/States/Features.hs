{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Address.States.Features
    ( IsOwned
    , IsOurs
    , TestFeatures (..)
    , Freedom (..)
    , defaultTestFeatures
    )
    where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), DerivationIndex, RewardAccount )
import Cardano.Wallet.Address.Derivation.Shared
    ()
import Cardano.Wallet.Address.States.Families
    ( CredFromOf, KeyOf )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Void
    ( Void )

type IsOwned s =
    s
    -> (KeyOf s 'RootK XPrv, Passphrase "encryption")
    -> Address
    -> Maybe (KeyOf s (CredFromOf s) XPrv, Passphrase "encryption")

data FeatureF
    = IsOwnedF
    | IsOurAddressF
    | IsOursRewardAccountF

type IsOurs s entity =
    entity
    -> s
    -> (Maybe (NonEmpty DerivationIndex), s)

data Freedom = Full | Model

type family FeatureExists (o :: Freedom) f s where
    FeatureExists 'Full 'IsOwnedF s = IsOwned s
    FeatureExists 'Model 'IsOwnedF s = Void
    FeatureExists 'Full 'IsOurAddressF s = IsOurs s Address
    FeatureExists 'Model 'IsOurAddressF s = IsOurs s Address
    FeatureExists 'Full 'IsOursRewardAccountF s = IsOurs s RewardAccount
    FeatureExists 'Model 'IsOursRewardAccountF s = IsOurs s RewardAccount

data TestFeatures o s = TestFeatures
    { isOwnedTest :: FeatureExists o 'IsOwnedF s
    , isOurAddressTest :: FeatureExists o 'IsOurAddressF s
    , isOurRewardAccountTest :: FeatureExists o 'IsOursRewardAccountF s
    }

defaultTestFeatures :: TestFeatures o s
defaultTestFeatures = TestFeatures
    { isOwnedTest = error "isOwned: not implemented"
    , isOurAddressTest = error "isOursAddress: not implemented"
    , isOurRewardAccountTest = error "isOursRewardAccount: not implemented"
    }
