{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Address.States.IsOurs
    ( isOurAddress
    ) where

import Cardano.Wallet.Address.States.Families
    ( DiscoveryPercentageOf, NetworkOf )
import Cardano.Wallet.Address.States.Features
    ( IsOurs, TestFeatures (..) )
import Cardano.Wallet.Flavor
    ( Excluding
    , FlavorOf
    , WalletFlavorS (..)
    , WalletFlavors (TestStateModelF)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId )
import GHC.TypeNats
    ( KnownNat )

import qualified Cardano.Wallet.Address.Discovery.Random as Rnd
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Discovery.Shared as Sha

isOurAddress
    :: ( HasSNetworkId (NetworkOf s)
       , KnownNat (DiscoveryPercentageOf s)
       , Excluding '[ 'TestStateModelF] (FlavorOf s)
       )
    => WalletFlavorS s
    -> IsOurs s Address
isOurAddress ByronWallet = Rnd.isOurAddress
isOurAddress IcarusWallet = Seq.isOurAddress
isOurAddress ShelleyWallet = Seq.isOurAddress
isOurAddress SharedWallet = Sha.isOurAddress
isOurAddress BenchByronWallet = Rnd.isOurAddressAnyState
isOurAddress BenchShelleyWallet = Seq.isOurAddressAnyState
isOurAddress (TestStateS f) = isOurAddressTest f
