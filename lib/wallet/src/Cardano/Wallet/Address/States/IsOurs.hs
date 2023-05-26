{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Address.States.IsOurs
    ( isOurAddress
    , isOurRewardAccount) where

import Cardano.Wallet.Address.Derivation
    ( RewardAccount, ToRewardAccount )
import Cardano.Wallet.Address.States.Families
    ( DiscoveryPercentageOf, KeyOf, NetworkOf )
import Cardano.Wallet.Address.States.Features
    ( IsOurs, TestFeatures (..) )
import Cardano.Wallet.Flavor
    ( WalletFlavorS (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId )
import GHC.TypeNats
    ( KnownNat )

import qualified Cardano.Wallet.Address.Derivation.Shelley as Seq
import qualified Cardano.Wallet.Address.Discovery.Random as Rnd
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Discovery.Shared as Sha

isOurAddress
    :: ( HasSNetworkId (NetworkOf s)
       , KnownNat (DiscoveryPercentageOf s)
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
isOurAddress (TestStateModelS f) = isOurAddressTest f

isOurRewardAccount
    :: ToRewardAccount (KeyOf s)
    => WalletFlavorS s
    -> IsOurs s RewardAccount
isOurRewardAccount ByronWallet = Rnd.isOurRewardAccount
isOurRewardAccount IcarusWallet = Seq.isOurRewardAccount
isOurRewardAccount ShelleyWallet = Seq.isOurRewardAccount
isOurRewardAccount SharedWallet = Sha.isOurRewardAccount
isOurRewardAccount BenchByronWallet = Rnd.isOurRewardAccountAnyState
isOurRewardAccount BenchShelleyWallet = Seq.isOurRewardAccountAnyState
isOurRewardAccount (TestStateS f) = isOurRewardAccountTest f
isOurRewardAccount (TestStateModelS f) = isOurRewardAccountTest f
