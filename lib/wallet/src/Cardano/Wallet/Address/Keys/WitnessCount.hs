{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- GHC 8.10.7 cannot figure out the constraint is necessary in
-- toWitnessCount, so we disable the warning.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Wallet.Address.Keys.WitnessCount
    ( toWitnessCountCtx)
    where

import Prelude

import Cardano.Address.Derivation
    ( xpubToBytes )
import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..) )
import Cardano.Wallet.Address.Derivation.Shared
    ( allCosignerStakingKeys )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState, policyXPub )
import Cardano.Wallet.Address.Discovery.Shared
    ( delegationTemplate )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKeyNew )
import Cardano.Wallet.Flavor
    ( FlavorOf
    , IncludingStates
    , WalletFlavor
    , WalletFlavorS (..)
    , WalletFlavors (..)
    , keyFlavor
    )
import Cardano.Wallet.Transaction
    ( WitnessCountCtx (..) )

toWitnessCountCtx
    :: IncludingStates '[ 'IcarusF, 'ShelleyF,  'SharedF] (FlavorOf s)
    => WalletFlavorS s
    -> s
    -> WitnessCountCtx
toWitnessCountCtx ShelleyWallet s = count s
toWitnessCountCtx IcarusWallet s = count s
toWitnessCountCtx SharedWallet s =
    let delegationTemplateM = delegationTemplate s
        stakingKeyHashes =
            maybe [] allCosignerStakingKeys delegationTemplateM
    in  SharedWalletCtx stakingKeyHashes

count :: forall s n k. (s ~ SeqState n k, WalletFlavor s)
    => s -> WitnessCountCtx
count s = case policyXPub s of
    Just key ->
        ShelleyWalletCtx
            $ KeyHash Policy
            $ xpubToBytes
            $ getRawKeyNew (keyFlavor @s) key
    Nothing -> AnyWitnessCountCtx
