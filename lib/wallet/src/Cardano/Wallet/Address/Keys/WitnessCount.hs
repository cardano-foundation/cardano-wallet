{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- GHC 8.10.7 cannot figure out the constraint is necessary in
-- toWitnessCount, so we disable the warning.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Wallet.Address.Keys.WitnessCount (toWitnessCountCtx)
where

import Prelude

import Cardano.Address.Derivation
    ( xpubToBytes
    )
import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (..)
    , ScriptTemplate (..)
    )
import Cardano.Wallet.Address.Derivation
    ( Role (MutableAccount)
    , deriveAddressPublicKey
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    , policyXPub
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( delegationTemplate
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    , hashVerificationKey
    )
import Cardano.Wallet.Flavor
    ( FlavorOf
    , IncludingStates
    , KeyFlavorS (..)
    , WalletFlavor
    , WalletFlavorS (..)
    , WalletFlavors (..)
    , keyFlavorFromState
    )
import Cardano.Wallet.Transaction
    ( WitnessCountCtx (..)
    )

import qualified Cardano.Address.Script as CA
import qualified Data.Map as Map

toWitnessCountCtx
    :: IncludingStates '[ 'IcarusF, 'ShelleyF, 'SharedF] (FlavorOf s)
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

count
    :: forall s n k
     . (s ~ SeqState n k, WalletFlavor s)
    => s
    -> WitnessCountCtx
count s = case policyXPub s of
    Just key ->
        ShelleyWalletCtx
            $ KeyHash Policy
            $ xpubToBytes
            $ getRawKey (keyFlavorFromState @s) key
    Nothing -> AnyWitnessCountCtx

allCosignerStakingKeys
    :: ScriptTemplate
    -> [KeyHash]
allCosignerStakingKeys (ScriptTemplate xpubs _) =
    map toKeyHash (Map.elems xpubs)
  where
    stakingKey accXPub =
        deriveAddressPublicKey (SharedKey accXPub) MutableAccount minBound
    toKeyHash =
        hashVerificationKey SharedKeyS CA.Delegation . stakingKey
