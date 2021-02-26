{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in CIP-XXX Multi-signature Wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Script
    (
      keyHashFromAccXPubIx
    , constructAddressFromIx
    , toNetworkTag
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptTemplate (..)
    , toScriptHash
    )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , delegationAddress
    , deriveMultisigForDelegationPublicKey
    , deriveMultisigForPaymentPublicKey
    , hashKey
    , liftXPub
    , mkNetworkDiscriminant
    , paymentAddress
    )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , SoftDerivation
    , WalletKey (..)
    , deriveVerificationKey
    , hashVerificationKey
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Data.Either.Combinators
    ( fromRight', rightToMaybe )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Type.Reflection
    ( Typeable, typeRep )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Data.Map.Strict as Map

keyHashFromAccXPubIx
    :: (SoftDerivation k, WalletKey k)
    => k 'AccountK XPub
    -> Index 'Soft 'ScriptK
    -> KeyHash
keyHashFromAccXPubIx accXPub ix =
    hashVerificationKey $ deriveVerificationKey accXPub ix

replaceCosignersWithVerKeys
    :: CA.Role
    -> ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Script KeyHash
replaceCosignersWithVerKeys role (ScriptTemplate xpubs scriptTemplate) ix =
    replaceCosigner scriptTemplate
  where
    replaceCosigner :: Script Cosigner -> Script KeyHash
    replaceCosigner = \case
        RequireSignatureOf c -> RequireSignatureOf $ toKeyHash c
        RequireAllOf xs      -> RequireAllOf (map replaceCosigner xs)
        RequireAnyOf xs      -> RequireAnyOf (map replaceCosigner xs)
        RequireSomeOf m xs   -> RequireSomeOf m (map replaceCosigner xs)
        ActiveFromSlot s     -> ActiveFromSlot s
        ActiveUntilSlot s    -> ActiveUntilSlot s
    toKeyHash :: Cosigner -> KeyHash
    toKeyHash c =
        let ix' = toEnum (fromEnum ix)
            (Just accXPub) = liftXPub <$> Map.lookup c xpubs
            verKey = deriveMultisigPublicKey accXPub ix'
        in hashKey verKey
    deriveMultisigPublicKey = case role of
        CA.MultisigForPayment -> deriveMultisigForPaymentPublicKey
        CA.MultisigForDelegation -> deriveMultisigForDelegationPublicKey
        _ ->  error "replaceCosignersWithVerKeys is supported only for role=3 and role=4"

toNetworkTag
    :: forall (n :: NetworkDiscriminant). Typeable n => CA.NetworkTag
toNetworkTag =  case tryMainnet of
    Just net -> net
    Nothing -> fromRight' $ mkNetworkDiscriminant 0
  where
    tryMainnet =
        case testEquality (typeRep @n) (typeRep @'Mainnet) of
            Just Refl  -> rightToMaybe $ mkNetworkDiscriminant 1
            Nothing -> Nothing

constructAddressFromIx
    :: forall (n :: NetworkDiscriminant).  Typeable n
    => ScriptTemplate
    -> Maybe ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Address
constructAddressFromIx pTemplate dTemplate ix =
    let delegationCredential = DelegationFromScript . toScriptHash
        paymentCredential = PaymentFromScript . toScriptHash
        tag = toNetworkTag @n
        createBaseAddress pScript' dScript' =
            CA.unAddress $
            delegationAddress tag
            (paymentCredential pScript') (delegationCredential dScript')
        createEnterpriseAddress pScript' =
            CA.unAddress $
            paymentAddress tag
            (paymentCredential pScript')
        pScript =
            replaceCosignersWithVerKeys CA.MultisigForPayment pTemplate ix
        dScript s =
            replaceCosignersWithVerKeys CA.MultisigForDelegation s ix
    in Address $ case dTemplate of
        Just dTemplate' ->
            createBaseAddress pScript (dScript dTemplate')
        Nothing ->
            createEnterpriseAddress pScript
