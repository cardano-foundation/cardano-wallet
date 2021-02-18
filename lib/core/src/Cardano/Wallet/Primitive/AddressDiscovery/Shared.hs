{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in CIP-XXX Multi-signature Wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Shared
    (
    -- ** State
      SharedState (..)
    , unsafeSharedState
    , newSharedState
    , extendSharedState
    , addCosignerAccXPub
    , purposeCIPXXX
    , isShared
    , keyHashFromAccXPubIx
    , constructAddressFromIx
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , foldScript
    , toScriptHash
    , validateScriptTemplate
    )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
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
    , DerivationPrefix (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , SoftDerivation
    , WalletKey (..)
    , deriveVerificationKey
    , hashVerificationKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( coinTypeAda )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Control.DeepSeq
    ( NFData )
import Data.Either
    ( isRight )
import Data.Map.Strict
    ( Map )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as CA
import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                Shared State
-------------------------------------------------------------------------------}

-- | Shared wallets are a new kind of wallet owned by one or more co-signers.
-- | In this type of wallet, addresses are defined by two monetary scripts
-- | (one for ownership of assets, one for ownership of stake).
--
-- | Shared wallet is instantiated with an account public key, derivation path needed to
-- | recreate it,  and a complete set of information that is needed to discover shared
-- | addresses. In order to enable the discovery the following is needed:
--
-- | - a way to determine what range of indices are checked on the
-- |   ledger. Mechanism of address pool gap, adopted for sequential wallets,
-- |   is used. The idea is to track all indices starting from 0 and up to N.
-- |   N is variable as addresses are discovered (and marked as Used in consequence).
-- |   The pool of addresses is enlarged in such way that the number of consecutive
-- |   Unsed addresses equals to address pool gap.
--
-- | - script template for payment credential contains information about all collected
-- |   account public keys for all parties engaged, here named cosigners. Also the skeleton
-- |   determining script structure is provided.  In this sense script is predetermined from
-- |   the beginning and can variate only in verification key part. The places where a specific
-- |   cosigner is present is to be replaced with the derived verfication keys from the cosigner's
-- |   account public key and the index that was chosen. The index for derivation is the same
-- |   for each cosigner's derivation. Moreover, script template could be translated into
-- |   a corresponding script only when account public keys for all cosigners specified in script
-- |   are collected. Hence, this module provides methods for updating script templates that handles
-- |   the act of collecting account public key for cosigner. Finally, verification keys are derived
-- |   using role=3 for payment credential.
--
-- | - optional script template for delegation credential contains all information as in case of
-- |   the script template for payment credential. One different is that the verification keys are derived
-- |   using role=4 for delegation credential.
--
-- | When both script are present, the base address (with both credential) is expected to be discovered.
-- | When script template for delegation credential is missing then enterprise address (non-stakable) is
-- | expected.
data SharedState (n :: NetworkDiscriminant) k = SharedState
    { shareStateAccountKey :: k 'AccountK XPub
        -- ^ Account public key associated with this shared wallet
    , shareStateDerivationPrefix :: !DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the account key
    , shareStateGap :: !AddressPoolGap
        -- ^ Number of consecutive unused addresses available at all time.
    , shareStatePaymentTemplate :: !ScriptTemplate
        -- ^ Script template together with a map of account keys and cosigners
        -- for payment credential
    , shareStateDelegationTemplate :: !(Maybe ScriptTemplate)
        -- ^ Script template together with a map of account keys and cosigners
        -- for staking credential. If not specified then the same template as for
        -- payment is used
    , shareStateOurAddresses :: !(Map Address (Index 'Soft 'ScriptK, AddressState))
        -- ^ Our addresses meaning the addresses containing our verification key hashes
        -- (if discovered) and represented here by corresponding indices of verification
        -- keys present in the given address.
    }
    deriving stock (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    ) => Show (SharedState n k)

deriving instance
    ( Eq (k 'AccountK XPub)
    ) => Eq (SharedState n k)

instance
    ( NFData (k 'AccountK XPub)
    ) => NFData (SharedState n k)

-- | Purpose for shared wallets is a constant set to 45' (or 0x8000002D) following the original
-- CIP-XXX Multi-signature Wallets.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIPXXX :: Index 'Hardened 'PurposeK
purposeCIPXXX = toEnum 0x8000002D

-- | Create a SharedState from the all needed ingredients.
-- There is no validation and it is unsafe way.
unsafeSharedState
    :: forall (n :: NetworkDiscriminant) k. k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Map Address (Index 'Soft 'ScriptK, AddressState)
    -> SharedState n k
unsafeSharedState accXPub accIx g pTemplate dTemplate ourAdresesses =
    SharedState
    { shareStateAccountKey = accXPub
    , shareStateDerivationPrefix = DerivationPrefix ( purposeCIPXXX, coinTypeAda, accIx )
    , shareStateGap = g
    , shareStatePaymentTemplate = pTemplate
    , shareStateDelegationTemplate = dTemplate
    , shareStateOurAddresses = ourAdresesses
    }

-- | Create a new SharedState.
newSharedState
    :: forall (n :: NetworkDiscriminant) k. k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
newSharedState accXPub accIx g pTemplate dTemplate =
    extendSharedState (toEnum 0) $
    unsafeSharedState accXPub accIx g pTemplate dTemplate Map.empty

replaceCosignersWithVerKeys
    :: Role
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
        MultisigForPayment -> deriveMultisigForPaymentPublicKey
        MultisigForDelegation -> deriveMultisigForDelegationPublicKey
        _ ->  error "replaceCosignersWithVerKeys is supported only for role=3 and role=4"

constructAddressFromIx
    :: CA.NetworkTag
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Address
constructAddressFromIx tag pTemplate dTemplate ix =
    let delegationCredential = DelegationFromScript . toScriptHash
        paymentCredential = PaymentFromScript . toScriptHash
        createBaseAddress pScript' dScript' =
            CA.unAddress $
            delegationAddress tag
            (paymentCredential pScript') (delegationCredential dScript')
        createEnterpriseAddress pScript' =
            CA.unAddress $
            paymentAddress tag
            (paymentCredential pScript')
        pScript =
            replaceCosignersWithVerKeys MultisigForPayment pTemplate ix
        dScript s =
            replaceCosignersWithVerKeys MultisigForDelegation s ix
    in Address $ case dTemplate of
        Just dTemplate' ->
            createBaseAddress pScript (dScript dTemplate')
        Nothing ->
            createEnterpriseAddress pScript

addNotDiscoveredAddressToMap
    :: CA.NetworkTag
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Map Address (Index 'Soft 'ScriptK, AddressState)
    -> Map Address (Index 'Soft 'ScriptK, AddressState)
addNotDiscoveredAddressToMap tag pTemplate dTemplate ix currentMap =
    let address = constructAddressFromIx tag pTemplate dTemplate ix
    in Map.insert address (ix, Unused) currentMap

templatesComplete
    :: ScriptTemplate
    -> Maybe ScriptTemplate
    -> Bool
templatesComplete pTemplate dTemplate =
    isRight (validateScriptTemplate RequiredValidation pTemplate) &&
    case dTemplate of
        Nothing -> True
        Just dTemplate' ->
            isRight (validateScriptTemplate RequiredValidation dTemplate')

keyHashFromAccXPubIx
    :: (SoftDerivation k, WalletKey k)
    => k 'AccountK XPub
    -> Index 'Soft 'ScriptK
    -> KeyHash
keyHashFromAccXPubIx accXPub ix =
    hashVerificationKey $ deriveVerificationKey accXPub ix

-- | The extension to the SharedState pool gap is done by adding next adjacent
-- indices and their corresponding public keys, marking them as Unused.
-- The number of added entries is determined by pool gap, and the start index is
-- the next to the the specified index.
extendSharedState
    :: forall (n :: NetworkDiscriminant) k. Index 'Soft 'ScriptK
    -> SharedState n k
    -> SharedState n k
extendSharedState ix state
    | isOnEdge  = state { shareStateOurAddresses = nextAddresses }
    | otherwise = state
  where
    edge = Map.size (shareStateOurAddresses state)
    startIx = if edge == 0 then (toEnum 0) else succ ix
    isOnEdge = edge - fromEnum ix <= fromEnum (shareStateGap state)
    nextIndices =
        L.take (fromEnum $ shareStateGap state) $ L.iterate succ startIx
    (Right tag) = mkNetworkDiscriminant 1
    nextAddresses
      | ix == maxBound = mempty
      | otherwise =
            let pTemplate = shareStatePaymentTemplate state
                dTemplate = shareStateDelegationTemplate state
            in if templatesComplete pTemplate dTemplate then
                foldr (addNotDiscoveredAddressToMap tag pTemplate dTemplate)
                (shareStateOurAddresses state) nextIndices
            else
                mempty

-- | The cosigner with his account public key is done per template.
-- For every template the script is checked if the cosigner is present.
-- If yes, then key is inserted. If the key is already present it is going to be
-- updated. If there is no cosigner present is the script then the cosigner -
-- account public key map is not changed.
addCosignerAccXPub
    :: forall (n :: NetworkDiscriminant) k. WalletKey k
    => k 'AccountK XPub
    -> Cosigner
    -> SharedState n k
    -> SharedState n k
addCosignerAccXPub accXPub cosigner (SharedState ourAccXPub prefix g pT dT ourAdresesses) =
    let updateScriptTemplate sc@(ScriptTemplate cosignerMap script') =
            if cosigner `elem` retrieveAllCosigners script' then
                ScriptTemplate (Map.insert cosigner (getRawKey accXPub) cosignerMap) script'
            else
                sc
    in SharedState
       { shareStateAccountKey = ourAccXPub
       , shareStateDerivationPrefix = prefix
       , shareStateGap = g
       , shareStatePaymentTemplate = updateScriptTemplate pT
       , shareStateDelegationTemplate = updateScriptTemplate <$> dT
       , shareStateOurAddresses = ourAdresesses
       }

retrieveAllCosigners :: Script Cosigner -> [Cosigner]
retrieveAllCosigners = foldScript (:) []

isShared
    :: (SoftDerivation k, WalletKey k)
    => Address
    -> SharedState n k
    -> (Maybe (Index 'Soft 'ScriptK, KeyHash), SharedState n k)
isShared address shared@(SharedState !accXPub !prefix !g !pT !dT !ourAddresses) =
    case Map.lookup address ourAddresses of
        Nothing -> (Nothing, shared)
        Just (ix, _) ->
            let ourAddresses' = Map.insert address (ix,Used) ourAddresses
            in ( Just (ix, keyHashFromAccXPubIx accXPub ix)
               , extendSharedState ix (SharedState accXPub prefix g pT dT ourAddresses') )
