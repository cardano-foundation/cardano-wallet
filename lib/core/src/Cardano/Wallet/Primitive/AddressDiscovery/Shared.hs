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

module Cardano.Wallet.Primitive.AddressDiscovery.Shared
    (
    -- ** State
      SharedState (..)
    , unsafeSharedState
    , newSharedState
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
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , SoftDerivation
    , WalletKey (..)
    , deriveVerificationKey
    , hashVerificationKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( coinTypeAda )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap, unsafePaymentKeyFingerprint )
import Cardano.Wallet.Primitive.Types
    ( invariant )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Control.DeepSeq
    ( NFData )
import Data.Bifunctor
    ( first )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Shelley as CA
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
    { shareStateDerivationPrefix :: !DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the account key
    , shareStatePaymentTemplate :: !ScriptTemplate
        -- ^ Script template together with a map of account keys and cosigners
        -- for payment credential
    , shareStateDelegationTemplate :: !(Maybe ScriptTemplate)
        -- ^ Script template together with a map of account keys and cosigners
        -- for staking credential. If not specified then the same template as for
        -- payment is used
    , shareStatePool :: !(AddressPool k)
        -- ^ Address pool tracking the shared addresses. Co-owning is based on
        -- payment credential only
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
    :: forall (n :: NetworkDiscriminant) k. Index 'Hardened 'AccountK
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> AddressPool k
    -> SharedState n k
unsafeSharedState accIx pTemplate dTemplate pool =
    SharedState
    { shareStateDerivationPrefix = DerivationPrefix ( purposeCIPXXX, coinTypeAda, accIx )
    , shareStatePaymentTemplate = pTemplate
    , shareStateDelegationTemplate = dTemplate
    , shareStatePool = pool
    }

-- | Create a new SharedState.
newSharedState
    :: forall (n :: NetworkDiscriminant) k.
    ( MkKeyFingerprint k Address, SoftDerivation k)
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
newSharedState accXPub accIx g pTemplate dTemplate =
    let pool = mkAddressPool @n accXPub g [] pTemplate dTemplate
    in unsafeSharedState accIx pTemplate dTemplate pool

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
            replaceCosignersWithVerKeys CA.MultisigForPayment pTemplate ix
        dScript s =
            replaceCosignersWithVerKeys CA.MultisigForDelegation s ix
    in Address $ case dTemplate of
        Just dTemplate' ->
            createBaseAddress pScript (dScript dTemplate')
        Nothing ->
            createEnterpriseAddress pScript

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
addCosignerAccXPub accXPub cosigner (SharedState prefix pT dT pool) =
    let updateScriptTemplate sc@(ScriptTemplate cosignerMap script') =
            if cosigner `elem` retrieveAllCosigners script' then
                ScriptTemplate (Map.insert cosigner (getRawKey accXPub) cosignerMap) script'
            else
                sc
    in SharedState
       { shareStateDerivationPrefix = prefix
       , shareStatePaymentTemplate = updateScriptTemplate pT
       , shareStateDelegationTemplate = updateScriptTemplate <$> dT
       , shareStatePool = pool
       }

retrieveAllCosigners :: Script Cosigner -> [Cosigner]
retrieveAllCosigners = foldScript (:) []

isShared
    :: forall (n :: NetworkDiscriminant) k.
       (SoftDerivation k, WalletKey k, MkKeyFingerprint k Address)
    => Address
    -> SharedState n k
    -> (Maybe (Index 'Soft 'ScriptK, KeyHash), SharedState n k)
isShared addr shared@(SharedState !prefix !pT !dT !pool) =
    if templatesComplete pT dT then
        case scriptIxM of
            Nothing ->
                (Nothing, shared)
            Just ix ->
                ( Just (ix, keyHashFromAccXPubIx (accountPubKey pool) ix)
                , SharedState prefix pT dT pool')
    else
       (Nothing, shared)
  where
    (scriptIxM, pool') = lookupAddress @n (const Used) addr pool pT dT

data AddressPool (key :: Depth -> * -> *) = AddressPool
    { accountPubKey
        :: !(key 'AccountK XPub)
        -- ^ Corresponding key for the pool (a pool is tied to only one account)
    , gap
        :: !AddressPoolGap
        -- ^ Number of consecutive unused addresses available at all time.
    , indexedKeys
        :: !(Map
                (KeyFingerprint "payment" key)
                (Index 'Soft 'ScriptK, AddressState)
            )
    } deriving (Generic)

deriving instance (Show (key 'AccountK XPub))
    => Show (AddressPool key)

deriving instance (Eq (key 'AccountK XPub))
    => Eq (AddressPool key)

instance (NFData (key 'AccountK XPub))
    => NFData (AddressPool key)

mkAddressPool
    :: forall (n :: NetworkDiscriminant) k.
        ( MkKeyFingerprint k Address
        , SoftDerivation k
        )
    => k 'AccountK XPub
    -> AddressPoolGap
    -> [(Address, AddressState)]
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> AddressPool k
mkAddressPool key g addrs pTemplate dTemplate = AddressPool
    { accountPubKey = key
    , gap = g
    , indexedKeys =
            if templatesComplete pTemplate dTemplate then
                mconcat
                [ Map.fromList $ zipWith (\(addr, status) ix -> (addr, (ix, status)))
                  (first (unsafePaymentKeyFingerprint @k) <$> addrs)
                  [minBound..maxBound]
                , nextAddresses @n
                  key
                  (getAddressPoolGap g)
                  minBound
                  pTemplate
                  dTemplate
                ]
            else
                Map.empty
    }

nextAddresses
    :: forall (n :: NetworkDiscriminant) k.
        ( MkKeyFingerprint k Address
        )
    => k 'AccountK XPub
    -> Word32
    -> Index 'Soft 'ScriptK
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Map (KeyFingerprint "payment" k) (Index 'Soft 'ScriptK, AddressState)
nextAddresses _ !g !fromIx !pTemplate !dTemplate =
    [fromIx .. min maxBound toIx]
        & map (\ix -> (newPaymentKey ix, (ix, Unused)))
        & Map.fromList
  where
    toIx = invariant
        "nextAddresses: toIx should be greater than fromIx"
        (toEnum $ fromEnum fromIx + fromEnum g - 1)
        (>= fromIx)
    (Right tag) = mkNetworkDiscriminant 1
    address ix = constructAddressFromIx tag pTemplate dTemplate ix
    newPaymentKey ix = unsafePaymentKeyFingerprint @k (address ix)

lookupAddress
    :: forall (n :: NetworkDiscriminant) k.
        ( MkKeyFingerprint k Address
        , SoftDerivation k
        )
    => (AddressState -> AddressState)
    -> Address
    -> AddressPool k
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> (Maybe (Index 'Soft 'ScriptK), AddressPool k)
lookupAddress alterSt !target !pool !pTemplate !dTemplate =
    case paymentKeyFingerprint @k target of
        Left _ ->
            (Nothing, pool)
        Right fingerprint ->
            case Map.alterF lookupF fingerprint (indexedKeys pool) of
                (Just ix, keys') ->
                    ( Just ix
                    , extendAddressPool @n ix (pool { indexedKeys = keys'}) pTemplate dTemplate
                    )
                (Nothing, _) ->
                    ( Nothing
                    , pool
                    )
  where
    lookupF = \case
        Nothing -> (Nothing, Nothing)
        Just (ix, st) -> (Just ix, Just (ix, alterSt st))

extendAddressPool
    :: forall (n :: NetworkDiscriminant) k.
        ( MkKeyFingerprint k Address
        )
    => Index 'Soft 'ScriptK
    -> AddressPool k
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> AddressPool k
extendAddressPool !ix !pool !pTemplate !dTemplate
    | isOnEdge  = pool { indexedKeys = indexedKeys pool <> next }
    | otherwise = pool
  where
    edge = Map.size (indexedKeys pool)
    isOnEdge = edge - fromEnum ix <= fromEnum (gap pool)
    next = if ix == maxBound then mempty else nextAddresses @n
        (accountPubKey pool)
        (getAddressPoolGap $ gap pool)
        (succ ix)
        pTemplate
        dTemplate
