{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
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
    , delegationAddress
    , deriveMultisigPublicKey
    , hashKey
    , liftXPub
    , mkNetworkDiscriminant
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
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as CA
import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                 State
-------------------------------------------------------------------------------}

-- | A state to keep track of script templates, account public keys of other cosigners,
-- | verification keys used and unused fitting in the address pool gap,
-- | and script addresses unused/used with the corresponding indices.
data SharedState (n :: NetworkDiscriminant) k = SharedState
    { shareStateAccountKey :: k 'AccountK XPub
        -- ^ Reward account public key associated with this wallet
    , shareStateDerivationPrefix :: !DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the account key
    , shareStateGap :: !AddressPoolGap
        -- ^ Number of first keys that are used to produce candidate addresses
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
    :: ScriptTemplate
    -> Int
    -> Script KeyHash
replaceCosignersWithVerKeys (ScriptTemplate xpubs scriptTemplate) index =
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
        let ix = toEnum index
            (Just accXPub) = liftXPub <$> Map.lookup c xpubs
            verKey = deriveMultisigPublicKey accXPub ix
        in hashKey verKey

constructAddressFromIx
    :: CA.NetworkTag
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Address
constructAddressFromIx tag pTemplate dTemplate ix =
    let delegationCredential = DelegationFromScript . toScriptHash
        paymentCredential = PaymentFromScript . toScriptHash
        createAddress pScript' dScript' =
            CA.unAddress $
            delegationAddress tag
            (paymentCredential pScript') (delegationCredential dScript')
        dTemplate' = fromMaybe pTemplate dTemplate
        pScript = replaceCosignersWithVerKeys pTemplate (fromEnum ix)
        dScript = replaceCosignersWithVerKeys dTemplate' (fromEnum ix)
    in Address $ createAddress pScript dScript

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
