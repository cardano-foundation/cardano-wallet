{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in BIP-0044.

module Cardano.Wallet.Primitive.AddressDiscovery.Shared
    (
    -- ** State
      SharedState (..)
    , unsafeSharedState
    , newSharedState
    , extendSharedState
    , addCosignerAccXPub
    , purposeCIPXXX
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
    , deriveAddressPublicKey
    , deriveMultisigPublicKey
    , getKey
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
import Control.Monad
    ( mapM )
import Data.Either
    ( isRight )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isNothing )
import Data.Word
    ( Word8 )
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
    , shareStateIndexedKeyHashes
        :: !(Map KeyHash (Index 'Soft 'ScriptK, AddressState))
        -- ^ verification key hashes belonging to the shared wallet
    , shareStateOurAddresses :: !(Map Address (Index 'Soft 'ScriptK, AddressState))
        -- ^ Our addresses meaning the addresses containing our verification key hashes
        -- represented here by corresponding indices of verification keys,
        -- irrespective of being discovered or not
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
    )
    => NFData (SharedState n k)

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
    -> Map KeyHash (Index 'Soft 'ScriptK, AddressState)
    -> Map Address (Index 'Soft 'ScriptK, AddressState)
    -> SharedState n k
unsafeSharedState accXPub accIx g pTemplate dTemplate vkPoolMap ourAdresesses =
    SharedState
    { shareStateAccountKey = accXPub
    , shareStateDerivationPrefix = DerivationPrefix ( purposeCIPXXX, coinTypeAda, accIx )
    , shareStateGap = g
    , shareStatePaymentTemplate = pTemplate
    , shareStateDelegationTemplate = dTemplate
    , shareStateIndexedKeyHashes = vkPoolMap
    , shareStateOurAddresses = ourAdresesses
    }

-- | Create a new SharedState.
newSharedState
    :: forall (n :: NetworkDiscriminant) k.
       (SoftDerivation k, WalletKey k)
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
newSharedState accXPub accIx g pTemplate dTemplate =
    let startVerKeyMap =
            Map.fromList $ map (createPristineMapEntry accXPub) $
            L.take (fromEnum g) $ L.iterate succ minBound
    in unsafeSharedState accXPub accIx g pTemplate dTemplate startVerKeyMap Map.empty

createPristineMapEntry
    :: (SoftDerivation k, WalletKey k)
    => k 'AccountK XPub
    -> Index 'Soft 'ScriptK
    -> (KeyHash, (Index 'Soft 'ScriptK, AddressState))
createPristineMapEntry accXPub ix =
    ( hashVerificationKey $ deriveVerificationKey accXPub ix
    , (ix, Unused) )

-- | The extension to the SharedState pool gap is done by adding next adjacent
-- indices and their corresponding public keys, marking them as Unused.
-- The number of added entries is determined by pool gap, and the start index is
-- the next to the the specified index.
extendSharedState
    :: forall (n :: NetworkDiscriminant) k.
       (SoftDerivation k, WalletKey k)
    => Index 'Soft 'ScriptK
    -> SharedState n k
    -> SharedState n k
extendSharedState ix state
    | isOnEdge  = state { shareStateIndexedKeyHashes =
                          Map.union (shareStateIndexedKeyHashes state) next }
    | otherwise = state
  where
    edge = Map.size (shareStateIndexedKeyHashes state)
    isOnEdge = edge - fromEnum ix <= fromEnum (shareStateGap state)
    next
      | ix == maxBound = mempty
      | otherwise =
            Map.fromList $ map (createPristineMapEntry (shareStateAccountKey state)) $
            L.take (fromEnum $ shareStateGap state) $ L.iterate succ (succ ix)

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
addCosignerAccXPub accXPub cosigner (SharedState ourAccXPub prefix g pT dT vkPoolMap ourAdresesses) =
    let allCosignersInScript = foldScript (:) []
        updateScriptTemplate sc@(ScriptTemplate cosignerMap script') =
            if cosigner `elem` allCosignersInScript script' then
                ScriptTemplate (Map.insert cosigner (getRawKey accXPub) cosignerMap) script'
            else
                sc
    in SharedState
       { shareStateAccountKey = ourAccXPub
       , shareStateDerivationPrefix = prefix
       , shareStateGap = g
       , shareStatePaymentTemplate = updateScriptTemplate pT
       , shareStateDelegationTemplate = updateScriptTemplate <$> dT
       , shareStateIndexedKeyHashes = vkPoolMap
       , shareStateOurAddresses = ourAdresesses
       }



{--

-- | Construct a SharedState for a wallet from public account key and its corresponding index,
-- script templates for both staking and spending, and the number of keys used for
-- generating the corresponding address candidates.
mkSharedState
    :: forall (n :: NetworkDiscriminant) k. WalletKey k
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> AddressPoolGap
    -> Maybe (SharedState n k)
mkSharedState accXPub accIx spendingTemplate stakingTemplate g =
    let
        prefix =
            DerivationPrefix ( purposeCIPXXX, coinTypeAda, accIx )
        accXPub' = liftXPub $ getRawKey accXPub
        rewardXPub =
            getKey $ deriveAddressPublicKey accXPub' Stake minBound
        addressesToFollow = case (spendingTemplate, stakingTemplate) of
            (Just template', Nothing) ->
                if (isRight $ validateScriptTemplate RecommendedValidation template') then
                    generateAddressCombination template' template' keyNum
                else
                    []
            (Just template1', Just template2') ->
                if (isRight (validateScriptTemplate RecommendedValidation template1') &&
                    isRight (validateScriptTemplate RecommendedValidation template2')) then
                    generateAddressCombination template1' template2' keyNum
                else
                    []
    in
        Just $ SharedState accXPub prefix keyNum spendingTemplate stakingTemplate addressesToFollow

replaceCosignersWithVerKeys
    :: ScriptTemplate
    -> Map Cosigner Word8
    -> Maybe (Script KeyHash)
replaceCosignersWithVerKeys (ScriptTemplate xpubs scriptTemplate) indices =
    replaceCosigner scriptTemplate
  where
    replaceCosigner :: Script Cosigner -> Maybe (Script KeyHash)
    replaceCosigner = \case
        RequireSignatureOf c -> RequireSignatureOf <$> toKeyHash c
        RequireAllOf xs      -> RequireAllOf <$> mapM replaceCosigner xs
        RequireAnyOf xs      -> RequireAnyOf <$> mapM replaceCosigner xs
        RequireSomeOf m xs   -> RequireSomeOf m <$> mapM replaceCosigner xs
        ActiveFromSlot s     -> pure $ ActiveFromSlot s
        ActiveUntilSlot s    -> pure $ ActiveUntilSlot s

    toKeyHash :: Cosigner -> Maybe KeyHash
    toKeyHash c =
        let ix = toEnum . fromIntegral <$> Map.lookup c indices
            accXPub = liftXPub <$> Map.lookup c xpubs
            verKey = deriveMultisigPublicKey <$> accXPub <*> ix
        in hashKey <$> verKey

generateAddressCombination
    :: ScriptTemplate
    -> XPub
    -> KeyNumberScope
    -> [Address]
generateAddressCombination st@(ScriptTemplate xpubs _) stakeXPub (KeyNumberScope num) =
    concatMap tryCreateAddress cosignerCombinations
  where
    cosigners' = Map.keys xpubs
    formMaps = map Map.fromList . zipWith zip (cycle [cosigners'])
    cosignerCombinations = case Map.size xpubs of
        1 -> zipWith Map.singleton (cycle cosigners') [0..num]
        2 -> formMaps (map (\(x1,x2) -> [x1,x2]) $
                       (,) <$> [0..num] <*> [0..num])
        3 -> formMaps (map (\(x1,x2,x3) -> [x1,x2,x3]) $
                       (,,) <$> [0..num] <*> [0..num] <*> [0..num])
        4 -> formMaps (map (\(x1,x2,x3,x4) -> [x1,x2,x3,x4]) $
                       (,,,) <$> [0..num] <*> [0..num] <*> [0..num] <*> [0..num])
        5 -> formMaps (map (\(x1,x2,x3,x4,x5) -> [x1,x2,x3,x4,x5]) $
                       (,,,,) <$> [0..num] <*> [0..num] <*> [0..num] <*> [0..num] <*> [0..num])
        6 -> formMaps (map (\(x1,x2,x3,x4,x5,x6) -> [x1,x2,x3,x4,x5,x6]) $
                       (,,,,,) <$> [0..num] <*> [0..num] <*> [0..num] <*> [0..num] <*> [0..num] <*> [0..num])
        7 -> formMaps (map (\(x1,x2,x3,x4,x5,x6,x7) -> [x1,x2,x3,x4,x5,x6,x7]) $
                       (,,,,,,) <$> [0..num] <*> [0..num] <*> [0..num] <*> [0..num] <*> [0..num] <*> [0..num] <*> [0..num])
        _ -> error "generateAddressCombination supports up to 7 cosigners"
    (Right tag) = mkNetworkDiscriminant 1
    delegationCredential = DelegationFromKey $ liftXPub stakeXPub
    paymentCredential = PaymentFromScript . toScriptHash
    createAddress s =
        CA.unAddress $
        delegationAddress tag (paymentCredential s) delegationCredential
    tryCreateAddress combination =
        case replaceCosignersWithVerKeys st combination of
            Nothing -> []
            Just scriptKeyHash -> [Address $ createAddress scriptKeyHash]
--}
