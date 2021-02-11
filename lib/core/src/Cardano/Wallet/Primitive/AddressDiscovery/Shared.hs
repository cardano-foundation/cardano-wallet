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
    , KeyNumberScope (..)
    , unsafeSharedState
    , purposeCIPXXX
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
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
    , WalletKey (..)
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
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                 State
-------------------------------------------------------------------------------}

-- | A state to keep track of script templates, account public keys for cosigners,
-- | verification keys used and unused fitting in the address pool gap,
-- | and script addresses discovered with the corresponding indices.
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
    , shareStateKnownAddresses :: !(Map Address (Index 'Soft 'ScriptK))
        -- ^ Known script hashes that contain our verification key hashes
        -- represented here by corresponding indices
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

newtype KeyNumberScope =
    KeyNumberScope { unKeyNumberScope :: Word8 }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | Purpose for shared wallets is a constant set to 45' (or 0x8000002D) following the original
-- CIP-XXX Multi-signature Wallets.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIPXXX :: Index 'Hardened 'PurposeK
purposeCIPXXX = toEnum 0x8000002D

-- | Create a SharedState from the all ingredients.
-- There is no validation and it is unsafe way.
unsafeSharedState
    :: forall (n :: NetworkDiscriminant) k. WalletKey k
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Map KeyHash (Index 'Soft 'ScriptK, AddressState)
    -> Map Address [Index 'Soft 'ScriptK]
    -> SharedState n k
unsafeSharedState accXPub accIx g template1 template2 vkPoolMap knownAdresesses =
    SharedState
    { shareStateAccountKey = accXPub
    , shareStateDerivationPrefix = DerivationPrefix ( purposeCIPXXX, coinTypeAda, accIx )
    , shareStateGap = g
    , shareStatePaymentTemplate = template1
    , shareStateDelegationTemplate = template2
    , shareStateIndexedKeyHashes = Map.empty
    , shareStateKnownAddresses = Map.empty
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
