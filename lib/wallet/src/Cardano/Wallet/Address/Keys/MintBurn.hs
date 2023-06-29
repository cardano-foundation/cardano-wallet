{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Address.Keys.MintBurn
    ( derivePolicyKeyAndHash
    , toTokenMapAndScript
    , toTokenPolicyId
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    , XPub
    )
import Cardano.Address.Script
    ( Cosigner
    , KeyHash
    , Script (..)
    , ScriptHash (unScriptHash)
    , toScriptHash
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    )
import Cardano.Wallet.Address.Derivation.MintBurn
    ( derivePolicyPrivateKey
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( AfterByron
    , getRawKey
    , hashVerificationKey
    , liftRawKey
    , publicKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName
    , TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Data.Map
    ( Map
    )
import GHC.Natural
    ( Natural
    )
import GHC.Stack
    ( HasCallStack
    )

import qualified Cardano.Address.Script as CA
import qualified Data.Map as Map

toTokenPolicyId
    :: forall key
     . (HasCallStack, AfterByron key)
    => KeyFlavorS key
    -> Script Cosigner
    -> Map Cosigner XPub
    -> TokenPolicyId
toTokenPolicyId kf scriptTempl cosignerMap =
    UnsafeTokenPolicyId
        . Hash
        . unScriptHash
        . toScriptHash
        $ replaceCosigner kf cosignerMap scriptTempl

toTokenMapAndScript
    :: forall key
     . (HasCallStack, AfterByron key)
    => KeyFlavorS key
    -> Script Cosigner
    -> Map Cosigner XPub
    -> TokenName
    -> Natural
    -> (AssetId, TokenQuantity, Script KeyHash)
toTokenMapAndScript kf scriptTempl cosignerMap tName val =
    ( AssetId (toTokenPolicyId kf scriptTempl cosignerMap) tName
    , TokenQuantity val
    , replaceCosigner kf cosignerMap scriptTempl
    )

replaceCosigner
    :: forall key
     . (HasCallStack, AfterByron key)
    => KeyFlavorS key
    -> Map Cosigner XPub
    -> Script Cosigner
    -> Script KeyHash
replaceCosigner kf cosignerMap = \case
    RequireSignatureOf c ->
        RequireSignatureOf $ toKeyHash c
    RequireAllOf xs ->
        RequireAllOf (map (replaceCosigner kf cosignerMap) xs)
    RequireAnyOf xs ->
        RequireAnyOf (map (replaceCosigner kf cosignerMap) xs)
    RequireSomeOf m xs ->
        RequireSomeOf m (map (replaceCosigner kf cosignerMap) xs)
    ActiveFromSlot s ->
        ActiveFromSlot s
    ActiveUntilSlot s ->
        ActiveUntilSlot s
  where
    toKeyHash :: HasCallStack => Cosigner -> KeyHash
    toKeyHash c = case Map.lookup c cosignerMap of
        Just xpub -> hashVerificationKey kf CA.Policy (liftRawKey kf xpub)
        Nothing -> error "Impossible: cosigner without xpub."

-- | Derive the policy private key that should be used to create mint/burn
-- scripts, as well as the key hash of the policy public key.
derivePolicyKeyAndHash
    :: AfterByron key
    => KeyFlavorS key
    -> Passphrase "encryption"
    -- ^ Passphrase for wallet
    -> key 'RootK XPrv
    -- ^ Root private key to derive policy private key from
    -> Index 'Hardened 'PolicyK
    -- ^ Index of policy script
    -> (key 'PolicyK XPrv, KeyHash)
    -- ^ Policy private key
derivePolicyKeyAndHash kf pwd rootPrv policyIx = (policyK, vkeyHash)
  where
    policyK = liftRawKey kf policyPrv
    policyPrv = derivePolicyPrivateKey pwd (getRawKey kf rootPrv) policyIx
    vkeyHash = hashVerificationKey kf CA.Payment (publicKey kf policyK)
