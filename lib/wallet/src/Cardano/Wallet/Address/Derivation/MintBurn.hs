{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Derivation of policy keys which are used to create scripts for the purposes
-- of minting and burning. Derived according to CIP-1855
-- (https://github.com/cardano-foundation/CIPs/blob/b2e9d02cb9a71ba9e754a432c78197428abf7e4c/CIP-1855/CIP-1855.md).
--
-- The policy keys are derived from the following path:
--
-- m / purpose' / coin_type' / policy_ix'
-- m / 1855'    / 1815'      / [2^31 .. 2^32-1]'
--
-- Where purpose' and coin_type' are fixed, and each new policy_ix' represents a
-- different policy key.

module Cardano.Wallet.Address.Derivation.MintBurn
    ( -- * Constants
      purposeCIP1855
      -- * Helpers
    , derivePolicyKeyAndHash
    , derivePolicyPrivateKey
    , policyDerivationPath
    , toTokenMapAndScript
    , toTokenPolicyId
    , scriptSlotIntervals
    , withinSlotInterval
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( Cosigner, KeyHash, Script (..), ScriptHash (..), toScriptHash )
import Cardano.Crypto.Wallet
    ( deriveXPrv )
import Cardano.Crypto.Wallet.Types
    ( DerivationScheme (DerivationScheme2) )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , WalletKey
    , getIndex
    , getRawKey
    , hashVerificationKey
    , liftRawKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( coinTypeAda )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Data.IntCast
    ( intCast )
import Data.Interval
    ( Interval, (<=..<=) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word64 )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Address.Script as CA
import qualified Data.Interval as I
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map


-- | Purpose for forged policy keys is a constant set to 1855' (or 0x8000073F)
-- following the original CIP-1855: "Forging policy keys for HD Wallets".
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1855 :: Index 'Hardened 'PurposeK
purposeCIP1855 = toEnum 0x8000073F

-- | Derive the policy private key that should be used to create mint/burn
-- scripts.
derivePolicyPrivateKey
    :: Passphrase purpose
    -- ^ Passphrase for wallet
    -> XPrv
    -- ^ Root private key to derive policy private key from
    -> Index 'Hardened 'PolicyK
    -- ^ Index of policy script
    -> XPrv
    -- ^ Policy private key
derivePolicyPrivateKey (Passphrase pwd) rootXPrv (Index policyIx) =
    let
        purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
            deriveXPrv DerivationScheme2 pwd rootXPrv (getIndex purposeCIP1855)
        coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
            deriveXPrv DerivationScheme2 pwd purposeXPrv (getIndex coinTypeAda)
     -- lvl3 derivation; hardened derivation of policy' index
    in deriveXPrv DerivationScheme2 pwd coinTypeXPrv policyIx

-- | Derive the policy private key that should be used to create mint/burn
-- scripts, as well as the key hash of the policy public key.
derivePolicyKeyAndHash
  :: WalletKey key
  => Passphrase "encryption"
  -- ^ Passphrase for wallet
  -> key 'RootK XPrv
  -- ^ Root private key to derive policy private key from
  -> Index 'Hardened 'PolicyK
  -- ^ Index of policy script
  -> (key 'PolicyK XPrv, KeyHash)
  -- ^ Policy private key
derivePolicyKeyAndHash pwd rootPrv policyIx = (policyK, vkeyHash)
  where
    policyK = liftRawKey policyPrv
    policyPrv = derivePolicyPrivateKey pwd (getRawKey rootPrv) policyIx
    vkeyHash = hashVerificationKey CA.Payment (publicKey policyK)

policyDerivationPath
    :: NonEmpty DerivationIndex
policyDerivationPath =  NE.fromList
    [ DerivationIndex $ getIndex purposeCIP1855
    , DerivationIndex $ getIndex coinTypeAda
    , DerivationIndex $ getIndex policyIx
    ]
  where
    policyIx :: Index 'Hardened 'PolicyK
    policyIx = minBound

toTokenPolicyId
    :: forall key. WalletKey key
    => Script Cosigner
    -> Map Cosigner XPub
    -> TokenPolicyId
toTokenPolicyId scriptTempl cosignerMap =
      UnsafeTokenPolicyId
    . Hash
    . unScriptHash
    . toScriptHash
    $ replaceCosigner @key cosignerMap scriptTempl

toTokenMapAndScript
    :: forall key. WalletKey key
    => Script Cosigner
    -> Map Cosigner XPub
    -> TokenName
    -> Natural
    -> (AssetId, TokenQuantity, Script KeyHash)
toTokenMapAndScript scriptTempl cosignerMap tName val =
    ( AssetId (toTokenPolicyId @key scriptTempl cosignerMap) tName
    , TokenQuantity val
    , replaceCosigner @key cosignerMap scriptTempl
    )

replaceCosigner
    :: forall key
     . HasCallStack
    => WalletKey key
    => Map Cosigner XPub
    -> Script Cosigner
    -> Script KeyHash
replaceCosigner cosignerMap = \case
    RequireSignatureOf c ->
        RequireSignatureOf $ toKeyHash c
    RequireAllOf xs ->
        RequireAllOf (map (replaceCosigner @key cosignerMap) xs)
    RequireAnyOf xs ->
        RequireAnyOf (map (replaceCosigner @key cosignerMap) xs)
    RequireSomeOf m xs ->
        RequireSomeOf m (map (replaceCosigner @key cosignerMap) xs)
    ActiveFromSlot s ->
        ActiveFromSlot s
    ActiveUntilSlot s ->
        ActiveUntilSlot s
  where
    toKeyHash :: HasCallStack => Cosigner -> KeyHash
    toKeyHash c = case Map.lookup c cosignerMap of
        Just xpub -> hashVerificationKey @key CA.Policy (liftRawKey xpub)
        Nothing -> error "Impossible: cosigner without xpub."

scriptSlotIntervals
    :: Script a
    -> [Interval Natural]
scriptSlotIntervals = \case
    RequireSignatureOf _ ->
        [allSlots]
    RequireAllOf xs ->
        let (timelocks, rest) = L.partition isTimelockOrSig xs
        in
        trimAllSlots
            $ I.intersections (concatMap scriptSlotIntervals timelocks)
            : concatMap scriptSlotIntervals rest
    RequireAnyOf xs ->
        trimAllSlots $ concatMap scriptSlotIntervals xs
    RequireSomeOf _ xs ->
        trimAllSlots $ concatMap scriptSlotIntervals xs
    ActiveFromSlot s ->
        [I.Finite s <=..<= maxSlot]
    ActiveUntilSlot s ->
        [minSlot <=..<= I.Finite s]
  where
    minSlot = I.Finite $ intCast $ minBound @Word64
    maxSlot = I.Finite $ intCast $ maxBound @Word64
    allSlots = minSlot <=..<= maxSlot

    isTimelockOrSig = \case
        ActiveFromSlot _ -> True
        ActiveUntilSlot _ -> True
        RequireSignatureOf _ -> True
        _ -> False

    trimAllSlots interval =
        let notAllSlots = filter (/= allSlots) interval
        in
        if L.null notAllSlots
        then interval
        else notAllSlots

-- tx validity interval must be a subset of a interval from script's timelock
-- tx validity interval is defined by specifying (from,to) slot interval
withinSlotInterval
    :: SlotNo
    -> SlotNo
    -> [Interval Natural]
    -> Bool
withinSlotInterval (SlotNo from) (SlotNo to) =
    L.any (txValidityInterval `I.isSubsetOf`)
  where
    txValidityInterval =
        I.Finite (intCast from) <=..<= I.Finite (intCast to)
