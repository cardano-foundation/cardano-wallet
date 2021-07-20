{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Definition of policy keys which are used to create scripts for the purposes
-- of minting and burning.
--
-- The policy keys are derived according to the following path:
--
-- m / purpose' / coin_type' / policy_ix'
-- m / 1855'    / 1815'      / [2^31 .. 2^32-1]'
--
-- Where purpose' and coin_type' are fixed, and each new policy_ix' represents a
-- different policy key.

module Cardano.Wallet.Primitive.AddressDerivation.MintBurn
    ( -- * Constants
      purposeCIP1855
      -- * Helpers
    , derivePolicyKey
    , derivePolicyPrivateKey
    ) where

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Address.Script
    ( KeyHash )
import Cardano.Crypto.Wallet
    ( deriveXPrv )
import Cardano.Crypto.Wallet.Types
    ( DerivationScheme (DerivationScheme2) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (PolicyK, PurposeK, RootK)
    , DerivationType (Hardened)
    , Index (getIndex)
    , Index (Index)
    , Passphrase (Passphrase)
    , Role (UtxoExternal)
    , WalletKey (publicKey)
    , getRawKey
    , hashVerificationKey
    , liftRawKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( coinTypeAda )
import Prelude

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
derivePolicyKey
  :: WalletKey key
  => Passphrase "encryption"
  -- ^ Passphrase for wallet
  -> key 'RootK XPrv
  -- ^ Root private key to derive policy private key from
  -> Index 'Hardened 'PolicyK
  -- ^ Index of policy script
  -> (key 'PolicyK XPrv, KeyHash)
  -- ^ Policy private key
derivePolicyKey pwd rootPrv policyIx = (policyK, vkeyHash)
  where
    policyK = liftRawKey policyPrv
    policyPrv = derivePolicyPrivateKey pwd (getRawKey rootPrv) policyIx
    vkeyHash = hashVerificationKey UtxoExternal (publicKey policyK)
