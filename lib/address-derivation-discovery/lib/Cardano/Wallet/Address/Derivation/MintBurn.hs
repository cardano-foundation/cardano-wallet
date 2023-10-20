{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
    , derivePolicyPrivateKey
    , policyDerivationPath
    , scriptSlotIntervals
    , withinSlotInterval
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    )
import Cardano.Address.Script
    ( Script (..)
    )
import Cardano.Crypto.Wallet
    ( deriveXPrv
    )
import Cardano.Crypto.Wallet.Types
    ( DerivationScheme (DerivationScheme2)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , getIndex
    )
import Cardano.Wallet.Address.Discovery
    ( coinTypeAda
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Data.IntCast
    ( intCast
    )
import Data.Interval
    ( Interval
    , (<=..<=)
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Word
    ( Word64
    )
import Numeric.Natural
    ( Natural
    )

import qualified Data.Interval as I
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

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
