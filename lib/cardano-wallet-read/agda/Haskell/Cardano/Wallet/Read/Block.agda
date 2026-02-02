{-# OPTIONS --erasure #-}

-- Synchronized manually with the corresponding Haskell module.
module Haskell.Cardano.Wallet.Read.Block where

open import Haskell.Prelude
open import Haskell.Law

open import Haskell.Cardano.Wallet.Read.Eras using (IsEra)
open import Haskell.Cardano.Wallet.Read.Tx using (Tx)

variable
  era : Set

{-----------------------------------------------------------------------------
    BlockNo
------------------------------------------------------------------------------}
record BlockNo : Set where
  constructor BlockNoC
  field
    unBlockNo : Nat

open BlockNo public

instance
  iEqBlockNo : Eq BlockNo
  iEqBlockNo ._==_ x y = unBlockNo x == unBlockNo y

  iOrdFromCompareBlockNo : OrdFromCompare BlockNo
  iOrdFromCompareBlockNo .OrdFromCompare.compare =
    λ x y → compare (unBlockNo x) (unBlockNo y)

  iOrdBlockNo : Ord BlockNo
  iOrdBlockNo = record {OrdFromCompare iOrdFromCompareBlockNo}

{-----------------------------------------------------------------------------
    SlotNo
------------------------------------------------------------------------------}
record SlotNo : Set where
  constructor SlotNoC
  field
    unSlotNo : Nat

open SlotNo public

instance
  iEqSlotNo : Eq SlotNo
  iEqSlotNo ._==_ x y = unSlotNo x == unSlotNo y

  iOrdFromCompareSlotNo : OrdFromCompare SlotNo
  iOrdFromCompareSlotNo .OrdFromCompare.compare =
    λ x y → compare (unSlotNo x) (unSlotNo y)

  iOrdSlotNo : Ord SlotNo
  iOrdSlotNo = record {OrdFromCompare iOrdFromCompareSlotNo}

postulate instance
  iIsLawfulOrdSlotNo : IsLawfulOrd SlotNo

{-----------------------------------------------------------------------------
    HeaderHash
------------------------------------------------------------------------------}
postulate
  HeaderHash : Set → Set

  RawHeaderHash : Set
  getRawHeaderHash : {{IsEra era}} → HeaderHash era → RawHeaderHash
  instance
    iEqRawHeaderHash : Eq RawHeaderHash

{-----------------------------------------------------------------------------
    BHeader
------------------------------------------------------------------------------}
postulate
  BHeader : Set → Set

  getEraBlockNo : {{IsEra era}} → BHeader era → BlockNo
  getEraSlotNo : {{IsEra era}} → BHeader era → SlotNo

{-----------------------------------------------------------------------------
    Block
------------------------------------------------------------------------------}
postulate
  Block : Set → Set

  getEraBHeader : {{IsEra era}} → Block era → BHeader era
  getEraHeaderHash : {{IsEra era}} → Block era → HeaderHash era
  getEraTransactions : {{IsEra era}} → Block era → List (Tx era)
