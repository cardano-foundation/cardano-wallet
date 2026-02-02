{-# OPTIONS --erasure #-}

{- Synchronized manually with the corresponding Haskell module.

Unfortunately, mirroring the hidden child modules is not supported
— we have to define here all the identifiers that are exported by
Cardano.Wallet.Read.Chain *here*.

-}
module Haskell.Cardano.Wallet.Read.Chain where

open import Haskell.Prelude
open import Haskell.Law

open import Haskell.Cardano.Wallet.Read.Eras using
    ( IsEra
    )
open import Haskell.Cardano.Wallet.Read.Block using
    ( Block
    ; BlockNo
    ; HeaderHash
    ; RawHeaderHash
    ; SlotNo
    )

import Haskell.Cardano.Wallet.Read.Block as Block

{-----------------------------------------------------------------------------
    Cardano.Wallet.Read.Chain.Point
------------------------------------------------------------------------------}
{-----------------------------------------------------------------------------
    Slot
------------------------------------------------------------------------------}
data WithOrigin (a : Set) : Set where
  Origin : WithOrigin a
  At     : a → WithOrigin a

instance
  iEqWithOrigin : {{Eq a}} → Eq (WithOrigin a)
  iEqWithOrigin ._==_ Origin Origin = True
  iEqWithOrigin ._==_ (At x) (At y) = x == y
  iEqWithOrigin ._==_ _      _      = False

  iOrdFromCompareWithOrigin : {{Ord a}} → OrdFromCompare (WithOrigin a)
  iOrdFromCompareWithOrigin .OrdFromCompare.compare = λ where
      Origin Origin → EQ
      Origin (At _) → LT
      (At _) Origin → GT
      (At x) (At y) → compare x y

  iOrdWithOrigin : {{Ord a}} → Ord (WithOrigin a)
  iOrdWithOrigin = record {OrdFromCompare iOrdFromCompareWithOrigin}

postulate instance
  iIsLawfulOrdWithOrigin
    : {{_ : Ord a}} → {{IsLawfulOrd a}} → IsLawfulOrd (WithOrigin a)

Slot : Set
Slot = WithOrigin SlotNo

{-----------------------------------------------------------------------------
    ChainTip
------------------------------------------------------------------------------}
data ChainTip : Set where
  GenesisTip : ChainTip
  BlockTip   : SlotNo → RawHeaderHash → BlockNo → ChainTip

instance
  iEqChainTip : Eq ChainTip
  iEqChainTip ._==_ GenesisTip GenesisTip = True
  iEqChainTip ._==_ (BlockTip x1 y1 z1) (BlockTip x2 y2 z2) =
    x1 == x2 && y1 == y2 && z1 == z2
  iEqChainTip ._==_ _      _      = False

getChainTip : ∀ {era} → {{IsEra era}} → Block era → ChainTip
getChainTip block =
    BlockTip
        (Block.getEraSlotNo (Block.getEraBHeader block))
        (Block.getRawHeaderHash (Block.getEraHeaderHash block))
        (Block.getEraBlockNo (Block.getEraBHeader block))

{-----------------------------------------------------------------------------
    ChainPoint
------------------------------------------------------------------------------}
-- | A point (block) on the Cardano blockchain.
data ChainPoint : Set where
  GenesisPoint : ChainPoint
  BlockPoint   : SlotNo → RawHeaderHash → ChainPoint

instance
  iEqChainPoint : Eq ChainPoint
  iEqChainPoint ._==_ GenesisPoint GenesisPoint = True
  iEqChainPoint ._==_ (BlockPoint x1 y1) (BlockPoint x2 y2) =
    x1 == x2 && y1 == y2
  iEqChainPoint ._==_ _      _      = False

instance postulate
  iOrdChainPoint : Ord ChainPoint
  iShowChainPoint : Show ChainPoint

slotFromChainPoint : ChainPoint → Slot
slotFromChainPoint GenesisPoint = WithOrigin.Origin
slotFromChainPoint (BlockPoint slotNo _) = WithOrigin.At slotNo

chainPointFromChainTip : ChainTip → ChainPoint
chainPointFromChainTip GenesisTip = GenesisPoint
chainPointFromChainTip (BlockTip slot hash _) = BlockPoint slot hash

getChainPoint : ∀ {era} → {{IsEra era}} → Block era → ChainPoint
getChainPoint block =
    BlockPoint
        (Block.getEraSlotNo (Block.getEraBHeader block))
        (Block.getRawHeaderHash (Block.getEraHeaderHash block))

{-----------------------------------------------------------------------------
    Cardano.Wallet.Read.Chain.Genesis
------------------------------------------------------------------------------}
postulate
  GenesisData : Set
  GenesisHash : Set
  GenesisDataError : Set

  genesisHashMainnet : GenesisHash
  mockGenesisDataMainnet : GenesisData

postulate
  NetworkMagic : Set

data NetworkId : Set where
  Mainnet : NetworkId
  Testnet : NetworkMagic → NetworkId

postulate
  getNetworkId : GenesisData → NetworkId
