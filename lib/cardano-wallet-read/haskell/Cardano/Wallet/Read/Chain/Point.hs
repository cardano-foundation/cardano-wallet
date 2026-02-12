{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Points on the blockchain.
module Cardano.Wallet.Read.Chain.Point
    ( -- * Slot
      WithOrigin (At, Origin)
    , Slot
    , slotFromChainPoint

      -- * ChainPoint
    , ChainPoint (..)
    , getChainPoint
    , prettyChainPoint
    , chainPointFromChainTip

      -- * ChainTip
    , ChainTip (..)
    , getChainTip
    , prettyChainTip
    ) where

import Cardano.Ledger.BaseTypes
    ( WithOrigin (At, Origin)
    )
import Cardano.Wallet.Read.Block
    ( Block
    , BlockNo (..)
    , RawHeaderHash
    , SlotNo (..)
    , getEraBHeader
    , getEraBlockNo
    , getEraHeaderHash
    , getEraSlotNo
    , getRawHeaderHash
    )
import Cardano.Wallet.Read.Eras
    ( IsEra
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Prelude

import Cardano.Wallet.Read.Hash qualified as Hash
import Data.Text qualified as T

{-----------------------------------------------------------------------------
    Slot
------------------------------------------------------------------------------}

-- | Either genesis or a numbered slot.
type Slot = WithOrigin SlotNo

-- | Get the 'Slot' of a 'ChainPoint'.
slotFromChainPoint :: ChainPoint -> Slot
slotFromChainPoint GenesisPoint = Origin
slotFromChainPoint (BlockPoint slotNo _) = At slotNo

{-----------------------------------------------------------------------------
    ChainPoint
------------------------------------------------------------------------------}

-- | A point (block) on the Cardano blockchain.
data ChainPoint
    = GenesisPoint
    | BlockPoint
        { slotNo :: !SlotNo
        , headerHash :: !RawHeaderHash
        }
    deriving (Eq, Ord, Show, Generic)

instance NoThunks ChainPoint

{-# INLINEABLE getChainPoint #-}

-- | Get 'ChainPoint' of this block.
getChainPoint :: IsEra era => Block era -> ChainPoint
getChainPoint block =
    BlockPoint
        { slotNo = getEraSlotNo $ getEraBHeader block
        , headerHash = getRawHeaderHash $ getEraHeaderHash block
        }

-- | Short printed representation of a 'ChainPoint'.
prettyChainPoint :: ChainPoint -> T.Text
prettyChainPoint GenesisPoint =
    "[point genesis]"
prettyChainPoint (BlockPoint slot hash) =
    "[point " <> hashF hash <> " at slot " <> slotF slot <> "]"
  where
    hashF = T.take 8 . Hash.hashToTextAsHex
    slotF (SlotNo n) = T.pack (show n)

-- | Get 'ChainPoint' of the given tip.
chainPointFromChainTip :: ChainTip -> ChainPoint
chainPointFromChainTip GenesisTip = GenesisPoint
chainPointFromChainTip (BlockTip slot hash _) = BlockPoint slot hash

{-----------------------------------------------------------------------------
    Tip
------------------------------------------------------------------------------}

-- | Point on the blockchain.
-- Used in chain-sync protocol to advertise the tip of the server's chain.
-- Records both the 'ChainPoint' and the 'BlockNo' of the block.
data ChainTip
    = GenesisTip
    | BlockTip
        { slotNo :: !SlotNo
        , headerHash :: !RawHeaderHash
        , blockNo :: !BlockNo
        }
    deriving (Eq, Ord, Show, Generic)

instance NoThunks ChainTip

{-# INLINEABLE getChainTip #-}

-- | Get 'ChainTip' corresponding to this block.
getChainTip :: IsEra era => Block era -> ChainTip
getChainTip block =
    BlockTip
        { slotNo = getEraSlotNo $ getEraBHeader block
        , headerHash = getRawHeaderHash $ getEraHeaderHash block
        , blockNo = getEraBlockNo $ getEraBHeader block
        }

-- | Short printed representation of a 'ChainPoint'.
prettyChainTip :: ChainTip -> T.Text
prettyChainTip GenesisTip =
    "[tip genesis]"
prettyChainTip BlockTip{slotNo, headerHash, blockNo} =
    "[tip "
        <> hashF headerHash
        <> " at slot "
        <> slotNoF slotNo
        <> " at blockNo "
        <> blockNoF blockNo
        <> "]"
  where
    hashF = T.take 8 . Hash.hashToTextAsHex
    slotNoF (SlotNo n) = T.pack (show n)
    blockNoF (BlockNo n) = T.pack (show n)
