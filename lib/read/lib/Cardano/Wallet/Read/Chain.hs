{-# LANGUAGE DeriveGeneric #-}
{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Data types relating to the consensus about the blockchain.
-}
module Cardano.Wallet.Read.Chain
    ( -- * ChainPoint
      ChainPoint (GenesisPoint, BlockPoint)
    , getChainPoint
    , prettyChainPoint
    , chainPointFromChainTip

    -- * ChainTip
    , ChainTip (GenesisTip, BlockTip)
    , getChainTip
    , prettyChainTip
    ) where

import Prelude

import Cardano.Wallet.Read.Block
    ( Block
    , BlockNo (..)
    , RawHeaderHash
    , SlotNo (..)
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

import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    ChainPoint
------------------------------------------------------------------------------}

-- | A point (block) on the Cardano blockchain.
data ChainPoint
    = GenesisPoint
    | BlockPoint !SlotNo !RawHeaderHash
    deriving (Eq, Ord, Show, Generic)

instance NoThunks ChainPoint

{-# INLINABLE getChainPoint #-}
getChainPoint :: IsEra era => Block era -> ChainPoint
getChainPoint block =
    BlockPoint
        (getEraSlotNo block)
        (getRawHeaderHash $ getEraHeaderHash block)

-- | Short printed representation of a 'ChainPoint'.
prettyChainPoint :: ChainPoint -> T.Text
prettyChainPoint GenesisPoint =
    "[point genesis]"
prettyChainPoint (BlockPoint slot hash) =
    "[point " <> hashF hash <> " at slot " <> slotF slot <> "]"
  where
    hashF = T.take 8 . Hash.hashToTextAsHex
    slotF (SlotNo n) = T.pack (show n)

chainPointFromChainTip :: ChainTip -> ChainPoint
chainPointFromChainTip GenesisTip = GenesisPoint
chainPointFromChainTip (BlockTip slot hash _) = BlockPoint slot hash

{-----------------------------------------------------------------------------
    Tip
------------------------------------------------------------------------------}

-- | Used in chain-sync protocol to advertise the tip of the server's chain.
-- Records the 'ChainPoint' and the 'BlockNo' of the block.
data ChainTip
    = GenesisTip
    | BlockTip !SlotNo !RawHeaderHash !BlockNo
    deriving (Eq, Ord, Show, Generic)

instance NoThunks ChainTip

{-# INLINABLE getChainTip #-}
getChainTip :: IsEra era => Block era -> ChainTip
getChainTip block =
    BlockTip
        (getEraSlotNo block)
        (getRawHeaderHash $ getEraHeaderHash block)
        (getEraBlockNo block)

-- | Short printed representation of a 'ChainPoint'.
prettyChainTip :: ChainTip -> T.Text
prettyChainTip GenesisTip =
    "[tip genesis]"
prettyChainTip (BlockTip slotNo hash blockNo) =
    "[tip " <> hashF hash
        <> " at slot " <> slotNoF slotNo
        <> " at blockNo " <> blockNoF blockNo
        <> "]"
  where
    hashF = T.take 8 . Hash.hashToTextAsHex
    slotNoF (SlotNo n) = T.pack (show n)
    blockNoF (BlockNo n) = T.pack (show n)