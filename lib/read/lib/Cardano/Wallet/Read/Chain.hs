{-# LANGUAGE DeriveGeneric #-}
{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Data types relating to the consensus about the blockchain.
-}
module Cardano.Wallet.Read.Chain
    ( ChainPoint (GenesisPoint, BlockPoint)
    , getChainPoint
    , prettyChainPoint
    ) where

import Prelude

import Cardano.Wallet.Read.Block
    ( Block
    , RawHeaderHash
    , SlotNo (..)
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
