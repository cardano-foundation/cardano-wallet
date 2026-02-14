-- |
-- Module      : Cardano.Read.Ledger.Block.BlockNo
-- Copyright   : © 2024 Cardano Foundation
-- License     : Apache-2.0
--
-- Block number extraction from block headers. Block numbers are
-- sequential identifiers for blocks in the chain.
module Cardano.Read.Ledger.Block.BlockNo
    ( -- * Block number type
      BlockNo (..)

      -- * Extraction
    , getEraBlockNo

      -- * Formatting
    , prettyBlockNo
    ) where

import Cardano.Read.Ledger.Block.BHeader
    ( BHeader (..)
    )
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( pHeaderBlock
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    (
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    (
    )
import Prelude

import Data.Text qualified as T
import Ouroboros.Network.Block qualified as O

{-# INLINEABLE getEraBlockNo #-}

-- | Extract the block number from a block header in any era.
getEraBlockNo :: forall era. IsEra era => BHeader era -> BlockNo
getEraBlockNo = case theEra @era of
    Byron -> \(BHeader h) -> k $ O.blockNo h
    Shelley -> \(BHeader h) -> k $ pHeaderBlock h
    Allegra -> \(BHeader h) -> k $ pHeaderBlock h
    Mary -> \(BHeader h) -> k $ pHeaderBlock h
    Alonzo -> \(BHeader h) -> k $ pHeaderBlock h
    Babbage -> \(BHeader h) -> k $ pHeaderBlock h
    Conway -> \(BHeader h) -> k $ pHeaderBlock h
    Dijkstra -> \(BHeader h) -> k $ pHeaderBlock h
  where
    k = BlockNo . fromIntegral . O.unBlockNo

-- |
-- Block number representing the sequential index of a block in the chain.
--
-- Block numbers start at 0 for the genesis block and increment by 1
-- for each subsequent block.
newtype BlockNo = BlockNo {unBlockNo :: Natural}
    deriving (Eq, Ord, Show, Generic, Enum)

instance NoThunks BlockNo

-- | Short printed representation of a 'BlockNo'.
prettyBlockNo :: BlockNo -> T.Text
prettyBlockNo (BlockNo n) = T.pack (show n)
