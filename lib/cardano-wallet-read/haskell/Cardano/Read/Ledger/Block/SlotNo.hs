-- |
-- Module      : Cardano.Read.Ledger.Block.SlotNo
-- Copyright   : © 2024 Cardano Foundation
-- License     : Apache-2.0
--
-- Slot number extraction from block headers. Slot numbers represent
-- the time slot in which a block was produced.
module Cardano.Read.Ledger.Block.SlotNo
    ( -- * Slot number type
      SlotNo (..)

      -- * Extraction
    , getEraSlotNo

      -- * Conversions
    , fromLedgerSlotNo
    , toLedgerSlotNo

      -- * Formatting
    , prettySlotNo
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
    ( pHeaderSlot
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    (
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    (
    )
import Prelude

import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Text qualified as T
import Ouroboros.Network.Block qualified as O

{-# INLINEABLE getEraSlotNo #-}

-- | Extract the slot number from a block header in any era.
getEraSlotNo :: forall era. IsEra era => BHeader era -> SlotNo
getEraSlotNo = case theEra @era of
    Byron -> \(BHeader h) -> fromLedgerSlotNo $ O.blockSlot h
    Shelley -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Allegra -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Mary -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Alonzo -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Babbage -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Conway -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h

-- |
-- Slot number representing a time slot in the blockchain.
--
-- Each slot corresponds to a fixed time interval (currently 1 second
-- on mainnet). A block can be produced in each slot.
newtype SlotNo = SlotNo {unSlotNo :: Natural}
    deriving (Eq, Ord, Show, Generic, Enum)

instance NoThunks SlotNo

-- | Convert 'SlotNo' from @cardano-ledger-core@.
fromLedgerSlotNo :: Ledger.SlotNo -> SlotNo
fromLedgerSlotNo = SlotNo . fromIntegral . Ledger.unSlotNo

-- | Convert 'SlotNo' to @cardano-ledger-core@.
toLedgerSlotNo :: SlotNo -> Ledger.SlotNo
toLedgerSlotNo = Ledger.SlotNo . fromInteger . fromIntegral . unSlotNo

-- | Short printed representation of a 'ChainPoint'.
prettySlotNo :: SlotNo -> T.Text
prettySlotNo (SlotNo n) = T.pack (show n)
