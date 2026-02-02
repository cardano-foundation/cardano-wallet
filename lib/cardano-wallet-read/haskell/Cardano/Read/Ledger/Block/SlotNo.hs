{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Read.Ledger.Block.SlotNo
    ( getEraSlotNo
    , SlotNo (..)
    , fromLedgerSlotNo
    , toLedgerSlotNo
    , prettySlotNo
    ) where

import Prelude

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
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Data.Text as T
import qualified Ouroboros.Network.Block as O

{-# INLINABLE getEraSlotNo #-}
getEraSlotNo :: forall era. IsEra era => BHeader era -> SlotNo
getEraSlotNo = case theEra @era of
    Byron -> \(BHeader h) -> fromLedgerSlotNo $ O.blockSlot h
    Shelley -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Allegra -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Mary -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Alonzo -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Babbage -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h
    Conway -> \(BHeader h) -> fromLedgerSlotNo $ pHeaderSlot h

newtype SlotNo = SlotNo {unSlotNo :: Natural}
    deriving (Eq, Ord, Show, Generic, Enum, Num)

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
