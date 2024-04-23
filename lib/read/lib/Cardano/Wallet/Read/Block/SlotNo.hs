{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Block.SlotNo
    ( getEraSlotNo
    , SlotNo (..)
    , prettySlotNo
    ) where

import Prelude

import Cardano.Wallet.Read.Block.BHeader
    ( BHeader (..)
    )
import Cardano.Wallet.Read.Eras.KnownEras
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

import qualified Data.Text as T
import qualified Ouroboros.Network.Block as O

{-# INLINABLE getEraSlotNo #-}
getEraSlotNo :: forall era. IsEra era => BHeader era -> SlotNo
getEraSlotNo = case theEra @era of
    Byron -> \(BHeader h) -> k $ O.blockSlot h
    Shelley -> \(BHeader h) -> k $ pHeaderSlot h
    Allegra -> \(BHeader h) -> k $ pHeaderSlot h
    Mary -> \(BHeader h) -> k $ pHeaderSlot h
    Alonzo -> \(BHeader h) -> k $ pHeaderSlot h
    Babbage -> \(BHeader h) -> k $ pHeaderSlot h
    Conway -> \(BHeader h) -> k $ pHeaderSlot h
  where
    k = SlotNo . fromIntegral . O.unSlotNo

newtype SlotNo = SlotNo {unSlotNo :: Natural}
    deriving (Eq, Ord, Show, Generic)

instance NoThunks SlotNo

-- | Short printed representation of a 'ChainPoint'.
prettySlotNo :: SlotNo -> T.Text
prettySlotNo (SlotNo n) = T.pack (show n)
