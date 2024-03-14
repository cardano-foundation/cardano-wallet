{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Block.SlotNo
    ( getEraSlotNo
    , SlotNo (..)
    ) where

import Prelude

import Cardano.Ledger.Binary.Group
    ( EncCBORGroup
    )
import Cardano.Ledger.Crypto
    ( Crypto
    )
import Cardano.Ledger.Era
    ( EraSegWits (..)
    )
import Cardano.Wallet.Read.Block.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Block.BlockNo
    ()
    -- ?! GHC 9.6.4: This import looks redundant, but the compilation
    -- of getSlotNoShelley will fail if we don't that. No idea why.
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Protocol.TPraos.BHeader as Shelley
import qualified Ouroboros.Consensus.Protocol.Praos.Header as O
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Network.Block as O

{-# INLINABLE getEraSlotNo #-}
getEraSlotNo :: forall era. IsEra era => Block era -> SlotNo
getEraSlotNo = case theEra @era of
    Byron -> \(Block block) -> k $ O.blockSlot block
    Shelley -> \(Block block) -> k $ getSlotNoShelley block
    Allegra -> \(Block block) -> k $ getSlotNoShelley block
    Mary -> \(Block block) -> k $ getSlotNoShelley block
    Alonzo -> \(Block block) -> k $ getSlotNoShelley block
    Babbage -> \(Block block) -> k $ getSlotNoBabbage block
    Conway -> \(Block block) -> k $ getSlotNoBabbage block

  where
    k = SlotNo . fromIntegral . O.unSlotNo

newtype SlotNo = SlotNo {unSlotNo :: Natural}
    deriving (Eq, Show)

getSlotNoShelley
    :: (L.Era era, EncCBORGroup (TxSeq era), Crypto c)
    => O.ShelleyBlock (TPraos c) era
    -> O.SlotNo
getSlotNoShelley
    (O.ShelleyBlock (Shelley.Block (Shelley.BHeader header _) _) _) =
        Shelley.bheaderSlotNo header

getSlotNoBabbage
    :: (L.Era era, EncCBORGroup (TxSeq era), Crypto crypto)
    => O.ShelleyBlock (Praos crypto) era
    -> O.SlotNo
getSlotNoBabbage
    (O.ShelleyBlock (Shelley.Block (O.Header header _) _) _) =
        O.hbSlotNo header
