{-# LANGUAGE FlexibleContexts #-}

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
    ( Era
    , EraSegWits (..)
    )
import Cardano.Wallet.Read
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
    )
import Generics.SOP
    ( K (..)
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

import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Slot as L
import qualified Cardano.Protocol.TPraos.BHeader as Shelley
import qualified Ouroboros.Consensus.Protocol.Praos.Header as O
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Network.Block as O

getEraSlotNo :: EraFun Block (K SlotNo)
getEraSlotNo =
    EraFun
        { byronFun = \(Block block) -> k $ O.blockSlot block
        , shelleyFun = \(Block block) -> k $ getSlotNoShelley block
        , allegraFun = \(Block block) -> k $ getSlotNoShelley block
        , maryFun = \(Block block) -> k $ getSlotNoShelley block
        , alonzoFun = \(Block block) -> k $ getSlotNoShelley block
        , babbageFun = \(Block block) -> k $ getSlotNoBabbage block
        , conwayFun = \(Block block) -> k $ getSlotNoBabbage block
        }
  where
    k = K . SlotNo . fromIntegral . L.unSlotNo

newtype SlotNo = SlotNo {unSlotNo :: Natural}
    deriving (Eq, Show)

getSlotNoShelley
    :: (Era era, EncCBORGroup (TxSeq era), Crypto c)
    => O.ShelleyBlock (TPraos c) era
    -> O.SlotNo
getSlotNoShelley
    (O.ShelleyBlock (Shelley.Block (Shelley.BHeader header _) _) _) =
        Shelley.bheaderSlotNo header
getSlotNoBabbage
    :: (Era era, EncCBORGroup (TxSeq era), Crypto crypto)
    => O.ShelleyBlock (Praos crypto) era
    -> O.SlotNo
getSlotNoBabbage
    (O.ShelleyBlock (Shelley.Block (O.Header header _) _) _) =
        O.hbSlotNo header
