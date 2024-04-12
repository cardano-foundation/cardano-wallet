{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Block.SlotNo
    ( getEraSlotNo
    , SlotNo (..)
    ) where

import Prelude

import Cardano.Ledger.Block
    ( bheader
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Block.Block
    ( Block (..)
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
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( pHeaderSlot
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

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
    deriving (Eq, Ord, Show, Generic)

instance NoThunks SlotNo

getSlotNoShelley
    :: O.ShelleyBlock (TPraos StandardCrypto) era
    -> O.SlotNo
getSlotNoShelley (O.ShelleyBlock block _) =
    pHeaderSlot $ bheader block

getSlotNoBabbage
    :: O.ShelleyBlock (Praos StandardCrypto) era
    -> O.SlotNo
getSlotNoBabbage (O.ShelleyBlock block _) =
    pHeaderSlot $ bheader block
