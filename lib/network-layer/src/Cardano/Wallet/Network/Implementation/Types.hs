{-|
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Conversions between types from @ouroboros-consensus@
and "Cardano.Wallet.Read".
-}
module Cardano.Wallet.Network.Implementation.Types
    ( fromOuroborosPoint
    , toOuroborosPoint
    ) where

import Prelude

import Cardano.Wallet.Read
    ( BHeader
    , ChainPoint (..)
    , SlotNo (..)
    )
import Cardano.Wallet.Read.Hash
    ( Blake2b_256
    , Hash
    , hashFromBytesShort
    , hashToBytesShort
    )
import Data.Maybe
    ( fromJust
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..)
    )

import qualified Ouroboros.Network.Block as O

{-----------------------------------------------------------------------------
    ChainPoint conversions
------------------------------------------------------------------------------}

toOuroborosPoint :: ChainPoint -> O.Point (CardanoBlock sc)
toOuroborosPoint GenesisPoint =
    O.GenesisPoint
toOuroborosPoint (BlockPoint slot h) =
    O.BlockPoint (toCardanoSlotNo slot) (toCardanoHash h)

toCardanoSlotNo :: SlotNo -> O.SlotNo
toCardanoSlotNo (SlotNo slot) = O.SlotNo (toEnum $ fromEnum slot)

toCardanoHash :: Hash Blake2b_256 BHeader -> OneEraHash (CardanoEras sc)
toCardanoHash = OneEraHash . hashToBytesShort

fromOuroborosPoint :: O.Point (CardanoBlock sc) -> ChainPoint
fromOuroborosPoint O.GenesisPoint =
    GenesisPoint
fromOuroborosPoint (O.BlockPoint slot h) =
    BlockPoint (fromCardanoSlotNo slot) (fromCardanoHash h)

fromCardanoSlotNo :: O.SlotNo -> SlotNo
fromCardanoSlotNo (O.SlotNo slot) = SlotNo (fromIntegral slot)

fromCardanoHash :: OneEraHash (CardanoEras sc) -> Hash Blake2b_256 BHeader
fromCardanoHash = fromJust . hashFromBytesShort . getOneEraHash
