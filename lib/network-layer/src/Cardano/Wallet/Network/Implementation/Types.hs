{-|
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Conversions between types from @ouroboros-consensus@
and "Cardano.Wallet.Read".
-}
module Cardano.Wallet.Network.Implementation.Types
    ( fromOuroborosPoint
    , toOuroborosPoint
    , fromOuroborosTip
    , toOuroborosTip
    ) where

import Prelude

import Cardano.Wallet.Read
    ( BHeader
    , BlockNo (..)
    , ChainPoint (..)
    , ChainTip (..)
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

fromOuroborosPoint :: O.Point (CardanoBlock sc) -> ChainPoint
fromOuroborosPoint O.GenesisPoint =
    GenesisPoint
fromOuroborosPoint (O.BlockPoint slot h) =
    BlockPoint (fromCardanoSlotNo slot) (fromCardanoHash h)

{-----------------------------------------------------------------------------
    ChainTip conversions
------------------------------------------------------------------------------}

toOuroborosTip :: ChainTip -> O.Tip (CardanoBlock sc)
toOuroborosTip GenesisTip =
    O.TipGenesis
toOuroborosTip (BlockTip slot h blockNo) =
    O.Tip (toCardanoSlotNo slot) (toCardanoHash h) (toCardanoBlockNo blockNo)

fromOuroborosTip :: O.Tip (CardanoBlock sc) -> ChainTip
fromOuroborosTip O.TipGenesis =
    GenesisTip
fromOuroborosTip (O.Tip slot h blockNo) =
    BlockTip
        (fromCardanoSlotNo slot)
        (fromCardanoHash h)
        (fromCardanoBlockNo blockNo)

{-----------------------------------------------------------------------------
    Helper conversions
------------------------------------------------------------------------------}
toCardanoSlotNo :: SlotNo -> O.SlotNo
toCardanoSlotNo (SlotNo slot) = O.SlotNo (toEnum $ fromEnum slot)

fromCardanoSlotNo :: O.SlotNo -> SlotNo
fromCardanoSlotNo (O.SlotNo slot) = SlotNo (fromIntegral slot)

toCardanoHash :: Hash Blake2b_256 BHeader -> OneEraHash (CardanoEras sc)
toCardanoHash = OneEraHash . hashToBytesShort

fromCardanoHash :: OneEraHash (CardanoEras sc) -> Hash Blake2b_256 BHeader
fromCardanoHash = fromJust . hashFromBytesShort . getOneEraHash

toCardanoBlockNo :: BlockNo -> O.BlockNo
toCardanoBlockNo (BlockNo blockNo) = O.BlockNo (toEnum $ fromEnum blockNo)

fromCardanoBlockNo :: O.BlockNo -> BlockNo
fromCardanoBlockNo (O.BlockNo blockNo) = BlockNo (fromIntegral blockNo)
