{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

The 'Block' type represents a block indexed by one of the known eras.
It is compatible with the types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Block
    ( module Cardano.Read.Ledger.Block.BHeader
    , module Cardano.Read.Ledger.Block.Block
    , module Cardano.Read.Ledger.Block.BlockNo
    , module Cardano.Read.Ledger.Block.HeaderHash
    , module Cardano.Read.Ledger.Block.SlotNo
    , module Cardano.Wallet.Read.Block.Txs
    ) where

import Cardano.Read.Ledger.Block.BHeader
    ( BHeader (..)
    , getEraBHeader
    )
import Cardano.Read.Ledger.Block.Block
    ( Block (..)
    , ConsensusBlock
    , fromConsensusBlock
    , toConsensusBlock
    )
import Cardano.Read.Ledger.Block.BlockNo
    ( BlockNo (..)
    , getEraBlockNo
    , prettyBlockNo
    )
import Cardano.Read.Ledger.Block.HeaderHash
    ( EraIndependentBlockHeader
    , HeaderHash (..)
    , HeaderHashT
    , PrevHeaderHash (..)
    , PrevHeaderHashT
    , RawHeaderHash
    , getEraHeaderHash
    , getEraPrevHeaderHash
    , getRawHeaderHash
    , mockRawHeaderHash
    )
import Cardano.Read.Ledger.Block.SlotNo
    ( SlotNo (..)
    , getEraSlotNo
    , prettySlotNo
    )
import Cardano.Wallet.Read.Block.Txs
    ( getEraTransactions
    )
