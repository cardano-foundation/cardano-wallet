{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

The 'Block' type represents a block indexed by one of the known eras.
It is compatible with the types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Block
    ( module Cardano.Wallet.Read.Block.Block
    , module Cardano.Wallet.Read.Block.BlockNo
    , module Cardano.Wallet.Read.Block.HeaderHash
    , module Cardano.Wallet.Read.Block.SlotNo
    , module Cardano.Wallet.Read.Block.Txs
    ) where

import Cardano.Wallet.Read.Block.Block
    ( Block (..)
    , ConsensusBlock
    , fromConsensusBlock
    , toConsensusBlock
    )
import Cardano.Wallet.Read.Block.BlockNo
    ( BlockNo (..)
    , getEraBlockNo
    )
import Cardano.Wallet.Read.Block.HeaderHash
    ( HeaderHash (..)
    , HeaderHashT
    , PrevHeaderHash (..)
    , PrevHeaderHashT
    , getEraHeaderHash
    , getEraPrevHeaderHash
    )
import Cardano.Wallet.Read.Block.SlotNo
    ( SlotNo (..)
    , getEraSlotNo
    )
import Cardano.Wallet.Read.Block.Txs
    ( getEraTransactions
    )
