-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Source of the blockchain data for the wallet
--
module Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    ) where

import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( NodeToClientVersionData
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance
    )

data BlockchainSource
    = NodeSource CardanoNodeConn NodeToClientVersionData SyncTolerance
