-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Source of the blockchain data for the wallet
--
module Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource(..)
    ) where

import Cardano.Launcher.Node
    ( CardanoNodeConn )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeToClientVersionData )

import qualified Blockfrost.Client as Blockfrost

data BlockchainSource
    = NodeSource
        CardanoNodeConn
        -- ^ Socket for communicating with the node
        NodeToClientVersionData
    | BlockfrostSource Blockfrost.Project
    -- ^ Blockfrost token when working in the light mode
