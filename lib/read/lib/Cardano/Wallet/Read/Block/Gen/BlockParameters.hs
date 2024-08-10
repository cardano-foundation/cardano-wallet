{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    , exampleBlockParameters
    , slotNumberL
    , blockNumberL
    , txsL
    )
where

import Cardano.Read.Ledger.Block.BlockNo
    ( BlockNo (..)
    )
import Cardano.Read.Ledger.Block.SlotNo
    ( SlotNo (..)
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx
    )
import Control.Lens.TH
    ( makeLensesFor
    )

data BlockParameters era = BlockParameters
    { slotNumber :: SlotNo
    , blockNumber :: BlockNo
    , txs :: [Tx era]
    }

makeLensesFor
    [ ("slotNumber", "slotNumberL")
    , ("blockNumber", "blockNumberL")
    , ("txs", "txsL")
    ] ''BlockParameters

exampleBlockParameters :: [Tx era] -> BlockParameters era
exampleBlockParameters txs =
    BlockParameters
        { slotNumber = SlotNo 0
        , blockNumber = BlockNo 0
        , txs
        }
