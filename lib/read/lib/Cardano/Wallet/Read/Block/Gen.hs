{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Block.Gen
where

import Prelude hiding
    ( (.)
    )

import Cardano.Ledger.BaseTypes
    ( natVersion
    )
import Cardano.Wallet.Read.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Block.Gen.Babbage
    ( mkBabbageBlock
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Cardano.Wallet.Read.Block.Gen.Byron
    ( mkByronBlock
    )
import Cardano.Wallet.Read.Block.Gen.Shelley
    ( mkShelleyBlock
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    )
import Control.Category
    ( (.)
    )

mkBlockEra :: EraFun BlockParameters Block
mkBlockEra =
    EraFun
        { byronFun = g mkByronBlock
        , shelleyFun = g $ mkShelleyBlock (natVersion @2)
        , allegraFun = g $ mkShelleyBlock (natVersion @3)
        , maryFun = g $ mkShelleyBlock (natVersion @4)
        , alonzoFun = g $ mkShelleyBlock (natVersion @6)
        , babbageFun = g $ mkBabbageBlock (natVersion @7)
        , conwayFun = g $ mkBabbageBlock (natVersion @8)
        }
  where
    g f = Block . f
