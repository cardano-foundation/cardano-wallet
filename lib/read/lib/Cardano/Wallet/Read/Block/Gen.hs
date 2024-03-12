{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ( Era (..)
    , IsEra (..)
    )
import Control.Category
    ( (.)
    )

mkBlockEra :: forall era . IsEra era => BlockParameters era -> Block era
mkBlockEra = case theEra @era of
    Byron -> g mkByronBlock
    Shelley -> g $ mkShelleyBlock (natVersion @2)
    Allegra -> g $ mkShelleyBlock (natVersion @3)
    Mary -> g $ mkShelleyBlock (natVersion @4)
    Alonzo -> g $ mkShelleyBlock (natVersion @6)
    Babbage -> g $ mkBabbageBlock (natVersion @7)
    Conway -> g $ mkBabbageBlock (natVersion @8)
  where
    g f = Block . f
