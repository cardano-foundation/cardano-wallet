{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.Gen
where

import Prelude

import Cardano.Ledger.BaseTypes
    ( natVersion
    )
import Cardano.Wallet.Read
    ( Tx (..)
    )
import Cardano.Wallet.Read.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Block.Gen.Babbage
    ( mkBabbageBlock
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    , exampleBlockParameters
    )
import Cardano.Wallet.Read.Block.Gen.Byron
    ( mkByronBlock
    )
import Cardano.Wallet.Read.Block.Gen.Shelley
    ( mkShelleyBlock
    )
import Cardano.Wallet.Read.Eras
    ( (:.:) (..)
    , EraFun (..)
    , EraValue
    , sequenceEraValue
    , (*.**)
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( AllEraValue
    , runAllEraValue
    )
import Cardano.Wallet.Read.Tx.Gen.Byron
    ( exampleByronTx
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( exampleShelleyTx
    )

mkBlockEra :: EraFun BlockParameters Block
mkBlockEra =
    EraFun
        { byronFun = g mkByronBlock
        , shelleyFun = g $ mkShelleyBlock (natVersion @2)
        , allegraFun = g $ mkShelleyBlock (natVersion @3)
        , maryFun = g $ mkShelleyBlock (natVersion @4)
        , alonzoFun = g $ mkShelleyBlock (natVersion @6)
        , babbageFun = g mkBabbageBlock
        , conwayFun = g mkBabbageBlock
        }
  where
    g f = Block . f

genBlocks
    :: AllEraValue ([] :.: BlockParameters)
    -> [EraValue Block]
genBlocks source =
    concatMap sequenceEraValue
        $ runAllEraValue
        $ mkBlockEra *.** source

exampleBlock :: [EraValue Block]
exampleBlock = genBlocks $ EraFun
    { byronFun = const $ Comp [exampleBlockParameters [Tx exampleByronTx]]
    , shelleyFun = const $ Comp [exampleBlockParameters [Tx exampleShelleyTx]]
    , allegraFun = const $ Comp []
    , maryFun = const $ Comp []
    , alonzoFun = const $ Comp []
    , babbageFun = const $ Comp []
    , conwayFun = const $ Comp []
    }
