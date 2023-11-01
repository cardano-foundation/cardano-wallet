{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.Gen
where

import Prelude

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

mkBlockEra :: EraFun BlockParameters Block
mkBlockEra =
    EraFun
        { byronFun = g mkByronBlock
        , shelleyFun = g mkShelleyBlock
        , allegraFun = g mkShelleyBlock
        , maryFun = g mkShelleyBlock
        , alonzoFun = g mkShelleyBlock
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
