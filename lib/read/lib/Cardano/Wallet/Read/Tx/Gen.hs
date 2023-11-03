module Cardano.Wallet.Read.Tx.Gen where

import Prelude

import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Gen.Allegra
    ( mkAllegraTx
    )
import Cardano.Wallet.Read.Tx.Gen.Alonzo
    ( mkAlonzoTx
    )
import Cardano.Wallet.Read.Tx.Gen.Byron
    ( mkByronTx
    )
import Cardano.Wallet.Read.Tx.Gen.Mary
    ( mkMaryTx
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( mkShelleyTx
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( TxParameters
    )
import Generics.SOP
    ( K
    , unK
    )

mkTxEra :: EraFun (K TxParameters) Tx
mkTxEra =
    EraFun
        { byronFun = g mkByronTx
        , shelleyFun = g mkShelleyTx
        , allegraFun = g mkAllegraTx
        , maryFun = g mkMaryTx
        , alonzoFun = g mkAlonzoTx
        , babbageFun = g mkBabbageTx
        , conwayFun = g mkConwayTx
        }
  where
    g f = Tx . f . unK

mkBabbageTx :: t
mkBabbageTx = error "Not implemented"

mkConwayTx :: t
mkConwayTx = error "Not implemented"
