{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Tx.Gen where

import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
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
import Cardano.Wallet.Read.Tx.Gen.Babbage
    ( mkBabbageTx
    )
import Cardano.Wallet.Read.Tx.Gen.Byron
    ( mkByronTx
    )
import Cardano.Wallet.Read.Tx.Gen.Conway
    ( mkConwayTx
    )
import Cardano.Wallet.Read.Tx.Gen.Dijkstra
    ( mkDijkstraTx
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
import Prelude

{-# INLINEABLE mkTxEra #-}
mkTxEra :: forall era. IsEra era => TxParameters -> Tx era
mkTxEra = case theEra @era of
    Byron -> g mkByronTx
    Shelley -> g mkShelleyTx
    Allegra -> g mkAllegraTx
    Mary -> g mkMaryTx
    Alonzo -> g mkAlonzoTx
    Babbage -> g mkBabbageTx
    Conway -> g mkConwayTx
    Dijkstra -> g mkDijkstraTx
  where
    g f = Tx . f
