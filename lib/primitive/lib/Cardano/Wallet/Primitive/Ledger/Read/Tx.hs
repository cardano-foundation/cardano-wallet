{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
    where

import Cardano.Wallet.Primitive.Ledger.Read.Tx.Allegra
    ( fromAllegraTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Alonzo
    ( fromAlonzoTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Babbage
    ( fromBabbageTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Byron
    ( fromTxAux
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Conway
    ( fromConwayTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Mary
    ( fromMaryTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Shelley
    ( fromShelleyTx'
    )
import Cardano.Wallet.Read
    ( IsEra (..)
    , Tx (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    )

import qualified Cardano.Wallet.Primitive.Types.Tx as W

primitiveTx :: forall era . IsEra era => Tx era -> W.Tx
primitiveTx = case theEra @era of
    Byron -> \(Tx tx) -> fromTxAux tx
    Shelley -> \(Tx tx) -> fromShelleyTx' tx
    Allegra -> \(Tx tx) -> fromAllegraTx' tx
    Mary -> \(Tx tx) -> fromMaryTx' tx
    Alonzo -> \(Tx tx) -> fromAlonzoTx' tx
    Babbage -> \(Tx tx) -> fromBabbageTx' tx
    Conway -> \(Tx tx) -> fromConwayTx' tx
