{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.TxExtended
    ( fromCardanoTx
    )
    where

import Cardano.Wallet.Primitive.Ledger.Read.Tx.Allegra
    ( fromAllegraTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Alonzo
    ( fromAlonzoTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Babbage
    ( fromBabbageTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Conway
    ( fromConwayTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Mary
    ( fromMaryTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Shelley
    ( fromShelleyTx
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano

fromCardanoTx
    :: Cardano.Tx era
    -> TxExtended
fromCardanoTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley ->
            fromShelleyTx tx
        Cardano.ShelleyBasedEraAllegra ->
            fromAllegraTx tx
        Cardano.ShelleyBasedEraMary ->
            fromMaryTx tx
        Cardano.ShelleyBasedEraAlonzo ->
            fromAlonzoTx tx
        Cardano.ShelleyBasedEraBabbage ->
            fromBabbageTx tx
        Cardano.ShelleyBasedEraConway ->
            fromConwayTx tx
