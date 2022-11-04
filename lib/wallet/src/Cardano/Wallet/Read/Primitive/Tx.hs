{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx
    ( fromCardanoTx
    )
    where

import Prelude

import Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromAllegraTx )
import Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTx )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTx )
import Cardano.Wallet.Read.Primitive.Tx.Byron
    ( fromTxAux )
import Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTx )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTx )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount
    , emptyTokenMapWithScripts
    , emptyWitnessCount
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
    ( Tx (ByronTx) )
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

fromCardanoTx
    :: Cardano.Tx era
    ->  ( W.Tx
        , TokenMapWithScripts
        , TokenMapWithScripts
        , [W.Certificate]
        , Maybe ValidityIntervalExplicit
        , WitnessCount
        )
fromCardanoTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley ->
            extract $ fromShelleyTx tx
        Cardano.ShelleyBasedEraAllegra ->
            extract $ fromAllegraTx tx
        Cardano.ShelleyBasedEraMary ->
            extract $ fromMaryTx tx
        Cardano.ShelleyBasedEraAlonzo ->
            extract $ fromAlonzoTx tx
        Cardano.ShelleyBasedEraBabbage ->
            extract $ fromBabbageTx tx
    Cardano.ByronTx tx ->
        ( fromTxAux tx
        , emptyTokenMapWithScripts
        , emptyTokenMapWithScripts
        , []
        , Nothing
        , emptyWitnessCount
        )
  where
    extract (tx, certs, mint, burn, validity, wits) =
        (tx, mint, burn, certs, validity, wits)
