{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.CardanoApi
    ( fromCardanoTx
    )
    where

import Prelude

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
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( TokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount
    , WitnessCountCtx
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

fromCardanoTx
    :: WitnessCountCtx
    -> Cardano.Tx era
    -> ( W.Tx
       , TokenMapWithScripts
       , TokenMapWithScripts
       , [W.Certificate]
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromCardanoTx witCtx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley ->
            extract $ fromShelleyTx tx
        Cardano.ShelleyBasedEraAllegra ->
            extract $ fromAllegraTx tx
        Cardano.ShelleyBasedEraMary ->
            extract $ fromMaryTx tx witCtx
        Cardano.ShelleyBasedEraAlonzo ->
            extract $ fromAlonzoTx tx witCtx
        Cardano.ShelleyBasedEraBabbage ->
            extract $ fromBabbageTx tx witCtx
        Cardano.ShelleyBasedEraConway ->
            extract $ fromConwayTx tx witCtx
  where
    extract (tx, certs, mint, burn, validity, wits) =
        (tx, mint, burn, certs, validity, wits)
