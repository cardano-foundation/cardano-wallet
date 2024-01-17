{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( fromCardanoTx
    , primitiveTx
    ) 
    where

import Prelude

import Cardano.Wallet.Primitive.Ledger.Read.Tx.Allegra
    ( fromAllegraTx
    , fromAllegraTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Alonzo
    ( fromAlonzoTx
    , fromAlonzoTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Babbage
    ( fromBabbageTx
    , fromBabbageTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Byron
    ( fromTxAux
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Conway
    ( fromConwayTx
    , fromConwayTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Mary
    ( fromMaryTx
    , fromMaryTx'
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Shelley
    ( fromShelleyTx
    , fromShelleyTx'
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
import Cardano.Wallet.Read
    ( Tx (..)
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    )
import Generics.SOP
    ( K (..)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

fromCardanoTx ::
    WitnessCountCtx ->
    Cardano.Tx era ->
    ( W.Tx
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

primitiveTx :: EraFun Tx (K W.Tx)
primitiveTx =
    EraFun
        { byronFun = \(Tx tx) -> K . fromTxAux $ tx
        , shelleyFun = \(Tx tx) -> K . fromShelleyTx' $ tx
        , allegraFun = \(Tx tx) -> K . fromAllegraTx' $ tx
        , maryFun = \(Tx tx) -> K . fromMaryTx' $ tx
        , alonzoFun = \(Tx tx) -> K . fromAlonzoTx' $ tx
        , babbageFun = \(Tx tx) -> K . fromBabbageTx' $ tx
        , conwayFun = \(Tx tx) -> K . fromConwayTx' $ tx
        }
