{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Read.Tx.Gen.Allegra
    ( mkAllegraTx
    , exampleAllegraTx
    , exampleValidity
    )
where

import Cardano.Ledger.Allegra.Tx
    ( Tx (MkAllegraTx)
    )
import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData
    )
import Cardano.Ledger.Allegra.TxBody
    ( StrictMaybe (..)
    , TxBody (AllegraTxBody)
    , ValidityInterval (..)
    )
import Cardano.Ledger.Api
    ( AllegraEra
    )
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx (ShelleyTx)
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( auxb
    , certs
    , txfee
    , txins
    , txouts
    , upd
    , wdrls
    , wits
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    , Index (..)
    , Lovelace (..)
    , TxParameters (..)
    , exampleTxParameters
    )
import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe.Strict
    ( maybeToStrictMaybe
    )
import Prelude

import Cardano.Ledger.Core qualified as L

mkAllegraTx
    :: TxParameters
    -> L.Tx AllegraEra
mkAllegraTx TxParameters{txInputs, txOutputs} =
    MkAllegraTx $ ShelleyTx (body txInputs txOutputs) wits aux

aux :: StrictMaybe (AllegraTxAuxData AllegraEra)
aux = maybeToStrictMaybe Nothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> TxBody AllegraEra
body ins outs =
    AllegraTxBody
        (txins ins)
        (txouts outs)
        certs
        wdrls
        txfee
        exampleValidity
        upd
        auxb

exampleValidity :: ValidityInterval
exampleValidity = ValidityInterval SNothing SNothing

exampleAllegraTx :: L.Tx AllegraEra
exampleAllegraTx = mkAllegraTx exampleTxParameters
