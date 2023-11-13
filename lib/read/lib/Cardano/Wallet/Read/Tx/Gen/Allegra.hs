{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Read.Tx.Gen.Allegra
    ( mkAllegraTx
    , exampleAllegraTx
    , exampleValidity
    )
where

import Prelude

import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData
    )
import Cardano.Ledger.Allegra.TxBody
    ( AllegraTxBody (..)
    , StrictMaybe (..)
    , ValidityInterval (..)
    )
import Cardano.Ledger.Api.Era
    ( AllegraEra
    , StandardCrypto
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
    , TxId (..)
    , TxParameters (..)
    , exampleTxParameters
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe.Strict
    ( maybeToStrictMaybe
    )

mkAllegraTx
    :: TxParameters
    -> ShelleyTx (AllegraEra StandardCrypto)
mkAllegraTx TxParameters{txInputs, txOutputs} =
    ShelleyTx (body txInputs txOutputs) wits aux

aux :: StrictMaybe (AllegraTxAuxData (AllegraEra StandardCrypto))
aux = maybeToStrictMaybe Nothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> AllegraTxBody (AllegraEra StandardCrypto)
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

exampleAllegraTx :: ShelleyTx (AllegraEra StandardCrypto)
exampleAllegraTx = mkAllegraTx exampleTxParameters
