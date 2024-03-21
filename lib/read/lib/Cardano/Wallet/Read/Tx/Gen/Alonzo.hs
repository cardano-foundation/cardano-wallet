{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Read.Tx.Gen.Alonzo
    ( mkAlonzoTx
    , exampleAlonzoTx
    )
where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( AlonzoTx (AlonzoTx)
    , IsValid (..)
    )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AlonzoTxAuxData
    , AuxiliaryDataHash
    )
import Cardano.Ledger.Alonzo.TxBody
    ( AlonzoTxBody (..)
    , AlonzoTxOut (..)
    , ScriptIntegrityHash
    )
import Cardano.Ledger.Alonzo.TxWits
    ( AlonzoTxWits
    )
import Cardano.Ledger.Api.Era
    ( AlonzoEra
    , StandardCrypto
    )
import Cardano.Ledger.Api.Tx.In
    ( TxIn
    )
import Cardano.Ledger.BaseTypes
    ( Network
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (..)
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Wallet.Read.Tx.Gen.Address
    ( decodeShelleyAddress
    )
import Cardano.Wallet.Read.Tx.Gen.Allegra
    ( exampleValidity
    )
import Cardano.Wallet.Read.Tx.Gen.Mary
    ( mkMaryValue
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( certs
    , txfee
    , txins
    , upd
    , wdrls
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    , Index (..)
    , Lovelace (..)
    , TxId (..)
    , TxParameters (..)
    , exampleTxParameters
    )
import Data.Foldable
    ( toList
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe.Strict
    ( StrictMaybe (SNothing)
    , maybeToStrictMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )
import Data.Set
    ( Set
    )

mkAlonzoTx
    :: TxParameters
    -> AlonzoTx (AlonzoEra StandardCrypto)
mkAlonzoTx TxParameters{txInputs, txOutputs} =
    AlonzoTx (body txInputs txOutputs) wits valid aux

valid :: IsValid
valid = IsValid True

wits :: AlonzoTxWits (AlonzoEra StandardCrypto)
wits = mempty

aux :: StrictMaybe (AlonzoTxAuxData (AlonzoEra StandardCrypto))
aux = maybeToStrictMaybe Nothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> AlonzoTxBody (AlonzoEra StandardCrypto)
body ins outs =
    AlonzoTxBody
        (txins ins)
        collateralIns
        (txouts outs)
        certs
        wdrls
        txfee
        exampleValidity
        upd
        whash
        mint
        integrity
        auxhash
        network

txouts
    :: NonEmpty (Address, Lovelace)
    -> StrictSeq (AlonzoTxOut (AlonzoEra StandardCrypto))
txouts xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure $ AlonzoTxOut (decodeShelleyAddress addr) (mkMaryValue val) SNothing

network :: StrictMaybe Network
network = SNothing

auxhash :: StrictMaybe (AuxiliaryDataHash StandardCrypto)
auxhash = SNothing

integrity :: StrictMaybe (ScriptIntegrityHash StandardCrypto)
integrity = SNothing

whash :: Set (KeyHash 'Witness StandardCrypto)
whash = mempty

collateralIns :: Set (TxIn StandardCrypto)
collateralIns = mempty

mint :: MultiAsset StandardCrypto
mint = mempty

exampleAlonzoTx :: AlonzoTx (AlonzoEra StandardCrypto)
exampleAlonzoTx = mkAlonzoTx exampleTxParameters
