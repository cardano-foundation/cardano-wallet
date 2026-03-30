{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Tx.Gen.Conway
    ( mkConwayTx
    , exampleConwayTx
    )
where

import Cardano.Ledger.Alonzo
    ( AlonzoTxAuxData
    )
import Cardano.Ledger.Alonzo.Tx
    ( IsValid (..)
    , ScriptIntegrityHash
    )
import Cardano.Ledger.Api
    ( ConwayEra
    , Datum (NoDatum)
    )
import Cardano.Ledger.Api.Tx.In
    ( TxIn
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut (BabbageTxOut)
    )
import Cardano.Ledger.BaseTypes
    ( Network
    )
import Cardano.Ledger.Binary
    ( Sized
    , Version
    , mkSized
    , natVersion
    )
import Cardano.Ledger.Conway.Governance
    ( ProposalProcedure
    , VotingProcedures (..)
    )
import Cardano.Ledger.Conway.Tx
    ( AlonzoTx (AlonzoTx)
    , Tx (MkConwayTx)
    )
import Cardano.Ledger.Conway.TxBody
    ( TxBody (ConwayTxBody)
    )
import Cardano.Ledger.Conway.TxCert
    ( ConwayTxCert
    )
import Cardano.Ledger.Conway.TxWits
    ( AlonzoTxWits
    )
import Cardano.Ledger.Hashes
    ( TxAuxDataHash
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
import Cardano.Wallet.Read.Tx.Gen.Babbage
    ( totalCollateral
    )
import Cardano.Wallet.Read.Tx.Gen.Mary
    ( mkMaryValue
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( txfee
    , txins
    , wdrls
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
import Data.Foldable
    ( toList
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe.Strict
    ( StrictMaybe (SNothing)
    )
import Data.OSet.Strict
    ( OSet
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )
import Data.Set
    ( Set
    )
import Prelude

import Cardano.Ledger.Core qualified as L

mkConwayTx
    :: TxParameters
    -> L.Tx ConwayEra
mkConwayTx TxParameters{txInputs, txOutputs} =
    MkConwayTx $ AlonzoTx (body txInputs txOutputs) wits valid aux

valid :: IsValid
valid = IsValid True

wits :: AlonzoTxWits ConwayEra
wits = mempty

aux :: StrictMaybe (AlonzoTxAuxData ConwayEra)
aux = SNothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> TxBody ConwayEra
body ins outs =
    ConwayTxBody
        (txins ins)
        collateralIns
        referenceIns
        (txouts (natVersion @8) outs)
        collateralReturn
        totalCollateral
        certs
        wdrls
        txfee
        exampleValidity
        witnesses
        mint
        integrity
        auxhash
        network
        votingProcedures
        proposalProcedures
        mempty
        mempty

collateralReturn
    :: StrictMaybe (Sized (BabbageTxOut ConwayEra))
collateralReturn = SNothing

proposalProcedures :: OSet (ProposalProcedure ConwayEra)
proposalProcedures = mempty

votingProcedures :: VotingProcedures ConwayEra
votingProcedures = VotingProcedures mempty

witnesses :: Set (KeyHash 'Witness)
witnesses = mempty

certs :: OSet (ConwayTxCert ConwayEra)
certs = mempty

referenceIns :: Set TxIn
referenceIns = mempty

txouts
    :: Version
    -> NonEmpty (Address, Lovelace)
    -> StrictSeq (Sized (BabbageTxOut ConwayEra))
txouts v xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure
        $ mkSized v
        $ BabbageTxOut
            (decodeShelleyAddress addr)
            (mkMaryValue val)
            NoDatum
            SNothing

network :: StrictMaybe Network
network = SNothing

auxhash :: StrictMaybe TxAuxDataHash
auxhash = SNothing

integrity :: StrictMaybe ScriptIntegrityHash
integrity = SNothing

collateralIns :: Set TxIn
collateralIns = mempty

mint :: MultiAsset
mint = mempty

exampleConwayTx :: L.Tx ConwayEra
exampleConwayTx = mkConwayTx exampleTxParameters
