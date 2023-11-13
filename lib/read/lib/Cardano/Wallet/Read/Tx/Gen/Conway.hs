{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Tx.Gen.Conway
    ( mkConwayTx
    , exampleConwayTx
    )
where

import Prelude

import Cardano.Ledger.Alonzo
    ( AlonzoTxAuxData
    )
import Cardano.Ledger.Alonzo.Tx
    ( IsValid (..)
    , ScriptIntegrityHash
    )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AuxiliaryDataHash
    )
import Cardano.Ledger.Api.Era
    ( ConwayEra
    , StandardCrypto
    )
import Cardano.Ledger.Babbage
    ( BabbageTxOut
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxOut (..)
    , Datum (..)
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
import Cardano.Ledger.Conway.Delegation.Certificates
    ( ConwayDCert
    )
import Cardano.Ledger.Conway.Governance
    ( ProposalProcedure
    , VotingProcedure
    )
import Cardano.Ledger.Conway.Tx
    ( AlonzoTx (AlonzoTx)
    )
import Cardano.Ledger.Conway.TxBody
    ( ConwayTxBody (..)
    )
import Cardano.Ledger.Conway.TxWits
    ( AlonzoTxWits
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (..)
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Ledger.Shelley.Tx
    ( TxIn
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
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )
import Data.Set
    ( Set
    )

mkConwayTx
    :: TxParameters
    -> AlonzoTx (ConwayEra StandardCrypto)
mkConwayTx TxParameters{txInputs, txOutputs} =
    AlonzoTx (body txInputs txOutputs) wits valid aux

valid :: IsValid
valid = IsValid True

wits :: AlonzoTxWits (ConwayEra StandardCrypto)
wits = mempty

aux :: StrictMaybe (AlonzoTxAuxData (ConwayEra StandardCrypto))
aux = SNothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> ConwayTxBody (ConwayEra StandardCrypto)
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

collateralReturn
    :: StrictMaybe (Sized (BabbageTxOut (ConwayEra StandardCrypto)))
collateralReturn = SNothing

proposalProcedures :: StrictSeq (ProposalProcedure (ConwayEra StandardCrypto))
proposalProcedures = mempty

votingProcedures :: StrictSeq (VotingProcedure (ConwayEra StandardCrypto))
votingProcedures = mempty

witnesses :: Set (KeyHash 'Witness StandardCrypto)
witnesses = mempty

certs :: StrictSeq (ConwayDCert StandardCrypto)
certs = mempty

referenceIns :: Set (TxIn StandardCrypto)
referenceIns = mempty

txouts
    :: Version
    -> NonEmpty (Address, Lovelace)
    -> StrictSeq (Sized (BabbageTxOut (ConwayEra StandardCrypto)))
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

auxhash :: StrictMaybe (AuxiliaryDataHash StandardCrypto)
auxhash = SNothing

integrity :: StrictMaybe (ScriptIntegrityHash StandardCrypto)
integrity = SNothing

collateralIns :: Set (TxIn StandardCrypto)
collateralIns = mempty

mint :: MultiAsset StandardCrypto
mint = mempty

exampleConwayTx :: AlonzoTx (ConwayEra StandardCrypto)
exampleConwayTx = mkConwayTx exampleTxParameters
