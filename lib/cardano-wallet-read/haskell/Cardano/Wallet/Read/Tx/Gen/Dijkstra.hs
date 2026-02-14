{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Tx.Gen.Dijkstra
    ( mkDijkstraTx
    )
where

import Cardano.Ledger.Alonzo
    ( AlonzoTxAuxData
    )
import Cardano.Ledger.Alonzo.Tx
    ( AlonzoTx (AlonzoTx)
    , IsValid (..)
    , ScriptIntegrityHash
    )
import Cardano.Ledger.Api
    ( DijkstraEra
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
    , mkSized
    , natVersion
    )
import Cardano.Ledger.Conway.Governance
    ( ProposalProcedure
    , VotingProcedures (..)
    )
import Cardano.Ledger.Dijkstra.Tx
    ( Tx (MkDijkstraTx)
    )
import Cardano.Ledger.Credential
    ( Credential
    )
import Cardano.Ledger.Dijkstra.TxBody
    ( TxBody (DijkstraTxBody)
    )
import Cardano.Ledger.Dijkstra.TxCert
    ( DijkstraTxCert
    )
import Cardano.Ledger.Hashes
    ( TxAuxDataHash
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
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

-- | Create a minimal Dijkstra era transaction from parameters.
mkDijkstraTx
    :: TxParameters
    -> L.Tx DijkstraEra
mkDijkstraTx TxParameters{txInputs, txOutputs} =
    MkDijkstraTx $ AlonzoTx (body txInputs txOutputs) wits valid aux

valid :: IsValid
valid = IsValid True

wits :: L.TxWits DijkstraEra
wits = mempty

aux :: StrictMaybe (AlonzoTxAuxData DijkstraEra)
aux = SNothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> L.TxBody DijkstraEra
body ins outs =
    DijkstraTxBody
        (txins ins)
        collateralIns
        referenceIns
        (txouts outs)
        collateralReturn
        totalCollateral
        certs
        wdrls
        txfee
        exampleValidity
        guards
        mint
        integrity
        auxhash
        network
        votingProcedures
        proposalProcedures
        mempty
        mempty

collateralReturn
    :: StrictMaybe (Sized (BabbageTxOut DijkstraEra))
collateralReturn = SNothing

proposalProcedures :: OSet (ProposalProcedure DijkstraEra)
proposalProcedures = mempty

votingProcedures :: VotingProcedures DijkstraEra
votingProcedures = VotingProcedures mempty

guards :: OSet (Credential 'Guard)
guards = mempty

certs :: OSet (DijkstraTxCert DijkstraEra)
certs = mempty

referenceIns :: Set TxIn
referenceIns = mempty

txouts
    :: NonEmpty (Address, Lovelace)
    -> StrictSeq (Sized (BabbageTxOut DijkstraEra))
txouts xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure
        $ mkSized (natVersion @12)
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
