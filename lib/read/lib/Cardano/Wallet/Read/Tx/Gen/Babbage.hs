{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Tx.Gen.Babbage
    ( mkBabbageTx
    , exampleBabbageTx
    , totalCollateral
    , txouts
    )
where

import Prelude

import Cardano.Ledger.Alonzo
    ( AlonzoTxAuxData
    )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AuxiliaryDataHash
    )
import Cardano.Ledger.Api
    ( Datum (NoDatum)
    )
import Cardano.Ledger.Api.Era
    ( BabbageEra
    , StandardCrypto
    )
import Cardano.Ledger.Babbage.Tx
    ( AlonzoTx (AlonzoTx)
    , IsValid (..)
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxBody (..)
    , BabbageTxOut (..)
    , ScriptIntegrityHash
    )
import Cardano.Ledger.Babbage.TxWits
    ( AlonzoTxWits
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
import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (..)
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Ledger.Shelley.API.Types
    ( TxIn
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

mkBabbageTx
    :: TxParameters
    -> AlonzoTx (BabbageEra StandardCrypto)
mkBabbageTx TxParameters{txInputs, txOutputs} =
    AlonzoTx (body txInputs txOutputs) wits valid aux

valid :: IsValid
valid = IsValid True

wits :: AlonzoTxWits (BabbageEra StandardCrypto)
wits = mempty

aux :: StrictMaybe (AlonzoTxAuxData (BabbageEra StandardCrypto))
aux = maybeToStrictMaybe Nothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> BabbageTxBody (BabbageEra StandardCrypto)
body ins outs =
    BabbageTxBody
        (txins ins)
        collateralIns
        referenceIns
        (txouts (natVersion @7) outs)
        collateralReturn
        totalCollateral
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

totalCollateral :: StrictMaybe Coin
totalCollateral = SNothing

collateralReturn
    :: StrictMaybe (Sized (BabbageTxOut (BabbageEra StandardCrypto)))
collateralReturn = SNothing

referenceIns :: Set (TxIn StandardCrypto)
referenceIns = mempty

txouts
    :: Version
    -> NonEmpty (Address, Lovelace)
    -> StrictSeq (Sized (BabbageTxOut (BabbageEra StandardCrypto)))
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

whash :: Set (KeyHash 'Witness StandardCrypto)
whash = mempty

collateralIns :: Set (TxIn StandardCrypto)
collateralIns = mempty

mint :: MultiAsset StandardCrypto
mint = mempty

exampleBabbageTx :: AlonzoTx (BabbageEra StandardCrypto)
exampleBabbageTx = mkBabbageTx exampleTxParameters
