{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Read.Tx.Gen.Mary
    ( mkMaryTx
    , exampleMaryTx
    , mkMaryValue
    )
where

import Cardano.Ledger.Allegra.Core
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData
    )
import Cardano.Ledger.Api
    ( MaryEra
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Mary
    ( Tx (MkMaryTx)
    )
import Cardano.Ledger.Mary.TxBody
    ( TxBody (MaryTxBody)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset
    )
import Cardano.Ledger.Shelley.API.Types
    ( ShelleyTxOut (ShelleyTxOut)
    , StrictMaybe (..)
    )
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx (ShelleyTx)
    )
import Cardano.Wallet.Read.Tx.Gen.Address
    ( decodeShelleyAddress
    )
import Cardano.Wallet.Read.Tx.Gen.Shelley
    ( auxb
    , certs
    , txfee
    , txins
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
import Data.Foldable
    ( toList
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe.Strict
    ( maybeToStrictMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )
import Prelude

import Cardano.Ledger.Core qualified as L

mkMaryTx
    :: TxParameters
    -> L.Tx MaryEra
mkMaryTx TxParameters{txInputs, txOutputs} =
    MkMaryTx $ ShelleyTx (body txInputs txOutputs) wits aux

aux :: StrictMaybe (AllegraTxAuxData MaryEra)
aux = maybeToStrictMaybe Nothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> L.TxBody MaryEra
body ins outs =
    MaryTxBody
        (txins ins)
        (txouts outs)
        certs
        wdrls
        txfee
        validity
        upd
        auxb
        mint

mint :: MultiAsset
mint = mempty

txouts
    :: NonEmpty (Address, Lovelace)
    -> StrictSeq (ShelleyTxOut MaryEra)
txouts xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure $ ShelleyTxOut (decodeShelleyAddress addr) $ mkMaryValue val

mkMaryValue :: Integer -> MaryValue
mkMaryValue lovelace = MaryValue (Coin lovelace) mempty

validity :: ValidityInterval
validity = ValidityInterval SNothing SNothing

exampleMaryTx :: L.Tx MaryEra
exampleMaryTx = mkMaryTx exampleTxParameters
