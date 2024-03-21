{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Read.Tx.Gen.Mary
    ( mkMaryTx
    , exampleMaryTx
    , mkMaryValue
    )
where

import Prelude

import Cardano.Ledger.Allegra.Core
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData
    )
import Cardano.Ledger.Api.Era
    ( MaryEra
    , StandardCrypto
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Mary.TxBody
    ( MaryTxBody (..)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset
    )
import Cardano.Ledger.Shelley.API.Types
    ( ShelleyTx (ShelleyTx)
    , ShelleyTxOut (ShelleyTxOut)
    , StrictMaybe (..)
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
    ( maybeToStrictMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )

mkMaryTx
    :: TxParameters
    -> ShelleyTx (MaryEra StandardCrypto)
mkMaryTx TxParameters{txInputs, txOutputs} =
    ShelleyTx (body txInputs txOutputs) wits aux

aux :: StrictMaybe (AllegraTxAuxData (MaryEra StandardCrypto))
aux = maybeToStrictMaybe Nothing

body
    :: NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> MaryTxBody (MaryEra StandardCrypto)
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

mint :: MultiAsset StandardCrypto
mint = mempty

txouts
    :: NonEmpty (Address, Lovelace)
    -> StrictSeq (ShelleyTxOut (MaryEra StandardCrypto))
txouts xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure $ ShelleyTxOut (decodeShelleyAddress addr) $ mkMaryValue val

mkMaryValue :: Integer -> MaryValue StandardCrypto
mkMaryValue lovelace = MaryValue (Coin lovelace) mempty

validity :: ValidityInterval
validity = ValidityInterval SNothing SNothing

exampleMaryTx :: ShelleyTx (MaryEra StandardCrypto)
exampleMaryTx = mkMaryTx exampleTxParameters
