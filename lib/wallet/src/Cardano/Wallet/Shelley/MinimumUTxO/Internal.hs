{-# LANGUAGE GADTs #-}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Computing minimum UTxO values: internal interface.
--
module Cardano.Wallet.Shelley.MinimumUTxO.Internal
    ( computeMinimumCoinForUTxO_CardanoLedger
    ) where

import Prelude

import Cardano.Ledger.Api
    ( getMinCoinTxOut )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxOForShelleyBasedEra (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toAllegraTxOut
    , toAlonzoTxOut
    , toBabbageTxOut
    , toConwayTxOut
    , toMaryTxOut
    , toShelleyTxOut
    , toWalletCoin
    )

import qualified Cardano.Api.Shelley as Cardano

-- | Computes a minimum UTxO value with Cardano Ledger.
--
-- Caution:
--
-- This function does /not/ attempt to reach a fixed point before returning its
-- result.
computeMinimumCoinForUTxO_CardanoLedger
    :: MinimumUTxOForShelleyBasedEra
    -> TxOut
    -> Coin
computeMinimumCoinForUTxO_CardanoLedger
    (MinimumUTxOForShelleyBasedEra era pp)
    txOut =
        toWalletCoin $ case era of
            Cardano.ShelleyBasedEraShelley ->
                getMinCoinTxOut pp $ toShelleyTxOut txOut
            Cardano.ShelleyBasedEraAllegra ->
                getMinCoinTxOut pp $ toAllegraTxOut txOut
            Cardano.ShelleyBasedEraMary ->
                getMinCoinTxOut pp $ toMaryTxOut txOut
            Cardano.ShelleyBasedEraAlonzo ->
                getMinCoinTxOut pp $ toAlonzoTxOut txOut
            Cardano.ShelleyBasedEraBabbage ->
                getMinCoinTxOut pp $ toBabbageTxOut txOut
            Cardano.ShelleyBasedEraConway ->
                getMinCoinTxOut pp $ toConwayTxOut txOut
