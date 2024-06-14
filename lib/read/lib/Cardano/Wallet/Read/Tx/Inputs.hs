{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2020-2024 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Tx.Inputs
    ( getInputs
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( Byron
    , Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    , fromLedgerTxId
    )
import Cardano.Wallet.Read.Tx.TxIn
    ( TxIn
    , pattern TxIn
    , pattern TxIx
    )
import Data.List.NonEmpty
    ( toList
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Read.Ledger.Tx.Inputs as L
import qualified Cardano.Read.Ledger.Tx.TxId as L
import qualified Data.Set as Set

{-# INLINABLE getInputs #-}
-- | Extract the inputs from a transaction in any era.
getInputs :: forall era . IsEra era => Tx era -> Set TxIn
getInputs = case theEra :: Era era of
    Byron -> byronInputs . L.getEraInputs
    Shelley -> unInputs . L.getEraInputs
    Allegra -> unInputs . L.getEraInputs
    Mary -> unInputs . L.getEraInputs
    Alonzo -> unInputs . L.getEraInputs
    Babbage -> unInputs . L.getEraInputs
    Conway -> unInputs . L.getEraInputs

{-# INLINE byronInputs #-}
byronInputs :: L.Inputs Byron -> Set TxIn
byronInputs (L.Inputs x)= Set.fromList . map fromByronTxIn $ toList x

unInputs :: L.Inputs era -> L.InputsType era
unInputs (L.Inputs x) = x

fromByronTxIn :: BY.TxIn -> TxIn
fromByronTxIn (BY.TxInUtxo txid ix) =
    TxIn (fromByronTxId $ L.TxId txid) (TxIx ix)

fromByronTxId :: L.TxId Byron -> TxId
fromByronTxId = fromLedgerTxId
