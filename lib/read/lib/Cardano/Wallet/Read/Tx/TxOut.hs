{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

'TxOut' — transaction output.
-}
module Cardano.Wallet.Read.Tx.TxOut
    ( TxOut
    , getCompactAddr
    , getValue
    , utxoFromEraTx
    )
    where

import Prelude

import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    , OutputType
    , getEraCompactAddr
    , getEraValue
    )
import Cardano.Read.Ledger.Tx.CollateralOutputs
    ( CollateralOutputs (..)
    , getEraCollateralOutputs
    )
import Cardano.Read.Ledger.Tx.Outputs
    ( Outputs (..)
    , getEraOutputs
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , EraValue (..)
    , IsEra (theEra)
    )
import Cardano.Wallet.Read.Tx.TxIn
    ( TxIn
    , pattern TxIn
    , pattern TxIx
    )
import Cardano.Wallet.Read.Address
    ( CompactAddr
    )
import Cardano.Wallet.Read.Value
    ( Value
    , fromMaryValue
    )
import Data.Foldable
    ( toList
    )
import Data.Word
    ( Word16
    )

import qualified Cardano.Read.Ledger.Address as Read
import qualified Cardano.Read.Ledger.Value as Read
import qualified Cardano.Wallet.Read.Tx.ScriptValidity as Read
import qualified Cardano.Wallet.Read.Tx.TxId as Read
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | A 'TxOut' is a transaction output from any era — past, present
-- or next one.
newtype TxOut = TxOutC (EraValue Output)

{-# INLINEABLE getCompactAddr #-}
-- | Get the address which controls who can spend the transaction output.
getCompactAddr :: TxOut -> CompactAddr
getCompactAddr (TxOutC (EraValue (txout :: Output era))) =
    case theEra :: Era era of
        Byron -> onAddress Read.fromByronCompactAddr txout
        Shelley -> onAddress id txout
        Allegra -> onAddress id txout
        Mary -> onAddress id txout
        Alonzo -> onAddress id txout
        Babbage -> onAddress id txout
        Conway -> onAddress id txout

-- Helper function for type inference.
onAddress :: IsEra era => (Read.CompactAddrType era -> t) -> Output era -> t
onAddress f x =
    case getEraCompactAddr x of
        Read.CompactAddr v -> f v

{-# INLINEABLE getValue #-}
-- | Get the monetary 'Value' in this transaction output.
getValue :: TxOut -> Value
getValue (TxOutC (EraValue (txout :: Output era))) =
    fromMaryValue $ case theEra :: Era era of
        Byron -> onValue Read.maryValueFromByronValue txout
        Shelley -> onValue Read.maryValueFromShelleyValue txout
        Allegra -> onValue Read.maryValueFromShelleyValue txout
        Mary -> onValue id txout
        Alonzo -> onValue id txout
        Babbage -> onValue id txout
        Conway -> onValue id txout

-- Helper function for type inference.
onValue :: IsEra era => (Read.ValueType era -> t) -> Output era -> t
onValue f x =
    case getEraValue x of
        Read.Value v -> f v

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}

{-# INLINEABLE utxoFromEraTx #-}
-- | Unspent transaction outputs (UTxO) created by the transaction.
utxoFromEraTx :: forall era. IsEra era => Tx era -> Map.Map TxIn TxOut
utxoFromEraTx tx =
    case Read.getScriptValidity tx of
        Read.IsValid True -> utxoFromEraTxCollateralOutputs tx
        Read.IsValid False -> utxoFromEraTxOutputs tx

{-# INLINEABLE utxoFromEraTxOutputs #-}
-- | UTxO corresponding to the ordinary outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromEraTxOutputs
    :: forall era. IsEra era => Tx era -> Map.Map TxIn TxOut
utxoFromEraTxOutputs tx =
    withFoldableOutputs toMap (getEraOutputs tx)
  where
    txid = Read.getTxId tx
    mkOutputInEra out = Output out :: Output era

    toMap
        :: forall t. Foldable t
        => t (OutputType era) -> Map.Map TxIn TxOut
    toMap =
        Map.fromList
        . zipWith (\ix -> mkTxInTxOutPair txid ix . mkOutputInEra) [0..]
        . toList

{-# INLINEABLE utxoFromEraTxCollateralOutputs #-}
-- | UTxO corresponding to the collateral outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromEraTxCollateralOutputs
    :: forall era. IsEra era => Tx era -> Map.Map TxIn TxOut
utxoFromEraTxCollateralOutputs tx =
    withFoldableCollateralOutputs singleton (getEraCollateralOutputs tx)
  where
    txid = Read.getTxId tx
    mkOutputInEra out = Output out :: Output era

    singleton
        :: forall t. Foldable t
        => t (OutputType era) -> Map.Map TxIn TxOut
    singleton =
        Map.fromList
        . map (mkTxInTxOutPair txid index . mkOutputInEra)
        . toList

    -- To reference a collateral output within transaction t, we specify an
    -- output index that is equal to the number of ordinary outputs within t.
    --
    -- See definition of function "collOuts" within "Formal Specification of
    -- the Cardano Ledger for the Babbage era".
    --
    -- https://github.com/IntersectMBO/cardano-ledger?tab=readme-ov-file
    --
    index :: Word16
    index = fromIntegral $
        withFoldableOutputs length (getEraOutputs tx)

-- Helper function: Create a pair @(TxIn, TxOut)@.
mkTxInTxOutPair
    :: forall era. IsEra era
    => Read.TxId -> Word16 -> Output era -> (TxIn, TxOut)
mkTxInTxOutPair txid ix out =
    ( TxIn txid (TxIx ix)
    , TxOutC (EraValue out)
    )

-- Helper function: Treat the 'Outputs' as a 'Foldable' container.
withFoldableOutputs
    :: forall era a. IsEra era
    => (forall t. Foldable t => t (OutputType era) -> a)
    -> Outputs era -> a
withFoldableOutputs f = case theEra :: Era era of
    Byron -> \(Outputs x) -> f x
    Shelley -> \(Outputs x) -> f x
    Allegra -> \(Outputs x) -> f x
    Mary -> \(Outputs x) -> f x
    Alonzo -> \(Outputs x) -> f x
    Babbage -> \(Outputs x) -> f x
    Conway -> \(Outputs x) -> f x

-- Helper function: Treat the 'CollateralOutputs' as a 'Foldable' container.
withFoldableCollateralOutputs
    :: forall era a. IsEra era
    => (forall t. Foldable t => t (OutputType era) -> a)
    -> CollateralOutputs era -> a
withFoldableCollateralOutputs f = case theEra :: Era era of
    Byron -> \(CollateralOutputs _) -> f []
    Shelley -> \(CollateralOutputs _) -> f []
    Allegra -> \(CollateralOutputs _) -> f []
    Mary -> \(CollateralOutputs _) -> f []
    Alonzo -> \(CollateralOutputs _) -> f []
    Babbage -> \(CollateralOutputs x) -> f x
    Conway -> \(CollateralOutputs x) -> f x
