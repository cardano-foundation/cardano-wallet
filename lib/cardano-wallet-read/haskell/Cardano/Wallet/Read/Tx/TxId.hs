{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- 'TxId' — unique identifier for a 'TxBody'.
module Cardano.Wallet.Read.Tx.TxId
    ( TxId
    , pattern TxId
    , EraIndependentTxBody
    , txIdFromHash
    , hashFromTxId
    , getTxId

      -- * Internal
    , fromLedgerTxId
    )
where

import Cardano.Ledger.Hashes
    ( EraIndependentTxBody
    )
import Cardano.Ledger.Shelley.API.ByronTranslation
    ( translateTxIdByronToShelley
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Hash
    ( Blake2b_256
    , Hash
    )
import Cardano.Wallet.Read.Tx.Tx
    ( Tx
    )
import Prelude

import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.TxIn qualified as SH.TxIn
import Cardano.Read.Ledger.Tx.TxId qualified as L

-- | Unique identifier for a transaction body,
-- obtained by hashing.
--
-- 'TxId' is an __era-independent__ concept:
-- The inputs of any transaction can refer to 'TxId's from previous eras.
-- Hence, 'TxId' does not have an @era@ type index.
--
-- Note: We use a type synonym here because we want zero-cost
-- coercion between @Set TxId@ and @Set (L.TxIdType Shelley)@.
-- Unfortunately, 'Set' expects a nominal role.
-- (See the design literature on 'Data.Coercible'.)
type TxId = SH.TxIn.TxId

{-# COMPLETE TxId #-}
pattern TxId :: Hash Blake2b_256 EraIndependentTxBody -> TxId
pattern TxId x <- (hashFromTxId -> x)
    where
        TxId x = txIdFromHash x

-- | Wrap hash of a transaction body as 'TxId'.
txIdFromHash
    :: Hash Blake2b_256 EraIndependentTxBody -> TxId
txIdFromHash = SH.TxIn.TxId . Hashes.unsafeMakeSafeHash

-- | Unwrap 'TxId' as hash of a transaction body.
hashFromTxId
    :: TxId -> Hash Blake2b_256 EraIndependentTxBody
hashFromTxId (SH.TxIn.TxId h) = Hashes.extractHash h

{-# INLINEABLE getTxId #-}

-- | Extract the 'TxId' of a transaction.
getTxId :: forall era. IsEra era => Tx era -> TxId
getTxId = fromLedgerTxId . L.getEraTxId

{-# INLINEABLE fromLedgerTxId #-}
fromLedgerTxId :: forall era. IsEra era => L.TxId era -> TxId
fromLedgerTxId = case theEra :: Era era of
    Byron -> translateTxIdByronToShelley . L.unTxId
    Shelley -> L.unTxId
    Allegra -> L.unTxId
    Mary -> L.unTxId
    Alonzo -> L.unTxId
    Babbage -> L.unTxId
    Conway -> L.unTxId
    Dijkstra -> L.unTxId
