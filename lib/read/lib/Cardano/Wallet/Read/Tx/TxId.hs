{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- 'TxId' — unique identifier for a 'TxBody'.
--

module Cardano.Wallet.Read.Tx.TxId
    ( TxId
    , pattern TxId
    , txIdFromHash
    , hashFromTxId
    , getTxId

    -- * Internal
    , fromLedgerTxId
    )
    where

import Prelude

import Cardano.Ledger.Hashes
    ( EraIndependentTxBody
    )
import Cardano.Wallet.Read
    ( Tx
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    , Shelley
    )
import Cardano.Wallet.Read.Hash
    ( Blake2b_256
    , Hash
    , hashFromBytes
    )
import Data.Maybe
    ( fromJust
    )

import qualified Cardano.Crypto.Hashing as BY
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.TxIn as SH.TxIn
import qualified Cardano.Read.Ledger.Tx.TxId as L

-- | A 'TxId' is a unique identifier for a transaction body.
-- It is obtained by hashing.
--
-- 'TxId' is an __era-independent__ concept:
-- The inputs of any transaction can refer to 'TxId's from previous eras.
-- Hence, 'TxId' does not have an @era@ type index.
--
-- Note: We use a type synonym here because we want zero-cost
-- coercion between @Set TxId@ and @Set L.TxIdType Shelley@.
-- Unfortunately, 'Set' expects a nominal role.
-- (See the design literature on 'Data.Coercible'.)
type TxId = L.TxIdType Shelley

{-# COMPLETE TxId #-}
pattern TxId :: Hash Blake2b_256 EraIndependentTxBody -> TxId
pattern TxId x <- (hashFromTxId -> x)
  where TxId x = txIdFromHash x

txIdFromHash
    :: Hash Blake2b_256 EraIndependentTxBody -> TxId
txIdFromHash = SH.TxIn.TxId . SafeHash.unsafeMakeSafeHash

hashFromTxId
    :: TxId -> Hash Blake2b_256 EraIndependentTxBody
hashFromTxId (SH.TxIn.TxId h) = SafeHash.extractHash h

{-# INLINEABLE getTxId #-}
-- | Extract the 'TxId' of a transaction.
getTxId :: forall era. IsEra era => Tx era -> TxId
getTxId = fromLedgerTxId . L.getEraTxId

{-# INLINEABLE fromLedgerTxId #-}
fromLedgerTxId :: forall era. IsEra era => L.TxId era -> TxId
fromLedgerTxId = case theEra :: Era era of
    Byron -> fromByronTxId . L.unTxId
    Shelley -> L.unTxId
    Allegra -> L.unTxId
    Mary -> L.unTxId
    Alonzo -> L.unTxId
    Babbage -> L.unTxId
    Conway -> L.unTxId

fromByronTxId :: BY.Hash a -> TxId
fromByronTxId =
    txIdFromHash . fromJust . hashFromBytes . BY.hashToBytes
