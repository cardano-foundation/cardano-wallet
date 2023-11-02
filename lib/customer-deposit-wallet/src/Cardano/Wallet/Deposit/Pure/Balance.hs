{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
-- | Wallet balance.
module Cardano.Wallet.Deposit.Pure.Balance
    ( balance
    , availableUTxO
    , applyBlock
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure.DeltaUTxO
    ( DeltaUTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO
    ( UTxO (..)
    , balance
    , excluding
    )
import Data.Foldable
    ( foldMap'
    , toList
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.Deposit.Pure.DeltaUTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Wallet Balance
------------------------------------------------------------------------------}
-- | Available = excluding pending transactions
availableUTxO :: UTxO -> Set Write.Tx -> UTxO
availableUTxO u pending =
    u `excluding` used
  where
    used :: Set Read.TxIn
    used = foldMap' getUsedTxIn pending

    -- UTxO which have been spent or committed as collateral in a pending
    -- transaction are not available to use in future transactions.
    getUsedTxIn :: Write.Tx -> Set Read.TxIn
    getUsedTxIn tx =
        Write.spendInputs (Write.txbody tx)
        <> Write.collInputs (Write.txbody tx)

{-----------------------------------------------------------------------------
    Applying Blocks
------------------------------------------------------------------------------}
type IsOurs addr = addr -> Bool

-- | Apply a 'Block' to the 'UTxO'.
--
-- Returns both a delta and the new value.
applyBlock
    :: IsOurs Read.Addr -> Read.Block -> UTxO -> (DeltaUTxO, UTxO)
applyBlock isOurs block u0 =
    (mconcat $ reverse dus, u1)
 where
    (dus, u1) = mapAccumL' (applyTx isOurs) u0 $ Read.transactions block

-- | Apply a transactions to the 'UTxO'.
--
-- Returns both a delta and the new value.
applyTx
    :: IsOurs Read.Addr -> Read.Tx -> UTxO -> (DeltaUTxO, UTxO)
applyTx isOurs tx u0 =
    if isUnchangedUTxO
        then (mempty, u0)
        else (du, u)
  where
    (du, u) = (du21 <> du10, u2)

    (du10, u1)   = spendTxD tx u0
    receivedUTxO = UTxO.filterByAddress isOurs (utxoFromTx tx)
    (du21, u2)   = UTxO.receiveD u1 receivedUTxO

    -- NOTE: Performance.
    -- 'applyTx' is part of a tight loop that inspects all transactions
    -- (> 30M Txs as of Feb 2022).
    -- Thus, we make a small performance optimization here.
    -- Specifically, we want to reject a transaction as soon as possible
    -- if it does not change the 'UTxO' set. The test
    isUnchangedUTxO = UTxO.null receivedUTxO && mempty == du10
    -- allocates slightly fewer new Set/Map than the definition
    --   isUnchangedUTxO =  mempty == du

{-----------------------------------------------------------------------------
    UTxO utilities
------------------------------------------------------------------------------}
-- | Remove unspent outputs that are consumed by the given transaction.
spendTxD :: Read.Tx -> UTxO -> (DeltaUTxO, UTxO)
spendTxD tx !u =
    u `UTxO.excludingD` Set.fromList inputsToExclude
  where
    inputsToExclude =
        if Read.txScriptInvalid tx
        then Read.collateralInputs tx
        else Read.inputs tx

-- | Generates a UTxO set from a transaction.
--
-- The generated UTxO set corresponds to the value provided by the transaction.
--
-- It is important for transaction outputs to be ordered correctly, as their
-- indices within this ordering will determine how they are referenced as
-- transaction inputs in subsequent blocks.
--
-- Assuming the transaction is not marked as having an invalid script, the
-- following properties should hold:
--
-- prop> balance (utxoFromTx tx) == foldMap tokens (outputs tx)
-- prop> size    (utxoFromTx tx) == length         (outputs tx)
-- prop> toList  (utxoFromTx tx) == toList         (outputs tx)
--
-- However, if the transaction is marked as having an invalid script, then the
-- following properties should hold:
--
-- prop> balance (utxoFromTx tx) == foldMap tokens (collateralOutput tx)
-- prop> size    (utxoFromTx tx) == length         (collateralOutput tx)
-- prop> toList  (utxoFromTx tx) == toList         (collateralOutput tx)
--
utxoFromTx :: Read.Tx -> UTxO
utxoFromTx tx =
    if Read.txScriptInvalid tx
    then utxoFromTxCollateralOutputs tx
    else utxoFromTxOutputs tx

-- | Generates a UTxO set from the ordinary outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromTxOutputs :: Read.Tx -> UTxO
utxoFromTxOutputs tx =
    UTxO
        $ Map.fromList
        $ zip (Read.TxIn txid <$> [0..])
        $ Read.outputs tx
  where
    txid = Read.toTxId tx

-- | Generates a UTxO set from the collateral outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromTxCollateralOutputs :: Read.Tx -> UTxO
utxoFromTxCollateralOutputs tx =
    UTxO
        $ Map.fromList
        $ toList
        $ (Read.TxIn txid index,) <$> Read.collateralOutput tx
  where
    txid = Read.toTxId tx

    -- To reference a collateral output within transaction t, we specify an
    -- output index that is equal to the number of ordinary outputs within t.
    --
    -- See definition of function "collOuts" within "Formal Specification of
    -- the Cardano Ledger for the Babbage era".
    --
    -- https://hydra.iohk.io/build/14336206/download/1/babbage-changes.pdf
    --
    index = fromIntegral (length $ Read.outputs tx)

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
-- | Strict variant of 'mapAccumL'.
mapAccumL' :: (a -> s -> (o,s)) -> s -> [a] -> ([o],s)
mapAccumL' f = go []
  where
    go os !s0 []     = (reverse os, s0)
    go os !s0 (x:xs) = case f x s0 of
        (!o,!s1) -> go (o:os) s1 xs
