{-# LANGUAGE BangPatterns #-}
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
    ( UTxO
    , balance
    , excluding
    )
import Cardano.Wallet.Primitive.Model
    ( utxoFromTx
    )
import Data.Foldable
    ( foldMap'
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.Deposit.Pure.DeltaUTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
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
