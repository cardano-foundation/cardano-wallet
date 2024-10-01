{-# LANGUAGE BangPatterns #-}
-- | Wallet balance.
module Cardano.Wallet.Deposit.Pure.Balance
    ( balance
    , availableUTxO
    , IsOurs
    , applyBlock
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO
    ( DeltaUTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO.Tx
    ( IsOurs
    , applyTx
    )
import Cardano.Wallet.Deposit.Pure.UTxO.UTxO
    ( UTxO
    , balance
    , excluding
    )
import Data.Foldable
    ( foldMap'
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO as DeltaUTxO
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write

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
-- | Apply a 'Block' to the 'UTxO'.
--
-- Returns both a delta and the new value.
applyBlock
    :: Read.IsEra era
    => IsOurs Read.Address -> Read.Block era -> UTxO -> (DeltaUTxO, UTxO)
applyBlock isOurs block u0 =
    (DeltaUTxO.concat $ reverse dus, u1)
  where
    (dus, u1) =
        mapAccumL' (applyTx isOurs) u0
            $ Read.getEraTransactions block

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
