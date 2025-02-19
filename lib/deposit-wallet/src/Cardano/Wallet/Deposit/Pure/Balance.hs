{-# LANGUAGE BangPatterns #-}

-- | Wallet balance.
module Cardano.Wallet.Deposit.Pure.Balance
    ( balance
    , availableUTxO
    , IsOurs
    , applyBlock
    , ValueTransferMap
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO
    ( DeltaUTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO.Tx
    ( IsOurs
    , applyTx
    , valueTransferFromDeltaUTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO.UTxO
    ( UTxO
    , balance
    , excluding
    )
import Cardano.Wallet.Deposit.Pure.UTxO.ValueTransfer
    ( ValueTransfer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , TxId
    )
import Cardano.Wallet.Read
    ( Block
    , IsEra
    , getTxId
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO as DeltaUTxO
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Cardano.Wallet.Read as Read
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Wallet Balance
------------------------------------------------------------------------------}

-- | Available = excluding pending transactions
availableUTxO :: UTxO -> [Write.Tx] -> UTxO
availableUTxO u pending =
    u `excluding` used
  where
    used :: Set Read.TxIn
    used = foldMap getUsedTxIn pending

    -- UTxO which have been spent or committed as collateral in a pending
    -- transaction are not available to use in future transactions.
    getUsedTxIn :: Read.Tx Read.Conway -> Set Read.TxIn
    getUsedTxIn tx =
        Read.getInputs tx
            <> Read.getCollateralInputs tx

{-----------------------------------------------------------------------------
    Applying Blocks
------------------------------------------------------------------------------}

-- | Get the value transfer of a 'DeltaUTxO'.
getDeltaUTxOValueTransfer
    :: UTxO
    -> DeltaUTxO
    -> TxId
    -> ValueTransferMap
getDeltaUTxOValueTransfer u du txId = fold $ do
    (addr, value) <- Map.assocs $ valueTransferFromDeltaUTxO u du
    pure
        $ MonoidalMap.singleton addr
        $ MonoidalMap.singleton
            txId
            value

-- | A summary of all value transfers in a block.
type ValueTransferMap =
    MonoidalMap Address (MonoidalMap TxId ValueTransfer)

-- | Apply a 'Block' to the 'UTxO'.
--
-- Returns both a delta and the new value.
applyBlock
    :: IsEra era
    => IsOurs Read.CompactAddr
    -> Block era
    -> UTxO
    -> (DeltaUTxO, UTxO, ValueTransferMap)
applyBlock isOurs block u0 =
    (DeltaUTxO.appends $ reverse dus, u1, totalValueTransfer)
  where
    (dus, (u1, totalValueTransfer)) =
        mapAccumL' applyTx' (u0, mempty)
            $ Read.getEraTransactions block
    applyTx' tx (u, total) =
        let
            (ds, u') = applyTx isOurs tx u
            value = getDeltaUTxOValueTransfer u ds (getTxId tx)
            total'
                | null value = total
                | otherwise = total <> value
        in
            (ds, (u', total'))

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}

-- | Strict variant of 'mapAccumL'.
mapAccumL' :: (a -> s -> (o, s)) -> s -> [a] -> ([o], s)
mapAccumL' f = go []
  where
    go os !s0 [] = (reverse os, s0)
    go os !s0 (x : xs) = case f x s0 of
        (!o, !s1) -> go (o : os) s1 xs
