{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Here we find the "business logic" to manage a Cardano wallet. This is a
-- direct implementation of the model from the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
-- Note that, this module is purposedly agnostic to how blocks are retrieved or
-- how various types are serialized.
--
-- This is really about how the wallet keep track of its internal state, and its
-- UTxO (where the relationship is defined via the 'IsOurs' abstraction to allow
-- this core code to be used with any sort of derivation scheme).
--
-- All those functions are pure and there's no reason to shove in any sort of
-- side-effects in here :)

module Cardano.Wallet.Primitive.Model
    (
      Wallet
    , initWallet
    , currentTip
    , getState
    , applyBlock
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , txOutsOurs
    , utxoFromTx
    , getTxMetas
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( txId )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , Direction (..)
    , Dom (..)
    , IsOurs (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , balance
    , excluding
    , invariant
    , restrictedBy
    , restrictedTo
    , txIns
    , updatePending
    )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad.Trans.State.Strict
    ( State, runState, state )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Traversable
    ( for )
import Numeric.Natural
    ( Natural )

import qualified Data.Map as Map
import qualified Data.Set as Set


-- | An opaque wallet type, see @initWallet@ and @applyBlock@ to construct and
-- update wallets.
--
-- Internally, this keeps track or a few things including:
--
--  - UTxOs
--  - Pending transaction
--  - Transaction history
--  - TODO: Known & used addresses
data Wallet s where
    Wallet
        :: (IsOurs s, NFData s, Show s)
        => UTxO -- Unspent tx outputs belonging to this wallet
        -> Set Tx -- Pending transactions
        -> [TxMeta] -- Transaction history
        -> SlotId -- Latest applied block (current tip)
        -> s -- Address derivation state
        -> Wallet s

deriving instance Show (Wallet s)
deriving instance Eq s => Eq (Wallet s)

instance NFData (Wallet s) where
    rnf (Wallet utxo pending txMeta sl s) =
        deepseq (rnf utxo) $
        deepseq (rnf pending) $
        deepseq (rnf txMeta) $
        deepseq (rnf sl) $
        deepseq (rnf s) ()

-- | Create an empty wallet from an initial state
initWallet
    :: (IsOurs s, NFData s, Show s)
    => s
    -> Wallet s
initWallet = Wallet mempty mempty mempty (SlotId 0 0)

-- | Apply Block is the only way to make the wallet evolve.
applyBlock
    :: Block
    -> NonEmpty (Wallet s)
    -> NonEmpty (Wallet s)
applyBlock !b (cp@(Wallet !utxo !pending !txMeta _ _) :| checkpoints) =
    let
        (ourUtxo, ourIns, s') = prefilterBlock b cp
        utxo' = (utxo <> ourUtxo) `excluding` ourIns
        pending' = updatePending b pending
        txMeta' = txMeta ++ blockTxMetas b utxo' s' -- fixme: list append O(n+m)
        cp' = Wallet utxo' pending' txMeta' (slotId (header b :: BlockHeader)) s'
    in
        -- NOTE
        -- k = 2160 is currently hard-coded here. In the short-long run, we do
        -- want to get that as an argument or, leave that decision to the caller
        -- though it is not trivial at all. If it shrinks, it's okay because we
        -- have enough checkpoints, but if it does increase, then we have
        -- problems in case of rollbacks.
        (cp' :| cp : take 2160 checkpoints)

-- | Get the wallet current tip
currentTip :: Wallet s -> SlotId
currentTip (Wallet _ _ _ tip _) = tip

-- | Get the wallet current state
getState :: Wallet s -> s
getState (Wallet _ _ _ _ s) = s

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: Wallet s -> Natural
availableBalance =
    balance . availableUTxO

-- | Total balance = 'balance' . 'totalUTxO'
totalBalance :: Wallet s -> Natural
totalBalance =
    balance . totalUTxO

-- | Available UTxO = UTxO that aren't part of pending txs
availableUTxO :: Wallet s -> UTxO
availableUTxO (Wallet utxo pending _ _ _) =
    utxo `excluding` txIns pending

-- | Total UTxO = 'availableUTxO' <> "pending UTxO"
totalUTxO :: Wallet s -> UTxO
totalUTxO wallet@(Wallet _ pending _ _ s) =
    let
        -- NOTE
        -- We _safely_ discard the state here because we aren't intending to
        -- discover any new addresses through this operation. In practice, we
        -- can only discover new addresses when applying blocks.
        discardState = fst
    in
        availableUTxO wallet <> discardState (changeUTxO pending s)

-- | Return all transaction outputs that are ours. This plays well within a
-- 'State' monad.
--
-- @
-- myFunction :: Block -> State s Result
-- myFunction b = do
--    ours <- state $ txOutsOurs (transaction b)
--    return $ someComputation ours
-- @
txOutsOurs
    :: forall s. (IsOurs s)
    => Set Tx
    -> s
    -> (Set TxOut, s)
txOutsOurs txs =
    runState $ Set.fromList <$> forMaybe (foldMap outputs txs) pick
  where
    pick :: TxOut -> State s (Maybe TxOut)
    pick out = do
        predicate <- state $ isOurs (address out)
        return $ if predicate then Just out else Nothing
    forMaybe :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
    forMaybe xs = fmap catMaybes . for xs

-- | Construct a UTxO corresponding to a given transaction. It is important for
-- the transaction outputs to be ordered correctly, since they become available
-- inputs for the subsequent blocks.
utxoFromTx :: Tx -> UTxO
utxoFromTx tx@(Tx _ outs) =
    UTxO $ Map.fromList $ zip (TxIn (txId tx) <$> [0..]) outs

-- | Get the transaction metadata for transactions associated with the wallet.
getTxMetas :: Wallet s -> [TxMeta]
getTxMetas (Wallet _ _ txs _ _) = txs

{-------------------------------------------------------------------------------
                               Internals
-------------------------------------------------------------------------------}

prefilterBlock
    :: Block
    -> Wallet s
    -> (UTxO, Set TxIn, s)
prefilterBlock b (Wallet !utxo _ _ _ !s) =
    let
        txs = transactions b
        (ourOuts, s') = txOutsOurs txs s
        ourUtxo = foldMap utxoFromTx txs `restrictedTo` ourOuts
        ourIns = txIns txs `Set.intersection` dom (utxo <> ourUtxo)
    in
        invariant "applyBlock requires: dom ourUtxo ∩ dom utxo = ∅"
            (ourUtxo, ourIns, s')
            (const $ Set.null $ dom ourUtxo `Set.intersection` dom utxo)

blockTxMetas
    :: forall s. (IsOurs s)
    => Block
    -> UTxO
    -> s
    -> [TxMeta]
blockTxMetas b utxo s = mapMaybe txMeta (Set.toList $ transactions b)
  where
    txMeta tx = staticMeta tx <$> isOurTx tx
    isOurTx tx | not (Set.null ourInputs) = Just Outgoing
               | not (null ourOutputs)    = Just Incoming
               | otherwise = Nothing
      where
        ourInputs = Set.fromList (inputs tx) `Set.intersection` dom utxo
        ourOutputs = filter (fst . flip isOurs s . address) (outputs tx)

    -- Some fields of TxMeta depend on current time and block height
    staticMeta tx dir = TxMeta (txId tx) (Quantity 0)
                        InLedger dir (slotId $ header b)

changeUTxO
    :: IsOurs s
    => Set Tx
    -> s
    -> (UTxO, s)
changeUTxO pending = runState $ do
    ours <- state $ txOutsOurs pending
    let utxo = foldMap utxoFromTx pending
    let ins = txIns pending
    return $ (utxo `restrictedTo` ours) `restrictedBy` ins
