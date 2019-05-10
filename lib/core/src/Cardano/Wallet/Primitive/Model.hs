{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
    -- * Type
      Wallet

    -- * Construction & Modification
    , initWallet
    , updateState
    , applyBlock
    , applyBlocks
    , newPending

    -- * Accessors
    , currentTip
    , getState
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , getPending
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , Direction (..)
    , Dom (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , balance
    , excluding
    , restrictedBy
    , txIns
    )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( foldM, forM )
import Control.Monad.Trans.State.Strict
    ( State, evalState, runState, state )
import Data.Foldable
    ( fold )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( foldl' )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )

import qualified Data.Map as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                     Type
-------------------------------------------------------------------------------}

-- | An opaque wallet type, see 'initWallet' and 'applyBlocks' to construct and
-- update wallets.
--
-- Internally, this keeps track or a few things including:
--
--  - UTxOs
--  - Pending transaction
--  - Transaction history
--  - TODO: Known & used addresses
--
-- The 'Wallet' is paremeterized over two types:
--
-- - @s@: A _state_ used to keep track of known addresses. The business logic
--   doesn't know how to answer the question 'Is this address ours?', so we
--   expect this state to be able to answer that for us.
--
-- - @t@: A target backend. This makes the wallet fairly agnostic to the type
--   of binary representation used by the underlying target network and it
--   allows us to re-use the same logic to provide a wallet backend for multiple
--   backends (for instance, Byron or Shelley) which may have divergence in
--   their binary formats. For the sake of this module, we only care about one
--   particular super-power, and its the ability to compute transaction id
--   (which is intrinsically linked to the transaction's binary format).
--
-- A few examples to make it concrete:
--
-- @
-- Wallet RndState Byron
-- Wallet SeqState Shelley
-- Wallet SeqState Bitcoin
-- @
data Wallet s t where
    Wallet :: (IsOurs s, NFData s, Show s, TxId t)
        => UTxO -- Unspent tx outputs belonging to this wallet
        -> Set Tx -- Pending outgoing transactions
        -> SlotId -- Latest applied block (current tip)
        -> s -- Address discovery state
        -> Wallet s t

deriving instance Show (Wallet s t)
deriving instance Eq s => Eq (Wallet s t)
instance NFData (Wallet s t) where
    rnf (Wallet utxo pending sl s) =
        deepseq (rnf utxo) $
        deepseq (rnf pending) $
        deepseq (rnf sl) $
        deepseq (rnf s) ()

{-------------------------------------------------------------------------------
                          Construction & Modification
-------------------------------------------------------------------------------}

-- | Create an empty wallet from an initial state
initWallet
    :: (IsOurs s, NFData s, Show s, TxId t)
    => s
    -> Wallet s t
initWallet = Wallet mempty mempty (SlotId 0 0)

-- | Update the state of an existing Wallet model
updateState
    :: (IsOurs s, NFData s, Show s)
    => s
    -> Wallet s t
    -> Wallet s t
updateState s (Wallet a b c _) = Wallet a b c s

-- | Apply Block is the only way to make the wallet evolve. It returns a new
-- updated wallet state, as well as the set of all our transaction discovered
-- while applying the block.
applyBlock
    :: forall s t. ()
    => Block
    -> Wallet s t
    -> (Map (Hash "Tx") (Tx, TxMeta), Wallet s t)
applyBlock !b (Wallet !utxo !pending _ s) =
    let
        -- Prefilter Block / Update UTxO
        ((txs, utxo'), s') = prefilterBlock (Proxy @t) b utxo s
        -- Update Pending
        newIns = txIns $ Set.fromList (map fst txs)
        pending' = pending `pendingExcluding` newIns
        -- Update Tx history
        txs' = Map.fromList $ map
            (\(tx, meta) -> (txId @t tx, (tx, meta)))
            txs
    in
        ( txs'
        , Wallet utxo' pending' (b ^. #header . #slotId) s'
        )

-- | Helper to apply multiple blocks in sequence to an existing wallet. It's
-- basically just a @foldl' applyBlock@ over the given blocks.
applyBlocks
    :: [Block]
    -> Wallet s t
    -> (Map (Hash "Tx") (Tx, TxMeta), Wallet s t)
applyBlocks blocks cp0 =
    foldl' applyBlock' (mempty, cp0) blocks
  where
    applyBlock' (txs, cp) b =
        let (txs', cp') = applyBlock b cp in (txs <> txs', cp')

newPending
    :: Tx
    -> Wallet s t
    -> Wallet s t
newPending !tx (Wallet !utxo !pending !tip !s) =
    Wallet utxo (Set.insert tx pending) tip s

{-------------------------------------------------------------------------------
                                   Accessors
-------------------------------------------------------------------------------}

-- | Get the wallet current tip
currentTip :: Wallet s t -> SlotId
currentTip (Wallet _ _ tip _) = tip

-- | Get the wallet current state
getState :: Wallet s t -> s
getState (Wallet _ _ _ s) = s

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: Wallet s t -> Natural
availableBalance =
    balance . availableUTxO

-- | Total balance = 'balance' . 'totalUTxO'
totalBalance :: Wallet s t -> Natural
totalBalance =
    balance . totalUTxO

-- | Available UTxO = @pending ⋪ utxo@
availableUTxO :: Wallet s t -> UTxO
availableUTxO (Wallet utxo pending _ _) =
    utxo `excluding` txIns pending

-- | Total UTxO = 'availableUTxO' @<>@ 'changeUTxO'
totalUTxO :: forall s t. Wallet s t -> UTxO
totalUTxO wallet@(Wallet _ pending _ s) =
    availableUTxO wallet <> changeUTxO (Proxy @t) pending s

-- | Get the set of pending transactions
getPending :: Wallet s t -> Set Tx
getPending (Wallet _ pending _ _) = pending

{-------------------------------------------------------------------------------
                               Internals
-------------------------------------------------------------------------------}

-- | Prefiltering returns all transactions of interest for the wallet. A
-- transaction is a matter of interest for the wallet if:
--
--    - It has known input(s)
--    - and/or It has known output(s)
--
-- In practice, most transactions that are of interest have an output to the
-- wallet but some may actually have no change output whatsoever and be only
-- linked to the wallet by their inputs.
--
-- In order to identify transactions that are ours, we do therefore look for
-- known inputs and known outputs. However, we can't naievely look at the domain
-- of the utxo constructed from all outputs that are ours (as the specification
-- would suggest) because some transactions may use outputs of a previous
-- transaction within the same block as an input. Therefore, Looking solely at
-- the final 'dom (UTxO ⊳ oursOuts)', we would be missing all intermediate txs
-- that happen from _within_ the block itself.
--
-- As a consequence, we do have to traverse the block, and look at transaction
-- in order, starting from the known inputs that can be spent (from the previous
-- UTxO) and, collect resolved tx outputs that are ours as we apply transactions.
prefilterBlock
    :: forall s t. (IsOurs s, TxId t)
    => Proxy t
    -> Block
    -> UTxO
    -> s
    -> (([(Tx, TxMeta)], UTxO), s)
prefilterBlock proxy b utxo0 = runState $ do
    (ourTxs, ourUtxo) <- foldM applyTx (mempty, utxo0) (transactions b)
    return (ourTxs, ourUtxo)
  where
    mkTxMeta :: Natural -> Direction -> TxMeta
    mkTxMeta amt dir = TxMeta
        { status = InLedger
        , direction = dir
        , slotId = b ^. #header . #slotId
        , amount = Quantity amt
        }
    applyTx
        :: ([(Tx, TxMeta)], UTxO)
        -> Tx
        -> State s ([(Tx, TxMeta)], UTxO)
    applyTx (!txs, !utxo) tx = do
        ourUtxo <- state $ utxoOurs proxy tx
        let ourIns = Set.fromList (inputs tx) `Set.intersection` dom (utxo <> ourUtxo)
        let utxo' = (utxo <> ourUtxo) `excluding` ourIns
        let received = fromIntegral @_ @Integer $ balance ourUtxo
        let spent = fromIntegral @_ @Integer $ balance (utxo `restrictedBy` ourIns)
        let amt = fromIntegral $ abs (received - spent)
        let hasKnownInput = ourIns /= mempty
        let hasKnownOutput = ourUtxo /= mempty
        return $ if hasKnownOutput && not hasKnownInput then
            ( (tx, mkTxMeta amt Incoming) : txs
            , utxo'
            )
        else if hasKnownInput then
            ( (tx, mkTxMeta amt Outgoing) : txs
            , utxo'
            )
        else
            (txs, utxo)

-- | Get the change UTxO
--
-- NOTE
-- We _safely_ discard the state here because we aren't intending to
-- discover any new addresses through this operation. In practice, we
-- can only discover new addresses when applying blocks. The state is
-- therefore use in a read-only mode here.
changeUTxO
    :: forall s t. (IsOurs s, TxId t)
    => Proxy t
    -> Set Tx
    -> s
    -> UTxO
changeUTxO proxy pending = evalState $ do
    ourUtxo <- mapM (state . utxoOurs proxy) (Set.toList pending)
    let ins = txIns pending
    return $ fold ourUtxo `restrictedBy` ins

-- | Construct our _next_ UTxO (possible empty) from a transaction by selecting
-- outputs that are ours. It is important for the transaction outputs to be
-- ordered correctly, since they become available inputs for the subsequent
-- blocks.
utxoOurs
    :: forall s t. (IsOurs s, TxId t)
    => Proxy t
    -> Tx
    -> s
    -> (UTxO, s)
utxoOurs _ tx = runState $ toUtxo <$> forM (zip [0..] (outputs tx)) filterOut
  where
    toUtxo = UTxO . Map.fromList . catMaybes
    filterOut (ix, out) = do
        predicate <- state $ isOurs $ address out
        return $ if predicate
            then Just (TxIn (txId @t tx) ix, out)
            else Nothing

-- | Remove transactions from the pending set if their inputs appear in the
-- given set.
pendingExcluding :: Set Tx -> Set TxIn -> Set Tx
pendingExcluding txs discovered =
    Set.filter isStillPending txs
  where
    isStillPending =
        Set.null . Set.intersection discovered . Set.fromList . inputs
