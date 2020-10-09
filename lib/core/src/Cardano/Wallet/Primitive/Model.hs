{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
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
    , FilteredBlock (..)
    , initWallet
    , updateState
    , applyBlock
    , applyBlocks
    , unsafeInitWallet

    -- * Accessors
    , currentTip
    , getState
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , utxo
    , blockchainParameters
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , ChimericAccount (..)
    , Coin (..)
    , DelegationCertificate (..)
    , Direction (..)
    , Dom (..)
    , GenesisParameters (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , balance
    , distance
    , dlgCertAccount
    , excluding
    , inputs
    , restrictedBy
    , txIns
    )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( foldM, forM )
import Control.Monad.Extra
    ( mapMaybeM )
import Control.Monad.Trans.State.Strict
    ( State, evalState, runState, state )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), indentF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                     Type
-------------------------------------------------------------------------------}

-- | An opaque wallet type, see 'initWallet', 'updateState', 'applyBlock', and
-- 'applyBlocks' to construct and update wallets.
--
-- Internally, this keeps track or a few things including:
--
--  - UTxOs
--  - Transaction history
--  - Known & used addresses, via address discovery state
--  - Blockchain parameters
--
-- The 'Wallet' is parameterized over two types:
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
data Wallet s = Wallet
    { -- | Unspent tx outputs belonging to this wallet
      utxo :: UTxO

      -- | Header of the latest applied block (current tip)
    , currentTip :: BlockHeader

      -- | Address discovery state
    , getState :: s

      -- | Parameters may change over time via protocol updates, so we keep
      -- track of them as part of the wallet checkpoints.
    , blockchainParameters :: GenesisParameters
    } deriving (Generic, Eq, Show)

instance NFData s => NFData (Wallet s) where
    rnf (Wallet u sl s gp) =
        deepseq (rnf u) $
        deepseq (rnf sl) $
        deepseq (rnf s) $
        deepseq (rnf gp) ()

instance Buildable s => Buildable (Wallet s) where
    build (Wallet u tip s gp) = "Wallet s\n"
        <> indentF 4 ("Tip: " <> build tip)
        <> indentF 4 ("Parameters:\n" <> indentF 4 (build gp))
        <> indentF 4 ("UTxO: " <> build u)
        <> indentF 4 (build s)

{-------------------------------------------------------------------------------
                          Construction & Modification
-------------------------------------------------------------------------------}

-- | Create a an empty wallet and apply the given genesis block
--
-- The wallet tip will be set to the header of the applied genesis block.
initWallet
    :: (IsOurs s Address, IsOurs s ChimericAccount)
    => Block
        -- ^ The genesis block
    -> GenesisParameters
        -- ^ Initial blockchain parameters
    -> s
        -- ^ Initial address discovery state
    -> ([(Tx, TxMeta)], Wallet s)
initWallet block gp s =
    let
        ((FilteredBlock _ txs, u), s') = prefilterBlock block mempty s
    in
        (txs, Wallet u (header block) s' gp)

-- | Constructs a wallet from the exact given state. Using this function instead
-- of 'initWallet' and 'applyBlock' allows the wallet invariants to be
-- broken. Therefore it should only be used in the special case of loading
-- wallet checkpoints from the database (where it is assumed a valid wallet was
-- stored into the database).
unsafeInitWallet
    :: UTxO
       -- ^ Unspent tx outputs belonging to this wallet
    -> BlockHeader
    -- ^ Header of the latest applied block (current tip)
    -> s
    -- ^ Address discovery state
    -> GenesisParameters
    -- ^ Blockchain parameters
    -> Wallet s
unsafeInitWallet = Wallet

-- | Update the state of an existing Wallet model
updateState
    :: s
    -> Wallet s
    -> Wallet s
updateState s (Wallet u tip _ gp) = Wallet u tip s gp

-- | Represents the subset of data from a single block that are relevant to a
--   particular wallet, discovered when applying a block to that wallet.
data FilteredBlock = FilteredBlock
    { delegations :: ![DelegationCertificate]
        -- ^ Stake delegations made on behalf of the wallet, listed in order of
        -- discovery. If the list contains more than element, those that appear
        -- later in the list supercede those that appear earlier on.
    , transactions :: ![(Tx, TxMeta)]
        -- ^ The set of transactions that affect the wallet.
    } deriving (Generic, Show, Eq)

-- | Apply a single block to a wallet.
--
-- This is the primary way of making a wallet evolve.
--
-- Returns an updated wallet, as well as the set of data relevant to the wallet
-- that were discovered while applying the block.
--
applyBlock
    :: (IsOurs s Address, IsOurs s ChimericAccount)
    => Block
    -> Wallet s
    -> (FilteredBlock, Wallet s)
applyBlock !b (Wallet !u _ s gp) =
    (filteredBlock, Wallet u' (b ^. #header) s' gp)
  where
    ((filteredBlock, u'), s') = prefilterBlock b u s

-- | Apply multiple blocks in sequence to an existing wallet, returning a list
--   of intermediate wallet states.
--
-- Each intermediate wallet state is paired with the set of transactions that
-- belong to that state but not the previous state.
--
-- For an original wallet state __@w@__ and a list of blocks __@b@__ such that:
--
-- > b = [b1, b2, ..., bn]
--
-- Returns the following list of updates:
--
-- > [ (t b1, w + b1)
-- > , (t b2, w + b1 + b2)
-- > , ...
-- > , (t bn, w + b1 + b2 + ... + bn) ]
--
-- Where:
--
-- * __@(t bi)@__   is the set of transactions contained within block __@bi@__.
-- * __@(w + bi)@__ is the wallet state after applying block __@bi@__ to wallet
--   __@w@__.
--
applyBlocks
    :: (IsOurs s Address, IsOurs s ChimericAccount)
    => NonEmpty (Block)
    -> Wallet s
    -> NonEmpty (FilteredBlock, Wallet s)
applyBlocks (block0 :| blocks) cp =
    NE.scanl (flip applyBlock . snd) (applyBlock block0 cp) blocks

{-------------------------------------------------------------------------------
                                   Accessors
-------------------------------------------------------------------------------}

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: Set Tx -> Wallet s -> Natural
availableBalance pending =
    balance . availableUTxO pending

-- | Total balance = 'balance' . 'totalUTxO' +? rewards
totalBalance
    :: (IsOurs s Address, IsOurs s ChimericAccount)
    => Set Tx
    -> Quantity "lovelace" Natural
    -> Wallet s
    -> Natural
totalBalance pending (Quantity rewards) wallet@(Wallet _ _ s _) =
    balance (totalUTxO pending wallet) +
        if hasPendingWithdrawals pending
        then 0
        else rewards
  where
    hasPendingWithdrawals =
        anyS (anyM (\acct _ -> fst (isOurs acct s)) . withdrawals)
      where
        anyS predicate = not . Set.null . Set.filter predicate
        anyM predicate = not . Map.null . Map.filterWithKey predicate

-- | Available UTxO = @pending ⋪ utxo@
availableUTxO
    :: Set Tx
    -> Wallet s
    -> UTxO
availableUTxO pending (Wallet u _ _ _) =
    u  `excluding` txIns pending

-- | Total UTxO = 'availableUTxO' @<>@ 'changeUTxO'
totalUTxO
    :: IsOurs s Address
    => Set Tx
    -> Wallet s
    -> UTxO
totalUTxO pending wallet@(Wallet _ _ s _) =
    availableUTxO pending wallet <> changeUTxO pending s

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
-- known inputs and known outputs. However, we can't naively look at the domain
-- of the utxo constructed from all outputs that are ours (as the specification
-- would suggest) because some transactions may use outputs of a previous
-- transaction within the same block as an input. Therefore, looking solely at
-- the final 'dom (UTxO ⊳ oursOuts)', we would be missing all intermediate txs
-- that happen from _within_ the block itself.
--
-- As a consequence, we do have to traverse the block, and look at transactions
-- in order, starting from the known inputs that can be spent (from the previous
-- UTxO) and collect resolved tx outputs that are ours as we apply transactions.
prefilterBlock
    :: (IsOurs s Address, IsOurs s ChimericAccount)
    => Block
    -> UTxO
    -> s
    -> ((FilteredBlock, UTxO), s)
prefilterBlock b u0 = runState $ do
    delegations <- mapMaybeM ourDelegation (b ^. #delegations)
    (transactions, ourU) <- foldM applyTx (mempty, u0) (b ^. #transactions)
    return (FilteredBlock {delegations, transactions}, ourU)
  where
    ourDelegation
        :: IsOurs s ChimericAccount
        => DelegationCertificate
        -> State s (Maybe DelegationCertificate)
    ourDelegation cert =
        state (isOurs $ dlgCertAccount cert) <&> \case
            False -> Nothing
            True -> Just cert
    ourWithdrawal
        :: IsOurs s ChimericAccount
        => (ChimericAccount, Coin)
        -> State s (Maybe (ChimericAccount, Coin))
    ourWithdrawal (acct, amt) =
        state (isOurs acct) <&> \case
            False -> Nothing
            True  -> Just (acct, amt)
    mkTxMeta :: Natural -> Direction -> TxMeta
    mkTxMeta amt dir = TxMeta
        { status = InLedger
        , direction = dir
        , slotNo = b ^. #header . #slotNo
        , blockHeight = b ^. #header . #blockHeight
        , amount = Quantity amt
        , expiry = Nothing
        }
    applyTx
        :: (IsOurs s Address, IsOurs s ChimericAccount)
        => ([(Tx, TxMeta)], UTxO)
        -> Tx
        -> State s ([(Tx, TxMeta)], UTxO)
    applyTx (!txs, !u) tx = do
        ourU <- state $ utxoOurs tx
        let ourIns = Set.fromList (inputs tx) `Set.intersection` dom (u <> ourU)
        let u' = (u <> ourU) `excluding` ourIns
        ourWithdrawals <- fmap (fromIntegral . getCoin . snd) <$>
            mapMaybeM ourWithdrawal (Map.toList $ withdrawals tx)
        let received = balance ourU
        let spent = balance (u `restrictedBy` ourIns) + sum ourWithdrawals
        let hasKnownInput = ourIns /= mempty
        let hasKnownOutput = ourU /= mempty
        let hasKnownWithdrawal = ourWithdrawals /= mempty
        return $ if hasKnownOutput && not hasKnownInput then
            ( (tx, mkTxMeta received Incoming) : txs
            , u'
            )
        else if hasKnownInput || hasKnownWithdrawal then
            let dir = if spent > received then Outgoing else Incoming in
            ( (tx, mkTxMeta (distance spent received) dir) : txs
            , u'
            )
        else
            (txs, u)

-- | Get the change UTxO
--
-- NOTE
-- We _safely_ discard the state here because we aren't intending to
-- discover any new addresses through this operation. In practice, we
-- can only discover new addresses when applying blocks. The state is
-- therefore use in a read-only mode here.
changeUTxO
    :: IsOurs s Address
    => Set Tx
    -> s
    -> UTxO
changeUTxO pending = evalState $
    mconcat <$> mapM (state . utxoOurs) (Set.toList pending)

-- | Construct our _next_ UTxO (possible empty) from a transaction by selecting
-- outputs that are ours. It is important for the transaction outputs to be
-- ordered correctly, since they become available inputs for the subsequent
-- blocks.
utxoOurs
    :: IsOurs s Address
    => Tx
    -> s
    -> (UTxO, s)
utxoOurs tx = runState $ toUtxo <$> forM (zip [0..] (outputs tx)) filterOut
  where
    toUtxo = UTxO . Map.fromList . catMaybes
    filterOut (ix, out) = do
        predicate <- state $ isOurs $ address out
        return $ if predicate
            then Just (TxIn (txId tx) ix, out)
            else Nothing
