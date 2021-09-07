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
    , applyTxToUTxO
    , utxoFromTx
    , spendTx

    -- * Accessors
    , currentTip
    , getState
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , changeUTxO
    , utxo
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DelegationCertificate (..)
    , dlgCertAccount
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), distance, sumCoins )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxStatus (..)
    , collateralInputs
    , failedScriptValidation
    , inputs
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( Dom (..), UTxO (..), balance, excluding, filterByAddressM )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( foldM )
import Control.Monad.Extra
    ( mapMaybeM )
import Control.Monad.Trans.State.Strict
    ( State, StateT, evalState, runState, state )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), indentF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Data.Foldable as F
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
-- The 'Wallet' is parameterized over a single type:
--
-- - @s@ is a /state/ used to keep track of known addresses. The business logic
--   doesn't know how to answer the question \"Is this address ours?\", so we
--   expect this state to be able to answer that for us.
--   Typically, this state will be an instance of the 'IsOurs' class,
--   e.g. @'IsOurs' s 'Address'@.
--
-- A few examples to make it concrete:
--
-- @
-- Wallet (RndState k n)
-- Wallet (SeqState n ShelleyKey)
-- @
data Wallet s = Wallet
    { -- | Unspent tx outputs belonging to this wallet
      utxo :: UTxO

      -- | Header of the latest applied block (current tip)
    , currentTip :: BlockHeader

      -- | Address discovery state
    , getState :: s
    } deriving (Generic, Eq, Show)

instance NFData s => NFData (Wallet s) where
    rnf (Wallet u sl s) =
        deepseq (rnf u) $
        deepseq (rnf sl) $
        deepseq (rnf s)
        ()

instance Buildable s => Buildable (Wallet s) where
    build (Wallet u tip s) = "Wallet s\n"
        <> indentF 4 ("Tip: " <> build tip)
        <> indentF 4 ("UTxO:\n" <> indentF 4 (build u))
        <> indentF 4 (build s)

{-------------------------------------------------------------------------------
                          Construction & Modification
-------------------------------------------------------------------------------}

-- | Create a an empty wallet and apply the given genesis block
--
-- The wallet tip will be set to the header of the applied genesis block.
initWallet
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
        -- ^ The genesis block
    -> s
        -- ^ Initial address discovery state
    -> ([(Tx, TxMeta)], Wallet s)
initWallet block s =
    let
        ((FilteredBlock _ txs, u), s') = prefilterBlock block mempty s
    in
        (txs, Wallet u (header block) s')

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
    -> Wallet s
unsafeInitWallet = Wallet

-- | Update the state of an existing Wallet model
updateState
    :: s
    -> Wallet s
    -> Wallet s
updateState s (Wallet u tip _) = Wallet u tip s

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
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
    -> Wallet s
    -> (FilteredBlock, Wallet s)
applyBlock !b (Wallet !u _ s) =
    (filteredBlock, Wallet u' (b ^. #header) s')
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
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => NonEmpty (Block)
    -> Wallet s
    -> NonEmpty (FilteredBlock, Wallet s)
applyBlocks (block0 :| blocks) cp =
    NE.scanl (flip applyBlock . snd) (applyBlock block0 cp) blocks

{-------------------------------------------------------------------------------
                                   Accessors
-------------------------------------------------------------------------------}

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: Set Tx -> Wallet s -> TokenBundle
availableBalance pending =
    balance . availableUTxO pending

-- | Total balance = 'balance' . 'totalUTxO' +? rewards
totalBalance
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Set Tx
    -> Coin
    -> Wallet s
    -> TokenBundle
totalBalance pending rewards wallet@(Wallet _ _ s) =
    balance (totalUTxO pending wallet) `TB.add` rewardsBalance
  where
    rewardsBalance
        | hasPendingWithdrawals pending = mempty
        | otherwise = TB.fromCoin rewards

    hasPendingWithdrawals =
        anyS (anyM (\acct _ -> isJust $ fst (isOurs acct s)) . withdrawals)
      where
        anyS predicate = not . Set.null . Set.filter predicate
        anyM predicate = not . Map.null . Map.filterWithKey predicate

-- | Available UTxO = @pending ⋪ utxo@
availableUTxO
    :: Set Tx
    -> Wallet s
    -> UTxO
availableUTxO pending (Wallet u _ _) =
    u `excluding` entriesToExclude
  where
    entriesToExclude :: Set TxIn
    entriesToExclude = F.foldMap' entriesToExcludeForTx pending

    entriesToExcludeForTx :: Tx -> Set TxIn
    entriesToExcludeForTx tx = Set.fromList $ fst <$> mconcat
        [ tx ^. #resolvedInputs
        , tx ^. #resolvedCollateral
        ]

-- | Computes the total UTxO set of a wallet.
--
-- This total UTxO set is a projection of how the wallet's UTxO set would look
-- if all pending transactions were applied successfully.
--
-- >>> totalUTxO pendingTxs wallet
-- >>>     = utxo wallet
-- >>>     − inputs pendingTxs
-- >>>     ∪ change pendingTxs
--
totalUTxO
    :: IsOurs s Address
    => Set Tx
    -> Wallet s
    -> UTxO
totalUTxO pending (Wallet u _ s) =
    (u `excluding` entriesToExclude)
        <> changeUTxO pending s
  where
    entriesToExclude :: Set TxIn
    entriesToExclude = F.foldMap' entriesToExcludeForTx pending

    -- Note that we must not exclude collateral inputs here, since the total
    -- UTxO set is indended to be a projection of how the UTxO set would look
    -- if all pending transactions are applied successfully: if a transaction
    -- is applied successfully, then its collateral inputs cannot be consumed.
    entriesToExcludeForTx :: Tx -> Set TxIn
    entriesToExcludeForTx tx = Set.fromList $ fst <$> tx ^. #resolvedInputs

-- | Applies a transaction to a UTxO, moving it from one state from another.
-- When applying a transaction to a UTxO:
--   1. We need to remove any unspents that have been spent in the transaction.
--   2. Add any unspents that we've received via the transaction.
--
-- We don't consider "ownership" here (is this address ours?), only "do we
-- know about this address" (i.e. is it present in our UTxO?).
--
-- balance (applyTxToUTxO tx u) = balance u
--                              + balance (utxoFromTx tx)
--                              - balance (u `restrictedBy` inputs tx)
-- unUTxO (applyTxToUTxO tx u) = unUTxO u
--     `Map.union` unUTxO (utxoFromTx tx)
--     `Map.difference` unUTxO (u `restrictedBy` inputs tx)
-- applyTxToUTxO tx u = spend tx u <> utxoFromTx tx
-- applyTxToUTxO tx u = spend tx (u <> utxoFromTx tx)
applyTxToUTxO
    :: Tx
    -> UTxO
    -> UTxO
applyTxToUTxO tx !u = spendTx tx u <> utxoFromTx tx

-- | Remove unspents that have been consumed by the given transaction.
--
-- spendTx tx u `isSubsetOf` u
-- balance (spendTx tx u) <= balance u
-- balance (spendTx tx u) = balance u - balance (u `restrictedBy` inputs tx)
-- spendTx tx u = u `excluding` inputs tx
-- spendTx tx (filterByAddress f u) = filterByAddress f (spendTx tx u)
-- spendTx tx (u <> utxoFromTx tx) = spendTx tx u <> utxoFromTx tx
spendTx :: Tx -> UTxO -> UTxO
spendTx tx !u =
    u `excluding` Set.fromList inputsToExclude
    where
        inputsToExclude =
            if failedScriptValidation (tx ^. #scriptValidity)
            then collateralInputs tx
            else inputs tx

-- | Construct a UTxO corresponding to a given transaction. It is important for
-- the transaction outputs to be ordered correctly, since they become available
-- inputs for the subsequent blocks.
--
-- balance (utxoFromTx tx) = foldMap tokens (outputs tx)
-- utxoFromTx tx `excluding` Set.fromList (inputs tx) = utxoFrom tx
utxoFromTx :: Tx -> UTxO
utxoFromTx Tx {txId, outputs, scriptValidity} =
    if failedScriptValidation scriptValidity
    then mempty
    else UTxO $ Map.fromList $ zip (TxIn txId <$> [0..]) outputs

isOurAddress
    :: forall s m
     . (Monad m, IsOurs s Address)
    => Address
    -> StateT s m Bool
isOurAddress = fmap isJust . state . isOurs

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
    :: (IsOurs s Address, IsOurs s RewardAccount)
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
        :: IsOurs s RewardAccount
        => DelegationCertificate
        -> State s (Maybe DelegationCertificate)
    ourDelegation cert =
        state (isOurs $ dlgCertAccount cert) <&> \case
            Nothing -> Nothing
            Just{}  -> Just cert
    ourWithdrawal
        :: IsOurs s RewardAccount
        => (RewardAccount, Coin)
        -> State s (Maybe (RewardAccount, Coin))
    ourWithdrawal (acct, amt) =
        state (isOurs acct) <&> \case
            Nothing -> Nothing
            Just{}  -> Just (acct, amt)
    mkTxMeta :: Coin -> Direction -> TxMeta
    mkTxMeta amount dir = TxMeta
        { status = InLedger
        , direction = dir
        , slotNo = b ^. #header . #slotNo
        , blockHeight = b ^. #header . #blockHeight
        , amount = amount
        , expiry = Nothing
        }
    applyTx
        :: (IsOurs s Address, IsOurs s RewardAccount)
        => ([(Tx, TxMeta)], UTxO)
        -> Tx
        -> State s ([(Tx, TxMeta)], UTxO)
    applyTx (!txs, !prevUTxO) tx = do
        -- The next UTxO state (apply a state transition) (e.g. remove
        -- transaction outputs we've spent).
        ourNextUTxO <-
            (spendTx tx prevUTxO <>)
            <$> filterByAddressM isOurAddress (utxoFromTx tx)

        ourWithdrawals <- Coin . sum . fmap (unCoin . snd) <$>
            mapMaybeM ourWithdrawal (Map.toList $ withdrawals tx)

        let received = balance (ourNextUTxO `excluding` dom prevUTxO)
        let spent =
                balance (prevUTxO `excluding` dom ourNextUTxO)
                `TB.add` TB.fromCoin ourWithdrawals

        (ownedAndKnownTxIns, ownedAndKnownTxOuts) <- do
            -- A new transaction expands the set of transaction inputs/outputs
            -- we know about, but not all those transaction inputs/outputs
            -- belong to us, so we filter any new inputs/outputs, presuming that
            -- the previous UTxO has already been filtered:
            ownedAndKnown <-
                (prevUTxO <>) <$> filterByAddressM isOurAddress (utxoFromTx tx)
            -- Also, the new transaction may spend some transaction
            -- inputs/outputs. But we don't want to apply that logic yet. If we
            -- do, any spent transaction input/output will be removed from our
            -- knowledge base.
            -- Therefore, because this is not technically an "Unspent TxO" set,
            -- let's just return the TxIns and TxOuts, as the type "UTxO" will
            -- create expectations which we explicitly aren't fulfilling:
            let m = unUTxO ownedAndKnown
            pure (Map.keys m, Map.elems m)

        -- A transaction has a known input if one of the transaction inputs
        -- matches a transaction input we know about.
        let hasKnownInput = not $ Set.disjoint
                (Set.fromList $ inputs tx)
                (Set.fromList ownedAndKnownTxIns)
        -- A transaction has a known output if one of the transaction outputs
        -- matches a transaction output we know about.
        let hasKnownOutput = not $ Set.disjoint
                (Set.fromList $ outputs tx)
                (Set.fromList ownedAndKnownTxOuts)
        let hasKnownWithdrawal = ourWithdrawals /= mempty

        -- NOTE 1: The only case where fees can be 'Nothing' is when dealing with
        -- a Byron transaction. In which case fees can actually be calculated as
        -- the delta between inputs and outputs.
        --
        -- NOTE 2: We do not have in practice the actual input amounts, yet we
        -- do make the assumption that if one input is ours, then all inputs are
        -- necessarily ours and therefore, known as part of our current UTxO.
        let actualFee direction = case (tx ^. #fee, direction) of
                (Just x, Outgoing) -> -- Shelley and beyond.
                    Just x

                (Nothing, Outgoing) -> -- Byron
                    let
                        totalOut = sumCoins (txOutCoin <$> outputs tx)

                        totalIn = TB.getCoin spent
                    in
                        Just $ distance totalIn totalOut

                (_, Incoming) ->
                    Nothing

        return $ if hasKnownOutput && not hasKnownInput then
            let dir = Incoming in
            ( ( tx { fee = actualFee dir }
              , mkTxMeta (TB.getCoin received) dir
              ) : txs
            , ourNextUTxO
            )
        else if hasKnownInput || hasKnownWithdrawal then
            let
                adaSpent = TB.getCoin spent
                adaReceived = TB.getCoin received
                dir = if adaSpent > adaReceived then Outgoing else Incoming
                amount = distance adaSpent adaReceived
            in
                ( (tx { fee = actualFee dir }, mkTxMeta amount dir) : txs
                , ourNextUTxO
                )
        else
            (txs, prevUTxO)

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
    mconcat
    <$> mapM (filterByAddressM isOurAddress . mkUTxOFromTx) (Set.toList pending)
    where
        -- Generate a UTxO from an transaction, assuming that it passes phase-2
        -- script validation. Crucially, utxoFromTx will exclude failed
        -- transactions, hence we define our own function.
        mkUTxOFromTx :: Tx -> UTxO
        mkUTxOFromTx Tx {txId, outputs} =
            UTxO $ Map.fromList $ zip (TxIn txId <$> [0..]) outputs
