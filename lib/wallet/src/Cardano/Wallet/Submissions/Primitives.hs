{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Primitive operations over the Submissions store.

These operations are guaranteed to follow the specifications individually.
For store consistence use 'Operations' module where they are composed for
that goal.

-}

module Cardano.Wallet.Submissions.Primitives
    ( Primitive (..)
    , applyPrimitive
    )
where

import Prelude

import Cardano.Wallet.Submissions.Submissions
    ( Submissions
    , finality
    , finalityL
    , tip
    , tipL
    , transactions
    , transactionsL
    )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (..), TxStatus (Expired, InLedger, InSubmission) )
import Control.Lens
    ( over, (%~), (&), (.~) )
import Control.Monad
    ( guard )
import Data.Foldable
    ( Foldable (..) )
import Data.Maybe
    ( fromMaybe )

import qualified Data.Map.Strict as Map

-- | Primitive operations to change a 'Submissions' store.
data Primitive slot tx where
    -- | Insert tx new transaction in the local submission store.
    AddSubmission ::
        {_expiring :: slot, _transaction :: tx} ->
        Primitive slot tx
    -- | Change a transaction state to 'InLedger'.
    MoveToLedger ::
        {_acceptance :: slot, _transaction :: tx} ->
        Primitive slot tx
    -- | Move the submission store tip slot.
    MoveTip ::
        {_tip :: slot} ->
        Primitive slot tx
    -- | Move the submission store finality slot.
    MoveFinality ::
        {_finality :: slot} ->
        Primitive slot tx
    -- | Remove a transaction from tracking in the submissions store.
    Forget ::
        {_transaction :: tx} ->
        Primitive slot tx
    deriving (Show)

defaultToDoNothing :: (t -> Maybe t) -> t -> t
defaultToDoNothing f x = fromMaybe x $ f x

-- | Apply a 'Primitive' to a submission.
--
-- Not all primitives will eventually change the store, the specifications
-- should be used to define when a primitive is allowed to act.
-- When a primitive doesn't meet the specs, a no-op will be computed.
applyPrimitive
    :: forall slot tx
    .  (Ord slot, Ord (TxId tx), HasTxId tx)
    => Primitive slot tx
    -> Submissions slot tx
    -> Submissions slot tx
applyPrimitive (AddSubmission expiring tx) =
    defaultToDoNothing $ \s -> do
        guard $ expiring > tip s
        guard $ Map.notMember (txId tx) (transactions s)
        pure $
            s & transactionsL
                %~ Map.insert (txId tx) (InSubmission expiring tx)
applyPrimitive (MoveToLedger acceptance tx) = \s ->
    s & over transactionsL (Map.adjust (f s) (txId tx))
    where
        f s x@(InSubmission expiring tx')
            | acceptance > (tip s) && acceptance <= expiring =
                InLedger expiring acceptance tx'
            | otherwise = x
        f _ x = x
applyPrimitive (MoveTip newTip) =
    defaultToDoNothing $ \s -> do
        pure $ s
            & (finalityL .~ if newTip <= finality s then newTip else finality s)
                . (tipL .~ newTip)
                . (transactionsL %~ fmap f)
    where
        f :: TxStatus slot tx -> TxStatus slot tx
        f s@(InLedger expiring acceptance tx)
            | acceptance > newTip = InSubmission expiring tx
            | otherwise = s
        f s@(InSubmission expiring tx)
            | expiring <= newTip = Expired expiring tx
            | otherwise = s
        f s@(Expired expiring tx)
            | expiring > newTip = InSubmission expiring tx
            | otherwise = s
        f s = s
applyPrimitive (MoveFinality newFinality) =
    defaultToDoNothing $ \s -> do
        let finality'
              | newFinality >= tip s = tip s
              | newFinality <= finality s = finality s
              | otherwise = newFinality
        pure $ s
          & (finalityL .~ finality')
          . (transactionsL %~ g finality')
    where
        g finality' m = foldl' (flip $ Map.alter (>>= f)) m (Map.keys m)
          where
            f :: TxStatus slot tx -> Maybe (TxStatus slot tx)
            f s@(InLedger _expiring acceptance _tx)
                | acceptance <= finality' = Nothing
                | otherwise = Just s
            f s@(Expired expiring _tx)
                | expiring <= finality' = Nothing
                | otherwise = Just s
            f s = Just s
applyPrimitive (Forget tx) = transactionsL %~ Map.delete (txId tx)
