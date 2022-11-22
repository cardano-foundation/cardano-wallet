{-# LANGUAGE FlexibleContexts #-}

module Cardano.Wallet.Submissions.Properties.Operations
    (properties)
    where

import Prelude

import Cardano.Wallet.Submissions.Operations
    ( Operation (..) )
import Cardano.Wallet.Submissions.Properties.Common
    ( Step (Step), forAllIn, that, verify, ( # ) )
import Cardano.Wallet.Submissions.Properties.Primitives
    ( txIds )
import Cardano.Wallet.Submissions.Submissions
    ( Submissions, finality, tip, transactions )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (TxId), TxStatus (Expired, InLedger, InSubmission), status )
import Test.QuickCheck
    ( Property, counterexample, property, (.&.) )

status' :: Ord (TxId tx) => TxId tx -> Submissions slot tx -> TxStatus slot tx
status' x = status x . transactions

-- | depending on the state we can promote a slot for every transaction in store
-- this observing slot is partitioning all transactions between finality and
-- tip
properties
    :: (Ord (TxId tx), Ord slot, Show (TxId tx))
    => Step Operation slot tx -> Property
properties (Step _ xs' _)
    = counterexample "submissions invariants" $ verify $ do
        that "finality precedes tip"
            $ property $ finality xs' <= tip xs'
        that "transactions are observed partitioned between finality and tip"
            $ let
                t = tip xs'
                f = finality xs'
                included w =  w > f .&. w <= t
            in forAllIn (txIds xs') $ \x -> case status' x xs' of
                InSubmission expiring _ -> expiring > t
                    # "any in-submission transaction should be observed\
                        \ after tip"
                InLedger _ acceptance _ -> included acceptance
                    # "any in ledger transaction should be observed\
                        \ included between finality and tip"
                Expired expiring _ -> included expiring
                    # "any expired transaction should be observed\
                        \ included between finality and tip"
                _ -> property True
