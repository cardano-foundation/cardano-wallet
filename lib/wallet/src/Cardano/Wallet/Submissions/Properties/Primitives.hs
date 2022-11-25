{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Cardano.Wallet.Submissions.Properties.Primitives
    ( properties
    , txIds
    )
where

import Prelude

import Cardano.Wallet.Submissions.Primitives
    ( Primitive (..) )
import Cardano.Wallet.Submissions.Properties.Common
    ( Step (Step), forAllIn, that, verify, ( # ) )

import Cardano.Wallet.Submissions.Submissions
    ( Submissions, finality, tip, transactions )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (..)
    , TxStatus (Expired, InLedger, InSubmission, Unknown)
    , status
    )
import Data.Set
    ( Set, singleton )
import Test.QuickCheck
    ( Property, counterexample, (.&.), (===) )

import qualified Data.Map.Strict as Map

txIds :: Submissions slot tx -> Set (TxId tx)
txIds = Map.keysSet . transactions

-- | Translations of primitive properties from specifications.
properties
    :: (Ord (TxId tx), Eq tx, HasTxId tx
        , Ord slot, Show (TxId tx), Show slot, Show tx)
    => Step Primitive slot tx -> Property
properties (Step xs xs' (AddSubmission expiring x)) = do
    let world = txIds xs <> txIds xs' <> singleton (txId x)
    counterexample "on add-submission" $ verify $ do
        that "tip and finality are not changed"
            $ tip xs === tip xs'
            .&. finality xs === finality xs'
        that "finality should never change"
            $ finality xs === finality xs'
        that "changes transaction statuses" $ forAllIn world $ \y ->
            let old = status y $ transactions xs
                new = status y $ transactions xs'
            in case old of
                Unknown
                    | expiring > tip xs
                        && txId x == y
                    ->
                        new === InSubmission expiring x
                        # "required should go to in-submission if unknown"
                _ -> new === old
properties (Step xs xs' (MoveToLedger acceptance x)) = do
    let world = txIds xs <> txIds xs' <> singleton (txId x)
    counterexample "on move-to-ledger" $ verify $ do
        that "tip and finality are not changed"
            $ tip xs === tip xs'
            .&. finality xs === finality xs'
        that "changes transaction statuses" $ forAllIn world $ \y ->
            let old = status y $ transactions xs
                new = status y $ transactions xs'
            in case old of
                InSubmission expiring _
                    | acceptance > tip xs
                        && acceptance <= expiring
                        && txId x == y
                    ->
                        new === InLedger expiring acceptance x
                        #   "required should go in ledger if acceptance is\
                            \ after tip and before expiration"
                _ -> old === new
properties (Step xs xs' (MoveTip newTip)) = do
    let world = txIds xs <> txIds xs'
    counterexample "on move-tip" $ verify $ do
        that "changes the tip"
            $ tip xs' === newTip
        that "can change finality" $ if
                | newTip < finality xs -> finality xs' === newTip
                | newTip >= finality xs -> finality xs' === finality xs
        that "changes transaction statuses" $ forAllIn world $ \y ->
            let old = status y $ transactions xs
                new = status y $ transactions xs'
            in case old of
                InSubmission expiring tx
                    | expiring <= newTip ->
                        new === Expired expiring tx
                        # "in-submission should have expired"
                Expired expiring tx
                    | expiring > newTip->
                        new === InSubmission expiring tx
                        # "expired should be in-submission"
                InLedger expiring acceptance tx
                    | acceptance > newTip ->
                        new === InSubmission expiring tx
                        # "accepted should be in-submission"
                _ -> new === old
properties (Step xs xs' (MoveFinality newFinality)) = do
    let world = txIds xs <> txIds xs'
    counterexample "on move-tip" $ verify $ do
        that "tip do not change"
            $ tip xs' === tip xs
        that "finality changes" $ if
            | newFinality > finality xs && newFinality < tip xs
                -> finality xs' === newFinality
            | newFinality <= finality xs
                -> finality xs' === finality xs
            | newFinality >= tip xs'
                -> finality xs' === tip xs
        that "changes transaction statuses" $ forAllIn world $ \y ->
            let old = status y $ transactions xs
                new = status y $ transactions xs'
            in case old of
                InLedger _expiring acceptance _
                    | acceptance <= finality xs'->
                    new === Unknown
                    # "accepted should have been pruned"
                Expired expiring _
                    | expiring <= finality xs' ->
                    new === Unknown
                    # "expired should have been pruned"
                _ -> new === old
properties (Step xs xs' (Forget x)) = do
    let world = txIds xs <> txIds xs' <> singleton (txId x)
    counterexample "on move-tip" $ verify $ do
        that "tip shouldn't have changed" $ tip xs === tip xs'
        that "finality shouldn't have changed" $ finality xs === finality xs'
        that "changes transaction statuses" $ forAllIn world $ \y ->
            let old = status y $ transactions xs
                new = status y $ transactions xs'
            in case old of
                _ | txId x == y
                    ->
                        new === Unknown
                        # "transaction should have been removed"
                _ -> new === old
