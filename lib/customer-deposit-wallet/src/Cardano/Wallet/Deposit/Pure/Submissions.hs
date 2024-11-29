{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Wallet.Deposit.Pure.Submissions
    ( TxSubmissions
    , TxSubmissionsStatus
    , DeltaTxSubmissions1
    , DeltaTxSubmissions

    , empty
    , add
    , listInSubmission
    , rollForward
    , rollBackward
    ) where

import Prelude

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Cardano.Wallet.Submissions.Operations as Sbm
import qualified Cardano.Wallet.Submissions.Submissions as Sbm
import qualified Cardano.Wallet.Submissions.TxStatus as Sbm
import qualified Data.Delta as Delta
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type ExpirySlot = WithInfinity Read.Slot

type TxSubmissions
    = Sbm.Submissions () ExpirySlot (Read.TxId, Write.Tx)
type TxSubmissionsStatus
    = Sbm.TxStatusMeta () ExpirySlot(Read.TxId, Write.Tx)
type DeltaTxSubmissions1
    = Sbm.Operation () ExpirySlot (Read.TxId, Write.Tx)
type DeltaTxSubmissions
    = [DeltaTxSubmissions1]

instance Delta.Delta DeltaTxSubmissions1 where
    type Base DeltaTxSubmissions1 = TxSubmissions
    apply = Sbm.applyOperations

instance Sbm.HasTxId (Read.TxId, Write.Tx) where
   type TxId (Read.TxId, Write.Tx) = Read.TxId
   txId = fst

-- | Data type used for tracking transactions
-- that will never become invalid.
data WithInfinity a
    = Finite a
    | Infinity
    deriving (Eq, Show)

infinityFromNothing :: Maybe a -> WithInfinity a
infinityFromNothing Nothing = Infinity
infinityFromNothing (Just x) = Finite x

instance Ord a => Ord (WithInfinity a) where
    compare (Finite x) (Finite y) = compare x y
    compare Infinity (Finite _) = GT
    compare (Finite _) Infinity = LT
    compare Infinity Infinity = EQ

instance Functor WithInfinity where
    fmap f (Finite x) = Finite (f x)
    fmap _ Infinity = Infinity

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | Empty collection of transaction in submission.
empty :: TxSubmissions
empty = Sbm.mkEmpty (Finite Read.Origin)

-- | Add a /new/ transaction to the local submission pool.
add :: Write.Tx -> DeltaTxSubmissions
add tx = [ Sbm.AddSubmission expiry (txId, tx) () ]
  where
    txId = Read.getTxId tx
    expiry =
        fmap Read.At
        . infinityFromNothing
        . Read.invalidHereafter
        $ Read.getValidityInterval tx

-- | List of transactions that are in submission, in no particular order.
listInSubmission :: TxSubmissions -> [Write.Tx]
listInSubmission submissions = do
    Sbm.InSubmission _ (_, tx) <- Map.elems (Sbm.transactions submissions)
    pure tx

-- | Rollforward the transactions that are in submission
rollForward :: Read.IsEra era => Read.Block era -> DeltaTxSubmissions
rollForward block = [ Sbm.RollForward slot txids ]
  where
    slot = Finite $ Read.slotFromChainPoint $ Read.getChainPoint block
    txids = map ((slot,) . Read.getTxId) $ Read.getEraTransactions block

-- | Roll backward the transactions that are in submission
rollBackward :: Read.Slot -> DeltaTxSubmissions
rollBackward slot = [ Sbm.RollBack (Finite slot) ]
