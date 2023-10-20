{-# LANGUAGE FlexibleInstances #-}
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

import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Submissions.Operations as Sbm
import qualified Cardano.Wallet.Submissions.Submissions as Sbm
import qualified Cardano.Wallet.Submissions.TxStatus as Sbm
import qualified Data.Delta as Delta

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type TxSubmissions
    = Sbm.Submissions () Read.Slot (Read.TxId, Read.Tx)
type TxSubmissionsStatus
    = Sbm.TxStatusMeta () Read.Slot (Read.TxId, Read.Tx)
type DeltaTxSubmissions1
    = Sbm.Operation () Read.Slot (Read.TxId, Read.Tx)
type DeltaTxSubmissions
    = [DeltaTxSubmissions1]

instance Delta.Delta DeltaTxSubmissions1 where
    type Base DeltaTxSubmissions1 = TxSubmissions
    apply = Sbm.applyOperations

instance Sbm.HasTxId (Read.TxId, Read.Tx) where
   type TxId (Read.TxId, Read.Tx) = Read.TxId
   txId = fst

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}

empty :: TxSubmissions
empty = Sbm.mkEmpty 0

-- | Add a /new/ transaction to the local submission pool
-- with the most recent submission slot.
add :: Read.Tx -> Read.Slot -> DeltaTxSubmissions
add = undefined

listInSubmission :: TxSubmissions -> Set Read.Tx
listInSubmission = undefined

rollForward :: Read.Block -> DeltaTxSubmissions
rollForward block = [ Sbm.RollForward tip txs ]
  where
    tip = undefined block
    txids = undefined block
    txs = map (tip,) txids

rollBackward :: Read.Slot -> DeltaTxSubmissions
rollBackward = undefined
