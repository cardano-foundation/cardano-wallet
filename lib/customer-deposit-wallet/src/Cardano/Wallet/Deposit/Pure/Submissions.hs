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
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Cardano.Wallet.Submissions.Operations as Sbm
import qualified Cardano.Wallet.Submissions.Submissions as Sbm
import qualified Cardano.Wallet.Submissions.TxStatus as Sbm
import qualified Data.Delta as Delta

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type TxSubmissions
    = Sbm.Submissions () Read.SlotNo (Read.TxId, Write.Tx)
type TxSubmissionsStatus
    = Sbm.TxStatusMeta () Read.SlotNo (Read.TxId, Write.Tx)
type DeltaTxSubmissions1
    = Sbm.Operation () Read.SlotNo (Read.TxId, Write.Tx)
type DeltaTxSubmissions
    = [DeltaTxSubmissions1]

instance Delta.Delta DeltaTxSubmissions1 where
    type Base DeltaTxSubmissions1 = TxSubmissions
    apply = Sbm.applyOperations

instance Sbm.HasTxId (Read.TxId, Write.Tx) where
   type TxId (Read.TxId, Write.Tx) = Read.TxId
   txId = fst

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}

empty :: TxSubmissions
empty = Sbm.mkEmpty 0

-- | Add a /new/ transaction to the local submission pool
-- with the most recent submission slot.
add :: Write.Tx -> Read.SlotNo -> DeltaTxSubmissions
add = undefined

listInSubmission :: TxSubmissions -> Set Write.Tx
listInSubmission = undefined

rollForward :: Read.Block era -> DeltaTxSubmissions
rollForward block = [ Sbm.RollForward tip txs ]
  where
    tip = undefined block
    txids = undefined block
    txs = map (tip,) txids

rollBackward :: Read.Slot -> DeltaTxSubmissions
rollBackward = undefined
