module Cardano.Wallet.Deposit.Pure.State.Submissions
    ( -- * Txs in submission queue management
      addTxSubmission
    , listTxsInSubmission

      -- * Balance considering pending transactions
    , availableBalance
    , availableUTxO
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Pure.Balance
    ( balance
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    )
import Cardano.Wallet.Deposit.Read
    ( UTxO
    )
import Cardano.Wallet.Read
    ( Value
    )

import qualified Cardano.Wallet.Deposit.Pure.Balance as Balance
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Delta as Delta

addTxSubmission :: Write.Tx -> WalletState -> WalletState
addTxSubmission tx w =
    w
        { submissions = Delta.apply (Sbm.add tx) (submissions w)
        }

listTxsInSubmission :: WalletState -> [Write.Tx]
listTxsInSubmission = Sbm.listInSubmission . submissions

-- | Compute the available balance from the current 'WalletState' considering
-- the pending transactions in the submission queue.
availableBalance :: WalletState -> Value
availableBalance = balance . availableUTxO

-- | Compute the available UTxO from the current 'WalletState' considering
-- the pending transactions in the submission queue.
availableUTxO :: WalletState -> UTxO
availableUTxO w =
    Balance.availableUTxO utxo pending
  where
    pending = listTxsInSubmission w
    utxo = UTxOHistory.getUTxO $ utxoHistory w
