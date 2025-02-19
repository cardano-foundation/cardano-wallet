module Cardano.Wallet.Deposit.Pure.State.Rolling
    ( rollForwardMany
    , rollForwardOne
    , rollBackward
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Pure.Balance
    ( ValueTransferMap
    )
import Cardano.Wallet.Deposit.Pure.State.Type
import Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory
    ( UTxOHistory
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , getEraSlotOfBlock
    )
import Cardano.Wallet.Deposit.Time
    ( LookupTimeFromSlot
    )
import Data.Foldable
    ( Foldable (..)
    , foldl'
    )
import Data.List.NonEmpty
    ( NonEmpty
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.API.TxHistory as TxHistory
import qualified Cardano.Wallet.Deposit.Pure.Balance as Balance
import qualified Cardano.Wallet.Deposit.Pure.RollbackWindow as Rollback
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.Delta as Delta

rollForwardMany
    :: LookupTimeFromSlot
    -> NonEmpty (Read.EraValue Read.Block)
    -> WalletState
    -> WalletState
rollForwardMany timeFromSlot blocks w =
    foldl' (flip $ rollForwardOne timeFromSlot) w blocks

rollForwardOne
    :: LookupTimeFromSlot
    -> Read.EraValue Read.Block
    -> WalletState
    -> WalletState
rollForwardOne timeFromSlot (Read.EraValue block) w =
    w
        { walletTip = Read.getChainPoint block
        , utxoHistory = utxoHistory'
        , submissions = Delta.apply (Sbm.rollForward block) (submissions w)
        , txHistory =
            TxHistory.rollForward
                valueTransfers
                (`addressToCustomer` w)
                timeFromSlot
                (getEraSlotOfBlock block)
                (txHistory w)
        }
  where
    (utxoHistory', valueTransfers) =
        rollForwardUTxO isOurs block (utxoHistory w)
    isOurs :: Address -> Bool
    isOurs = Address.isOurs (addresses w)

rollForwardUTxO
    :: Read.IsEra era
    => (Address -> Bool)
    -> Read.Block era
    -> UTxOHistory
    -> (UTxOHistory, ValueTransferMap)
rollForwardUTxO isOurs block u =
    (UTxOHistory.rollForward slot deltaUTxO u, valueTransfers)
  where
    (deltaUTxO, _, valueTransfers) =
        Balance.applyBlock isOurs block (UTxOHistory.getUTxO u)
    slot = Read.getEraSlotNo $ Read.getEraBHeader block

rollBackward
    :: LookupTimeFromSlot
    -> Read.ChainPoint
    -> WalletState
    -> (WalletState, Read.ChainPoint)
rollBackward timeFromSlot targetPoint w =
    ( w
        { walletTip = actualPoint
        , utxoHistory =
            UTxOHistory.rollBackward actualSlot (utxoHistory w)
        , submissions =
            Delta.apply (Sbm.rollBackward actualSlot) (submissions w)
        , txHistory =
            TxHistory.rollBackward timeFromSlot actualSlot (txHistory w)
        }
    , actualPoint
    )
  where
    h = utxoHistory w

    targetSlot = Read.slotFromChainPoint targetPoint
    actualSlot = Read.slotFromChainPoint actualPoint

    -- NOTE: We don't keep enough information about
    -- the block hashes to roll back to
    -- any other point than the target point (or genesis).
    actualPoint =
        if (targetSlot `Rollback.member` UTxOHistory.getRollbackWindow h)
            then -- FIXME: Add test for rollback window of `submissions`
                targetPoint
            else Read.GenesisPoint
