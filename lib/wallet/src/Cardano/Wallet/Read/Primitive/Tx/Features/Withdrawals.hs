{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
  ( getWithdrawals
  , fromLedgerWithdrawals
  )
where

import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin
  )
import Cardano.Wallet.Primitive.Types.Coin qualified as W
import Cardano.Wallet.Primitive.Types.RewardAccount
  ( RewardAccount
  )
import Cardano.Wallet.Read.Eras
  ( EraFun (..)
  , K (..)
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates qualified as Certificates
import Cardano.Wallet.Read.Tx.Withdrawals
  ( Withdrawals (..)
  )
import Cardano.Wallet.Shelley.Compatibility.Ledger qualified as Ledger
import Data.Map qualified as Map
import Data.Map.Strict
  ( Map
  )
import Prelude

getWithdrawals :: EraFun Withdrawals (K (Maybe (Map RewardAccount Coin)))
getWithdrawals =
  EraFun
    { byronFun = \_withdrawals -> K Nothing
    , shelleyFun = eraFromWithdrawals
    , allegraFun = eraFromWithdrawals
    , maryFun = eraFromWithdrawals
    , alonzoFun = eraFromWithdrawals
    , babbageFun = eraFromWithdrawals
    , conwayFun = eraFromWithdrawals
    }
  where
    eraFromWithdrawals (Withdrawals withdrawals) =
      K . Just $ fromLedgerWithdrawals withdrawals

fromLedgerWithdrawals
  :: (Map (Ledger.RewardAcnt crypto) Ledger.Coin) -> Map RewardAccount W.Coin
fromLedgerWithdrawals withdrawals =
  Map.fromList
    [ (Certificates.fromStakeCredential cred, Ledger.toWalletCoin coin)
    | (Ledger.RewardAcnt _network cred, coin) <- Map.toList withdrawals
    ]
