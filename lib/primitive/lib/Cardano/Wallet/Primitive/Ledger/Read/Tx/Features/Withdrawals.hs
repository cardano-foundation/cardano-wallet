{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( getWithdrawals
    , fromLedgerWithdrawals
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( Withdrawals (..)
    )
import Data.Map.Strict
    ( Map
    )

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates as Certificates
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Data.Map as Map

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
fromLedgerWithdrawals withdrawals = Map.fromList
    [ (Certificates.fromStakeCredential cred, Ledger.toWalletCoin coin)
    | (Ledger.RewardAcnt _network cred, coin) <- Map.toList withdrawals
    ]
