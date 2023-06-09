{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( getWithdrawals
    , fromRewardWithdrawals
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( fromStakeCredential )
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( RewardWithdrawals, Withdrawals (..) )
import Data.Map.Strict
    ( Map )

import qualified Cardano.Ledger.Api as Ledger
import qualified Data.Map.Strict as Map

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
        K . Just $ fromRewardWithdrawals withdrawals

fromRewardWithdrawals :: RewardWithdrawals -> Map RewardAccount Coin
fromRewardWithdrawals withdrawals = Map.fromList
    [ (fromStakeCredential cred, fromShelleyCoin coin)
    | (Ledger.RewardAcnt _network cred, coin) <- Map.toList withdrawals
    ]
