{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( getWithdrawals
    , fromShelleyWdrl
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

import qualified Cardano.Ledger.Shelley.TxBody as SL
import qualified Data.Map.Strict as Map

getWithdrawals :: EraFun Withdrawals (K (Maybe (Map RewardAccount Coin)))
getWithdrawals = EraFun
    { byronFun = noWithdrawals
    , shelleyFun = yesWithdrawals
    , allegraFun = yesWithdrawals
    , maryFun = yesWithdrawals
    , alonzoFun = yesWithdrawals
    , babbageFun = yesWithdrawals
    , conwayFun = yesWithdrawals
    }
    where
        noWithdrawals = const $ K Nothing
        yesWithdrawals (Withdrawals ttl)
            = K . Just . fromShelleyWdrl $ ttl

fromShelleyWdrl :: RewardWithdrawals -> Map RewardAccount Coin
fromShelleyWdrl m = Map.fromList $ do
    -- TODO conway: Do we need the network value here?
    (SL.RewardAcnt _network cred, coin) <- Map.toList m
    pure (fromStakeCredential cred, fromShelleyCoin coin)
