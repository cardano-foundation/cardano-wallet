{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    ( Era (..)
    , IsEra (..)
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

getWithdrawals :: forall era. IsEra era => Withdrawals era -> Maybe (Map RewardAccount Coin)
getWithdrawals = case theEra @era of
    Byron -> \_withdrawals -> Nothing
    Shelley -> eraFromWithdrawals
    Allegra -> eraFromWithdrawals
    Mary -> eraFromWithdrawals
    Alonzo -> eraFromWithdrawals
    Babbage -> eraFromWithdrawals
    Conway -> eraFromWithdrawals

  where
    eraFromWithdrawals (Withdrawals withdrawals) =
        Just $ fromLedgerWithdrawals withdrawals

fromLedgerWithdrawals
    :: (Map (Ledger.RewardAcnt crypto) Ledger.Coin) -> Map RewardAccount W.Coin
fromLedgerWithdrawals withdrawals = Map.fromList
    [ (Certificates.fromStakeCredential cred, Ledger.toWalletCoin coin)
    | (Ledger.RewardAcnt _network cred, coin) <- Map.toList withdrawals
    ]
