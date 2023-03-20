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

import Cardano.Ledger.Crypto
    ( StandardCrypto )
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
    ( Withdrawals (..) )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.Map.Strict
    ( Map )

import qualified Cardano.Ledger.Shelley.TxBody as SL
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
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

fromShelleyWdrl :: SL.Wdrl StandardCrypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl
