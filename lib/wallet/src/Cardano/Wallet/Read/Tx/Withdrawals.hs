{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw withdrawals data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Withdrawals
    ( WithdrawalsType
    , Withdrawals (..)
    , getEraWithdrawals
    , RewardWithdrawals
    , fromLedgerWithdrawals
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra,
    ShelleyEra )
import Cardano.Ledger.Address
    ( RewardAcnt, unWithdrawals )
import Cardano.Ledger.Coin
    ( Coin )
import Cardano.Ledger.Core
    ( bodyTxL, withdrawalsTxBodyL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( view )
import Data.Map
    ( Map )

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as Wallet
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as Wallet
import qualified Cardano.Wallet.Read.Primitive.Coin as Coin
import qualified Cardano.Wallet.Read.Primitive.Tx.Features.Certificates as Certificates
import qualified Cardano.Wallet.Read.Primitive.Tx.Features.Fee as Fee
import qualified Data.Map.Strict as Map

type family WithdrawalsType era where
  WithdrawalsType ByronEra = ()
  WithdrawalsType ShelleyEra = RewardWithdrawals
  WithdrawalsType AllegraEra = RewardWithdrawals
  WithdrawalsType MaryEra = RewardWithdrawals
  WithdrawalsType AlonzoEra = RewardWithdrawals
  WithdrawalsType BabbageEra = RewardWithdrawals
  WithdrawalsType ConwayEra = RewardWithdrawals

type RewardWithdrawals = Map (RewardAcnt StandardCrypto) Coin

newtype Withdrawals era
    = Withdrawals { withdrawalsAsMap :: WithdrawalsType era }

deriving instance Show (WithdrawalsType era) => Show (Withdrawals era)
deriving instance Eq (WithdrawalsType era) => Eq (Withdrawals era)

-- | Extract withdrawals from tx for any available era.
getEraWithdrawals :: EraFun Tx Withdrawals
getEraWithdrawals =
    EraFun
        { byronFun = \_ -> Withdrawals ()
        , shelleyFun = withdrawals
        , allegraFun = withdrawals
        , maryFun = withdrawals
        , alonzoFun = withdrawals
        , babbageFun = withdrawals
        , conwayFun = withdrawals
        }
  where
    withdrawals = onTx $
        Withdrawals . unWithdrawals . view (bodyTxL . withdrawalsTxBodyL)
