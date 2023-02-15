{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw witnesses data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Withdrawals
    ( WithdrawalsType
    , Withdrawals (..)
    , getEraWithdrawals
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.TxBody
    ( wdrlsTxBodyL )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )

import qualified Cardano.Ledger.Shelley.API as SH

type family WithdrawalsType era where
  WithdrawalsType ByronEra = ()
  WithdrawalsType ShelleyEra = SH.Wdrl StandardCrypto
  WithdrawalsType AllegraEra = SH.Wdrl StandardCrypto
  WithdrawalsType MaryEra = SH.Wdrl StandardCrypto
  WithdrawalsType AlonzoEra = SH.Wdrl StandardCrypto
  WithdrawalsType BabbageEra = SH.Wdrl StandardCrypto

newtype Withdrawals era = Withdrawals (WithdrawalsType era)

deriving instance Show (WithdrawalsType era) => Show (Withdrawals era)
deriving instance Eq (WithdrawalsType era) => Eq (Withdrawals era)

getEraWithdrawals :: EraFun Tx Withdrawals
getEraWithdrawals = EraFun
    { byronFun = \_ -> Withdrawals ()
    , shelleyFun = onTx $ \tx -> Withdrawals (tx ^. bodyTxL . wdrlsTxBodyL)
    , allegraFun = onTx $ \tx -> Withdrawals (tx ^. bodyTxL . wdrlsTxBodyL)
    , maryFun = onTx $ \tx -> Withdrawals (tx ^. bodyTxL . wdrlsTxBodyL)
    , alonzoFun = onTx $ \tx -> Withdrawals (tx ^. bodyTxL . wdrlsTxBodyL)
    , babbageFun = onTx $ \tx -> Withdrawals (tx ^. bodyTxL . wdrlsTxBodyL)
    }
