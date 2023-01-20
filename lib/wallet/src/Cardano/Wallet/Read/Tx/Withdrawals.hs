{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import GHC.Records
    ( HasField (getField) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
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
    , shelleyFun = onTx $ \(SH.Tx b _ _ )  -> getWithdrawals b
    , allegraFun = onTx $ \(SH.Tx b _ _ )  -> getWithdrawals b
    , maryFun = onTx $ \(SH.Tx b _ _ ) -> getWithdrawals b
    , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) ->  getWithdrawals b
    , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getWithdrawals b
    }

getWithdrawals
    :: ( HasField "wdrls" a (WithdrawalsType b))
    => a -> Withdrawals b
getWithdrawals =  Withdrawals . getField @"wdrls"
