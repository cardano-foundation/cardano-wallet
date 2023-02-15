{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Mint
    ( MintType
    , Mint (..)
    , getEraMint
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Alonzo.TxBody
    ( mintTxBodyL )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )

import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.Shelley.API as SH

type family MintType era where
  MintType ByronEra = ()
  MintType ShelleyEra = ()
  MintType AllegraEra = SH.Coin
  MintType MaryEra = Mary.MaryValue StandardCrypto
  MintType AlonzoEra = Mary.MaryValue StandardCrypto
  MintType BabbageEra = Mary.MaryValue StandardCrypto
  MintType ConwayEra = Mary.MaryValue StandardCrypto

newtype Mint era = Mint (MintType era)

deriving instance Show (MintType era) => Show (Mint era)
deriving instance Eq (MintType era) => Eq (Mint era)

getEraMint :: EraFun Tx Mint
getEraMint = EraFun
    { byronFun = \_ -> Mint ()
    , shelleyFun = \_ -> Mint ()
    , allegraFun = onTx $ \tx -> Mint $ tx ^. bodyTxL . mintTxBodyL
    , maryFun = onTx $ \tx -> Mint $ tx ^. bodyTxL . mintTxBodyL
    , alonzoFun = onTx $ \tx -> Mint $ tx ^. bodyTxL . mintTxBodyL
    , babbageFun = onTx $ \tx -> Mint $ tx ^. bodyTxL . mintTxBodyL
    , conwayFun = onTx $ \tx -> Mint $ tx ^. bodyTxL . mintTxBodyL
    }
