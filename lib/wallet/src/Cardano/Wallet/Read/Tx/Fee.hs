{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw fee data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Fee
    ( FeeType
    , Fee (..)
    , getEraFee
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Coin
    ( Coin )
import Cardano.Ledger.Core
    ( bodyTxL, feeTxBodyL )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )

type family FeeType era where
    FeeType ByronEra = ()
    FeeType ShelleyEra = Coin
    FeeType AllegraEra = Coin
    FeeType MaryEra = Coin
    FeeType AlonzoEra = Coin
    FeeType BabbageEra = Coin

newtype Fee era = Fee (FeeType era)

deriving instance Show (FeeType era) => Show (Fee era)
deriving instance Eq (FeeType era) => Eq (Fee era)

getEraFee :: EraFun Tx Fee
getEraFee
    = EraFun
        { byronFun =  onTx $ \_ -> Fee ()
        , shelleyFun = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
        , allegraFun = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
        , maryFun = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
        , alonzoFun = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
        , babbageFun = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
        }
