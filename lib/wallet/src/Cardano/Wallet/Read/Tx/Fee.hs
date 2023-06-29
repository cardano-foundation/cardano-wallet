{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw fee data extraction from 'Tx'
module Cardano.Wallet.Read.Tx.Fee
    ( FeeType
    , Fee (..)
    , getEraFee
    )
where

import Prelude

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , feeTxBodyL
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )

type family FeeType era where
    FeeType ByronEra = ()
    FeeType ShelleyEra = Coin
    FeeType AllegraEra = Coin
    FeeType MaryEra = Coin
    FeeType AlonzoEra = Coin
    FeeType BabbageEra = Coin
    FeeType ConwayEra = Coin

newtype Fee era = Fee (FeeType era)

deriving instance Show (FeeType era) => Show (Fee era)
deriving instance Eq (FeeType era) => Eq (Fee era)

-- | Extract fee from 'Tx' in all available eras.
getEraFee :: EraFun Tx Fee
getEraFee =
    EraFun
        { byronFun = onTx $ \_ -> Fee ()
        , shelleyFun = mkFee
        , allegraFun = mkFee
        , maryFun = mkFee
        , alonzoFun = mkFee
        , babbageFun = mkFee
        , conwayFun = mkFee
        }
  where
    mkFee = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
