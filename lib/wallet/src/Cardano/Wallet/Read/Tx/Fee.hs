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
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

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
        , shelleyFun = onTx $ \((SH.Tx b _ _)) -> getFee b
        , allegraFun = onTx $ \((SH.Tx b _ _)) -> getFee b
        , maryFun = onTx $ \(SH.Tx b _ _) -> getFee b
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getFee b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getFee b
        }

getFee
    :: ( HasField "txfee" a (FeeType b))
    => a -> Fee b
getFee =  Fee . getField @"txfee"
