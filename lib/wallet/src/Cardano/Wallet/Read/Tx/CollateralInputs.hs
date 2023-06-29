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
-- Raw collateral inputs data extraction from 'Tx'
module Cardano.Wallet.Read.Tx.CollateralInputs
    ( CollateralInputsType
    , CollateralInputs (..)
    , getEraCollateralInputs
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
import Cardano.Ledger.Babbage.TxBody
    ( collateralInputsTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
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
import Data.Set
    ( Set
    )

import qualified Cardano.Ledger.Shelley.API as SH

type family CollateralInputsType era where
    CollateralInputsType ByronEra = ()
    CollateralInputsType ShelleyEra = ()
    CollateralInputsType AllegraEra = ()
    CollateralInputsType MaryEra = ()
    CollateralInputsType AlonzoEra = Set (SH.TxIn StandardCrypto)
    CollateralInputsType BabbageEra = Set (SH.TxIn StandardCrypto)
    CollateralInputsType ConwayEra = Set (SH.TxIn StandardCrypto)

newtype CollateralInputs era = CollateralInputs (CollateralInputsType era)

deriving instance
    (Show (CollateralInputsType era)) => Show (CollateralInputs era)
deriving instance (Eq (CollateralInputsType era)) => Eq (CollateralInputs era)

-- | Extract the collateral inputs from a 'Tx' in any era.
getEraCollateralInputs :: EraFun Tx CollateralInputs
getEraCollateralInputs =
    EraFun
        { byronFun = \_ -> CollateralInputs ()
        , shelleyFun = \_ -> CollateralInputs ()
        , allegraFun = \_ -> CollateralInputs ()
        , maryFun = \_ -> CollateralInputs ()
        , alonzoFun = mkCollateralInputs
        , babbageFun = mkCollateralInputs
        , conwayFun = mkCollateralInputs
        }
  where
    mkCollateralInputs = onTx $ \tx ->
        CollateralInputs
            $ tx ^. bodyTxL . collateralInputsTxBodyL
