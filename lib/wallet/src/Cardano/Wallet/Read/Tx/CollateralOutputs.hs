{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw certificate data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.CollateralOutputs
    ( CollateralOutputsType
    , CollateralOutputs (..)
    , getEraCollateralOutputs
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Babbage.Collateral
    ()
import Cardano.Ledger.Babbage.Rules
    ()
import Cardano.Ledger.Babbage.Tx
    ()
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxBody, BabbageTxOut (..), collateralReturnTxBodyL )
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
import Data.Maybe.Strict
    ( StrictMaybe )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Babbage as BA
type family CollateralOutputsType era where
    CollateralOutputsType ByronEra = ()
    CollateralOutputsType ShelleyEra = ()
    CollateralOutputsType AllegraEra = ()
    CollateralOutputsType MaryEra = ()
    CollateralOutputsType AlonzoEra =  ()
    CollateralOutputsType BabbageEra
        = StrictMaybe (BabbageTxOut (BA.BabbageEra StandardCrypto))

newtype CollateralOutputs era = CollateralOutputs (CollateralOutputsType era)

deriving instance Show (CollateralOutputsType era) => Show (CollateralOutputs era)
deriving instance Eq (CollateralOutputsType era) => Eq (CollateralOutputs era)

getEraCollateralOutputs :: EraFun Tx CollateralOutputs
getEraCollateralOutputs
    = EraFun
        { byronFun =  \_ -> CollateralOutputs ()
        , shelleyFun = \_ -> CollateralOutputs ()
        , allegraFun = \_ -> CollateralOutputs ()
        , maryFun = \_ -> CollateralOutputs ()
        , alonzoFun = \_ -> CollateralOutputs ()
        , babbageFun = onTx
            $ \(AL.AlonzoTx b _ _ _) -> getCollateralOutputs b
        }

getCollateralOutputs
    :: BabbageTxBody (BA.BabbageEra StandardCrypto)
    -> CollateralOutputs BabbageEra
getCollateralOutputs txBody =
    CollateralOutputs (txBody ^. collateralReturnTxBodyL)
