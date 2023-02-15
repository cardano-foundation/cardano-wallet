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

module Cardano.Wallet.Read.Tx.CollateralInputs
    ( CollateralInputsType
    , CollateralInputs (..)
    , getEraCollateralInputs
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Babbage.TxBody
    ( collateralInputsTxBodyL )
import Cardano.Ledger.Core
    ( TxBody )
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
import Data.Set
    ( Set )

import qualified Cardano.Ledger.Alonzo as AL
import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Babbage as Bab
import qualified Cardano.Ledger.Shelley.API as SH

type family CollateralInputsType era where
    CollateralInputsType ByronEra = ()
    CollateralInputsType ShelleyEra = ()
    CollateralInputsType AllegraEra = ()
    CollateralInputsType MaryEra = ()
    CollateralInputsType AlonzoEra = Set (SH.TxIn StandardCrypto)
    CollateralInputsType BabbageEra = Set (SH.TxIn StandardCrypto)

newtype CollateralInputs era = CollateralInputs (CollateralInputsType era)

deriving instance Show (CollateralInputsType era) => Show (CollateralInputs era)
deriving instance Eq (CollateralInputsType era) => Eq (CollateralInputs era)

getEraCollateralInputs :: EraFun Tx CollateralInputs
getEraCollateralInputs = EraFun
    { byronFun =
        \_ -> CollateralInputs ()
    , shelleyFun =
        \_ -> CollateralInputs ()
    , allegraFun =
        \_ -> CollateralInputs ()
    , maryFun =
        \_ -> CollateralInputs ()
    , alonzoFun =
        onTx $ \(AL.AlonzoTx b _ _ _) -> getAlonzoCollateralInputs b
    , babbageFun =
        onTx $ \(AL.AlonzoTx b _ _ _) -> getBabbageCollateralInputs b
    }

getAlonzoCollateralInputs
    :: TxBody (AL.AlonzoEra StandardCrypto)
    -> CollateralInputs AlonzoEra
getAlonzoCollateralInputs txBody =
    CollateralInputs (txBody ^. collateralInputsTxBodyL)

getBabbageCollateralInputs
    :: TxBody (Bab.BabbageEra StandardCrypto)
    -> CollateralInputs BabbageEra
getBabbageCollateralInputs txBody =
    CollateralInputs (txBody ^. collateralInputsTxBodyL)
