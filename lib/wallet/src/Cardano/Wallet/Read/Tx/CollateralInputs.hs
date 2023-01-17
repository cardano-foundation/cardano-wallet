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
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Data.Set
    ( Set )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
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
getEraCollateralInputs
    = EraFun
        { byronFun =  \_ -> CollateralInputs ()
        , shelleyFun = \_ -> CollateralInputs ()
        , allegraFun = \_ -> CollateralInputs ()
        , maryFun = \_ -> CollateralInputs ()
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getCollateralInputs b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getCollateralInputs b
        }

getCollateralInputs
    :: ( HasField "collateral" a (CollateralInputsType b))
    => a -> CollateralInputs b
getCollateralInputs =  CollateralInputs . getField @"collateral"
