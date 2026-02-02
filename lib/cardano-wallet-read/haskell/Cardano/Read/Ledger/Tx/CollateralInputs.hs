{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw collateral inputs data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.CollateralInputs
    ( -- * Collateral input type
      CollateralInputsType
    , CollateralInputs (..)

      -- * Extraction
    , getEraCollateralInputs
    )
where

import Prelude

import Cardano.Ledger.Api
    ( collateralInputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.In
    ( TxIn
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    )
import Control.Lens
    ( (^.)
    )
import Data.Set
    ( Set
    )

-- |
-- Era-specific collateral inputs type.
--
-- Pre-Alonzo eras return unit @()@ as collateral is not supported.
-- Alonzo and later return a set of transaction inputs used as collateral.
type family CollateralInputsType era where
    CollateralInputsType Byron = ()
    CollateralInputsType Shelley = ()
    CollateralInputsType Allegra = ()
    CollateralInputsType Mary = ()
    CollateralInputsType Alonzo = Set TxIn
    CollateralInputsType Babbage = Set TxIn
    CollateralInputsType Conway = Set TxIn

-- | Era-indexed collateral inputs wrapper.
newtype CollateralInputs era = CollateralInputs (CollateralInputsType era)

deriving instance
    Show (CollateralInputsType era) => Show (CollateralInputs era)
deriving instance
    Eq (CollateralInputsType era) => Eq (CollateralInputs era)

{-# INLINEABLE getEraCollateralInputs #-}

-- | Extract the collateral inputs from a 'Tx' in any era.
getEraCollateralInputs
    :: forall era. IsEra era => Tx era -> CollateralInputs era
getEraCollateralInputs = case theEra @era of
    Byron -> \_ -> CollateralInputs ()
    Shelley -> \_ -> CollateralInputs ()
    Allegra -> \_ -> CollateralInputs ()
    Mary -> \_ -> CollateralInputs ()
    Alonzo -> mkCollateralInputs
    Babbage -> mkCollateralInputs
    Conway -> mkCollateralInputs
  where
    mkCollateralInputs = onTx $ \tx ->
        CollateralInputs
            $ tx ^. bodyTxL . collateralInputsTxBodyL
