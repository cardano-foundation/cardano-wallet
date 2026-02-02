{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw collateral output data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.CollateralOutputs
    ( -- * Collateral output type
      CollateralOutputsType
    , CollateralOutputs (..)

      -- * Extraction
    , getEraCollateralOutputs
    )
where

import Prelude

import Cardano.Ledger.Babbage.Collateral
    ()
import Cardano.Ledger.Babbage.Rules
    ()
import Cardano.Ledger.Babbage.Tx
    ()
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxOut (..)
    , collateralReturnTxBodyL
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
import Data.Maybe.Strict
    ( StrictMaybe
    )

-- |
-- Era-specific collateral return output type.
--
-- Pre-Babbage eras return unit @()@ as collateral return is not supported.
-- Babbage and later return an optional output for collateral change.
type family CollateralOutputsType era where
    CollateralOutputsType Byron = ()
    CollateralOutputsType Shelley = ()
    CollateralOutputsType Allegra = ()
    CollateralOutputsType Mary = ()
    CollateralOutputsType Alonzo = ()
    CollateralOutputsType Babbage =
        StrictMaybe (BabbageTxOut Babbage)
    CollateralOutputsType Conway =
        StrictMaybe (BabbageTxOut Conway)

-- | Era-indexed collateral return output wrapper.
newtype CollateralOutputs era = CollateralOutputs (CollateralOutputsType era)

deriving instance
    Show (CollateralOutputsType era)
    => Show (CollateralOutputs era)
deriving instance
    Eq (CollateralOutputsType era) => Eq (CollateralOutputs era)

{-# INLINEABLE getEraCollateralOutputs #-}

-- | Get the 'CollateralOutputs' for a given 'Tx' in any era.
getEraCollateralOutputs
    :: forall era. IsEra era => Tx era -> CollateralOutputs era
getEraCollateralOutputs = case theEra @era of
    Byron -> \_ -> CollateralOutputs ()
    Shelley -> \_ -> CollateralOutputs ()
    Allegra -> \_ -> CollateralOutputs ()
    Mary -> \_ -> CollateralOutputs ()
    Alonzo -> \_ -> CollateralOutputs ()
    Babbage -> mkCollateralOutputs
    Conway -> mkCollateralOutputs
  where
    mkCollateralOutputs = onTx $ \tx ->
        CollateralOutputs
            $ tx ^. bodyTxL . collateralReturnTxBodyL
