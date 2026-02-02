{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Copyright: © 2020-2024 IOHK
License: Apache-2.0
-}
module Cardano.Read.Ledger.Tx.Inputs
    ( -- * Input types
      InputsType
    , Inputs (..)

      -- * Extraction
    , getEraInputs
    )
where

import Prelude

import Cardano.Chain.UTxO qualified as BY
import Cardano.Ledger.Core
    ( bodyTxL
    , inputsTxBodyL
    )
import Cardano.Ledger.Shelley.API qualified as SH
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
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Set
    ( Set
    )

-- |
-- Era-specific input collection type.
--
-- Byron uses a non-empty list while later eras use a set.
type family InputsType era where
    InputsType Byron = NonEmpty BY.TxIn
    InputsType Shelley = Set SH.TxIn
    InputsType Allegra = Set SH.TxIn
    InputsType Mary = Set SH.TxIn
    InputsType Alonzo = Set SH.TxIn
    InputsType Babbage = Set SH.TxIn
    InputsType Conway = Set SH.TxIn

-- | Era-indexed transaction inputs wrapper.
newtype Inputs era = Inputs (InputsType era)

deriving instance Show (InputsType era) => Show (Inputs era)
deriving instance Eq (InputsType era) => Eq (Inputs era)

{-# INLINEABLE getEraInputs #-}

-- | Extract the transaction inputs from a transaction in any era.
getEraInputs :: forall era. IsEra era => Tx era -> Inputs era
getEraInputs = case theEra @era of
    Byron -> onTx $ \tx -> Inputs $ BY.txInputs $ BY.taTx tx
    Shelley -> shelleyInputs
    Allegra -> shelleyInputs
    Mary -> shelleyInputs
    Alonzo -> shelleyInputs
    Babbage -> shelleyInputs
    Conway -> shelleyInputs
  where
    shelleyInputs = onTx $ \tx -> Inputs (tx ^. bodyTxL . inputsTxBodyL)
