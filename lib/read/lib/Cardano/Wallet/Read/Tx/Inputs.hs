{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw inputs data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Inputs
    ( InputsType
    , Inputs (..)
    , getEraInputs
    )
    where

import Prelude

import Cardano.Ledger.Core
    ( bodyTxL
    , inputsTxBodyL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Eras
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
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
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

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Ledger.Shelley.API as SH

type family InputsType era where
    InputsType Byron = NonEmpty BY.TxIn
    InputsType Shelley = Set (SH.TxIn StandardCrypto)
    InputsType Allegra = Set (SH.TxIn StandardCrypto)
    InputsType Mary = Set (SH.TxIn StandardCrypto)
    InputsType Alonzo = Set (SH.TxIn StandardCrypto)
    InputsType Babbage = Set (SH.TxIn StandardCrypto)
    InputsType Conway = Set (SH.TxIn StandardCrypto)

newtype Inputs era = Inputs (InputsType era)

deriving instance Show (InputsType era) => Show (Inputs era)
deriving instance Eq (InputsType era) => Eq (Inputs era)

{-# INLINABLE getEraInputs #-}
-- | Extract the inputs from a transaction in any era.
getEraInputs :: forall era . IsEra era => Tx era -> Inputs era
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
