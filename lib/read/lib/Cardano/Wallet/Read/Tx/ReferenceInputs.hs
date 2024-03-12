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
-- Reference input data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.ReferenceInputs
    ( ReferenceInputsType
    , ReferenceInputs (..)
    , getEraReferenceInputs
    )
    where

import Prelude

import Cardano.Ledger.Babbage.TxBody
    ( referenceInputsTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
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
import Data.Set
    ( Set
    )

import qualified Cardano.Ledger.Shelley.API as SH

type family ReferenceInputsType era where
    ReferenceInputsType Byron = ()
    ReferenceInputsType Shelley = ()
    ReferenceInputsType Allegra = ()
    ReferenceInputsType Mary = ()
    ReferenceInputsType Alonzo = ()
    ReferenceInputsType Babbage = Set (SH.TxIn StandardCrypto)
    ReferenceInputsType Conway = Set (SH.TxIn StandardCrypto)

newtype ReferenceInputs era = ReferenceInputs (ReferenceInputsType era)

deriving instance Show (ReferenceInputsType era) => Show (ReferenceInputs era)
deriving instance Eq (ReferenceInputsType era) => Eq (ReferenceInputs era)

getEraReferenceInputs :: forall era. IsEra era => Tx era -> ReferenceInputs era
getEraReferenceInputs = case theEra @era of
    Byron -> \_ -> ReferenceInputs ()
    Shelley -> \_ -> ReferenceInputs ()
    Allegra -> \_ -> ReferenceInputs ()
    Mary -> \_ -> ReferenceInputs ()
    Alonzo -> \_ -> ReferenceInputs ()
    Babbage -> referenceInputsBabbage
    Conway -> referenceInputsBabbage
  where
    referenceInputsBabbage = onTx $ \tx ->
        ReferenceInputs $ tx ^. bodyTxL . referenceInputsTxBodyL
