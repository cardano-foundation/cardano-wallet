{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra,
    ShelleyEra )
import Cardano.Ledger.Babbage.TxBody
    ( referenceInputsTxBodyL )
import Cardano.Ledger.Core
    ( bodyTxL )
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

import qualified Cardano.Ledger.Shelley.API as SH

type family ReferenceInputsType era where
    ReferenceInputsType ByronEra = ()
    ReferenceInputsType ShelleyEra = ()
    ReferenceInputsType AllegraEra = ()
    ReferenceInputsType MaryEra = ()
    ReferenceInputsType AlonzoEra = ()
    ReferenceInputsType BabbageEra = Set (SH.TxIn StandardCrypto)
    ReferenceInputsType ConwayEra = Set (SH.TxIn StandardCrypto)

newtype ReferenceInputs era = ReferenceInputs (ReferenceInputsType era)

deriving instance Show (ReferenceInputsType era) => Show (ReferenceInputs era)
deriving instance Eq (ReferenceInputsType era) => Eq (ReferenceInputs era)

getEraReferenceInputs :: EraFun Tx ReferenceInputs
getEraReferenceInputs =
    EraFun
        { byronFun = \_ -> ReferenceInputs ()
        , shelleyFun = \_ -> ReferenceInputs ()
        , allegraFun = \_ -> ReferenceInputs ()
        , maryFun = \_ -> ReferenceInputs ()
        , alonzoFun = \_ -> ReferenceInputs ()
        , babbageFun = referenceInputsBabbage
        , conwayFun = referenceInputsBabbage
        }
  where
    referenceInputsBabbage = onTx $ \tx ->
        ReferenceInputs $ tx ^. bodyTxL . referenceInputsTxBodyL
